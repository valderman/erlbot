-module(erlbot).
%% Expose main and connect as parts of the interface to facilitate hot code
%% update.
-export([start/3, quakenet/0, reload/0, load/1, unload/1,
	 main/5, connect/4]).

quakenet() ->
    {"irc.quakenet.org", 6667}.


%% Sends a reload message and waits for confirmation.
reload() ->
    erlbot ! {reload, self()},
    receive X -> X end.

%% Attempts to load a module.
load(Module) ->
    erlbot ! {load, Module, self()},
    receive X -> X end.

%% Attempts to unload a module.
unload(Module) ->
    erlbot ! {unload, Module, self()},
    receive X -> X end.

%% Start the bot.
%% The first argument is the pair of {address, port} of the IRC service to
%%   connect to.
%% The second argument is the nickname to use for the bot, and the admin
%%   password, used to give the bot administrative commands.
%% The third argument is a list of handler modules for various events.
%%   A handler module must export the functions priv/2, chan/2, noise/2,
%%   init/2 and die/0.
%%   See dpress.erl for an example of what such a module might look like.
start(Server, Config, Handlers) ->
    case reload([irc, erlbot] ++ Handlers) of
	ok -> ok;
	_  -> erlang:error("Unable to load modules!")
    end,

    %% Spawn our main process and register it
    Pid = spawn(fun() -> connect(Server, Config, Handlers, 0) end),
    NeedToUnregister = lists:member(erlbot, registered()),
    if
	NeedToUnregister ->
	    unregister(erlbot);
	true ->
	    whatever
    end,
    register(erlbot, Pid),
    ok.

%% Connect to the given service.
%% If we already failed three times, wait ten minutes and retry.
connect(Server, Config, Handlers, 3) ->
    io:format("Failed three connection attempts, retrying in 10 mins.~n"),
    timer:send_after(10*60*1000, retry),
    connect(Server, Config, Handlers, await_timeout);

connect(Server, Config, Handlers, await_timeout) ->
    receive
	retry ->
	    connect(Server, Config, Handlers, 0);
	die ->
	    kill_self(no_socket, Handlers);
	{reload, From} ->
	    reload_all(Handlers, From),
	    erlbot:connect(Server, Config, Handlers, await_timeout)
    end;

connect({Addr, Port}, {Nick, AdminPass}, Handlers, FailedTries) ->
    %% Open connection to IRC service
    io:format("Connecting to ~s...~n", [Addr]),
    case gen_tcp:connect(Addr, Port, [binary]) of
	{ok, Socket} ->
	    io:format("OK! Initializing IRC connection...~n"),
	    %% Init IRC connection
	    irc:init(Socket, Nick),

	    io:format("OK! Initializing plugins...~n"),
	    %% Init plugins
	    lists:map(fun(M) ->
			      spawn(fun() -> M:init(Socket, Nick) end)
		      end, Handlers),
	    Reconnect = fun() ->
				connect({Addr, Port},
					{Nick, AdminPass},
					Handlers,
					0)
			end,
	    io:format("OK! Entering main loop.~n"),
	    main(Socket, AdminPass, Nick, Handlers, Reconnect);
	_ ->
	    io:format("Failed! Retrying...~n"),
	    connect({Addr, Port}, {Nick, AdminPass}, Handlers, FailedTries+1)
    end.


%% Reload a bunch of modules
reload([F|Files]) ->
    case compile:file(F) of
	{ok, _} ->
	    code:purge(F),
	    case code:load_file(F) of
		{module, _} ->
		    reload(Files);
		Err ->
		    Err
	    end;
	Error ->
	    Error
    end;
reload(_) ->
    ok.

%% Reload all running code, then notify whoever requested it.
reload_all(Handlers, From) ->
    io:format("Trying to reload...~n", []),
    case reload([irc, erlbot] ++ Handlers) of
	ok ->
	    From ! ok,
	    io:format("Reload succeeded!~n", []);
	Error ->
	    From ! Error,
	    io:format("Reload FAILED!~nReason: ~w~n", [Error])
    end.

%% Our main message loop
main(Sock, AdmPass, Nick, Handlers, Reconnect) ->
    %% Keep the loop call in a variable; less hassle that way.
    Loop = fun() ->
		   erlbot:main(Sock, AdmPass, Nick, Handlers, Reconnect)
	   end,

    receive
	%% The connection died; kill all plugins, then try to reconnect.
	{tcp_closed, _} ->
	    io:format("Connection lost; reconnecting!~n"),
	    lists:map(fun(M) ->
			      spawn(fun() -> M:die() end)
		      end, Handlers),	    
	    Reconnect();

	%% Reload all running code, then report error or success to the caller.
	{reload, From} ->
	    reload_all(Handlers, From),
	    Loop();

	%% Adds and starts a new plugin.
	{load, Plug, From} ->
	    io:format("Loading plugin ~w~n", [Plug]),
	    case lists:member(Plug, Handlers) of
		true ->
		    io:format("Plugin already loaded!", []),
		    From ! already_loaded,
		    Loop();
		_ ->
		    case reload([Plug]) of
			ok ->
			    Plug:init(Sock, Nick),
			    From ! ok,
			    main(Sock,
				 AdmPass,
				 Nick,
				 [Plug | Handlers],
				 Reconnect);
			Err ->
			    io:format("Loading plugin FAILED!~n", []),
			    io:format("Readon: ~w~n", [Err]),
			    From ! Err,
			    Loop()
		    end
	    end;

	%% Stops and unloads a plugin.
	{unload, Plug, From} ->
	    io:format("Unloading plugin ~w~n", [Plug]),
	    case lists:member(Plug, Handlers) of
		true ->
		    Plug:die(),
		    Hs = lists:delete(Plug, Handlers),
		    From ! ok,
		    main(Sock, AdmPass, Nick, Hs, Reconnect);
		_ ->
		    io:format("Plugin not loaded!~n", []),
		    From ! not_loaded,
		    Loop()
	    end;

	%% Restart all plugins.
	restart_plugins ->
	    io:format("Restarting all plugins...", []),
	    lists:map(fun(M) -> M:die() end, Handlers),
	    lists:map(fun(M) -> M:init(Sock, Nick) end, Handlers),
	    io:format("OK!~n", []),
	    Loop();

	%% Someone wants us dead; let's obey!
	die ->
	    kill_self(Sock, Handlers);

	%% Respond to PING messages
	{tcp, S, <<"PING ", Data/binary>>} ->
	    irc:pong(S, Data),
	    Loop();

	%% Unknown message; discard it.
	{tcp, _, Data} ->
	    lists:map(fun(Msg) ->
			      handle_message(Sock, AdmPass, Nick, Handlers, Msg)
		      end,
		      irc:lines(binary_to_list(Data))),
	    Loop()
    end.

%% Kill the bot.
kill_self(no_socket, Handlers) ->
    io:format("OK, dying...~n"),
    unregister(erlbot),
    lists:map(fun(M) -> M:die() end, Handlers);
kill_self(Sock, Handlers) ->
    irc:quit(Sock, "No particular reason."),
    gen_tcp:close(Sock),
    kill_self(no_socket, Handlers).

%% Handles a single IRC message.
handle_message(Sock, AdmPass, Nick, Handlers, Message) ->
    pong_if_necessary(Sock, Message),
    case irc:parse(Message, Nick) of
	{privmsg, Msg} ->
	    spawn(fun() -> priv_handler(Sock, AdmPass, Handlers, Msg) end);
	{chanmsg, Msg} ->
	    spawn(fun() -> chan_handler(Sock, Handlers, Msg) end);
	{noise,   Msg} ->
	    spawn(fun() -> noise_handler(Sock, Handlers, Msg) end);
	_ ->
	    whatever
    end.

%% Sends a PONG message if given a PING.
pong_if_necessary(Sock, Msg) ->
    case irc:words(Msg) of
	["PING" | Rest] ->
	    [_|Response] = lists:concat(Rest),
	    irc:pong(Sock, list_to_binary(Response));
	_ ->
	    whatever
    end.

%% Handle private (whispered) messages, but only if prefixed with the correct
%% password.
priv_handler(Sock, AdmPass, Handlers, Mess={From, _, Message}) ->
    case irc:words(Message) of
	[AdmPass | Cmd] ->
	    case Cmd of
		%% Command to join a channel.
		["join", Channel] ->
		    irc:join(Sock, Channel);
		%% Command to leave a channel.
		["part", Channel] ->
		    irc:part(Sock, Channel);
		%% Command to reload all running code.
		["reload"] ->
		    case reload() of
			ok ->
			    irc:privmsg(Sock, From, "Reloaded!");
			_ ->
			    ErrMsg = "Reload failed!",
			    irc:privmst(Sock, From, ErrMsg)
		    end;
		%% Command to kill the bot and go home.
		["die"] ->
		    erlbot ! die;
		_ ->
		    irc:privmsg(Sock, From, "No such command!")
	    end;
	_ ->
	    lists:map(fun(M) ->
			      spawn(fun() ->
					    M:priv(Sock, Mess)
				    end)
		      end, Handlers)
    end.

%% Handle messages directed to us in a channel.
chan_handler(Sock, Handlers, Msg) ->
    lists:map(fun(M) -> spawn(fun() -> M:chan(Sock, Msg) end) end, Handlers).

%% Handle messages directed to us in a channel.
noise_handler(Sock, Handlers, Msg) ->
    lists:map(fun(M) -> spawn(fun() -> M:noise(Sock, Msg) end) end, Handlers).
