-module(erlbot).
%% Expose main and connect as parts of the interface to facilitate hot code
%% update.
-export([start/3, quakenet/0, reload/0, load/1, unload/1,
	 main/5, connect/4]).

quakenet() ->
    {"irc.quakenet.org", 6667}.

%% Unload plugins that die more often than this many microseconds.
min_err_interval() ->
    1000000.

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
%% The second argument is the nickname to use for the bot, the admin password
%%   used to give the bot administrative commands, and the list of channels to
%%   connect to, in that order.
%% The third argument is a list of handler modules for various events.
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

connect({Addr, Port}, {Nick, AdminPass, Chans}, Handlers, FailedTries) ->
    %% Open connection to IRC service
    io:format("Connecting to ~s...~n", [Addr]),
    case gen_tcp:connect(Addr, Port, [binary]) of
	{ok, Socket} ->
	    io:format("OK! Initializing IRC connection...~n"),
	    %% Init IRC connection
	    irc:init(Socket, Nick),

	    io:format("OK! Initializing plugins...~n"),
	    %% Init plugins
	    lists:map(fun(M) -> watchdog(M, Socket, Nick) end, Handlers),
	    Reconnect = fun(Chs) ->
				connect({Addr, Port},
					{Nick, AdminPass, Chs},
					Handlers,
					0)
			end,
	    io:format("OK! Entering main loop.~n"),

	    %% Post rejoin messages to self, then enter main loop.
	    %% Messages are to be delivered after we can be reasonably sure
	    %% that the server will accept them; 10 seconds delay will do.
	    lists:map(fun(C) -> timer:send_after(10000, {join, C}) end, Chans),
	    main(Socket, AdminPass, Nick, Handlers, {Chans, Reconnect});
	_ ->
	    io:format("Failed! Retrying...~n"),
	    connect({Addr, Port}, {Nick, AdminPass, Chans},
		    Handlers, FailedTries+1)
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

%% Start a plugin with a watchdog process that restarts it if it fails.
watchdog(Plug, Sock, Nick) ->
    io:format("Watchdogging ~w...~n", [Plug]),
    spawn(fun() ->
        process_flag(trap_exit, true),
	Pid = Plug:start(Sock, Nick),
	link(Pid),
	register(Plug, Pid),
	watch(Plug, Sock, Nick, {0,0,0})
        end).

%% Watch over a process, restarting it if it dies.
watch(Plug, Sock, Nick, LastError) ->
    receive
	{'EXIT', _, normal} ->
	    bye_bye;
	{'EXIT', _, Why} ->
	    T = now(),
	    Dies_too_often = timer:now_diff(T, LastError) < min_err_interval(),
	    if
		%% If the plugin dies too often, something is probably
		%% permanently wrong, so we disable it.
		Dies_too_often ->
		    io:format("Plugin ~w dies too often; unloading.~n",
			      [Plug]),
		    erlbot:unload(Plug);
		%% Otherwise, reload it.
		true ->
		    io:format("Plugin ~w died because '~w'; restarting...~n",
			      [Plug, Why]),
		    unregister(Plug),
		    Pid = Plug:start(Sock, Nick),
		    link(Pid),
		    register(Plug, Pid),
		    watch(Plug, Sock, Nick, now())
	    end;
	{die, From} ->
	    Plug ! die,
	    receive ok -> ok end,
	    From ! ok
    end.

%% Our main message loop
main(Sock, AdmPass, Nick, Handlers, {Chs, Reconnect}) ->
    %% Keep the loop call in a variable; less hassle that way.
    Loop = fun() ->
		   erlbot:main(Sock, AdmPass, Nick, Handlers, {Chs, Reconnect})
	   end,

    receive
	%% The connection died; kill all plugins, then try to reconnect.
	{tcp_closed, _} ->
	    io:format("Connection lost; reconnecting!~n"),
	    lists:map(fun(M) -> M ! {die, self()} end, Handlers),	    
	    Reconnect(Chs);

	%% Reload all running code, then report error or success to the caller.
	{reload, From} ->
	    reload_all(Handlers, From),
	    Loop();

	%% Someone wants us to join a channel; let's do it!
	{join, Channel} ->
	    irc:join(Sock, Channel),
	    main(Sock, AdmPass, Nick, Handlers, {[Channel | Chs], Reconnect});

	%% Parting from a channel
	{part, Channel} ->
	    irc:part(Sock, Channel),
	    main(Sock, AdmPass, Nick, Handlers,
		 {lists:delete(Channel, Chs), Reconnect});

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
			    watchdog(Plug, Sock, Nick),
			    From ! ok,
			    main(Sock,
				 AdmPass,
				 Nick,
				 [Plug | Handlers],
				 {Chs, Reconnect});
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
		    Plug ! {die, self()},
		    Hs = lists:delete(Plug, Handlers),
		    From ! ok,
		    main(Sock, AdmPass, Nick, Hs, {Chs, Reconnect});
		_ ->
		    io:format("Plugin not loaded!~n", []),
		    From ! not_loaded,
		    Loop()
	    end;

	%% Restart all plugins.
	restart_plugins ->
	    io:format("Restarting all plugins...", []),
	    lists:map(fun(M) -> M ! {die, self()} end, Handlers),
	    lists:map(fun(M) -> watchdog(M, Sock, Nick) end, Handlers),
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
    lists:map(fun(M) -> M ! {die, self()} end, Handlers);
kill_self(Sock, Handlers) ->
    irc:quit(Sock, "No particular reason."),
    gen_tcp:shutdown(Sock, read_write),
    gen_tcp:close(Sock),
    kill_self(no_socket, Handlers).

%% Handles a single IRC message.
handle_message(Sock, AdmPass, Nick, Handlers, Message) ->
    pong_if_necessary(Sock, Message),
    case irc:parse(Message, Nick) of
	{privmsg, Msg} ->
	    priv_handler(Sock, AdmPass, Handlers, Msg);
	{chanmsg, Msg} ->
	    send_all(Handlers, {chan, Msg, self()});
	{noise,   Msg} ->
	    send_all(Handlers, {noise, Msg, self()});
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
		    erlbot ! {join, Channel};
		%% Command to leave a channel.
		["part", Channel] ->
		    erlbot ! {part, Channel};
		%% Command to leave a channel.
		["load", Plugin] ->
		    erlbot:load(list_to_atom(Plugin));
		%% Command to leave a channel.
		["unload", Plugin] ->
		    erlbot:unload(list_to_atom(Plugin));
		%% Command to reload all running code.
		["reload"] ->
		    case reload() of
			ok ->
			    irc:privmsg(Sock, From, "Reloaded!");
			_ ->
			    ErrMsg = "Reload failed!",
			    irc:privmsg(Sock, From, ErrMsg)
		    end;
		%% Command to kill the bot and go home.
		["die"] ->
		    erlbot ! die;
		_ ->
		    irc:privmsg(Sock, From, "No such command!")
	    end;
	_ ->
	    lists:map(fun(M) -> M ! {priv, {From, Mess}, self()} end, Handlers)
    end.

%% Send a particular message to all handlers in the given list.
send_all(Handlers, Msg) ->
    lists:map(fun(M) -> M ! Msg end, Handlers).
