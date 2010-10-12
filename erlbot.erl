-module(erlbot).
-export([start/3, quakenet/0]).

quakenet() ->
    {"irc.quakenet.org", 6667}.

%% Start the bot.
%% The first argument is the pair of {address, port} of the IRC service to
%%   connect to.
%% The second argument is the nickname to use for the bot, and the admin
%%   password, used to give the bot administrative commands.
%% The third argument is a list of handler modules for various events.
%%   A handler module must export the functions priv/2, chan/2, noise/2,
%%   init/2 and die/0.
%%   See dpress.erl for an example of what such a module might look like.
start({Addr, Port}, {Nick, AdminPass}, Handlers) ->
    case reload([irc, erlbot] ++ Handlers) of
	ok -> ok;
	_  -> erlang:error("Unable to load modules!")
    end,

    %% Open connection to IRC service
    S = case gen_tcp:connect(Addr, Port, [binary]) of
	    {ok, Socket} -> Socket;
	    _            -> erlang:error("Unable to connect!")
	end,

    %% Create connection handler and register
    register(erlbot, spawn(fun()->main(S, AdminPass, Nick, Handlers) end)),
    gen_tcp:controlling_process(S, whereis(erlbot)),

    %% Init IRC connection
    irc:init(S, Nick),

    %% Init plugins
    lists:map(fun(M) -> spawn(fun() -> M:init(S, Nick) end) end, Handlers),
    erlbot.

%% Reload a bunch of modules
reload([F|Files]) ->
    case compile:file(F) of
	{ok, M} ->
	    code:purge(F),
	    {module, _} = code:load_file(M),
	    reload(Files);
	Error ->
	    Error
    end;
reload(_) ->
    ok.

%% Our main message loop
main(Sock, AdmPass, Nick, Handlers) ->
    receive
	%% Reload all running code, then report error or success to the caller.
	{reload, From} ->
	    io:format("Trying to reload...~n", []),
	    case reload([irc, erlbot] ++ Handlers) of
		ok ->
		    From ! ok,
		    io:format("Reload succeeded!~n", []),
		    main(Sock, AdmPass, Nick, Handlers);
		Error ->
		    From ! Error,
		    io:format("Reload FAILED!~n", []),
		    main(Sock, AdmPass, Nick, Handlers)
	    end;

	%% Restart all plugins
	{restart_plugins, From} ->
	    io:format("Restarting all plugins...", []),
	    lists:map(fun(M) -> M:die() end, Handlers),
	    lists:map(fun(M) -> M:init(Sock, Nick) end, Handlers),
	    io:format("OK!~n", []);

	%% Someone wants us dead; let's obey!
	die ->
	    io:format("OK, dying...~n"),
	    irc:quit(Sock, "No particular reason."),
	    unregister(erlbot),
	    gen_tcp:close(Sock);

	%% Respond to PING messages
	{tcp, S, <<"PING ", Data/binary>>} ->
	    irc:pong(S, Data),
	    main(Sock, AdmPass, Nick, Handlers);

	%% Unknown message; discard it.
	{tcp, _, Data} ->
	    lists:map(fun(Msg) ->
			      handle_message(Sock, AdmPass, Nick, Handlers, Msg)
		      end,
		      irc:lines(binary_to_list(Data))),
	    main(Sock, AdmPass, Nick, Handlers)
    end.

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

%% Sends a reload message and waits for confirmation.
reload() ->
    erlbot ! {reload, self()},
    receive X -> X end.

%% Handle private (whispered) messages, but only if prefixed with the correct
%% password.
priv_handler(Sock, AdmPass, Handlers, Mess={From, _, Message}) ->
    case irc:words(Message) of
	[AdmPass | Cmd] ->
	    case Cmd of
		["join", Channel] ->
		    irc:join(Sock, Channel);
		["part", Channel] ->
		    irc:part(Sock, Channel);
		["reload"] ->
		    case reload() of
			ok ->
			    irc:privmsg(Sock, From, "Reloaded!");
			_ ->
			    ErrMsg = "Reload failed!",
			    irc:privmst(Sock, From, ErrMsg)
		    end;
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
    io:format("börjar chanspamma~n", []),
    lists:map(fun(M) -> spawn(fun() -> M:chan(Sock, Msg) end) end, Handlers).

%% Handle messages directed to us in a channel.
noise_handler(Sock, Handlers, Msg) ->
    lists:map(fun(M) -> spawn(fun() -> M:noise(Sock, Msg) end) end, Handlers).
