-module(erlbot).
-export([start/2, main/4, quakenet/0]).

quakenet() ->
    {"irc.quakenet.org", 6667}.

%% Start the bot
start({Addr, Port}, {Nick, AdminPass}) ->
    I = case compile:file(irc) of
	    {ok, Irc} -> Irc;
	    _         -> erlang:error("Unable to compile IRC module!")
	end,
    
    S = case gen_tcp:connect(Addr, Port, [binary]) of
	    {ok, Socket} -> Socket;
	    _            -> erlang:error("Unable to connect!")
	end,

    register(erlbot, spawn(fun() -> main(I, S, AdminPass, Nick) end)),
    gen_tcp:controlling_process(S, whereis(erlbot)),
    irc:init(S, Nick),
    erlbot.

%% Reload a bunch of modules
reload([F|Files], Mods) ->
    case compile:file(F) of
	{ok, M} ->
	    code:purge(F),
	    {module, Mod} = code:load_file(M),
	    reload(Files, [Mod | Mods]);
	Error ->
	    Error
    end;
reload(_, Mods) ->
    {ok, lists:reverse(Mods)}.

%% Our main message loop
main(Irc, Sock, AdmPass, Nick) ->
    receive
	%% Reload all running code, then report error or success to the caller.
	{reload, From} ->
	    io:format("Trying to reload...~n", []),
	    case reload([irc, erlbot], []) of
		{ok, [I, B]} ->
		    From ! ok,
		    io:format("Reload succeeded!~n", []),
		    B:main(I, Sock, AdmPass, Nick);
		Error ->
		    From ! Error,
		    io:format("Reload FAILED!~n", []),
		    main(Irc, Sock, AdmPass, Nick)
	    end;

	%% Someone wants us dead; let's obey!
	die ->
	    io:format("OK, dying...~n"),
	    Irc:quit(Sock, "No particular reason."),
	    unregister(erlbot),
	    gen_tcp:close(Sock);

	%% Respond to PING messages
	{tcp, S, <<"PING ", Data/binary>>} ->
	    Irc:pong(S, Data),
	    main(Irc, Sock, AdmPass, Nick);

	%% Unknown message; discard it.
	{tcp, S, Data} ->
	    lists:map(fun(Msg) ->
			      handle_message(Sock, Irc, AdmPass, Nick, Msg)
		      end,
		      lines(binary_to_list(Data))),
	    main(Irc, Sock, AdmPass, Nick)
    end.

%% Handles a single IRC message.
handle_message(Sock, Irc, AdmPass, Nick, Message) ->
    pong_if_necessary(Sock, Message),
    case Irc:parse(Message, Nick) of
	{privmsg, Msg} ->
	    spawn(fun() -> priv_handler(Sock, Irc, AdmPass, Msg) end);
	{chanmsg, Msg} ->
	    spawn(fun() -> chan_handler(Sock, Irc, Msg) end);
	{noise,   Msg} ->
	    spawn(fun() -> noise_handler(Sock, Irc, Msg) end);
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

%% Break the string on every newline.
lines(Xs) ->
    lines(Xs, []).

lines([13|Xs], L) ->
    case L of
	[_|_] -> [lists:reverse(L) | lines(Xs, [])];
	_     -> lines(Xs, [])
    end;
lines([10|Xs], L) ->
    case L of
	[_|_] -> [lists:reverse(L) | lines(Xs, [])];
	_     -> lines(Xs, [])
    end;
lines([X|Xs], L) ->
    lines(Xs, [X | L]);
lines(_, L=[_|_]) ->
    [lists:reverse(L)];
lines(_, _) ->
    [].

%% Sends a reload message and waits for confirmation.
reload() ->
    erlbot ! {reload, self()},
    receive X -> X end.

%% Handle private (whispered) messages, but only if prefixed with the correct
%% password.
priv_handler(Sock, Irc, AdmPass, {From, To, Message}) ->
    case irc:words(Message) of
	[AdmPass | Cmd] ->
	    case Cmd of
		["join", Channel] ->
		    Irc:join(Sock, Channel);
		["part", Channel] ->
		    Irc:part(Sock, Channel);
		["reload"] ->
		    case reload() of
			ok ->
			    Irc:privmsg(Sock, From, "Reloaded!");
			Err ->
			    ErrMsg = "Reload failed!",
			    Irc:privmst(Sock, From, ErrMsg)
		    end;
		["die"] ->
		    erlbot ! die;
		_ ->
		    Irc:privmsg(Sock, From, "No such command!")
	    end;
	_ ->
	    Irc:privmsg(Sock, From, "Wrong password!")
    end.

%% Handle messages directed to us in a channel.
chan_handler(Sock, Irc, Message) ->
    ok.

%% Handle messages directed to us in a channel.
noise_handler(Sock, Irc, Message) ->
    ok.
