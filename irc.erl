-module(irc).
-export([init/2, nick/2, quit/2, pong/2, join/2, part/2, privmsg/3, parse/2,
	 words/1, lines/1]).

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

%% Break a string on every space.
words(Xs) -> words(Xs, []).

words([X|Xs], W) when X /= 32 ->
    words(Xs, [X|W]);
words([_|Xs], W) ->
    [lists:reverse(W) | words(Xs, [])];
words(_, W) -> 
    [lists:reverse(W)].

%% Stuff Elem between all the elements in the given list.
intersperse(Elem, [X|Xs=[_|_]]) ->
    [X | [Elem | intersperse(Elem, Xs)]];
intersperse(_, Xs) ->
    Xs.

%% Remove the colon, if any, from the last argument and turn everything in that
%% argument into a single string.
fix_lastarg([[$:|X]|Xs]) ->
    [lists:concat(intersperse(" ", [X | Xs]))];
fix_lastarg([X|Xs]) ->
    [X | fix_lastarg(Xs)];
fix_lastarg(_) ->
    [].

%% Extract the nick part of an IRC message prefix.
extract_nick(Prefix) ->
    lists:takewhile(fun(X) -> X /= $! end, Prefix).

%% Is the given message of the form "<bot_nick>[:|,] some message"?
to_me(Text, Nick) ->
    N = lists:reverse(Nick),
    case lists:reverse(lists:takewhile(fun(X) -> X /= 32 end, Text)) of
	[C | N] ->
	    (C == $,) or (C == $:);
	_ ->
	    false
    end.

%% If we got a message like "bot_nick: omg hi2u" we want to strip our own nick
%% from the message.
msg_to_me(Text) ->
    [_|Msg] = lists:dropwhile(fun(X) -> X /= 32 end, Text),
    Msg.

%% Parse a message from the server.
%% We return a tuple of the format:
%% {msg_type, {from, to, message}}
parse(Msg, Nick) ->
    case readmsg(Msg) of
	%% A private message directly to us!
	{Pre, "PRIVMSG", [Nick, Text]} ->
	    {privmsg, {extract_nick(Pre), Nick, Text}};
	%% A channel message; what to do?
	{Pre, "PRIVMSG", [Channel, Text]} ->
	    ToMe = to_me(Text, Nick),
	    if ToMe ->
		    %% If it's to us, it's a chanmsg
		    {chanmsg, {extract_nick(Pre), Channel, msg_to_me(Text)}};
	       true ->
		    %% Otherwise it's just noise
		    {noise, {extract_nick(Pre), Channel, Text}}
	    end;
	_ ->
	    unknown
    end.

%% Read an IRC message, splitting it into prefix, message and arguments.
readmsg(Msg) ->
    case words(Msg) of
	[ [$: | Prefix] | [Message | Args]] ->
	    {Prefix, Message, fix_lastarg(Args)};
	[Message | Args] ->
	    {Message, Args};
	_ ->
	    unknown
    end.

privmsg(Sock, To, Msg) ->
    T = list_to_binary(To),
    M = list_to_binary(Msg),
    gen_tcp:send(Sock, <<"PRIVMSG ", T/binary, " :", M/binary, "\r\n">>).

join(Sock, Chan) ->
    C = list_to_binary(Chan),
    gen_tcp:send(Sock, <<"JOIN ", C/binary, "\r\n">>).


part(Sock, Chan) ->
    C = list_to_binary(Chan),
    gen_tcp:send(Sock, <<"PART ", C/binary, "\r\n">>).

init(Sock, Nick) ->
    pass(Sock, "_-$£}rJRASDa"),
    nick(Sock, Nick),
    user(Sock, Nick, "-", "-", "-").

%% BEWARE - pong takes a binary, NOT a list!
pong(Sock, Data) ->
    gen_tcp:send(Sock, <<"PONG ", Data/binary, "\r\n">>).

pass(Sock, Pass) ->
    P = list_to_binary(Pass),
    gen_tcp:send(Sock, <<"PASS ", P/binary, "\r\n">>).

nick(Sock, Nick) ->
    N = list_to_binary(Nick),
    gen_tcp:send(Sock, <<"NICK ", N/binary, "\r\n">>).

user(Sock, User, Host, Server, Real) ->
    U = list_to_binary(User),
    H = list_to_binary(Host),
    S = list_to_binary(Server),
    R = list_to_binary(Real),
    Msg = <<"USER ",
	    U/binary, " ",
	    H/binary, " ",
	    S/binary, " ",
	    ":", R/binary, "\r\n">>,
    gen_tcp:send(Sock, Msg).

quit(Sock, Msg) ->
    M = list_to_binary(Msg),
    gen_tcp:send(Sock, <<"QUIT ", M/binary, "\r\n">>).
