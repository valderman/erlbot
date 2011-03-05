-module(interjection).
-export([start/2]).

start(Sock, Nick) ->
    spawn(fun() -> main(Sock) end).

main(Sock) ->
    receive
	{die, Pid} ->
	    Pid ! ok;
	{priv, _, Pid} ->
	    Pid ! ok,
	    main(Sock);
	{chan, _, Pid} ->
	    Pid ! ok,
	    main(Sock);
	{noise, {From, Chan, Msg}, Pid} ->
	    GnuPos = string:str(Msg, "GNU/Linux"),
	    LinPos = string:str(Msg, "Linux"),
	    LinPos2 = string:str(Msg, "linux"),
	    if
		GnuPos > 0 ->
		    ok;
		(LinPos > 0) or (LinPos2 > 0) ->
		    interject(Sock, From, Chan)
	    end,
	    Pid ! ok,
	    main(Sock)
    end.

interject(Sock, From, Chan) ->
    irc:privmsg(Sock, Chan, From ++ ": " ++ interjection()).

interjection() ->
    "I'd just like to interject for a moment. What you're referring to as Linux, is in fact, GNU/Linux. Linux is not an operating system unto itself, but rather another free component of a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX".
