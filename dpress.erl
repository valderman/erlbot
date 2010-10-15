-module(dpress).
-export([init/2, die/0, priv/2, chan/2, noise/2,
	 mediator/1, ready_to_connect/0]).

%% Truncate a string to the first N chars.
truncate(Xs, N) ->
    truncate(Xs, N, []).

truncate(_, 0, Acc) ->
    lists:reverse(Acc);
truncate([X|Xs], N, Acc) ->
    truncate(Xs, N-1, [X | Acc]);
truncate([], _, Acc) ->
    lists:reverse(Acc).


%% Initialize module; in our case, we just start the dpress mediator process.
init(IrcSocket, Nick) ->
    %% If dpress is already registered, attempt to kill the process. If that
    %% doesn't result in dpress unregistering within a second, then the process
    %% is clearly defunct so we unregister the atom ourselves.
    case lists:member(dpress, registered()) of
	true ->
	    die(),
	    timer:sleep(1000),
	    case lists:member(dpress, registered()) of
		true ->
		    unregister(dpress);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end,
    Mediator = spawn(fun dpress:ready_to_connect/0),
    register(dpress, Mediator).

%% Wait until we get any messages. When we do, try to open a connection. If we
%% fail, stay in ready_to_connect state and notify the caller of the failure.
ready_to_connect() ->
    receive
	Msg ->
	    case gen_tcp:connect(localhost, 1917, [list]) of
		{ok, Sock} ->
		    %% Go into the active state, then arrange for the message
		    %% to be resent after the handshake.
		    timer:send_after(3000, Msg),
		    dpress:mediator(Sock);
		die ->
		    unregister(dpress);
		_ ->
		    case Msg of
			{_, _, From} ->
			    From ! sorry;
			{_, From} ->
			    From ! sorry
		    end,
		    dpress:ready_to_connect()
	    end
    end.

%% Mediates connections with the dpress service on localhost.
mediator(Sock) ->
    receive
	%% Ignore unwanted messages.
	{tcp, _, _} ->
	    dpress:mediator(Sock);

	%% Someone wants to ask a question; respond.
	{ask, Q, From} ->
	    gen_tcp:send(Sock, Q ++ "\n"),
	    receive
		{tcp, _, Ans} ->
		    From ! {reply, truncate(Ans, 450)},
		    dpress:mediator(Sock)
	    end;

	%% Feed the text generator.
	{feed, Q, From} ->
	    gen_tcp:send(Sock, ": " ++ Q ++ "\n"),
	    receive
		{tcp, _, _} ->
		    From ! ok,
		    dpress:mediator(Sock)
	    end;

	%% Save the current dictionary.
	{save, From} ->
	    gen_tcp:send(Sock, ":save\n"),
	    receive
		{tcp, _, Ans} ->
		    From ! ok,
		    dpress:mediator(Sock)
	    end;

	%% Load the default dictionary.
	{load, From} ->
	    gen_tcp:send(Sock, ":load\n"),
	    receive
		{tcp, _, Ans} ->
		    From ! ok,
		    dpress:mediator(Sock)
	    end;

	%% Connection died; go to ready_to_connect state.
	{tcp_closed, S} ->
	    dpress:ready_to_connect();

	%% Shutdown requested; bye!
	die ->
	    gen_tcp:close(Sock),
	    unregister(dpress),
	    ok
    end.
    

%% Perform any cleanup before unloading here.
die() ->    
    dpress ! die.

%% If we got a private message, treat it as a question.
priv(Sock, {From, Chan, Msg}) ->
    dpress ! {ask, Msg, self()},
    receive
	{reply, Ans} ->
	    [Text | _] = irc:lines(Ans),
	    irc:privmsg(Sock, From, Text);
	_ ->
	    ok
    end.

%% If we got a directed channel message, treat it as a question.
chan(Sock, {From, Chan, Msg}) ->
    dpress ! {ask, Msg, self()},
    receive
	{reply, Ans} ->
	    [Text | _] = irc:lines(Ans),
	    irc:privmsg(Sock, Chan, From ++ ": " ++ Text);
	_ ->
	    ok
    end.

%% If we got channel noise, feed it to the text generator.
noise(Sock, {From, Chan, Msg}) ->
    dpress ! {feed, Msg, self()},
    receive
	_ -> ok
    end.
