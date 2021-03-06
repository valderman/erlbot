-module(dpress).
-export([start/2, mediator/1, ready_to_connect/0]).

%% Truncate a string to the first N chars.
truncate(Xs, N) ->
    truncate(Xs, N, []).

truncate(_, 0, Acc) ->
    lists:reverse(Acc);
truncate([X|Xs], N, Acc) ->
    truncate(Xs, N-1, [X | Acc]);
truncate([], _, Acc) ->
    lists:reverse(Acc).

%% Entry point of plugin; starts a new process for the plugin, which then
%% receives events as messages.
%% This function must return the PID of the plugin's message handler process.
start(IrcSocket, Nick) ->
    spawn(fun() ->
             Mediator = spawn_link(fun dpress:ready_to_connect/0),
	     main(IrcSocket, Mediator)
	 end).



%% Main event handler for the plugin; must handle messages die, priv, chan and
%% noise.
%% Each handled message must be acknowleged by replying ok to the sender.
main(Sock, Mediator) ->
    receive
	%% die message; we're being unloaded, so clean up and quit.
	{die, Pid} ->
	    unregister(dpress),
	    Pid ! ok,
	    Mediator ! die;

	%% priv message; someone /msg'd us and we treat it as a question.
	{priv, {From, Msg}, Pid} ->
	    Mediator ! {ask, Msg, self()},
	    receive
		{reply, Ans} ->
		    [Text | _] = irc:lines(Ans),
		    irc:privmsg(Sock, From, Text),
		    Pid ! ok;
		_ ->
		    Pid ! ok
	    end,
	    main(Sock, Mediator);

	%% chan message; someone addressed us in a public channel.
	%% That's also a question.
	{chan, {From, Chan, Msg}, Pid} ->
	    Mediator ! {ask, Msg, self()},
	    receive
		{reply, Ans} ->
		    [Text | _] = irc:lines(Ans),
		    irc:privmsg(Sock, Chan, From ++ ": " ++ Text),
		    Pid ! ok;
		_ ->
		    Pid ! ok
	    end,
	    main(Sock, Mediator);

	%% noise message; someone said something in a public channel, but it
	%% wasn't directed at us. Feed the generator with it.
	{noise, {From, Chan, Msg}, Pid} ->
	    Mediator ! {feed, Msg, self()},
	    receive _ -> ok end,
	    Pid ! ok,
	    main(Sock, Mediator)
    end.


%% Lazy connection to the dpress server.
%% Wait until we get any messages. When we do, try to open a connection. If we
%% fail, stay in ready_to_connect state and notify the caller of the failure.
ready_to_connect() ->
    receive
	die ->
	    ok;
	Msg ->
	    case gen_tcp:connect(localhost, 1917, [list]) of
		{ok, Sock} ->
		    %% Go into the active state, then arrange for the message
		    %% to be resent after the handshake.
		    timer:send_after(3000, Msg),
		    dpress:mediator(Sock);
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
	    after 5000 ->
		exit(no_response_from_dpress_server)
	    end;

	%% Feed the text generator.
	{feed, Q, From} ->
	    gen_tcp:send(Sock, ": " ++ Q ++ "\n"),
	    receive
		{tcp, _, _} ->
		    From ! ok,
		    dpress:mediator(Sock)
	    after 5000 ->
		exit(no_response_from_dpress_server)
	    end;

	%% Save the current dictionary.
	{save, From} ->
	    gen_tcp:send(Sock, ":save\n"),
	    receive
		{tcp, _, _} ->
		    From ! ok,
		    dpress:mediator(Sock)
	    end;

	%% Load the default dictionary.
	{load, From} ->
	    gen_tcp:send(Sock, ":load\n"),
	    receive
		{tcp, _, _} ->
		    From ! ok,
		    dpress:mediator(Sock)
	    end;

	%% Connection died; go to ready_to_connect state.
	{tcp_closed, _} ->
	    dpress:ready_to_connect();

	%% Shutdown requested; bye!
	die ->
	    gen_tcp:close(Sock),
	    ok
    end.
