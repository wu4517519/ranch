%% Feel free to use, reuse and abuse the code in this file.

-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

%% 启动协议进程
start_link(Ref, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	%% 注意例如gen_statem 和gen_server的start_link 在init回调函数返回前是不会返回的
	%% 故上述两个模块不能在init回调函数内执行handshake，否则回产生死锁
	{ok, Socket} = ranch:handshake(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 60000) of
		{ok, Data} when Data =/= <<4>> ->
			Transport:send(Socket, Data),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
