%% Copyright (c) 2011-2021, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ranch_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([profile_output/0]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
%% ranch应用程序主入口
start(_, _) ->
	_ = consider_profiling(),
	%% 创建所需ETS表
	ranch_server = ets:new(ranch_server, [
		ordered_set, public, named_table]),
	%% 在这里启动ranch_sup
	ranch_sup:start_link().

-spec stop(term()) -> ok.
stop(_) ->
	ok.

-spec profile_output() -> ok.
profile_output() ->
	%% 将进程活动信息输出分析并输出到日志文件中
	eprof:stop_profiling(),
	eprof:log("procs.profile"),
	eprof:analyze(procs),
	eprof:log("total.profile"),
	eprof:analyze(total).

consider_profiling() ->
	case application:get_env(profile) of
		{ok, true} ->
			{ok, _Pid} = eprof:start(),
			eprof:start_profiling([self()]);
		_ ->
			not_profiling
	end.
