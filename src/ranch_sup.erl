%% Copyright (c) 2011-2021, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2020-2021, Jan Uhlig <juhlig@hnc-agency.org>
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

-module(ranch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

%% 该监督者首次启动时会启动ranch_server
init([]) ->
	%% 默认子进程最大重启次数
	Intensity = case application:get_env(ranch_sup_intensity) of
		{ok, Value1} -> Value1;
		undefined -> 1
	end,
	%% 默认重启周期
	Period = case application:get_env(ranch_sup_period) of
		{ok, Value2} -> Value2;
		undefined -> 5
	end,

	%% 启动ranch_server
	Procs = [
		%% #{id => atom, start => {M,F,A}}
		#{id => ranch_server, start => {ranch_server, start_link, []}}
	],
	%% {ok, one_for_one(默认) 监督者启动标识, 子规范列表(从左到右启动子进程)}
	{ok, {#{intensity => Intensity, period => Period}, Procs}}.
