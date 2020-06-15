-module(ranch_concuerror).

-export([start_stop/0]).
-export([info/0]).

-concuerror_options([
	{after_timeout, 5000},
	{treat_as_normal, [
		killed, %% Acceptors are killed on shutdown.
		shutdown %% This is a normal exit reason in OTP.
	]}
]).

%% Convenience functions.

do_start() ->
	{ok, SupPid} = ranch_app:start(temporary, []),
	SupPid.

do_stop(SupPid) ->
	exit(SupPid, shutdown),
	%% We make sure that SupPid terminated before we return,
	%% because otherwise the shutdown will not be ordered and
	%% can produce error exit reasons.
	MRef = monitor(process, SupPid),
	receive {'DOWN', MRef, process, SupPid, _} -> ok end.

%% Tests.

start_stop() ->
	%% Start a listener then stop it.
	SupPid = do_start(),

% @todo Concuerror gets stuck when stopping.
	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
		ranch_erlang_transport, #{
			num_acceptors => 1
		},
		echo_protocol, []),
	ok = ranch:stop_listener(?FUNCTION_NAME),

	do_stop(SupPid).

%% @todo start_stop_twice

info() ->
	%% Ensure we can call ranch:info/1 after starting a listener.
	SupPid = do_start(),
	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
		ranch_erlang_transport, #{
			num_acceptors => 1
		},
		echo_protocol, []),
	#{} = ranch:info(?FUNCTION_NAME),
	do_stop(SupPid).
