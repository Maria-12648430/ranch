-module(ranch_concuerror).

-export([start_stop/0]).
-export([info/0]).

-concuerror_options([
	{after_timeout, 5000},
	{treat_as_normal, [shutdown]}
]).

start_stop() ->
	{ok, SupPid} = ranch_app:start(temporary, []),

	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
		ranch_erlang_transport, #{
			num_acceptors => 1
		},
		echo_protocol, []),
	ok = ranch:stop_listener(?FUNCTION_NAME),

	exit(SupPid, shutdown),

	ok.

info() ->
	ok.
