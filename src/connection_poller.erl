-module(connection_poller).

-define(SERVER, ?MODULE).

-export([start/0, stop/0]).
-export([pinger/0, poller/0]).

start() ->
  launch_process(poller_process, {?SERVER, poller, []}),
  launch_process(pinger_process, {?SERVER, pinger, []}).

stop() ->
  terminate_process(poller),
  terminate_process(pinger).

launch_process(ServerName, {Module, Function, Args}) ->
  global:trans({ServerName, ServerName},
  fun() ->
   case global:whereis_name(ServerName) of
     undefined ->
       Pid = spawn(Module, Function, Args),
       global:register_name(ServerName, Pid);
     _ ->
       ok
   end
  end).

terminate_process(ServerName) ->
  global:trans({ServerName, ServerName},
	 fun() ->
		 case global:whereis_name(ServerName) of
		     undefined ->
			 ok;
		     _ ->
			 global:send(ServerName, shutdown)
		 end
	 end).

poller() ->
  process_flag(trap_exit, true),
  try gen_fsm:send_event(connection, poll) of
    ok ->
      timer:sleep(5000),
      poller()
  catch
    error:X -> {nothing, caught, error, X}
  end.

pinger() ->
  process_flag(trap_exit, true),
  try gen_fsm:send_event(connection, ping) of
    ok ->
      timer:sleep(60000),
      pinger()
  catch
    error:X -> {nothing, caught, error, X}
  end.