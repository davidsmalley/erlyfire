-module(logger).

-define(SERVER, ?MODULE).

-compile([export_all]).

start() ->
  server_util:start(logger, {?SERVER, logger, []}).

stop() ->
  server_util:stop(logger).

log(Message) ->
  P = global:whereis_name(logger),
  P ! {log, Message}.

logger() ->
  receive
    {log, Message} ->
      {{_,_,_},{Hour,Minute,Second}} = erlang:localtime(),
      io:format("~p:~p:~p - ~p~n", [Hour, Minute, Second, Message]),
      logger();
    shutdown ->
      ok
    end.