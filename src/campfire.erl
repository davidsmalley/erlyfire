-module(campfire).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, shutdown/0]).

start() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

shutdown() ->
  gen_server:call(?SERVER, stop).

init([]) ->
  process_flag(trap_exit, true),
  {ok, 0}.

handle_call({send_message, Message, Paste}, _From, State) ->
  gen_fsm:send_event(connection, {message,Message, Paste}),
  {reply, ok, State};

handle_call(stop, _From, State) ->
  {stop, normal, State};

handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.