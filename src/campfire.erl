-module(campfire).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, shutdown/0]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, [], []),
  connection:start_link(),
  web_server:start(5050).

shutdown() ->
  connection:stop(),
  gen_server:call(?SERVER, stop),
  web_server:stop(5050).

init([]) ->
  {ok, 0}.

handle_call({send_message, Message, Paste}, _From, State) ->
  gen_fsm:send_event(connection, {message, Message, Paste}),
  {reply, ok, State};

handle_call(stop, _From, State) ->
  {stop, normal, State};

handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.