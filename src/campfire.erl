-module(campfire).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, shutdown/0]).

start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  connection:start().
  
shutdown() ->
  connection:stop(),
  gen_server:call(?SERVER, stop).

init([]) ->
  {ok, 0}.

handle_call(stop, _From, State) ->
  {stop, normal, State};
  
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.