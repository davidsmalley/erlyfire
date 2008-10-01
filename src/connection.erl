-module(connection).

-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3, handle_sync_event/4]).
-export([stop/0]).
-export([poll/2, connect/2]).

start_link() ->
    gen_fsm:start_link({local, connection}, connection, [], []).

stop() ->
    gen_fsm:send_all_state_event(connection, stop).
    
init([]) ->
    case file:consult("../conf/erlyfire.conf") of
      {ok, ConfigData} ->
        io:format("ConfigData=~p~n",[ConfigData]),
        application:start(inets),
        ssl:start(),
        http:set_options([{cookies, enabled}]),
        {ok, connect, ConfigData};
      {error, Why} ->
        {stop, Why}
      end.

connect(_Event, ConfigData) ->
  io:format("ConfigData in connect=~p~n",[ConfigData]),
  [{domain, Domain}, {use_ssl, Ssl}, {username, Username}, {password, Password}, {room_id, _}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/login"],
  Post = ["email_address=", ibrowse_lib:url_encode(Username), "&", "password=", ibrowse_lib:url_encode(Password)],
  case http:request(post, {lists:flatten(Url) , [], "application/x-www-form-urlencoded", lists:flatten(Post)}, [], []) of
    % {ok, {{_, 302, _}, _, _}} ->
    %   campfire:shutdown(),
    %   stop();
    {ok, {{_, 302, _}, _, _}} ->
      {next_state, poll, ConfigData};
    {_, _} ->
      {next_state, connect, ConfigData}
    end.

poll(_Event, ConfigData) ->
  [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid, "/tabs"],
  http:request(post, {lists:flatten(Url), [], [], []}, [], []),
  {next_state, poll, ConfigData}.
  

handle_event(stop, StateName, ConfigData) ->
  [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid, "/leave"],
  http:request(post, {lists:flatten(Url)}),
  {next_state, StateName, ConfigData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, _StateName, _StateData) ->
    ok.

terminate(Reason, _State, _ConfigData) ->
    io:format("Closing down Campfire Connection ~p", [Reason]),
    application:stop(inets),
    ssl:stop(),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.