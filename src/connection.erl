-module(connection).

-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3, handle_sync_event/4]).
-export([stop/0, pinger/0]).
-export([disconnected/2, connected/2, active/2, check_login_headers/3, close_connection/1]).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, connection, [], []),
    server_util:start(ping_process, {?SERVER, pinger, []}),
    ok.

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).

pinger() ->
    process_flag(trap_exit, true),
    gen_fsm:send_event(?SERVER, poll),
    timer:sleep(60000),
    pinger().

init([]) ->
    case file:consult("../conf/erlyfire.conf") of
      {ok, ConfigData} ->
        io:format("ConfigData=~p~n",[ConfigData]),
        application:start(inets),
        ssl:start(),
        http:set_options([{cookies, enabled}]),
        {ok, disconnected, ConfigData};
      {error, Why} ->
        {stop, Why}
      end.

disconnected(poll, ConfigData) ->
  io:format("ConfigData in connect=~p~n",[ConfigData]),
  [{domain, Domain}, {use_ssl, Ssl}, {username, Username}, {password, Password}, {room_id, _}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/"],
  Post = ["email_address=", ibrowse_lib:url_encode(Username), "&", "password=", ibrowse_lib:url_encode(Password)],
  case http:request(post, {lists:flatten(Url) ++ "login", [], "application/x-www-form-urlencoded", lists:flatten(Post)}, [], []) of
    {ok, {{_, 302, _}, Headers, _}} ->
      check_login_headers(Url, Headers, ConfigData);
    _ ->
      {stop, "Error: Campfire did not redirect us"}
    end.

connected(poll, ConfigData) ->
  [{domain, Domain}, {use_ssl, Ssl}, {username, _}, {password, _}, {room_id, RoomId}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", RoomId],
  case http:request(lists:flatten(Url)) of
    {ok, {{_, 200, _}, _, _}} ->
      {next_state, active, ConfigData};
    _ ->
      {stop, "Error: Campfire did not return a 200"}
    end.

active(poll, ConfigData) ->
  [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid, "/tabs"],
  http:request(post, {lists:flatten(Url), [], [], []}, [], []),
  {next_state, active, ConfigData}.

handle_event(stop, _StateName, ConfigData) ->
  {stop, "Called Shutdown", ConfigData}.

% handle_event(_Event, StateName, ConfigData) ->
%   {next_state, StateName, ConfigData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, _StateName, _StateData) ->
    ok.

terminate(Reason, _State, ConfigData) ->
    io:format("Closing down Campfire Connection ~p", [Reason]),
    close_connection(ConfigData),
    application:stop(inets),
    ssl:stop(),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

check_login_headers(Url, Headers, ConfigData) ->
  Stringurl = lists:flatten(Url),
  Loginurl = Stringurl ++ "login",
  case lists:keysearch("location", 1, Headers) of
    {value,{"location",Stringurl}} ->
      {next_state, connected, ConfigData};
    {value,{"location",Loginurl}} ->
      {stop, "Incorrect Login Details"};
    false ->
      {stop, "Error: didn't receive a location header"}
  end.

close_connection(ConfigData) ->
  [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  LeaveUrl = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid, "/leave"],
  http:request(post, {lists:flatten(LeaveUrl), [], [], []}, [], []),
  LogoutUrl = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "logout"],
  http:request(lists:flatten(LogoutUrl)).
