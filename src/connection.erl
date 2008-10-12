-module(connection).

-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, terminate/3, code_change/4, handle_event/3, handle_info/3, handle_sync_event/4]).
-export([stop/0, pinger/0, poller/0]).
-export([disconnected/2, connected/2, active/2, check_login_headers/3, close_connection/1]).

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, connection, [], []),
    server_util:start(poller_process, {?SERVER, poller, []}),
    server_util:start(pinger_process, {?SERVER, pinger, []}).

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).

poller() ->
  process_flag(trap_exit, true),
  try gen_fsm:send_event(?SERVER, poll) of
    ok ->
      timer:sleep(5000),
      logger:log("Poll request"),
      poller()
  catch
    error:X -> {nothing, caught, error, X}
  end.

pinger() ->
  process_flag(trap_exit, true),
  try gen_fsm:send_event(?SERVER, ping) of
    ok ->
      timer:sleep(60000),
      logger:log("Ping request"),
      pinger()
  catch
    error:X -> {nothing, caught, error, X}
  end.

init([]) ->
    case file:consult("../conf/erlyfire.conf") of
      {ok, ConfigData} ->
        application:start(inets),
        ssl:start(),
        http:set_options([{cookies, enabled}]),
        logger:log("Started"),
        {ok, disconnected, [ConfigData, []]};
      {error, Why} ->
        {stop, Why}
      end.

disconnected(poll, [ConfigData, _RoomData]) ->
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
      logger:log("Logged in"),
      check_login_headers(Url, Headers, [ConfigData, _RoomData]);
    _ ->
      {next_state, disconnected, [ConfigData, _RoomData]}
    end;

disconnected(ping, [ConfigData, _RoomData]) ->
  {next_state, disconnected, ConfigData}.

connected(poll, [ConfigData, _RoomData]) ->
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
      logger:log("Joined the room"),
      {next_state, active, [ConfigData, _RoomData]};
    _ ->
      {next_state, connected, [ConfigData, _RoomData]}
    end;

connected(ping, [ConfigData, _RoomData]) ->
  {next_state, connected, [ConfigData, _RoomData]}.

active(poll, [ConfigData, _RoomData]) ->
  % [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  % case Ssl of
  %   true ->
  %     Scheme = "https";
  %   false ->
  %     Scheme = "http"
  %   end,
  % Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid],
  % http:request(lists:flatten(Url)),
  logger:log("Polling campfire for messages"),
  {next_state, active, [ConfigData, _RoomData]};

active({message, Message, Paste}, [ConfigData, _RoomData]) ->
  [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  case Paste of
    true ->
      PasteParam = "&paste=true";
    "true" ->
      PasteParam = "&paste=true";
    _ ->
      PasteParam = ""
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid, "/speak"],
  {First, Second, _} = erlang:now(),
  Post = ["message=", ibrowse_lib:url_encode(Message), "&", "t=", lists:concat([First, Second]), PasteParam],
  case http:request(post, {lists:flatten(Url), [], "application/x-www-form-urlencoded", lists:flatten(Post)}, [], []) of
    {ok, {{_, 200, _}, _, _}} ->
      logger:log("posted a message"),
      {next_state, active, [ConfigData, _RoomData]};
    _ ->
      {next_state, disconnected, [ConfigData, _RoomData]}
    end;

active(ping, [ConfigData, _RoomData]) ->
  [{domain, Domain}, {use_ssl, Ssl}, _, _, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/", "room/", Roomid, "/tabs"],
  http:request(post, {lists:flatten(Url), [], [], []}, [], []),
  logger:log("Just pinged tabs"),
  {next_state, active, [ConfigData, _RoomData]}.

handle_event(stop, _StateName, [ConfigData, _RoomData]) ->
  {stop, "Called Shutdown", [ConfigData, _RoomData]}.

% handle_event(_Event, StateName, ConfigData) ->
%   {next_state, StateName, ConfigData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, _StateName, _StateData) ->
    ok.

terminate(_Reason, _State, [ConfigData, _RoomData]) ->
    close_connection(ConfigData),
    application:stop(inets),
    ssl:stop(),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

check_login_headers(Url, Headers, [ConfigData, _RoomData]) ->
  Stringurl = lists:flatten(Url),
  Loginurl = Stringurl ++ "login",
  case lists:keysearch("location", 1, Headers) of
    {value,{"location",Stringurl}} ->
      {next_state, connected, [ConfigData, _RoomData]};
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
