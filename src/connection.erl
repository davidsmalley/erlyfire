-module(connection).

-define(SERVER, ?MODULE).

-export([start/0, init_config/0, server_loop/1, stop/0]).

start() ->
  {_State, ConfigData} = init_config(),
  server_util:start(?SERVER, {connection, server_loop, [{start, ConfigData}]}).
  % server_loop({start, ConfigData}).

stop() ->
  server_util:stop(?SERVER).
  
init_config() -> 
  case file:consult("../conf/erlyfire.conf") of
    {ok, ConfigData} ->
      io:format("ConfigData=~p~n",[ConfigData]),
      application:start(inets),
      ssl:start(),
      http:set_options([{cookies, enabled}]),
      {ok, ConfigData};
    {error, Why} ->
      exit({eDeamonConfig, Why})
    end.
    
server_loop({loop, ConfigData}) ->
  io:format("We started the main loop!"),
  [{domain, Domain}, {use_ssl, Ssl}, {username, _}, {password, _}, {room_id, Roomid}] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/room/", ibrowse_lib:url_encode(Roomid)],
  case http:request(lists:flatten(Url)) of
    % {ok, {{_, 302, _}, _, _}} ->
    %   campfire:shutdown(),
    %   stop();
    % {ok, {{_, 200, _}, _, _}} ->
    %   io:format("Success ~p~n", [Data]);
      % server_loop({loop, ConfigData});
    {ok, Data} ->
      % io:format("Success ~p~n", [Data])
      server_loop({loop, ConfigData})
    end,
  ok;
  
server_loop({start, ConfigData}) ->
  Se = "&",
  [{domain, Domain}, {use_ssl, Ssl}, {username, Username}, {password, Password}, _] = ConfigData,
  case Ssl of
    true ->
      Scheme = "https";
    false ->
      Scheme = "http"
    end,
  Url = [Scheme, "://", ibrowse_lib:url_encode(Domain), ".campfirenow.com/"],
  Post = ["email_address=", ibrowse_lib:url_encode(Username), Se, "password=", ibrowse_lib:url_encode(Password)],
  case http:request(post, {lists:flatten(Url) ++ "login", [], "application/x-www-form-urlencoded", lists:flatten(Post)}, [], []) of
    % {ok, {{_, 302, _}, _, _}} ->
    %   campfire:shutdown(),
    %   stop();
    {ok, {{_, 302, _}, _, _}} ->
      server_loop({loop, ConfigData});
    {_, _} ->
      server_loop({loop, ConfigData})
    end;

server_loop(_) ->
  io:format("Dead!"),
  ok.
      