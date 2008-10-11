-module(web_server).

-export([start/1,stop/0,dispatch_requests/1]).

-define(JSON_CT, "application/json").
-define(HTML_CT, "text/html").
-define(FORM_CT, "application/x-www-form-urlencoded").

start(Port) ->
  mochiweb_http:start([{port, Port},
		       {loop, fun dispatch_requests/1}]).

stop() ->
  mochiweb_http:stop().

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

handle("/post", Req) ->
  logger:log("Received request"),
  case Req:get_primary_header_value("content-type") of
    ?FORM_CT ++ _ ->
      logger:log("Parsing a form"),
      parse_html_post(Req);
    ?JSON_CT ++ _ ->
      logger:log("Parsing json"),
      parse_json(Req);
    _ ->
      error(Req, ?HTML_CT, <<"Please send a mime type of application/x-www-form-urlencoded or application/json">>)
    end;

handle(Unknown, Req) ->
  Req:respond({400, [{"Content-Type", ?HTML_CT}], subst("Bad Request: ~s", [Unknown])}).

error(Req, Type, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", Type}], Body}).

success(Req, Type, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", Type}], Body}).

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.

parse_json(Req) ->
  Payload = mochijson2:decode(Req:recv_body()),
  Response = list_to_binary(mochijson2:encode({struct,[{<<"response">>,<<"ok">>}]})),
  success(Req, ?JSON_CT, Response).

parse_html_post(Req) ->
  Payload = Req:parse_post(),
  Response = <<"OK">>,
  success(Req, ?HTML_CT, Response).
