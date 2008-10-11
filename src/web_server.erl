-module(web_server).

-export([start/1,stop/0,dispatch_requests/1]).

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
  Params = Req:parse_qs(),
  Message = proplists:get_value("message", Params),
  success(Req, "text/html", <<"ok">>);

handle("/json", Req) ->
  Params = Req:parse_qs(),
  success(Req, "application/json", <<"{'ok'}">>);

handle(Unknown, Req) ->
  Req:respond({400, [{"Content-Type", "text/html"}], subst("Bad Request: ~s", [Unknown])}).

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
