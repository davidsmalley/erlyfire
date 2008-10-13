{application, erlyfire,
  [{description, "The Erlang Campfire Bot"},
  {vsn, "0.1"},
  {modules, [erlyfire_app, campfire_supervisor, campfire,
            connection, connection_poller, web_server]},
  {registered, [campfire, connection, web_server, poller_process, pinger_process]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {erlyfire_app,[]}}
  ]}.