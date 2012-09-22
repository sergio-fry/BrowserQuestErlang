-module(bq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = [
      {'_', [
        {[<<"client">>, '...'], cowboy_http_static, [
          {directory, <<"node/client">>},
          {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        ]},
        {[<<"shared">>, '...'], cowboy_http_static, [
          {directory, <<"node/shared">>},
          {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        ]}
        % ,
        % {[<<"proxy">>, '...'], bq_proxy, [{upstream, "http://localhost:8001/"}]},
        % {'_', bq_client, []}
      ]}
  ],

  cowboy:start_listener(bq_http_server, 10,
      cowboy_tcp_transport, [{port, 8000}],
      cowboy_http_protocol, [{dispatch, Dispatch}]
  ),
  
    bq_sup:start_link().

stop(_State) ->
    ok.