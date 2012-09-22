-module(bq).

-export([start/0]).

start() ->
  lager:start(),
  application:start(cowboy),
  application:start(bq),
  application:start(mimetypes),
  ok.