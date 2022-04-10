-module(ranks_app).

-behaviour(application).

-include("ranks.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gtKvsToBeam:load(?ranksCfg, [{?workCnt, 0}]),
    ranks_sup:start_link().

stop(_State) ->
    ok.
