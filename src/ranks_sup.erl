-module(ranks_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 0, period => 1},
    ChildSpecs = [
       #{
          id => rankMgr,
          start => {rankMgr, start_link, []},
          restart => permanent,
          shutdown => 3000,
          type => worker,
          modules => [rankMgr]
       },
       #{
          id => rankWork_sup,
          start => {rankWork_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [rankWork_sup]
       }
    ],
    {ok, {SupFlags, ChildSpecs}}.
