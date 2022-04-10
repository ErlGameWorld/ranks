-module(rankWork_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 100, period => 3600},
    ChildSpecs = [
       #{
          id => rankWork,
          start => {rankWork, start_link, []},
          restart => permanent,
          shutdown => 3000,
          type => worker,
          modules => [rankWork]
       }
    ],
    {ok, {SupFlags, ChildSpecs}}.
