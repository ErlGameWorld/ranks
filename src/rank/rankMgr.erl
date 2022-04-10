-module(rankMgr).

-behavior(gen_srv).

-include("ranks.hrl").

-export([
   start_link/0
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, {}).

%% ********************************************  API *******************************************************************
start_link() ->
   gen_srv:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   ets:new(?etsRankInfo, [set, public, named_table, {keypos, #etsRankRecord.key}, {write_concurrency, auto}, {read_concurrency, true}]),
   {ok, #state{}}.

handleCall({mInitRank, RankType}, _State, _FROM) ->
   ets:new(RankType, [ordered_set, public, named_table, {write_concurrency, auto}, {read_concurrency, true}]),
   {reply, ok};
handleCall(_Msg, _State, _FROM) ->
   {reply, ok}.

%% 默认匹配
handleCast(_Msg, _State) ->
   kpS.

handleInfo(_Msg, _State) ->
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
%% ****************************************************** logic ********************************************************
