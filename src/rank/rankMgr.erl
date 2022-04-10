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

%% ********************************************  API *******************************************************************
start_link() ->
   gen_srv:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   ets:new(?etsRankInfo, [set, public, named_table, {keypos, #etsRankRecord.key}, {write_concurrency, auto}, {read_concurrency, true}]),
   {ok, #{}}.

handleCall({mInitRank, RankType, CntLimit, CntMax}, State, _FROM) ->
   case State of
      #{RankType := _} ->
         {reply, {error, used}};
      _ ->
         ets:new(RankType, [ordered_set, public, named_table, {write_concurrency, auto}, {read_concurrency, true}]),
         NewState = State#{RankType => {CntLimit, CntMax}},
         gtKvsToBeam:load(?ranksLimit, maps:to_list(NewState)),
         {reply, ok, NewState}
   end;
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
