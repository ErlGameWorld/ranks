-module(ranks).

-include("ranks.hrl").

-export([
   start/0
   , stop/0
   , startWork/1
   , initRank/1                %% 创建新的排行榜
   , updateScore/3           %% 更新分数
   , updateInfo/2            %% 更新其他分数
   , getRankInfo/4           %% 获取排行某页的信息
]).

start() ->
   application:ensure_all_started(ranks).

stop() ->
   application:stop(ranks).

workName(Idx) ->
   binary_to_atom(<<"$rankWork_", (integer_to_binary(Idx))/binary>>).

fieldIdx([], _Idx, Acc) ->
   Acc;
fieldIdx([Field | Fields], Idx, Acc) ->
   fieldIdx(Fields, Idx + 1, [{Field, Idx} | Acc]).

-spec startWork(Cnt :: non_neg_integer()) -> ok | {error, term()}.
startWork(Cnt) when Cnt > 0 ->
   case ?ranksCfg:getV(?workCnt) of
      0 ->
         NameList = [{Idx, workName(Idx)} || Idx <- lists:seq(1, Cnt)],
         [supervisor:start_child(rankWork_sup, [WorkName]) || {_Idx, WorkName} <- NameList],
         CfgList = [{?workCnt, Cnt} | NameList],
         Fields = record_info(fields, etsRankRecord),
         gtKvsToBeam:load(?ranksCfg, fieldIdx(Fields, 1, CfgList)),
         ok;
      _Cnt ->
         {error, started}
   end.

initRank(RankType) ->
   gen_srv:call(rankMgr, {mInitRank, RankType}).

%% 根据排行榜类型更新分数 需要根据key 分配到指定的排行榜工程进程执行
updateScore(RankType, Key, Score) ->
   WorkName = ?ranksCfg:getV(erlang:phash2(Key, ?ranksCfg:getV(?workCnt)) + 1),
   %% 这里就看业务层面是否需要同步更新分数了 需要根据key 分配到指定的排行榜工程进程执行
   RankPos = ?ranksCfg:getV(RankType),
   gen_srv:cast(WorkName, {mUpdateScore, RankPos, Score}).

%% 更新非排行榜分数的其他其他字段的信息 需要根据key 分配到指定的排行榜工程进程执行
updateInfo(Key, RecordKvs) ->
   WorkName = ?ranksCfg:getV(erlang:phash2(Key, ?ranksCfg:getV(?workCnt)) + 1),
   %% 这里就看业务层面是否需要同步更新信息了
   gen_srv:cast(WorkName, {mUpdateInfo, RecordKvs}).

%% 根据排行榜类型 获取指定数量 指定页的排行榜的信息 不需要在排行榜工作进程执行
getRankInfo(RankType, Cnt, Page, PageInfo) ->

   ok.

