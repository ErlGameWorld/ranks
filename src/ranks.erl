-module(ranks).

-include("ranks.hrl").

-export([
   start/0
   , stop/0
   , startWork/1             %% 创建指定数量的排行榜工作者
   , initRank/3              %% 创建新的类型排行榜
   , updateScore/3           %% 更新某类型的排行榜分数
   , updateInfo/2            %% 更新公共信息
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

initRank(RankType, CntLimit, CntMax) ->
   gen_srv:call(rankMgr, {mInitRank, RankType, CntLimit, CntMax}).

%% 根据排行榜类型更新分数 需要根据key 分配到指定的排行榜工程进程执行
updateScore(RankType, Key, Score) ->
   WorkName = ?ranksCfg:getV(erlang:phash2(Key, ?ranksCfg:getV(?workCnt)) + 1),
   RankPos = ?ranksCfg:getV(RankType),
   %% 这里就看业务层面是否需要同步更新分数了
   %% 同步请求
   %%  gen_srv:clfn(WorkName, rank_work, mUpdateScore, [Key, RankPos, Score]),
   %% 异步请求
   gen_srv:csfn(WorkName, rank_work, mUpdateScore, [Key, RankPos, Score]).

%% 更新非排行榜分数的其他其他字段的信息 需要根据key 分配到指定的排行榜工程进程执行
updateInfo(Key, RecordKvs) ->
   WorkName = ?ranksCfg:getV(erlang:phash2(Key, ?ranksCfg:getV(?workCnt)) + 1),
   %% 这里就看业务层面是否需要同步更新信息了
   %% 同步请求
   %%  gen_srv:clfn(WorkName, rank_work, mUpdateInfo, [Key, RecordKvs]),
   %% 异步请求
   gen_srv:csfn(WorkName, rank_work, mUpdateInfo, [Key, RecordKvs]).

%% 根据排行榜类型 获取指定数量 指定页的排行榜的信息 不需要在排行榜工作进程执行
getRankInfo(RankType, Cnt, Page, PageInfo) ->
   rankWork:mGetRankInfo(RankType, Cnt, Page, PageInfo).

