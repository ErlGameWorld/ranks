-module(ranks).

-include("ranks.hrl").

-export([
   start/0
   , stop/0
   , startWork/1             %% 创建指定数量的排行榜工作者
   , initRank/3              %% 创建新的类型排行榜
   , updateScore/3           %% 更新某类型的排行榜分数 todo 这里可能差个第一次插入的接口函数
   , updateInfo/2            %% 更新公共信息
   , getRankInfo/5           %% 获取排行某页的信息
]).

start() ->
   application:ensure_all_started(ranks).

stop() ->
   application:stop(ranks).

-spec workName(Idx :: integer()) -> atom().
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
         NameList = [{Idx, workName(Idx)} || Idx <- lists:seq(0, Cnt - 1)],
         [supervisor:start_child(rankWork_sup, [WorkName]) || {_Idx, WorkName} <- NameList],
         CfgList = [{?workCnt, Cnt} | NameList],
         Fields = record_info(fields, etsRankRecord),
         rsKvsToBeam:load(?ranksCfg, fieldIdx(Fields, 2, CfgList)),
         ok;
      _Cnt ->
         {error, started}
   end.

-spec initRank(RankType :: atom(), CntLimit :: non_neg_integer(), CntMax :: non_neg_integer()) -> ok | {error, atom()}.
initRank(RankType, CntLimit, CntMax) ->
   gen_srv:call(rankMgr, {mInitRank, RankType, CntLimit, CntMax}).

%% 根据排行榜类型更新分数 需要根据key 分配到指定的排行榜工程进程执行 Score 是个元组 {Score1, Score2, ..., Key}
-spec updateScore(RankType :: atom(), Key :: term(), Score :: tuple()) -> no_return().
updateScore(RankType, Key, Score) ->
   WorkName = ?ranksCfg:getV(erlang:phash2(Key, ?ranksCfg:getV(?workCnt))),
   %% 这里就看业务层面是否需要同步更新分数了
   %% 同步请求
   % gen_srv:clfn(WorkName, rankWork, mUpdateScore, [RankType, Key, Score], 1000000000).
   %% 异步请求
   gen_srv:csfn(WorkName, rankWork, mUpdateScore, [RankType, Key, Score]).

%% 更新非排行榜分数的其他其他字段的信息 需要根据key 分配到指定的排行榜工程进程执行
-spec updateInfo(Key :: term(), RecordKvs :: {non_neg_integer(), term()} | [{non_neg_integer(), term()}, ...]) -> no_return().
updateInfo(Key, RecordKvs) ->
   WorkName = ?ranksCfg:getV(erlang:phash2(Key, ?ranksCfg:getV(?workCnt))),
   %% 这里就看业务层面是否需要同步更新信息了
   %% 同步请求
   %%  gen_srv:clfn(WorkName, rank_work, mUpdateInfo, [Key, RecordKvs]),
   %% 异步请求
   gen_srv:csfn(WorkName, rankWork, mUpdateInfo, [Key, RecordKvs]).

%% 根据排行榜类型 获取指定数量 指定页的排行榜的信息 不需要在排行榜工作进程执行  返回{IsOver 是否获取到所有数据, SelfRank  -1 未上榜 0 未查询自己排名 名次, RankData 排行榜数据}.
-spec getRankInfo(RankType :: atom(), MyKey :: term(), Cnt :: non_neg_integer(), Page :: integer(), PageInfo :: binary()) -> {IsOver :: boolean(), SelfRank :: integer(), RankData :: list()}.
getRankInfo(RankType, MyKey, Cnt, Page, PageInfo) ->
   rankWork:mGetRankInfo(RankType, MyKey, Cnt, Page, PageInfo).

