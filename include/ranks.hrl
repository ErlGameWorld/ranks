-ifndef(RANKS_H_).
-define(RANKS_H_, true).

%% 相关配置模块名
-define(ranksCfg, ranksCfg).
-define(ranksLimit, ranksLimit).

%% 工作者数量
-define(workCnt, workCnt).

%% 公共信息字段
-define(publicInfoPos, #etsRankRecord.publicInfo).

%% 三元表达式
-define(IIF(Cond, Ret1, Ret2), (case Cond of true -> Ret1; _ -> Ret2 end)).

-define(etsRankInfo, etsRankInfo).

%% 排行榜类型 需要与该定义中的 rankTypeXScore 名字一样
-record(etsRankRecord, {
   key :: integer()
   , publicInfo :: term()
   , rankType1Score :: term()
   , rankType2Score :: term()
   , rankTypeNScore :: term()
}).

-define(AllRankType, [rankType1Score, rankType2Score, rankTypeNScore]).

-record(rankInfo, {
   rank :: integer()
   , key :: integer()
   , publicInfo :: term()
   , rankTypeScore :: term()
}).

-endif.
