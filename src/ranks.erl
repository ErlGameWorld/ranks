-module(ranks).

-export([
   start/0
   , stop/0
   ,
   , initRank/1                %% 创建新的排行榜
   , updateScore/3           %% 更新分数
   , updateInfo/2            %% 更新其他分数
   , getRankInfo/3           %% 获取排行某页的信息
]).

start() ->
   application:ensure_all_started(ranks).

stop() ->
   application:stop(ranks).

startWork(Cnt) ->
   ok.

initRank(RankType) ->
   ok.

updateScore(RankType, Key, Score) ->
   ok.

updateInfo(Key, Infos) ->
   ok.

getRankInfo(RankType, Cnt, PageInfo) ->
   ok.

