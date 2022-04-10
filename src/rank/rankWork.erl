-module(rankWork).

-behavior(gen_srv).

-include("ranks.hrl").

-export([
   start_link/1
   , mUpdateScore/3
   , mUpdateInfo/2
   , mGetRankInfo/5
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-record(state, {}).

%% ********************************************  API *******************************************************************
start_link(SrvName) ->
   gen_srv:start_link({local, SrvName}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   {ok, #state{}}.

handleCall(_Msg, _State, _FROM) ->
   {reply, ok}.

handleCast(_Msg, _State) ->
   kpS.

handleInfo(_Msg, _State) ->
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
%% ****************************************************** logic ********************************************************

mUpdateScore(Key, RankType, Score) ->
   {_RankLimit, RankMax} = ?ranksLimit:getV(RankType),
   RankPos = ?ranksCfg:getV(RankType),
   case ets:lookup(etsRankInfo, Key) of
      [RankRecord] ->
         OldScore = element(RankPos, RankRecord),
         ets:delete(RankType, OldScore),

         RankSize = ets:info(RankType, size),
         case RankSize >= RankMax of
            true ->
               FirstKey = ets:first(RankType),
               case Score > FirstKey of
                  true ->
                     ets:insert(RankType, Score),
                     ets:delete(RankType, FirstKey);
                  _ ->
                     ignore
               end;
            _ ->
               ets:insert(RankType, Score)
         end,
         ets:update_element(etsRankInfo, Key, {RankPos, Score});
      _ ->
         %% 插入新的数据
         RankSize = ets:info(RankType, size),
         case RankSize >= RankMax of
            true ->
               FirstKey = ets:first(RankType),
               case Score > FirstKey of
                  true ->
                     ets:insert(RankType, Score),
                     ets:delete(RankType, FirstKey),
                     NewRecord = #etsRankRecord{key = Key},
                     RankRecord = setelement(RankPos, NewRecord, Score),
                     ets:insert(etsRankInfo, RankRecord);
                  _ ->
                     ignore
               end;
            _ ->
               ets:insert(RankType, Score),
               NewRecord = #etsRankRecord{key = Key},
               RankRecord = setelement(RankPos, NewRecord, Score),
               ets:insert(etsRankInfo, RankRecord)
         end
   end,
   {mayReply, ok}.

mUpdateInfo(Key, RecordKvs) ->
   ets:update_element(etsRankInfo, Key, RecordKvs),
   {mayReply, ok}.

mGetRankInfo(RankType, MyKey, Cnt, Page, PageInfo) ->
   {RankLimit, _RankMax} = ?ranksLimit:getV(RankType),

   %% 请求第一页的时候 附加上自己的排名
   SelfRank =
      case Page of
         0 ->
            case ets:lookup_element(etsRankInfo, MyKey) of
               [RankRecord] ->
                  RankPos = ?ranksCfg:getV(RankType),
                  CurScore = element(RankPos, RankRecord),
                  MyIndex = ets:select_count(RankType, [{{'$1', '$2'}, [{'>=', '$1', CurScore}], [true]}]),
                  max(RankLimit, MyIndex);
               _ ->
                  -1
            end;
         _ ->
            0
      end,

   MS =
      case PageInfo == 0 orelse PageInfo of
         true ->
            [{'$1', [], ['$1']}];
         <<"">> ->
            [{'$1', [], ['$1']}];
         _ ->
            try binary_to_term(PageInfo) of
               KeyTerm when KeyTerm == <<"">>; KeyTerm == "" ->
                  [{'$1', [], ['$1']}];
               KeyTerm ->
                  % ets:fun2ms(fun({K, _V} = T) when K < KeyTerm -> T end)
                  [{{'$1', '$2'}, [{'<', '$1', KeyTerm}], ['$_']}]
            catch _C:_R ->
               [{'$1', [], ['$1']}]
            end
      end,

   case ets:select_reverse(RankType, MS, Cnt) of
      '$end_of_table' ->
         {true, {<<"">>, []}};
      {KeyIds, '$end_of_table'} ->
         {true, SelfRank, makeRankData(KeyIds, Cnt * Page + 1)};
      {KeyIds, _NextKey} ->
         Length = length(KeyIds),
         if
            Length < Cnt ->
               {true, SelfRank, makeRankData(KeyIds, Cnt * Page + 1)};
            true ->
               NewCurCnt = Page * Cnt + Length,
               case NewCurCnt >= RankLimit of
                  true ->
                     {true, SelfRank, makeRankData(KeyIds, Cnt * Page + 1)};
                  _ ->
                     {false, SelfRank, makeRankData(KeyIds, Cnt * Page + 1)}
               end
         end
   end.

makeRankData(KeyIds, Idx) ->
   makeRankData(KeyIds, Idx, <<"">>, []).

makeRankData([], _Idx, LastKey, Acc) ->
   case LastKey of
      <<"">> ->
         {LastKey, lists:reverse(Acc)};
      _ ->
         {erlang:term_to_binary(LastKey), lists:reverse(Acc)}
   end;
makeRankData([{CurKey, Score} | KeyIds], Idx, LastKey, Acc) ->
   case ets:lookup(ets_pub_rank_info, CurKey) of
      [OneData] ->
         makeRankData(KeyIds, Idx + 1, CurKey, [#rankInfo{rank = Idx, key = CurKey, publicInfo = element(?publicInfoPos, OneData), rankTypeScore = Score} | Acc]);
      _ ->
         makeRankData(KeyIds, Idx, LastKey, Acc)
   end.