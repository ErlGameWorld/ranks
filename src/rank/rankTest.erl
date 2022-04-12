-module(rankTest).

-behavior(gen_srv).

-include("ranks.hrl").

-export([
   start_link/2
   , start/3
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

start(Cnt, Num, Limit) ->
   ranks:startWork(4) ,
   [ranks:initRank(RankType, ceil(Limit * 0.6), Limit) || RankType <- ?AllRankType],
   doTest(Cnt, Num).

doTest(0, _) ->
   ok;
doTest(Cnt, Num) ->
   start_link(Cnt, Num),
   doTest(Cnt - 1, Num).

-record(state, {id, num}).

%% ********************************************  API *******************************************************************
start_link(Id, Num) ->
   gen_srv:start_link(?MODULE, {Id, Num}, []).

%% ********************************************  callback **************************************************************
init({Id, Num}) ->
   ranks:updateInfo(Id, {?publicInfoPos, {Id, Num, self()}}),
   {ok, #state{id = Id, num = Num}, 0}.

handleCall(_Msg, _State, _FROM) ->
   {reply, ok}.

handleCast(_Msg, _State) ->
   kpS.

handleInfo(timeout, #state{id = Id, num = Num} = State) ->
   RankType = lists:nth(rand:uniform(3), ?AllRankType),
   Score = rand:uniform(10000),
   ranks:updateScore(RankType, Id, {Score, Id}),
   NewNum = Num - 1,
   case NewNum < 0 of
      true ->
         io:format("test over ~p~n", [Id]),
         {stop, normal, State};
      _ ->
         {noreply, State#state{num = NewNum}, 0}
   end;
handleInfo(_Msg, _State) ->
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
%% ****************************************************** logic ********************************************************

