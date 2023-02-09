-module(rankTest).

-behavior(gen_srv).

-include("ranks.hrl").

-export([
   start_link/3
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
   io:format("start test ~p ~p ~p ~n", [Cnt, Num, erlang:system_time(second)]),
   ranks:startWork(erlang:system_info(schedulers)) ,
   [ranks:initRank(RankType, ceil(Limit * 0.6), Limit) || RankType <- ?AllRankType],
   doTest(Cnt, Num),
   doWait(Cnt, 0).

doTest(0, _) ->
   ok;
doTest(Cnt, Num) ->
   start_link(self(), Cnt, Num),
   doTest(Cnt - 1, Num).

doWait(Cnt, Sum) ->
   receive
      over ->
         if Cnt == Sum + 1 ->
               io:format("end test ~p ~p ~n", [Cnt, erlang:system_time(second)]);
            true ->
               %  io:format("doWait test ~p ~p ~n", [Cnt, Sum + 1]),
               doWait(Cnt, Sum + 1)
         end

   end.

-record(state, {parent, id, num}).

%% ********************************************  API *******************************************************************
start_link(Parent, Id, Num) ->
   gen_srv:start(?MODULE, {Parent, Id, Num}, []).

%% ********************************************  callback **************************************************************
init({Parent, Id, Num}) ->
   ranks:updateInfo(Id, {?publicInfoPos, {Id, Num, self()}}),
   {ok, #state{parent = Parent, id = Id, num = Num}, 0}.

handleCall(_Msg, _State, _FROM) ->
   {reply, ok}.

handleCast(_Msg, _State) ->
   kpS.

handleInfo(timeout, #state{parent = Parent, id = Id, num = Num} = State) ->
   loopScore(Num, Id),
   Parent ! over,
   {stop, normal, State};
handleInfo(_Msg, _State) ->
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
%% ****************************************************** logic ********************************************************
loopScore(0, _Id) ->
   ok;
loopScore(Num, Id) ->
   RankType = lists:nth(rand:uniform(3), ?AllRankType),
   Score = rand:uniform(10000),
   ranks:updateScore(RankType, Id, {Score, Id}),
   loopScore(Num - 1, Id).


