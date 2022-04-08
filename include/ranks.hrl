-ifndef(RANKS_H_).
-define(RANKS_H_, true).

-record(rank_info, {
   key :: integer()
   , otherInfo :: term()
   , score1 :: term()
   , score2 :: term()
   , scoreN :: term()
}).


-endif.
