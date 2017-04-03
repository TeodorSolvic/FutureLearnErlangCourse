%%%-------------------------------------------------------------------
%%% @author alex.vekeenko@gmail.com
%%%-------------------------------------------------------------------
-module(nub).
-author("alex").

%% API
-export([in_list/2, nub_first/1, nub_last/1]).
-export([nub_first_test/0, nub_last_test/0]).

% check is V already in list
in_list(_V,[])-> false;
in_list(V,[X|_Xs]) when V == X -> true;
in_list(V, [X|Xs]) when V /= X -> in_list(V, Xs).

% except the first instance
nub_first([], R)->R;
nub_first([X|Xs], R)->
  C = in_list(X, R),
  case C of
    true ->
      nub_first(Xs, R);
    false ->
      nub_first(Xs,[X|R])
  end.
nub_first(X)->lists:reverse(nub_first(X,[])).

% except the final instance
nub_last(X)->lists:reverse(nub_first(lists:reverse(X))).

% simple tests
nub_first_test()-> nub_first([1,2,2,3,21,3,333,3,3]) == [1,2,3,21,333].
nub_last_test()-> nub_last([1,2,2,3,21,3,333,3,3]) == [1,2,21,333,3].
