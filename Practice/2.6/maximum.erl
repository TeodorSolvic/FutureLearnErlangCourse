%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(maximum).
-author("alex").

%% API
-export([maximum/1, maximum_tail/1]).

maximum([X|[]])->
  X;
maximum([X|Xs])
  ->erlang:max(X, maximum(Xs)).


maximum_tail([X|[]], M)->
  erlang:max(X, M);

maximum_tail([X|Xs], M)->
  maximum_tail(Xs, erlang:max(X, M)).

maximum_tail([X|Xs])->
  maximum_tail([X|Xs], X-1).
