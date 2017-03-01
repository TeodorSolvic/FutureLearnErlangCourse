%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(product).
-author("alex").

%% API
-export([product/1, product_tail/1]).

product([])->
  1;
product([X|Xs])->
  X * product(Xs).

product_tail([], P)->
  P;

product_tail([X|Xs], P)->
  product_tail(Xs, P*X).

product_tail(X)->
  product_tail(X, 1).
