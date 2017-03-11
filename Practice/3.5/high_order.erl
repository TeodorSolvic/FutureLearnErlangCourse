%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(high_order).
-author("alex").

%% API
-export([doubleAll/1, evens/1, product/1]).

doubleAll(List)->lists:map(fun (X)-> 2*X end, List).

evens(List)->lists:filter(fun(X)-> X rem 2 == 0 end, List).

product(List)->lists:foldr(fun(X, Y) -> X*Y end, 1, List).