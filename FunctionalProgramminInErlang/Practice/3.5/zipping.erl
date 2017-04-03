%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(zipping).
-author("alex").

%% API
-export([zip/2, zip_with/3, zip_with_map/3, zip_combination/2]).

zip([_X|_Xs],[])->[];
zip([],[_Y|_Ys])->[];
zip([X|Xs],[Y|Ys])->[{X,Y}|zip(Xs, Ys)].

zip_with(_F, [_X|_Xs],[])->[];
zip_with(_F, [],[_Y|_Ys])->[];
zip_with(F, [X|Xs],[Y|Ys])->[F(X,Y)|zip_with(F, Xs, Ys)].

zip_with_map(F,Xs,Ys) -> lists:map(fun({X,Y}) -> F(X,Y) end, zip(Xs,Ys)).

zip_combination(Xs,Ys) -> zip_with(fun(X,Y) -> {X,Y} end, Xs, Ys).
