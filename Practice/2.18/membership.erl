%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(membership).
-author("alex").

%% API
-export([member/2]).

member(_V,[])-> false;
member(V,[X|_Xs]) when V == X -> true;
member(V, [X|Xs]) when V /= X -> member(V, Xs).
