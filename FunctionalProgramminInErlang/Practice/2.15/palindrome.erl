%%%-------------------------------------------------------------------
%%% @author alex
%%%-------------------------------------------------------------------
-module(palindrome).
-author("alex").

%% API
-export([palindrome/1]).

clear([])->[];
clear([X|Xs]) when X >= 65, X =< 90 ; X >= 97, X =< 122 -> [X|clear(Xs)];
clear([_X|Xs]) -> clear(Xs).

palindrome(X)->
  S = string:to_lower(clear(X)),
  S == lists:reverse(S).

