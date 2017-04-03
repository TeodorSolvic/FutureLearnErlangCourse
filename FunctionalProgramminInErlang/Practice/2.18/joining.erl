%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(joining).
-author("alex").

%% API
-export([join/2, concat/1]).

add_to_head([],R)->R;
add_to_head([X|Xs], R) ->add_to_head(Xs, [X|R]).

join(X, Y)->lists:reverse(add_to_head(Y, lists:reverse(X))).

concat([], R)->R;
concat([X|Xs], R) -> concat(Xs, join(R, X)).

concat(X)->concat(X,[]).
