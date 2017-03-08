%%%-------------------------------------------------------------------
%%% @author alex
%%%-------------------------------------------------------------------
-module(worker).
-author("alex").

%% API
-export([process/1]).


%% test(N)->
%%   S = index:get_file_contents(N),
%%   index:show_file_contents(S).

process(F)->
  S = index:get_file_contents(F),
  lower_case(lists:append(get_words(S))).



lower_case([])->[];
lower_case([X|Xs])->
  [string:to_lower(X)|lower_case(Xs)].

get_words([])->[];
get_words([X|Xs])->
  [string:tokens(X, " ")|get_words(Xs)].
