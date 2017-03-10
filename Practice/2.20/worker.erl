%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(worker).
-author("alex").

%% API
-export([process/1, sort_occurs/1]).

process(F)->
  S = index:get_file_contents(F),
  WORDS_LISTS = get_words(S),
  CLEAR_WORDS_LISTS = clear_words(WORDS_LISTS),
  R = lists:usort(lists:append(CLEAR_WORDS_LISTS)),
  entry(R, CLEAR_WORDS_LISTS).

entry([], _S)->[];
entry([X|Xs], S)->[{X, lists:reverse(sort_occurs(lists:reverse(occurs(X, S))))}|entry(Xs, S)].

sort_occurs(X)->sort_occurs(X,[],[]).

sort_occurs([],[],Acc)->Acc;
sort_occurs([],[X|[]],Acc)->[{X,X}|Acc];
sort_occurs([],[X|Xs],Acc)->[{lists:last(Xs),X}|Acc];
sort_occurs([X,Y|Xs],H, Acc) when Y == X+1 -> sort_occurs([Y|Xs],[X|H], Acc);
sort_occurs([X,Y|Xs],[], Acc)-> sort_occurs([Y|Xs],[], [{X,X}|Acc]);
sort_occurs([X,Y|Xs], H, Acc)-> sort_occurs([Y|Xs],[], [{lists:last(H),X}|Acc]);
sort_occurs([X|[]],[],Acc)->[{X,X}|Acc];
sort_occurs([X|[]],H,Acc)->sort_occurs([],[X|H],Acc).

occurs(X, S)->occurs(X, S, [], 1).
occurs(_X, [], Acc, _I)->Acc;
occurs(X, [Y|Ys], Acc, I) ->
  case lists:member(X, Y) of
    true -> occurs(X, Ys, [I|Acc], I+1);
    false -> occurs(X, Ys, Acc, I+1)
  end.

clear_words([])->[];
clear_words([X|Xs])->[remove_short(lower_case(remove_non_chars(X)))|clear_words(Xs)].

get_words([])->[];
get_words([X|Xs]) -> [string:tokens(X, " ")|get_words(Xs)].

lower_case([]) -> [];
lower_case([X|Xs]) -> [string:to_lower(X)|lower_case(Xs)].

remove_short([])->[];
remove_short([X|Xs])->
  case X of
    [_A,_B,_C|[]]->remove_short(Xs);
    [_A,_B|[]]->remove_short(Xs);
    [_A|[]]->remove_short(Xs);
    _->[X|remove_short(Xs)]
  end.

remove_non_chars([])->[];
remove_non_chars([X|Xs])->
  C = clear_str(X),
  case C of
    [] -> remove_non_chars(Xs);
    _ -> [C|remove_non_chars(Xs)]
  end.

clear_str([])->[];
clear_str([X|Xs]) when X >= 65, X =< 90 ; X >= 97, X =< 122 -> [X|clear_str(Xs)];
clear_str([_X|Xs]) -> clear_str(Xs).