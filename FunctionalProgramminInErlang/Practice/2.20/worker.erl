%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(worker).
-include_lib("eunit/include/eunit.hrl").
-author("alex").

%% API
-export([process/1, sort_occurs/1]).

% You can run test with:
% c(worker).
% worker:test().

process(File)->
  WORDS_LISTS = prepare_words(lines_to_words_list(index:get_file_contents(File))),
  ELEMENTS_TO_INDEX = lists:usort(lists:append(WORDS_LISTS)),
  entry(ELEMENTS_TO_INDEX, WORDS_LISTS).

entry([], _S)->[];
entry([X|Xs], S)->[{X, lists:reverse(sort_occurs(lists:reverse(occurs(X, S))))}|entry(Xs, S)].

sort_occurs(X)->sort_occurs(X,[],[]).

sort_occurs([],[],Acc)->Acc;
sort_occurs([],[X|[]],Acc)->[{X,X}|Acc];
sort_occurs([],[X|Xs],Acc)->[{lists:last(Xs),X}|Acc];
sort_occurs([X|[]],[],Acc)->[{X,X}|Acc];
sort_occurs([X|[]],H,Acc)->sort_occurs([],[X|H],Acc);
sort_occurs([X,Y|Xs],H, Acc) when Y == X+1 -> sort_occurs([Y|Xs],[X|H], Acc);
sort_occurs([X,Y|Xs],[], Acc)-> sort_occurs([Y|Xs],[], [{X,X}|Acc]);
sort_occurs([X,Y|Xs], H, Acc)-> sort_occurs([Y|Xs],[], [{lists:last(H),X}|Acc]).


occurs(X, S)->occurs(X, S, [], 1).
occurs(_X, [], Acc, _I)->Acc;
occurs(X, [Y|Ys], Acc, I) ->
  case lists:member(X, Y) of
    true -> occurs(X, Ys, [I|Acc], I+1);
    false -> occurs(X, Ys, Acc, I+1)
  end.

prepare_words([])->[];
prepare_words([X|Xs])->[remove_short(lower_case(remove_non_chars(X)))|prepare_words(Xs)].

lines_to_words_list([])->[];
lines_to_words_list([X|Xs]) -> [string:tokens(X, " ")|lines_to_words_list(Xs)].

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

% All tests
clear_str_test() ->
  ?assert(clear_str("1,@#$%^&*()") == "").

remove_non_chars_test() ->
  ?assert(remove_non_chars(["a1,b@#$%^&c*()"]) == ["abc"]).

remove_short_test() ->
  ?assert(remove_short(["one","two","three","four"]) == ["three","four"]).

lower_case_test() ->
  ?assert(lower_case(["One","TWO","thRee","fouR"]) == ["one","two","three","four"]).

lines_to_words_list_test() ->
  ?assert(lines_to_words_list(["one two","three four"]) == [["one", "two"],["three", "four"]]).

prepare_words_test() ->
  ?assert(prepare_words([["one", "12313", "two", "five"],["three", "QWEReq", "four"]]) == [["five"],["three","qwereq","four"]]).

occurs_test() ->
  ?assert(occurs("test", [["test"],["something"],["test"]]) == [3, 1]).

sort_occurs_test() ->
  ?assert(sort_occurs([1, 2, 3, 5, 7]) == [{7,7},{5,5},{1,3}]).

entry_test() ->
  ?assert(entry(["test"], [["test"],["something"],["test"]]) == [{"test",[{1,1}, {3,3}]}]).