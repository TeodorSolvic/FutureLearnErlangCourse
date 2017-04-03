-module(hof).
-export([add/1,times/1,compose/2,id/1,iterate/1, compose_list/1, twice/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) -> X.

iterate(0) -> fun id/1;
iterate(1) -> fun id/1;
iterate(N) -> fun(F) ->  iter(N,F) end.

iter(2,F)  ->  compose(F,F);
iter(N,F) -> compose(iter(N-1,F),F).

compose_list(X, [])->X;
compose_list(X, [F|Fs])->compose_list(F(X), Fs).

compose_list([]) -> fun(X)->X end;
compose_list([F|Fs]) -> fun (X) -> compose_list(X, [F|Fs]) end.

twice(F) -> fun (X) ->F(F(X)) end.
