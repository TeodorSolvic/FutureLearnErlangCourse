%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(list_functions).
-author("alex").

%% API
-export([double/1, evens/1, sort/1]).

double([])->[];
double([N|Ns])->[N*2|double(Ns)].

evens([])->[];
evens([N|Ns]) when N rem 2 == 0-> [N|evens(Ns)];
evens([_N|Ns])->evens(Ns).
