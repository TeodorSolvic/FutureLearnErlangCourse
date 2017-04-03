%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(bits).
-author("alex").

%% API
-export([bits/1]).

bits(0) ->
  0;
bits(X) ->
  (X band 1) + bits(X bsr 1).
