%%%-------------------------------------------------------------------
%%% @author alex
%%%-------------------------------------------------------------------
-module(receiver).
-author("alex").

%% API
-export([receiver/0, receiver_case/0, receiver_top_level/0, processing/0]).

receiver() ->
  receive
    Msg -> io:format("message:~w~n",[Msg]),
    receiver()
  end.

receiver_case() ->
  timer:sleep(10000),
  receive
    Msg ->
      case Msg of
        stop -> io:format("stopped~n");
        _ ->
          io:format("message:~w~n",[Msg]),
          receiver_case()
      end
  end.

receiver_top_level() ->
  timer:sleep(10000),
  receive
      stop ->
        io:format("stopped~n");
      Msg ->
        io:format("message:~w~n",[Msg]),
        receiver_case()
  end.

processing_receiver(First, Second)->
  receive
    {First, S}
      -> io:format("message:~w~n",[S]),
      processing_receiver(Second, First);
    _ ->
      processing_receiver(First, Second)
  end.

processing()->
  processing_receiver(first, second).
