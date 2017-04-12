%% Based on code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([allocate/0,deallocate/1,stop/0]).
-export([init/1, init_client/2, supervisor/0]).

%% Direct start function was removed, now supervisor will handle server start
%% initialize the server.

%% Added supervisor process ID as initialize parameter
init(Supervisor) ->
  process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(Frequencies,Supervisor).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies, Supervisor) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies, Supervisor);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies, Supervisor);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Supervisor, _Reason} -> %% added condition for exit after supervisor die
      io:format("Supervisor was dead, and server have to die too~n");
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid), 
      loop(NewFrequencies,Supervisor)
  end.

%% Functional interface

allocate() -> 
    frequency ! {request, self(), allocate},
    receive 
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) -> 
    frequency ! {request, self(), {deallocate, Freq}},
    receive 
	    {reply, Reply} -> Reply
    end.

stop() -> 
    frequency ! {request, self(), stop},
    receive 
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated}; 
      false ->
        {Free,Allocated} 
    end.

%% added modelling client functions

init_client(Allocate_timeout, Deallocate_timeout)->
  process_flag(trap_exit, true),
  client_allocate(Allocate_timeout),
  client_loop(Allocate_timeout, Deallocate_timeout).

client_loop(Allocate_timeout, Deallocate_timeout) ->
    receive
      {reply, ok} ->
          client_allocate(Allocate_timeout),
          client_loop(Allocate_timeout, Deallocate_timeout);
      {reply, {ok, Freq}} ->
          io:format("aloocated:~w~n",[Freq]),
          client_deallocate(Deallocate_timeout, Freq),
          io:format("dealoocated:~w~n",[Freq]),
          client_loop(Allocate_timeout, Deallocate_timeout);
      {'EXIT', Pid, _Reason} ->
        io:format("Server was dead, here we can do something useful:~w~n",[Pid])
    end.

client_allocate(Timeout) ->
  timer:sleep(Timeout),
  frequency ! {request, self(), allocate}.

client_deallocate(Timeout, Freq) ->
  timer:sleep(Timeout),
  frequency ! {request, self(), {deallocate, Freq}}.

%% added supervisor functions section

supervisor() ->
  process_flag(trap_exit, true),
  supervisor_receiver(supervisor_server_run()).

supervisor_receiver(Pid) ->
  receive
    {'EXIT', Pid, _Reason} ->
      NewPid = supervisor_server_run(),
      io:format("Server was dead, and restarted with new Pid:~w~n:",[NewPid]),
      supervisor_receiver(NewPid)
  end.

supervisor_server_run() ->
  Pid = spawn(frequency, init, [self()]),
  register(frequency, Pid),
  link(Pid),
  Pid.
