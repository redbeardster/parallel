-module(parallel).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([process_task/2]).
-record(state, {pid}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

process_task(Function, ArgList) ->
  gen_server:call(?SERVER, {process_task, Function, ArgList}).

init(Args) ->

    self() ! {new_task},
    {ok, Args}.

handle_call({process_task, Function, ArgList}, From, State) ->

  supervisor:start_child(task_sup, [From, Function, ArgList]),

  {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({new_task}, State) ->

  Task_sup_spec = {task_sup, {task_sup, start_link, []},permanent, 5000, supervisor, [task_sup]},
  {ok, Pid} = supervisor:start_child(parallel_sup, Task_sup_spec),
  io:format("Started child supervisor: ~p~n", [Pid]),

  unlink(Pid),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


