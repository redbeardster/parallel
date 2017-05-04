-module(worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-export([init/1]).
-export([handle_cast/2, handle_call/3, terminate/2,handle_info/2,code_change/3]).
-export([start_link/3]).
-export([do_it/3]).

-record(state, {from, count}).

do_it(Ppid, Function, Argument) ->
  Ppid ! {response, erlang:apply(Function, Argument)}.

start_link(From, Function, ArgList) ->
       gen_server:start_link(?MODULE, [From, Function, ArgList], []).

init([From, Function, ArgList]) ->

    ets:new(result, [set, named_table, public]),

    process_flag(trap_exit, true),
    PidList = [spawn_link(worker, do_it, [self(), Function, [Argument]]) || Argument <- ArgList],

    {ok, #state{from = From, count = length(PidList)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({grrr}, State) ->
  io:format("Stopping~n"),
  {stop, normal, State };

handle_cast(_Msg, State) ->
  {noreply, State }.

handle_info({response, Result}, State) ->
%%  io:format("Result: ~p~n", [Result]),
  ets:insert(result, {Result}),
  {noreply, State };

handle_info({'EXIT', _Pid, _Reason}, State = #state{from = From, count = Count}) ->
  case Count-1 of
   0   ->
          Res =  [Arg || [{Arg}] <- ets:match(result, '$1') ],
          gen_server:reply(From, Res),
         ets:delete(result),
         exit(self(), kill);
    _ -> ok
  end,

  {noreply, State#state{count = Count-1} };

handle_info(_Message, State) ->
  { noreply, State }.

code_change(_OldVersion, State, _Extra) ->

  { ok, State }.

terminate(_Reason, _State) ->
     ok.
