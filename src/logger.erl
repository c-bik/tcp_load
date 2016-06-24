-module(logger).
-behaviour(gen_server).

-include("tcp_load.hrl").

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/1]).

-record(state, {type, cons_sup}).
-define(PERIOD, 5000).

start_link(Type) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Type], []).

init([Type]) ->
    process_flag(trap_exit, true),
    ?L("started"),
    case Type of
        server ->
            [ListenerSupPid] =
            [P || {{ranch_listener_sup,tcp_echo}, P, supervisor,
                   [ranch_listener_sup]} <- supervisor:which_children(ranch_sup)],
            [ConnsSupPid] =
            [P1 || {ranch_conns_sup,P1,supervisor,[ranch_conns_sup]}
                   <- supervisor:which_children(ListenerSupPid)],
            {ok, #state{type = server, cons_sup = ConnsSupPid}, 1000};
        client ->
            {ok, #state{type = client}, ?PERIOD}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, #state{type = server, cons_sup = ConnsSupPid} = State) ->
    ?L("server ~p", [stats(ConnsSupPid)]),
    {noreply, State, ?PERIOD};
handle_info(_Info, #state{type = client} = State) ->
    ?L("client ~p", [stats(tcp_load)]),
    {noreply, State, ?PERIOD}.

terminate(Reason, _State) ->
    ?L("stopped ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stats(SupRef) ->
    Children = [P || {_,P,worker,[tcp_client]} <- supervisor:which_children(SupRef)],
    StatsSum =
    lists:foldl(
      fun(Pid, Acc) ->
              {links, [_,Socket]} = process_info(Pid, links),
              {ok, OptValues} = inet:getstat(Socket),
              {Msgs, AvgDly} = gen_server:call(Pid, stat),
              PStat = (maps:from_list(OptValues))#{msgs => Msgs, avg_rxt => AvgDly},
              if map_size(Acc) > 0 ->
                     maps:map(
                       fun(K, V) ->
                               V + maps:get(K, PStat)
                       end, Acc);
                 true ->
                     PStat
              end
      end, #{}, Children),
    Childs = length(Children),
    (maps:map(fun(msgs, V) -> V;
                 (_, V) -> V / Childs
              end, StatsSum))#{connections => Childs}.
