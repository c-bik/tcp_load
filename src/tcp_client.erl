-module(tcp_client).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("tcp_load.hrl").

%% ------------------------------------------------------------------
%% ranch_protocol Function Exports
%% ------------------------------------------------------------------
-export([start_link/4, init/4]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([connect/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% ranch_protocol API Function Definitions
%% ------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
	%% Perform any required state initialization here.
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
    {ok, {PeerIp, Port}} = Transport:peername(Socket),
    ?L("accept ~p:~p", [PeerIp,Port]),
    process_flag(trap_exit, true),
	gen_server:enter_loop(?MODULE, [], {state, Socket, Transport}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
connect(Id, SrcIp, Ip, Port) ->
    gen_server:start_link(?MODULE, [Id, SrcIp, Ip, Port], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Id, SrcIp, Ip, Port]) when is_integer(Id) ->
    process_flag(trap_exit, true),
    {ok, {{Id, SrcIp, Ip, Port}}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, {{Id, SrcIp,Ip, Port}}) ->
    T1 = os:timestamp(),
    case gen_tcp:connect(
            Ip, Port,
            [{packet, 0}, binary, {active, once}, {ip, SrcIp}]) of
        {ok, Sock} ->
            {ok, {_, LPort}} = inet:sockname(Sock),
            T2 = os:timestamp(),
            ConDelay = timer:now_diff(T2, T1),
            if ConDelay > 100000 ->
                ?L("[~p] connect ~s:~p <- ~p in ~p us",
                   [Id, inet:ntoa(Ip), Port, LPort, ConDelay]);
               true ->
                ?L("[~p] connect ~s:~p <- ~p",
                   [Id, inet:ntoa(Ip), Port, LPort])
            end,
            erlang:send_after(5000, self(), msg),
            {noreply, {Ip, Port, Sock}};
        {error, Reason} ->
            {stop, Reason, {{Id, SrcIp,Ip, Port}}}
    end;
handle_info(msg, {Ip, Port, Sock}) ->
    Data = term_to_binary(os:timestamp()),
    case gen_tcp:send(Sock, Data) of
        ok ->
            erlang:send_after(5000, self(), msg),
            {noreply, {Ip, Port, Sock}};
        {error, Reason} ->
            {stop, Reason, {Ip, Port, Sock}}
    end;
handle_info({tcp_error, Sock, Reason}, State) ->
    catch gen_tcp:close(Sock),
    {stop, Reason, State};
handle_info({tcp_closed, Sock}, State) ->
    catch gen_tcp:close(Sock),
    {stop, normal, State};
handle_info({tcp, Sock, Data}, {Ip, Port, Sock}) ->
    try
        RTT = timer:now_diff(os:timestamp(), binary_to_term(Data)),
        if RTT > 1000000 -> ?L("~p Client RTT ~p us", [self(), RTT]); true -> ok end
    catch
        _:_ ->
            ?L("ERROR erlang term ~p Client RX : ~p", [self(), byte_size(Data)])
    end,
    case inet:setopts(Sock, [{active, once}]) of
        ok -> {noreply, {Ip, Port, Sock}};
        Error -> {stop, Error, {Ip, Port, Sock}}
    end;
handle_info({tcp, Sock, Data}, {state, Sock, Transport} = State) ->
    try
        RXT = timer:now_diff(os:timestamp(), binary_to_term(Data)),
        if RXT > 500000 -> ?L("~p Server RX ~p us", [self(), RXT]); true -> ok end
    catch
        _:_ ->
            ?L("~p Accept RX : ~p", [self(), byte_size(Data)])
    end,
    case gen_tcp:send(Sock, Data) of
        ok ->
            case Transport:setopts(Sock, [{active, once}]) of
                ok -> {noreply, State};
                Error -> {stop, Error, State}
            end;
        {error, Reason} ->
            {stop, Reason, State}
    end.

terminate(Reason, State) ->
    ?L("die ~p ~p", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

