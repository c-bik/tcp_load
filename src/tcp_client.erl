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

-record(sst, {transport, msg_cnt = 0, avg_dly = 0, sock}).
init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
	%% Perform any required state initialization here.
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),
    {ok, {PeerIp, Port}} = Transport:peername(Socket),
    ?L("accept ~s:~p", [inet:ntoa(PeerIp),Port]),
    process_flag(trap_exit, true),
	gen_server:enter_loop(?MODULE, [], #sst{sock = Socket, transport = Transport}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-record(cst, {id, sip, ip, port, msg_cnt = 0, avg_dly = 0, sock}).
connect(Id, SrcIp, Ip, Port) ->
    gen_server:start_link(
      ?MODULE, [#cst{id = Id, sip = SrcIp, ip = Ip, port = Port}], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#cst{id = Id} = Cst]) when is_integer(Id) ->
    process_flag(trap_exit, true),
    {ok, Cst, 0}.

handle_call(stat, _From, #sst{msg_cnt = MsgCnt, avg_dly = AvgDly} = Sst) ->
    {reply, {MsgCnt,AvgDly}, Sst};
handle_call(stat, _From, #cst{msg_cnt = MsgCnt, avg_dly = AvgDly} = Cst) ->
    {reply, {MsgCnt,AvgDly}, Cst};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #cst{id = Id, sip = SrcIp, ip = Ip, port = Port} = Cst) ->
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
            {noreply, Cst#cst{sock=Sock}};
        {error, Reason} ->
            {stop, Reason, Cst}
    end;
handle_info(msg, #cst{sock = Sock} = Cst) ->
    Data = term_to_binary(os:timestamp()),
    case gen_tcp:send(Sock, Data) of
        ok ->
            erlang:send_after(5000, self(), msg),
            {noreply, Cst};
        {error, Reason} ->
            {stop, Reason, Cst}
    end;
handle_info({tcp_error, Sock, Reason}, State) ->
    catch gen_tcp:close(Sock),
    {stop, Reason, State};
handle_info({tcp_closed, Sock}, State) ->
    catch gen_tcp:close(Sock),
    {stop, normal, State};
handle_info({tcp, Sock, Data},
            #cst{sock = Sock, msg_cnt = OldMsgCnt, avg_dly = OldAvgDly} = Cst) ->
    {NewMsgCnt, NewAvgDly} =
    try
        RTT = timer:now_diff(os:timestamp(), binary_to_term(Data)),
        {OldMsgCnt + 1,
         (RTT + OldMsgCnt * OldAvgDly) / (OldMsgCnt + 1)}
    catch
        _:_ ->
            ?L("ERROR erlang term ~p Client RX : ~p", [self(), byte_size(Data)]),
            {OldMsgCnt+1, OldAvgDly}
    end,
    case inet:setopts(Sock, [{active, once}]) of
        ok -> {noreply, Cst#cst{msg_cnt = NewMsgCnt, avg_dly = NewAvgDly}};
        Error -> {stop, Error, Cst}
    end;
handle_info({tcp, Sock, Data},
            #sst{sock = Sock, transport = Transport,
                 msg_cnt = OldMsgCnt, avg_dly = OldAvgDly} = Sst) ->
    {NewMsgCnt, NewAvgDly} =
    try
        RXT = timer:now_diff(os:timestamp(), binary_to_term(Data)),
        {OldMsgCnt + 1,
         (RXT + OldMsgCnt * OldAvgDly) / (OldMsgCnt + 1)}
    catch
        _:_ ->
            ?L("Error ~p Accept RX : ~p", [self(), byte_size(Data)]),
            {OldMsgCnt+1, OldAvgDly}
    end,
    case gen_tcp:send(Sock, Data) of
        ok ->
            case Transport:setopts(Sock, [{active, once}]) of
                ok -> {noreply, Sst#sst{msg_cnt = NewMsgCnt, avg_dly = NewAvgDly}};
                Error -> {stop, Error, Sst}
            end;
        {error, Reason} ->
            {stop, Reason, Sst}
    end.

terminate(Reason, State) ->
    ?L("die ~p ~p", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

