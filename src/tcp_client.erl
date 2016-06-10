-module(tcp_client).
-behaviour(gen_server).

-include("tcp_load.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(P1, P2, P3) ->
    gen_server:start_link(?MODULE, [P1, P2, P3], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Id, Ip, Port]) when is_integer(Id) ->
    process_flag(trap_exit, true),
    case gen_tcp:connect(Ip, Port, [{packet, 0}, binary, {active, once}]) of
        {ok, Sock} ->
            {ok, {_, LPort}} = inet:sockname(Sock),
            ?L("[~p] connect ~s:~p <- ~p", [Id, inet:ntoa(Ip), Port, LPort]),
            erlang:send_after(5000, self(), msg),
            {ok, {Ip, Port, Sock}};
        {error, Reason} -> {stop, Reason}
    end;
init([Ip, Port, Sock]) ->
    ?L("accept ~p ~p ~p", [Ip, Port, Sock]),
    process_flag(trap_exit, true),
    {ok, Sock}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
        if RTT > 100000 -> ?L("~p Client RX : RTT ~p us", [self(), RTT]); true -> ok end
    catch
        _:_ ->
            ?L("ERROR erlang term ~p Client RX : ~p", [self(), byte_size(Data)])
    end,
    case inet:setopts(Sock, [{active, once}]) of
        ok -> {noreply, {Ip, Port, Sock}};
        Error -> {stop, Error, {Ip, Port, Sock}}
    end;
handle_info({tcp, Sock, Data}, Sock) ->
    try
        RXT = timer:now_diff(os:timestamp(), binary_to_term(Data)),
        if RXT > 100000 -> ?L("~p Accept RX : ~p us", [self(), RXT]); true -> ok end
    catch
        _:_ ->
            ?L("~p Accept RX : ~p", [self(), byte_size(Data)])
    end,
    case gen_tcp:send(Sock, Data) of
        ok ->
            case inet:setopts(Sock, [{active, once}]) of
                ok -> {noreply, Sock};
                Error -> {stop, Error, Sock}
            end;
        {error, Reason} ->
            {stop, Reason, Sock}
    end;
handle_info(arm, Sock) ->
    case inet:setopts(Sock, [{active, once}]) of
        ok -> {noreply, Sock};
        Error -> {stop, Error, Sock}
    end.

terminate(Reason, State) ->
    ?L("die ~p ~p", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

