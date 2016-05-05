-module(tcp_client).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, start_link/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Ip, Port) ->
    gen_server:start_link(?MODULE, [Ip, Port], []).
start_link(Ip, Port, Sock) ->
    gen_server:start_link(?MODULE, [Ip, Port, Sock], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ip, Port]) ->
    process_flag(trap_exit, true),
    case gen_tcp:connect(Ip, Port, [{packet, 0}, binary, {active, once}]) of
        {ok, Sock} ->
            io:format("connect ~p ~p ~p~n", [Ip, Port, Sock]),
            erlang:send_after(5000, self(), msg),
            {ok, {Ip, Port, Sock}};
        {error, Reason} -> {stop, Reason}
    end;
init([Ip, Port, Sock]) ->
    io:format("accept ~p ~p ~p~n", [Ip, Port, Sock]),
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
        if RTT > 0 -> io:format("~p Client RX : RTT ~p us~n", [self(), RTT]); true -> ok end
    catch
        _:_ ->
            io:format("~p Client RX : ~p~n", [self(), byte_size(Data)])
    end,
    self() ! arm,
    {noreply, {Ip, Port, Sock}};
handle_info({tcp, Sock, Data}, Sock) ->
    try
        RXT = timer:now_diff(os:timestamp(), binary_to_term(Data)),
        if RXT > 0 -> io:format("~p Accept RX : ~p us~n", [self(), RXT]); true -> ok end
    catch
        _:_ ->
            io:format("~p Accept RX : ~p~n", [self(), byte_size(Data)])
    end,
    case gen_tcp:send(Sock, Data) of
        ok ->
            self() ! arm,
            {noreply, Sock};
        {error, Reason} ->
            {stop, Reason, Sock}
    end;
handle_info(arm, {Ip, Port, Sock}) ->
    case inet:setopts(Sock, [{active, once}]) of
        ok -> {noreply, {Ip, Port, Sock}};
        Error -> {stop, Error, {Ip, Port, Sock}}
    end;
handle_info(arm, Sock) ->
    case inet:setopts(Sock, [{active, once}]) of
        ok -> {noreply, Sock};
        Error -> {stop, Error, Sock}
    end.

terminate(Reason, State) ->
    io:format("die ~p ~p~n", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

