-module(tcp_server).
-behaviour(gen_server).

-include("tcp_load.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Ip, Port]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [{ip, Ip}, binary, {active, false}]) of
        {ok, LSock} ->
            ?L("Listen ~p ~p ~p", [Ip, Port,LSock]),
            self() ! accept,
            {ok, {Ip, Port, LSock}};
        {error, Reason} -> {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, {Ip, Port, LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            case tcp_load:client(Ip, Port, Socket) of
                {ok, ChildPid} when is_pid(ChildPid) ->
                    gen_tcp:controlling_process(Socket, ChildPid),
                    ChildPid ! arm,
                    self() ! accept,
                    {noreply, {Ip, Port, LSock}};
                Error ->
                    {stop, Error, {Ip, Port, LSock}}
            end;
        {error, Reason} ->
            {stop, Reason, {Ip, Port, LSock}}
    end.

terminate(Reason, {Ip, Port, Sock}) ->
    catch gen_tcp:close(Sock),
    ?L("die ~p ~p", [Reason, {Ip, Port, Sock}]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
