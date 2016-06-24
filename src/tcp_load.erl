-module(tcp_load).
-behaviour(application).
-behaviour(supervisor).

-include("tcp_load.hrl").

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-export([server/2, clients/4]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    {Function, Args} =
    case init:get_argument(type) of
        {ok, [["server"]]} ->
            {ok, [[Pip]]}   = init:get_argument(ip),
            {ok, [[Pport]]} = init:get_argument(port),            
            {ok, Ip} = inet:getaddr(Pip, inet),
            Port = list_to_integer(Pport),
            
            {server, [Ip, Port]};
        {ok, [["client"]]} ->
            {ok, [[Pcount]]}    = init:get_argument(count),
            {ok, [[Psip]]}      = init:get_argument(sip),
            {ok, [[Pdip]]}      = init:get_argument(dip),
            {ok, [[Pport]]}     = init:get_argument(port),

            Count = list_to_integer(Pcount),
            {ok, SIp} = inet:getaddr(Psip, inet),
            {ok, DIp} = inet:getaddr(Pdip, inet),
            Port = list_to_integer(Pport),

            {clients, [Count,SIp,DIp,Port]}
    end,
    application:start(ranch),
    application:start(?MODULE),
    apply(tcp_load, Function, Args).
start(_StartType, _StartArgs) ->    
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    application:stop(?MODULE),
    application:stop(ranch).
stop(_State) -> ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, []}}.

server(Ip, Port) ->
    ?L("server listening on ~s:~p", [inet:ntoa(Ip), Port]),
    ranch:start_listener(tcp_echo, 100,
                         ranch_tcp, [{ip, Ip}, {port, Port}, {max_connections, infinity}],
                         tcp_client, []),
    supervisor:start_child(
      ?MODULE, #{id => logger, start => {logger, start_link, [server]},
                 restart => permanent, shutdown => 1000, type => worker,
                 modules => [logger]}).

clients(Count, SrcIp, Ip, Port) ->
    supervisor:start_child(
      ?MODULE, #{id => logger, start => {logger, start_link, [client]},
                 restart => permanent, shutdown => 1000, type => worker,
                 modules => [logger]}),
    clients_i(Count, SrcIp, Ip, Port).

clients_i(0, _SrcIp, _Ip, _Port) -> ok;
clients_i(Count, SrcIp, Ip, Port) when Count > 0 ->
    client(Count, SrcIp, Ip, Port),
    clients_i(Count - 1, SrcIp, Ip, Port).

client(Id, SrcIp, Ip, Port) when is_integer(Id) ->
    supervisor:start_child(?MODULE, {{client, make_ref(), Ip, Port},
                           {tcp_client, connect, [Id, SrcIp, Ip, Port]},
                           permanent, 5000, worker, [tcp_client]}).
