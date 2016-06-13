-module(tcp_load).
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-export([server/2, client/3, clients/4]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> application:start(?MODULE).
start(_StartType, _StartArgs) ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() -> application:stop(?MODULE).
stop(_State) -> ok.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

server(Ip, Port) ->
    supervisor:start_child(?MODULE, {{server, make_ref(), Ip, Port}, {tcp_server, start_link, [Ip, Port]}, permanent, 5000, worker, [tcp_server]}).

clients(0, _SrcIp, _Ip, _Port) -> ok;
clients(Count, SrcIp, Ip, Port) when Count > 0 ->
    client(Count, SrcIp, Ip, Port),
    clients(Count - 1, SrcIp, Ip, Port).

client(Id, SrcIp, Ip, Port) when is_integer(Id) ->
    supervisor:start_child(?MODULE, {{client, make_ref(), Ip, Port},
                           {tcp_client, connect, [Id, SrcIp, Ip, Port]},
                           permanent, 5000, worker, [tcp_client]}).

client(Ip, Port, Sock) ->
    supervisor:start_child(?MODULE, {{accept, make_ref(), Ip, Port},
                           {tcp_client, accept, [Ip, Port, Sock]},
                           temporary, 5000, worker, [tcp_client]}).
