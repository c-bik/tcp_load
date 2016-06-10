-module(tcp_load).
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-export([server/2, client/3, clients/3]).

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

clients(0, _Ip, _Port) -> ok;
clients(Count, Ip, Port) when Count > 0 ->
    client(Count, Ip, Port),
    clients(Count - 1, Ip, Port).

client(Id, Ip, Port) when is_integer(Id) ->
    supervisor:start_child(?MODULE, {{client, make_ref(), Ip, Port},
                           {tcp_client, start_link, [Id, Ip, Port]},
                           permanent, 5000, worker, [tcp_client]});
client(Ip, Port, Sock) ->
    supervisor:start_child(?MODULE, {{accept, make_ref(), Ip, Port},
                           {tcp_client, start_link, [Ip, Port, Sock]},
                           temporary, 5000, worker, [tcp_client]}).

