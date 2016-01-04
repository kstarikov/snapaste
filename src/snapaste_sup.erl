%% @author Kirill Starikov <kstarikov@kstarikov.com>
%% @copyright 2015- Kirill Starikov <kstarikov@kstarikov.com>

%% @doc Supervisor for the snapaste application.

-module(snapaste_sup).
-author("Kirill Starikov <kstarikov@kstarikov.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc Gets the necessary parameters from the application envrironment and
%% starts the main module, `snapaste_web'.
init([]) ->
    {ok, Host}     = application:get_env(snapaste, host),
    {ok, Port}     = application:get_env(snapaste, port),
    {ok, Certfile} = application:get_env(snapaste, certfile),
    {ok, Keyfile}  = application:get_env(snapaste, keyfile),
    Processes = [{snapaste_web,
                 {snapaste_web, start, [Host, Port, Certfile, Keyfile]},
                 permanent, 5000, worker, dynamic}],
    {ok, {{one_for_one, 10, 10}, lists:flatten(Processes)}}.
