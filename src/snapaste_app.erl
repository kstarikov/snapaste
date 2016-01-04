%% @author Kirill Starikov <kstarikov@kstarikov.com>
%% @copyright snapaste Kirill Starikov <kstarikov@kstarikov.com>

%% @doc Callbacks for the snapaste application.

-module(snapaste_app).
-author("Kirill Starikov <kstarikov@kstarikov.com>").

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for snapaste.
start(_Type, _StartArgs) -> snapaste_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for snapaste.
stop(_State) -> ok.
