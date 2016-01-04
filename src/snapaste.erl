%% @author Kirill Starikov <kstarikov@kstarikov.com>
%% @copyright 2015- Kirill Starikov <kstarikov@kstarikov.com>

%% @doc snapaste entry point. ensures deps are installed and starts/stops the
%%      app.

-module(snapaste).
-author("Kirill Starikov <kstarikov@kstarikov.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the snapaste server.
start() ->
    snapaste_deps:ensure(),
    ensure_started(crypto),
    application:start(snapaste).

%% @spec stop() -> ok
%% @doc Stop the snapaste server.
stop() -> application:stop(snapaste).
