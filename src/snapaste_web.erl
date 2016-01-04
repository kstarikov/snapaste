%% @author Kirill Starikov <kstarikov@kstarikov.com>
%% @copyright 2015- Kirill Starikov <kstarikov@kstarikov.com>

%% @doc Web server module for snapaste. Does all the actual work.

-module(snapaste_web).
-author("Kirill Starikov <kstarikov@kstarikov.com>").

-define(URL_KEY_CONTENTS,
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890").

-export([start/4, stop/0, dispatch/1, loop/1]).

-define(HTTPS_OPTS, [
    {loop, {?MODULE, dispatch}},
    {name, https_srv},
    {ssl, true}
]).

-record(sd, {host, port, https}).
-record(resource, {type, data}).

start(Host, Port, Certfile, Keyfile) ->
    HttpsOpts = ?HTTPS_OPTS ++
                [{port, Port},
                 {ssl_opts, [{certfile, Certfile},
                             {keyfile,  Keyfile}]}],
    {ok, Https} = mochiweb_http:start(HttpsOpts),
    SD = #sd{https=Https, host = Host, port = Port},
    Pid = spawn_link(fun() ->
                         ?MODULE = ets:new(?MODULE, [named_table]),
                         loop(SD)
                     end),
    register(http_store, Pid),
    {ok, self()}.

stop() ->
    http_store ! stop,
    ok.

dispatch(Req) ->
    case Req:get(method) of
        'GET' ->
            get_resource(Req);
        'POST' ->
            put_resource(Req);
        _ ->
            Headers = [{"Allow", "GET,POST"}],
            Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

get_resource(Req) ->
    [_Slash | Key] = Req:get(path),
    case ets:lookup(?MODULE, Key) of
        [{Path, #resource{data=Data}}] ->
            http_store ! {self(), {delete, Path}},
            Req:ok({"text/plain", Data});
        [] ->
            Req:respond({404, [], "404 Not Found\r\n"})
    end.

put_resource(Req) ->
    ContentType = case Req:get_header_value("Content-Type") of
                      undefined -> "application/octet-stream";
                      S         -> S
                  end,
    Resource = #resource{type=ContentType, data=Req:recv_body()},
    Key = generate_key(64),
    http_store ! {self(), {put, Key, Resource}},
    Pid = whereis(http_store),
    receive
        {Pid, created, Location} ->
            Resp = {201, [], "201 Created\r\nhttps://" ++ Location
                   ++ "/" ++ Key ++ "\r\n"},
            Req:respond(Resp)
    end.

loop(#sd{https=Https, host = Host, port = Port} = SD) ->
    receive
        stop ->
            ok = mochiweb_http:stop(Https),
            exit(normal);
        {From, {put, Key, Val}} ->
            Exists = ets:member(?MODULE, Key),
            ets:insert(?MODULE, {Key, Val}),
            case Exists of
                true ->
                    From ! {self(), updated};
                false ->
                    From ! {self(), created, location(Host, Port)}
            end;
        {From, {delete, Key}} ->
            ets:delete(?MODULE, Key),
            From ! {self(), ok};
        _ -> ok
    end,
    loop(SD).

generate_key(Length) ->
    MaxLength = length(?URL_KEY_CONTENTS),
    lists:foldl(
        fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), ?URL_KEY_CONTENTS)] ++ Acc end,
        [], lists:seq(1, Length)
    ).

location(Host, _Port = 443) -> Host;
location(Host, Port) -> Host ++ ":" ++ integer_to_list(Port).
