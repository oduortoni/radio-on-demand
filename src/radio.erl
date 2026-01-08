-module(radio).
-export([start/0, stop/0]).

start() ->
    % Ensure all dependencies are started
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jsone),
    
    % Create listeners table
    ets:new(radio_listeners, [named_table, public, set]),
    
    % Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/favicon.ico", cowboy_static, {priv_file, radio, "favicon.ico"}},
            {"/[...].css", cowboy_static, {priv_dir, radio, ""}},
            {"/[...].js", cowboy_static, {priv_dir, radio, ""}},
            {"/", static_handler, []},
            {"/radio", static_handler, []},
            {"/broadcast", static_handler, []},
            {"/listener", static_handler, []},
            {"/ws", ws_handler, []},
            {"/[...]", cowboy_static, {priv_dir, radio, ""}}
        ]}
    ]),
    
    % Start Cowboy
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 9000}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("~n"),
    io:format("================================~n"),
    io:format("Radio server started!~n"),
    io:format("URL: http://localhost:9000~n"),
    io:format("================================~n"),
    io:format("~n"),
    ok.

stop() ->
    cowboy:stop_listener(http_listener),
    ets:delete(radio_listeners),
    ok.