%% WebSocket test server for LHC integration tests
%% Starts a Cowboy HTTP server with WebSocket support
-module(ws_test_server).
-export([start/0, start/1, stop/0]).

-define(DEFAULT_PORT, 8080).

%% Start server on default port
start() ->
    start(?DEFAULT_PORT).

%% Start server on specified port
start(Port) ->
    application:ensure_all_started(cowboy),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", ws_echo_handler, []}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        ws_test_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("WebSocket test server started on port ~p~n", [Port]),
    {ok, Port}.

%% Stop server
stop() ->
    cowboy:stop_listener(ws_test_listener),
    io:format("WebSocket test server stopped~n"),
    ok.
