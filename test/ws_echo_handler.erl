%% WebSocket echo server handler for LHC integration tests
%% Echoes back text and binary frames, responds to ping with pong
-module(ws_echo_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

%% Handle text frames - echo back
websocket_handle({text, Msg}, State) ->
    {reply, {text, Msg}, State};

%% Handle binary frames - echo back
websocket_handle({binary, Data}, State) ->
    {reply, {binary, Data}, State};

%% Handle ping frames - respond with pong
websocket_handle(ping, State) ->
    {reply, pong, State};

%% Handle pong frames - ignore
websocket_handle(pong, State) ->
    {ok, State};

%% Handle close frames
websocket_handle({close, Code, Reason}, State) ->
    {reply, {close, Code, Reason}, State};

%% Catch-all
websocket_handle(_Frame, State) ->
    {ok, State}.

%% Handle Erlang messages (none expected)
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
