-module(ws_handler).
-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    Pid = self(),
    ets:insert(radio_listeners, {Pid}),
    Count = ets:info(radio_listeners, size),
    io:format("New listener connected: ~p (total: ~p)~n", [Pid, Count]),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    % Parse JSON message
    try jsone:decode(Msg, [{object_format, map}]) of
        #{<<"type">> := <<"audio">>, <<"audio">> := Audio} ->
            % Broadcast to all listeners except sender
            Listeners = ets:tab2list(radio_listeners),
            lists:foreach(
                fun({ListenerPid}) when ListenerPid =/= self() ->
                    ListenerPid ! {audio, Audio};
                   (_) -> ok
                end,
                Listeners
            ),
            {ok, State};
        _ ->
            {ok, State}
    catch
        _:Error ->
            io:format("Error parsing message: ~p~n", [Error]),
            {ok, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({audio, AudioData}, State) ->
    % Send audio data to client
    Response = jsone:encode(#{
        <<"type">> => <<"audio">>,
        <<"audio">> => AudioData
    }),
    {reply, {text, Response}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    Pid = self(),
    ets:delete(radio_listeners, Pid),
    Count = ets:info(radio_listeners, size),
    io:format("Listener disconnected: ~p (remaining: ~p)~n", [Pid, Count]),
    ok.