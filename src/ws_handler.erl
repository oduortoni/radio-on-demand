-module(ws_handler).
-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    Pid = self(),
    ets:insert(radio_listeners, {Pid}),
    Count = ets:info(radio_listeners, size),
    io:format("~n[~s] ✓ New listener connected~n", [timestamp()]),
    io:format("  PID: ~p~n", [Pid]),
    io:format("  Total listeners: ~p~n~n", [Count]),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    io:format("~n[~s] → Received WebSocket message~n", [timestamp()]),
    io:format("  Message size: ~p bytes~n", [byte_size(Msg)]),
    io:format("  First 100 chars: ~s~n", [string:slice(Msg, 0, 100)]),
    
    % Parse JSON message
    try jsone:decode(Msg, [{object_format, map}]) of
        #{<<"type">> := <<"audio">>, <<"audio">> := Audio} = DecodedMsg ->
            io:format("  ✓ JSON parsed successfully~n", []),
            io:format("  Message type: audio~n", []),
            io:format("  Audio data size: ~p bytes~n", [byte_size(Audio)]),
            
            % Check if it's base64 or what format
            case Audio of
                <<"data:", Rest/binary>> ->
                    io:format("  Audio format: Data URI~n", []),
                    io:format("  Data URI prefix: ~s~n", [string:slice(Rest, 0, 50)]);
                _ ->
                    io:format("  Audio format: Raw base64 or other~n", []),
                    io:format("  First 50 chars: ~s~n", [string:slice(Audio, 0, 50)])
            end,
            
            % Get all listeners except sender
            AllListeners = ets:tab2list(radio_listeners),
            OtherListeners = lists:filter(
                fun({ListenerPid}) -> ListenerPid =/= self() end,
                AllListeners
            ),
            
            io:format("  Broadcasting to ~p other listener(s)~n", [length(OtherListeners)]),
            
            % Broadcast to all listeners except sender
            lists:foreach(
                fun({ListenerPid}) ->
                    io:format("    → Sending to PID: ~p~n", [ListenerPid]),
                    ListenerPid ! {audio, Audio}
                end,
                OtherListeners
            ),
            
            io:format("  ✓ Broadcast complete~n~n", []),
            {ok, State};
        Other ->
            io:format("  ⚠ Unknown message format: ~p~n~n", [Other]),
            {ok, State}
    catch
        Error:Reason:Stacktrace ->
            io:format("  ✗ Error parsing message~n", []),
            io:format("  Error: ~p~n", [Error]),
            io:format("  Reason: ~p~n", [Reason]),
            io:format("  Stacktrace: ~p~n~n", [Stacktrace]),
            {ok, State}
    end;
websocket_handle(Data, State) ->
    io:format("~n[~s] ⚠ Received non-text WebSocket frame~n", [timestamp()]),
    io:format("  Data: ~p~n~n", [Data]),
    {ok, State}.

websocket_info({audio, AudioData}, State) ->
    io:format("~n[~s] ← Relaying audio to client~n", [timestamp()]),
    io:format("  Audio data size: ~p bytes~n", [byte_size(AudioData)]),
    
    % Encode response
    Response = jsone:encode(#{
        <<"type">> => <<"audio">>,
        <<"audio">> => AudioData
    }),
    
    io:format("  Response size: ~p bytes~n", [byte_size(Response)]),
    io:format("  ✓ Sending to client~n~n", []),
    {reply, {text, Response}, State};
websocket_info(Info, State) ->
    io:format("~n[~s] ⚠ Received unknown info message~n", [timestamp()]),
    io:format("  Info: ~p~n~n", [Info]),
    {ok, State}.

terminate(_Reason, _PartialReq, _State) ->
    Pid = self(),
    ets:delete(radio_listeners, Pid),
    Count = ets:info(radio_listeners, size),
    io:format("~n[~s] ✗ Listener disconnected~n", [timestamp()]),
    io:format("  PID: ~p~n", [Pid]),
    io:format("  Remaining listeners: ~p~n~n", [Count]),
    ok.

%% Helper function to get timestamp
timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                  [Y, M, D, H, Min, S]).