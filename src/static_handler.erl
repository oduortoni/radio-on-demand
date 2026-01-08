-module(static_handler).
-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    PrivDir = code:priv_dir(radio),
    
    FileName = case Path of
        <<"/">> -> "index.html";
        <<"/radio">> -> "listener.html";
        <<"/broadcast">> -> "broadcaster.html";
        <<"/listener">> -> "listener.html";
        _ -> "index.html"
    end,
    
    FilePath = filename:join(PrivDir, FileName),
    
    case file:read_file(FilePath) of
        {ok, Data} ->
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/html">>
            }, Data, Req0),
            {ok, Req, State};
        {error, Reason} ->
            io:format("Error reading file: ~p~n", [Reason]),
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"text/plain">>
            }, <<"File not found">>, Req0),
            {ok, Req, State}
    end.