-module(static_handler).
-export([init/2]).

init(Req0, State) ->
    % Read file from priv directory
    PrivDir = code:priv_dir(radio),
    FilePath = filename:join(PrivDir, "broadcaster.html"),
    
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