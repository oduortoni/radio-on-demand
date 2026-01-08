-module(template_handler).
-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    
    {ContentFile, Title, ExtraVars} = case Path of
        <<"/">> -> 
            {"templates/index_content.html", "Radio On Demand", 
             [{"scripts", "<script src=\"reactive-demo.js\"></script>"}]};
        <<"/broadcast">> -> 
            {"templates/broadcaster_content.html", "Radio Broadcaster - Debug Mode",
             [{"extra_css", "<link rel=\"stylesheet\" href=\"broadcaster.css\">"},
              {"scripts", "<script src=\"broadcaster.js\"></script>"}]};
        <<"/radio">> -> 
            {"templates/listener_content.html", "Radio Listener - Debug Mode",
             [{"extra_css", "<link rel=\"stylesheet\" href=\"listener.css\">"},
              {"scripts", "<script src=\"listener.js\"></script>"}]}
    end,
    
    {ok, ContentBin} = file:read_file(ContentFile),
    Content = binary_to_list(ContentBin),
    Html = layout:render(Title, Content, ExtraVars),
    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    {ok, Req, State}.