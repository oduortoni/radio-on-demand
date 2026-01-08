-module(layout).
-export([render/2, render/3]).

read_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    binary_to_list(Bin).

replace_vars(Str, Vars) ->
    lists:foldl(
      fun({K, V}, S) -> string:replace(S, "{{" ++ K ++ "}}", V, all) end,
      Str,
      Vars
    ).

render(Title, Content) ->
    render(Title, Content, []).

render(Title, Content, ExtraVars) ->
    Header = read_file("templates/header.html"),
    Footer = read_file("templates/footer.html"),
    PageBody = read_file("templates/page.html"),

    % Default variables
    DefaultVars = [
        {"title", Title},
        {"content", Content},
        {"extra_css", ""},
        {"scripts", ""}
    ],
    
    % Merge with extra variables
    AllVars = ExtraVars ++ DefaultVars,
    
    % Build full page
    PageWithContent = replace_vars(PageBody, AllVars),
    HeaderWithVars = replace_vars(Header, AllVars),
    FooterWithVars = replace_vars(Footer, AllVars),
    
    HeaderWithVars ++ PageWithContent ++ FooterWithVars.