-module(elgit_yaws).
-export ([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

%%%
%   request methods
%%%
out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    ReqTypes = [{index, "^/$"},
                {xhr, "^/xhr/.+"}],
    ReqType = elgit_shared:list_match(ReqTypes, Path),
    case ReqType of
        xhr -> elgit_xhr:out(Arg);
        index -> out_index();
        _ -> {redirect_local, "/"}
    end.

out_index() ->
    [{html, [<<"
<html>
    <head>
        <link href=\"/css/bootstrap.css\" rel=\"stylesheet\"/>
        <link href=\"/css/bootstrap-responsive.css\" rel=\"stylesheet\">
        <title>El Git</title>
    </head>
    <body>
        <div class=\"navbar\">
            <div class=\"navbar-inner\">
                <span class=\"brand\" href=\"#\">El Git</span>
                <ul class=\"nav\">
                    <li class=\"active\"><a href=\"#\">Home</a></li>
                </ul>
            </div>
        </div>

        <div id=\"page\"></div>

        <script data-main=\"/js/elgit.js\"
                src=\"/js/lib/require.js\"></script>
    </body>
</html>
">>]}].