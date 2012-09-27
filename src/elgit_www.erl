-module(elgit_www).
-export([
    footer/1,
    header/1
]).

-include_lib("yaws_api.hrl").

footer(_Arg) ->
    [{html, [<<"
        </div>
    </body>
</html>
    ">>]}].

header(_Arg) ->
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
                <span class=\"brand\">El Git</span>
                <ul class=\"nav\">
                    <li class=\"active\"><a href=\"/\">Home</a></li>
                </ul>
            </div>
        </div>

        <div id=\"page\">
    ">>]}].