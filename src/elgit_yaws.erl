-module(elgit_yaws).
-export ([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    if
        Path /= "/" ->
            {redirect_local, "/"};
        true ->
            out_index(Arg)
    end.

out_index(Arg) ->
    HeadSha = gert:get_head_sha(Arg#arg.docroot ++ "/.git"),
    [{html, [<<"
<html>
    <head>
        <title>El Git</title>
    </head>
    <body>
        <h1>Hello World!</h1>
        <h2>HEAD: ">>, HeadSha, <<"</h2>
    </body>
</html>
">>]}].