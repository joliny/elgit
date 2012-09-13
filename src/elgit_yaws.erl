-module(elgit_yaws).
-export ([out/1]).

-include_lib("gert/include/gert.hrl").
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
    RepoPath = Arg#arg.docroot ++ "/.git",
    HeadSha = gert:get_head_sha(RepoPath),
    HeadCommit = gert:get_commit_record(RepoPath, HeadSha),
    HeadCommitMsg = HeadCommit#commit.message,
    [{html, [<<"
<html>
    <head>
        <link href=\"http://static.elgit.dev/css/bootstrap.css\" rel=\"stylesheet\"/>
        <link href=\"http://static.elgit.dev/css/bootstrap-responsive.css\" rel=\"stylesheet\">
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

        <h1>Hello World!</h1>
        <h2>">>, HeadCommitMsg, <<" @ ">>, HeadSha, <<"</h2>

        <script src=\"http://static.elgit.dev/js/jquery.js\"></script>
        <script src=\"http://static.elgit.dev/js/bootstrap.js\"></script>
    </body>
</html>
">>]}].