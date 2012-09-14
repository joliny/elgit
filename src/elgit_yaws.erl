-module(elgit_yaws).
-export ([out/1]).

-include_lib("gert/include/gert.hrl").
-include_lib("yaws/include/yaws_api.hrl").

get_request_type(xhr, Path) ->
    case re:run(Path, "^/xhr/.+") of
        {match, _} ->
            xhr;
        nomatch ->
            notok
    end.

get_request_type(Path) ->
    if
        Path == "/" -> index;
        true -> get_request_type(xhr, Path)
    end.

out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    case get_request_type(Path) of
        xhr -> out_xhr(Arg);
        index -> out_index(Arg);
        _ -> {redirect_local, "/"}
    end.

out_xhr(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    XhrAction = string:substr(Path, 6),
    case XhrAction of
        "repo/init" ->
            out_xhr_repo_init(Arg);
        _ ->
            out_xhr_invalid(XhrAction)
    end.

out_xhr_invalid(XhrAction) ->
    [{html, [<<"{\"action\": \"">>, XhrAction, <<"\", \"state\": \"invalid\"}">>]}].

out_xhr_repo_init(Arg) ->
    RepoPath = Arg#arg.docroot ++ "/../.git",
    HeadSha = gert:get_head_sha(RepoPath),
    HeadCommit = gert:get_commit_record(RepoPath, HeadSha),
    HeadCommitMessage = re:replace(re:replace(HeadCommit#commit.message,
                                              "\"", "\\\\\"", [global]),
                                   "\n$", "", [global]),
    HeadCommitAuthor = re:replace(HeadCommit#commit.author, "\"", "\\\\\"", [global]),
    HeadCommitTimestamp = list_to_binary(integer_to_list(HeadCommit#commit.timestamp)),
    [{html, [<<"{\"action\": \"repo_init\",
                 \"state\": \"ok\",
                 \"repo\": {\"HEAD\": {\"sha\": \"">>, HeadSha, <<"\",
                                       \"message\": \"">>, HeadCommitMessage, <<"\",
                                       \"author\": \"">>, HeadCommitAuthor, <<"\",
                                       \"timestamp\": ">>, HeadCommitTimestamp, <<"}}}">>]}].

out_index(Arg) ->
    RepoPath = Arg#arg.docroot ++ "/../.git",
    HeadSha = gert:get_head_sha(RepoPath),
    HeadCommit = gert:get_commit_record(RepoPath, HeadSha),
    HeadCommitMessage = HeadCommit#commit.message,
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

        <h1>Hello World!</h1>
        <h2>">>, HeadCommitMessage, <<" @ ">>, HeadSha, <<"</h2>

        <div id=\"page\"></div>

        <script data-main=\"/js/elgit.js\"
                src=\"/js/lib/require.js\"></script>
    </body>
</html>
">>]}].