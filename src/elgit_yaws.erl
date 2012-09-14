-module(elgit_yaws).
-export ([out/1]).

-include_lib("gert/include/gert.hrl").
-include_lib("yaws/include/yaws_api.hrl").

join([First|Rest], JoinWith) ->
    lists:flatten([First] ++ [JoinWith ++ X || X <- Rest]).

recursive_re([], _) ->
    nomatch;
recursive_re([RE|REs], String) ->
    {REAtom, REExp} = RE,
    case re:run(String, REExp, [{capture, none}]) of
        match ->
            REAtom;
        nomatch ->
            recursive_re(REs, String)
    end.

out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    ReqTypes = [{index, "^/$"},
                {xhr, "^/xhr/.+"}],
    ReqType = recursive_re(ReqTypes, Path),
    case ReqType of
        xhr -> out_xhr(Arg);
        index -> out_index();
        _ -> {redirect_local, "/"}
    end.

out_xhr(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    XhrAction = string:substr(Path, 6),
    XhrTypes = [{repo_init, "^repo/init$"},
                {repo_tree, "^repo/tree/[a-z0-9]{40}/.*"}],
    XhrType = recursive_re(XhrTypes, XhrAction),
    case XhrType of
        repo_init ->
            out_xhr_repo_init(Arg);
        repo_tree ->
            out_xhr_repo_tree(Arg);
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

out_xhr_repo_tree(Arg) ->
    RepoPath = Arg#arg.docroot ++ "/../.git",
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    TreeInfo = string:substr(Path, 16),
    TreeSha = string:substr(TreeInfo, 1, 40),
    TreePath = string:substr(TreeInfo, 42),
    case TreePath of
        "" ->
            out_xhr_repo_tree(Arg, TreeSha, TreePath, gert:get_tree(RepoPath, TreeSha));
        _ ->
            out_xhr_invalid(string:substr(Path, 6))
    end.
out_xhr_repo_tree(Arg, TreeSha, TreePath, TreeEntries) ->
    TreeEntriesStr = join(lists:map(fun(E) -> "\"" ++ E ++ "\"" end, TreeEntries), ","),
    [{html, [<<"{\"action\": \"repo_tree\",
                 \"state\": \"ok\",
                 \"tree\": {\"sha\": \"">>, TreeSha, <<"\",
                            \"path\": \"">>, TreePath, <<"\",
                            \"entries\": [">>, TreeEntriesStr, <<"]}}">>]}].

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