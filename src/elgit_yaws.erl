-module(elgit_yaws).
-export ([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").


%%%
%   helper methods
%%%
join([], _) ->
    "";
join([First|Rest], JoinWith) ->
    lists:flatten([First] ++ [JoinWith ++ X || X <- Rest]).

list_match([], _) ->
    nomatch;
list_match([RE|REs], String) ->
    {REAtom, REExp} = RE,
    case re:run(String, REExp, [{capture, none}]) of
        match ->
            REAtom;
        nomatch ->
            list_match(REs, String)
    end.

list_replace([], String) ->
    String;
list_replace([RE|REs], String) ->
    Match = element(1, RE),
    Replace = element(2, RE),
    Options = element(3, RE),
    re:replace(list_replace(REs, String), Match, Replace, Options).


%%%
%   request methods
%%%
out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    ReqTypes = [{index, "^/$"},
                {xhr, "^/xhr/.+"}],
    ReqType = list_match(ReqTypes, Path),
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
    XhrType = list_match(XhrTypes, XhrAction),
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
    HeadCommit = gert:get_commit_record(RepoPath, "refs/heads/master"),
    HeadCommitOid = HeadCommit#commit.oid,
    HeadCommitMessage = list_replace([{"\"", "\\\\\"", [global]},
                                      {"\r", "", [global]},
                                      {"\n$", "", [global]},
                                      {"\n", "\\\\n", [global]}], HeadCommit#commit.message),
    HeadCommitAuthor = re:replace(HeadCommit#commit.author, "\"", "\\\\\"", [global]),
    HeadCommitTimestamp = list_to_binary(integer_to_list(HeadCommit#commit.timestamp)),
    [{html, [<<"{\"action\": \"repo_init\",
                 \"state\": \"ok\",
                 \"repo\": {\"HEAD\": {\"oid\": \"">>, HeadCommitOid, <<"\",
                                       \"message\": \"">>, HeadCommitMessage, <<"\",
                                       \"author\": \"">>, HeadCommitAuthor, <<"\",
                                       \"timestamp\": ">>, HeadCommitTimestamp, <<"}}}">>]}].

out_xhr_repo_tree(Arg) ->
    RepoPath = Arg#arg.docroot ++ "/../.git",
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    TreeInfo = string:substr(Path, 16),
    TreeOid = string:substr(TreeInfo, 1, 40),
    TreePath = string:substr(TreeInfo, 42),
    out_xhr_repo_tree(Arg, TreeOid, TreePath, gert:get_tree(RepoPath, TreeOid, TreePath)).
out_xhr_repo_tree(Arg, TreeOid, TreePath, TreeEntries) ->
    TreeTreeEntries = [L || {tree, _} = L <- TreeEntries],
    TreeBlobEntries = [L || {blob, _} = L <- TreeEntries],
    TreeSubmoduleEntries = [L || {submodule, _} = L <- TreeEntries],
    TreeTrees = join(lists:map(fun(E) -> "\"" ++ element(2, E) ++ "\"" end, TreeTreeEntries), ","),
    TreeBlobs = join(lists:map(fun(E) -> "\"" ++ element(2, E) ++ "\"" end, TreeBlobEntries), ","),
    TreeSubmodules = join(lists:map(fun(E) -> "\"" ++ element(2, E) ++ "\"" end, TreeSubmoduleEntries), ","),
    [{html, [<<"{\"action\": \"repo_tree\",
                 \"state\": \"ok\",
                 \"tree\": {\"oid\": \"">>, TreeOid, <<"\",
                            \"path\": \"">>, TreePath, <<"\",
                            \"entries\": {\"trees\": [">>, TreeTrees, <<"],
                                          \"blobs\": [">>, TreeBlobs, <<"],
                                          \"submodules\": [">>, TreeSubmodules, <<"]}}}">>]}].

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