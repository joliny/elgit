-module(elgit_xhr).
-export ([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

%%%
%   request methods
%%%
out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    XhrAction = string:substr(Path, 5), % strip "/xhr"
    XhrRepo = elgit_shared:get_repo(Arg, XhrAction),
    case XhrRepo of
        {_, _} ->
            RepoName = element(1, XhrRepo),
            RepoPath = element(2, XhrRepo),
            XhrRepoAction = string:substr(XhrAction, 7 + string:len(RepoName)), % strip "/repo/[RepoName]"
            out_xhr_repo(Arg, RepoPath, XhrRepoAction);
        _ ->
            out_xhr_invalid(Arg)
    end.

out_xhr_repo(Arg, RepoPath, XhrRepoAction) ->
    XhrTypes = [{repo_init, "^/init$"},
                {repo_tree, "^/tree/[a-z0-9]{40}/.*"}],
    XhrType = elgit_shared:list_match(XhrTypes, XhrRepoAction),
    case XhrType of
        repo_init ->
            out_xhr_repo_init(RepoPath);
       repo_tree ->
            XhrTreeAction = string:substr(XhrRepoAction, 7), % strip "/tree/"
           out_xhr_repo_tree(RepoPath, XhrTreeAction);
       _ ->
           out_xhr_invalid(Arg)
    end.

out_xhr_invalid(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    XhrAction = string:substr(Path, 6), % strip "/xhr/"
    [{html, [<<"{\"action\": \"">>, XhrAction, <<"\", \"state\": \"invalid\"}">>]}].

out_xhr_repo_init(RepoPath) ->
    HeadCommit = gert:get_commit_record(RepoPath, "refs/heads/master"),
    HeadCommitOid = HeadCommit#commit.oid,
    HeadCommitMessage = elgit_shared:list_replace([{"\"", "\\\\\"", [global]},
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

out_xhr_repo_tree(RepoPath, XhrTreeAction) ->
    TreeOid = string:substr(XhrTreeAction, 1, 40),
    TreePath = string:substr(XhrTreeAction, 42),
    out_xhr_repo_tree(TreeOid, TreePath, gert:get_tree(RepoPath, TreeOid, TreePath)).
out_xhr_repo_tree(TreeOid, TreePath, TreeEntries) ->
    TreeTreeEntries = [L || {tree, _} = L <- TreeEntries],
    TreeBlobEntries = [L || {blob, _} = L <- TreeEntries],
    TreeSubmoduleEntries = [L || {submodule, _} = L <- TreeEntries],
    TreeTrees = elgit_shared:join(lists:map(fun(E) -> "\"" ++ element(2, E) ++ "\"" end, TreeTreeEntries), ","),
    TreeBlobs = elgit_shared:join(lists:map(fun(E) -> "\"" ++ element(2, E) ++ "\"" end, TreeBlobEntries), ","),
    TreeSubmodules = elgit_shared:join(lists:map(fun(E) -> "\"" ++ element(2, E) ++ "\"" end, TreeSubmoduleEntries), ","),
    [{html, [<<"{\"action\": \"repo_tree\",
                 \"state\": \"ok\",
                 \"tree\": {\"oid\": \"">>, TreeOid, <<"\",
                            \"path\": \"">>, TreePath, <<"\",
                            \"entries\": {\"trees\": [">>, TreeTrees, <<"],
                                          \"blobs\": [">>, TreeBlobs, <<"],
                                          \"submodules\": [">>, TreeSubmodules, <<"]}}}">>]}].