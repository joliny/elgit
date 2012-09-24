-module(elgit_xhr).
-export([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

%%%
%   request methods
%%%
out(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    XhrAction = string:substr(Path, 11), % strip "/xhr/repo/"
    XhrRepo = elgit_shared:get_repo(XhrAction),
    case XhrRepo of
        {_, _, _} ->
            XhrRepoAction = string:substr(XhrAction, 2 + string:len(element(1, XhrRepo))), % strip "[RepoName]/"
            out_xhr_repo(Arg, XhrRepo, XhrRepoAction);
        _ ->
            out_xhr_invalid(Arg)
    end.

out_xhr_repo(Arg, XhrRepo, XhrRepoAction) ->
    XhrTypes = [{repo_init, "^init$"},
                {repo_tree, "^tree/[a-z0-9]{40}/.*"}],
    XhrType = elgit_shared:list_match(XhrTypes, XhrRepoAction),
    case XhrType of
        repo_init ->
            out_xhr_repo_init(Arg, XhrRepo);
        repo_tree ->
            XhrTreeAction = string:substr(XhrRepoAction, 6), % strip "tree/"
           out_xhr_repo_tree(Arg, XhrRepo, XhrTreeAction);
        _ ->
           out_xhr_invalid(Arg)
    end.

out_xhr_invalid(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    XhrAction = string:substr(Path, 6), % strip "/xhr/"
    [{html, [<<"{\"action\": \"">>, XhrAction, <<"\", \"state\": \"invalid\"}">>]}].

out_xhr_repo_init(Arg, XhrRepo) ->
    HeadCommit = gert:get_commit_record(Arg#arg.docroot ++ element(3, XhrRepo), "refs/heads/master"),
    HeadCommitOid = HeadCommit#commit.oid,
    HeadCommitMessage = elgit_shared:list_replace([{"\n", "\\\\n", [global]},
                                                   {"\n$", "", [global]},
                                                   {"\r", "", [global]},
                                                   {"\"", "\\\\\"", [global]}], HeadCommit#commit.message),
    HeadCommitAuthor = re:replace(HeadCommit#commit.author, "\"", "\\\\\"", [global]),
    HeadCommitTimestamp = list_to_binary(integer_to_list(HeadCommit#commit.timestamp)),
    [{html, [<<"{\"action\": \"repo_init\",
                 \"state\": \"ok\",
                 \"repo\": {\"id\": \"">>, element(1, XhrRepo), <<"\",
                            \"HEAD\": {\"oid\": \"">>, HeadCommitOid, <<"\",
                                       \"message\": \"">>, HeadCommitMessage, <<"\",
                                       \"author\": \"">>, HeadCommitAuthor, <<"\",
                                       \"timestamp\": ">>, HeadCommitTimestamp, <<"}}}">>]}].

out_xhr_repo_tree(Arg, XhrRepo, XhrTreeAction) ->
    TreeOid = string:substr(XhrTreeAction, 1, 40),
    TreePath = string:substr(XhrTreeAction, 42),
    TreeEntries = gert:get_tree(Arg#arg.docroot ++ element(3, XhrRepo), TreeOid, TreePath),
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