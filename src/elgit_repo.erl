-module(elgit_repo).
-export([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

out(Arg) ->
    case elgit_shared:is_partial(Arg) of
        false ->
            [elgit_www:header(Arg),
             header(Arg),
             out_partial(Arg),
             footer(Arg),
             elgit_www:footer(Arg)];
         true ->
            out_partial(Arg)
    end.

out_partial(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    Repo = elgit_shared:get_repo(string:substr(Path, 2)),
    ActionPath = string:substr(Path, 2 + string:len(element(1, Repo))),
    RepoActions = [{tree, "^/tree/.*"},
                   {index, "^/$"}],
    RepoAction = elgit_shared:list_match(RepoActions, ActionPath),
    case RepoAction of
        tree ->
            tree(Arg, ActionPath, Repo);
        index ->
            tree(Arg, "/tree/master/", Repo);
        _ ->
            {redirect_local, "/"}
    end.

header(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    Repo = elgit_shared:get_repo(string:substr(Path, 2)),
    RepoPath = Arg#arg.docroot ++ element(3, Repo),
    BranchList = gert:get_branches(RepoPath),
    {html, [<<"
<div id=\"repo\">
    <div id=\"repo-head\">
        Branches:
        <select>">>, header_select_branch(BranchList), <<"</select>
    </div>
    ">>]}.

header_select_branch([]) ->
    [];
header_select_branch([Branch|Branches]) ->
    [<<"<option>">>,
     re:replace(Branch, "refs/heads/", ""),
     <<"</option>">>] ++ header_select_branch(Branches).

footer(_Arg) ->
    {html, [<<"</div>">>]}.

repo_header(Arg, Repo, CommitOid) ->
    RepoPath = Arg#arg.docroot ++ element(3, Repo),
    Commit = gert:get_commit_record(RepoPath, CommitOid),
    CommitMessage = elgit_shared:list_replace([{"\n", "\\\\n", [global]},
                                               {"\n$", "", [global]},
                                               {"\r", "", [global]},
                                               {"\"", "\\\\\"", [global]}
                                              ], Commit#commit.message),
    CommitAuthor = re:replace(Commit#commit.author, "\"", "\\\\\"", [global]),
    {html, [<<"
<div id=\"last-commit\">
    <p>">>, CommitOid, <<"</p>
    <p>">>, CommitMessage, <<"</p>
    <p>">>, CommitAuthor, <<"</p>
</div>
<!--<script data-main=\"/js/elgit.js\" src=\"/js/lib/require.js\"></script>-->
    ">>]}.

tree(Arg, ActionPath, Repo) ->
    ActionParts = re:run(ActionPath, "^/tree/([[:alnum:]]+)/(.*)", [{capture, [1,2], list}]),
    case ActionParts of
        {match, Matches} ->
            TreeOid = lists:nth(1, Matches),
            TreePath = lists:nth(2, Matches),
            tree_partial(Arg, Repo, TreeOid, TreePath);
        nomatch ->
            {redirect_local, "/"}
    end.

tree_crumb(Repo, TreeOid, TreePath) ->
    TreeCrumbs = re:split(TreePath, "/", [{return, list}]),
    {html, [<<"
<ul id=\"tree-crumb\">
    ">>,
        tree_crumb_root(Repo, TreeOid),
        tree_crumb_entries(Repo, TreeOid, [], TreeCrumbs),
    <<"
</ul>
    ">>]}.

tree_crumb_root(Repo, TreeOid) ->
    [<<"
<li>
    <a href=\"/">>, element(1, Repo), <<"/tree/">>,
                    TreeOid, <<"/\">">>,
                    element(1, Repo), <<"</a>
</li>
    ">>].

tree_crumb_entry(Repo, TreeOid, TreeLink, TreeCrumb) ->
    [<<"
<li>
    <a href=\"/">>, element(1, Repo), <<"/tree/">>,
                    TreeOid, <<"/">>,
                    TreeLink, <<"/\">">>,
                    TreeCrumb, <<"</a>
</li>
    ">>].

tree_crumb_entries(_Repo, _TreeOid, _TreePath, []) ->
    [];
tree_crumb_entries(Repo, TreeOid, TreePath, [[]|TreeCrumbs]) ->
    tree_crumb_entries(Repo, TreeOid, TreePath, TreeCrumbs); % ignore "empty" matches from string split
tree_crumb_entries(Repo, TreeOid, TreePath, [TreeCrumb|TreeCrumbs]) ->
    case TreePath of
        [] ->
            TreeLink = TreeCrumb;
        _ ->
            TreeLink = elgit_shared:join([TreePath, TreeCrumb], "/")
    end,
    [tree_crumb_entry(Repo, TreeOid, TreeLink, TreeCrumb),
     tree_crumb_entries(Repo, TreeOid, [TreePath, TreeCrumb], TreeCrumbs)].

tree_partial(Arg, Repo, TreeOid, TreePath) ->
    RepoPath = Arg#arg.docroot ++ element(3, Repo),
    BranchList = gert:get_branches(RepoPath),
    BranchMatchREs = lists:map(fun(E) -> {valid, "^" ++ E ++ "$"} end, BranchList),
    BranchMatch = elgit_shared:list_match(BranchMatchREs, "refs/heads/" ++ TreeOid),
    case BranchMatch of
        valid ->
            CommitOid = gert:get_commit_oid(RepoPath, "refs/heads/" ++ TreeOid);
        nomatch ->
            CommitOid = TreeOid % we should already have a commit oid
    end,
    case TreePath of
        [] ->
            [repo_header(Arg, Repo, CommitOid),
             tree_crumb(Repo, TreeOid, TreePath),
             tree_entries(Arg, Repo, TreeOid, CommitOid, TreePath)];
        _ ->
            [tree_crumb(Repo, TreeOid, TreePath),
             tree_entries(Arg, Repo, TreeOid, CommitOid, TreePath)]
    end.

tree_entries(Arg, Repo, TreeOid, CommitOid, TreePath) ->
    Entries = gert:get_tree(Arg#arg.docroot ++ element(3, Repo), CommitOid, TreePath),
    TreeEntries = [L || {tree, _} = L <- Entries],
    BlobEntries = [L || {blob, _} = L <- Entries],
    SubmoduleEntries = [L || {submodule, _} = L <- Entries],

    {html, [<<"
<div id=\"tree\">
    <ul>
    ">>,
        tree_folders(Repo, TreeOid, TreePath, lists:sort(lists:merge(TreeEntries, SubmoduleEntries))),
        tree_files(Repo, TreeOid, TreePath, lists:sort(BlobEntries)),
    <<"
    </ul>
</div>
    ">>]}.

tree_folders(_, _, _, []) ->
    [];
tree_folders(Repo, TreeOid, TreePath, [Folder|Folders]) ->
    case Folder of
        {tree, _} ->
            FolderType = "tree";
        {submodule, _} ->
            FolderType = "submodule"
    end,
    RepoSlug = element(1, Repo),
    FolderName = element(2, Folder),
    [<<"
<li class=\"">>, FolderType, <<"\">
    <a href=\"/">>, RepoSlug, <<"/tree/">>,
                    TreeOid, <<"/">>,
                    TreePath, FolderName, <<"/\">">>,
                    FolderName, <<"</a>
</li>
    ">>] ++ tree_folders(Repo, TreeOid, TreePath, Folders).

tree_files(_, _, _, []) ->
    [];
tree_files(Repo, TreeOid, TreePath, [File|Files]) ->
    RepoSlug = element(1, Repo),
    FileName = element(2, File),
    [<<"
<li class=\"blob\">
    <a href=\"/">>, RepoSlug, <<"/blob/">>,
                    TreeOid, <<"/">>,
                    TreePath, FileName, <<"\">">>,
                    FileName, <<"</a>
</li>
    ">>] ++ tree_files(Repo, TreeOid, TreePath, Files).