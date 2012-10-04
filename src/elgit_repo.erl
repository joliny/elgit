-module(elgit_repo).
-export([out/1]).

-include_lib("elgit_records.hrl").
-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

out(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    Repo = elgit_shared:get_repo(string:substr(Path, 2)),
    ActionPath = string:substr(Path, 2 + string:len(Repo#elgit_repo.slug)),
    RepoActions = [{commits, "^/commits/.*"},
                   {tree, "^/tree/.*"},
                   {index, "^/$"}],
    RepoAction = elgit_shared:list_match(RepoActions, ActionPath),
    case RepoAction of
        nomatch ->
            {redirect_local, "/"};
        _ ->
            out_repo(Arg, RepoAction, Repo, ActionPath)
    end.

out_repo(Arg, RepoAction, Repo, ActionPath) ->
    case elgit_shared:is_partial(Arg) of
        false ->
            [elgit_www:header(Arg),
             header(RepoAction, ActionPath, Repo),
             out_partial(RepoAction, ActionPath, Repo),
             footer(),
             elgit_www:footer(Arg)];
        true ->
            out_partial(RepoAction, ActionPath, Repo)
    end.

out_partial(RepoAction, ActionPath, Repo) ->
    case RepoAction of
        commits ->
            commits(ActionPath, Repo);
        tree ->
            tree(ActionPath, Repo);
        index ->
            tree("/tree/master/", Repo);
        _->
            {redirect_local, "/"}
    end.

header(RepoAction, ActionPath, Repo) ->
    case RepoAction of
        commits ->
            SelectedAction = "commits";
        _ ->
            SelectedAction = "tree"
    end,
    PathBranch = re:run(ActionPath,
                        "^/.+/([[:alnum:]]+)/.*",
                        [{capture, [1], list}]),
    case PathBranch of
        {match, [Branch]} ->
            SelectedBranch = Branch;
        nomatch ->
            SelectedBranch = "master"
    end,
    BranchList = gert:get_branches(Repo#elgit_repo.path),
    ActionHrefPrefix = "/" ++ Repo#elgit_repo.slug ++ "/",
    ActionHrefSuffix = "/" ++ SelectedBranch ++ "/",
    {html, [<<"
<div id=\"repo-head\" class=\"form-inline\">
    <ul class=\"nav nav-tabs\">
        <li class=\"tree-tab\">
            <label>Branch:</label>
            <select id=\"tree-branch\">
                <option value=\"\">-- select a branch --</option>
                ">>, header_select_branch(BranchList, SelectedBranch), <<"
            </select>
        </li>

        ">>,
        header_select_action([{"tree", "Files", ActionHrefPrefix ++ "tree" ++ ActionHrefSuffix},
                              {"commits", "Commits", ActionHrefPrefix ++ "commits" ++ ActionHrefSuffix}],
                             SelectedAction),
        <<"
    </ul>
</div>

<div id=\"repo\">
    ">>]}.

header_select_branch([], _) ->
    [];
header_select_branch([Branch|Branches], SelectedBranch) ->
    Slug = re:replace(Branch, "refs/heads/", "", [{return, list}]),
    if
        Slug =:= SelectedBranch ->
            SelectedAttr = <<" selected=\"selected\"">>;
        true ->
            SelectedAttr = <<"">>
    end,
    [<<"<option value=\"">>, Slug, <<"\"">>, SelectedAttr, <<">">>,
     Slug,
     <<"</option>">>] ++ header_select_branch(Branches, SelectedBranch).

header_select_action([], _) ->
    [];
header_select_action([Action|Actions], SelectedAction) ->
    {ActionSlug, ActionTitle, ActionHref} = Action,
    if
        ActionSlug =:= SelectedAction ->
            SelectedAttr = <<" class=\"active\"">>;
        true ->
            SelectedAttr = <<"">>
    end,
    [<<"
<li">>, SelectedAttr, <<">
    <a href=\"">>, ActionHref, <<"\">">>, ActionTitle, <<"</a>
</li>
    ">>] ++ header_select_action(Actions, SelectedAction).

footer() ->
    {html, [<<"</div>">>]}.

repo_header(Repo, CommitOid) ->
    Commit = gert:get_commit_record(Repo#elgit_repo.path, CommitOid),
    CommitMessage = elgit_shared:list_replace([{"\n", "\\\\n", [global]},
                                               {"\n$", "", [global]},
                                               {"\r", "", [global]},
                                               {"\"", "\\\\\"", [global]}
                                              ], Commit#commit.message),
    CommitAuthor = re:replace(Commit#commit.author, "\"", "\\\\\"", [global]),
    {html, [<<"
<div id=\"last-commit\" class=\"well well-small\">
    <p class=\"message\">">>, CommitMessage, <<"</p>
    <div class=\"meta clearfix\">
        <span class=\"author\">">>, CommitAuthor, <<"</span>
        <span class=\"oid\">">>, CommitOid, <<"</span>
    </div>
</div>
    ">>]}.

commits(ActionPath, Repo) ->
    ActionParts = re:run(ActionPath, "^/commits/([[:alnum:]]+)/", [{capture, [1], list}]),
    case ActionParts of
        {match, [HeadOid]} ->
            commits_partial(Repo, HeadOid);
        nomatch ->
            {redirect_local, "/"}
    end.

commits_partial(_Repo, HeadOid) ->
    {html, [<<"
<div class=\"well well-small\">
    Commits at <strong>">>, HeadOid, <<"</strong>
</div>
    ">>]}.

tree(ActionPath, Repo) ->
    ActionParts = re:run(ActionPath, "^/tree/([[:alnum:]]+)/(.*)", [{capture, [1,2], list}]),
    case ActionParts of
        {match, Matches} ->
            TreeOid = lists:nth(1, Matches),
            TreePath = lists:nth(2, Matches),
            tree_partial(Repo, TreeOid, TreePath);
        nomatch ->
            {redirect_local, "/"}
    end.

tree_crumb(Repo, TreeOid, TreePath) ->
    TreeCrumbs = re:split(TreePath, "/", [{return, list}]),
    {html, [<<"
<ul id=\"tree-crumb\" class=\"well breadcrumb\">
    ">>,
    case TreeCrumbs of
        [[]|[]] ->
            tree_crumb_root(Repo, TreeOid, active);
        _ ->
            [tree_crumb_root(Repo, TreeOid, inactive),
             tree_crumb_entries(Repo, TreeOid, [], TreeCrumbs)]
    end,
    <<"
</ul>
    ">>]}.

tree_crumb_root(Repo, TreeOid, IsActive) ->
    case IsActive of
        active ->
            [<<"<li class=\"active\">">>, Repo#elgit_repo.slug, <<"</li>">>];
        _ ->
            [<<"
<li>
    <a href=\"/">>, Repo#elgit_repo.slug, <<"/tree/">>,
                    TreeOid, <<"/\">">>,
                    Repo#elgit_repo.slug, <<"</a>
    <span class=\"divider\">/</span>
</li>
            ">>]
    end.

tree_crumb_entry(Repo, TreeOid, TreeLink, TreeCrumb, IsActive) ->
    case IsActive of
        active ->
            [<<"<li class=\"active\">">>, TreeCrumb, <<"</li>">>];
        _ ->
            [<<"
<li>
    <a href=\"/">>, Repo#elgit_repo.slug, <<"/tree/">>,
                    TreeOid, <<"/">>,
                    TreeLink, <<"/\">">>,
                    TreeCrumb, <<"</a>
    <span class=\"divider\">/</span>
</li>
            ">>]
    end.

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
    case TreeCrumbs of
        [[]] ->
            IsActive = active;
        _ ->
            IsActive = inactive
    end,
    [tree_crumb_entry(Repo, TreeOid, TreeLink, TreeCrumb, IsActive),
     tree_crumb_entries(Repo, TreeOid, [TreePath, TreeCrumb], TreeCrumbs)].

tree_partial(Repo, TreeOid, TreePath) ->
    BranchList = gert:get_branches(Repo#elgit_repo.path),
    BranchMatchREs = lists:map(fun(E) -> {valid, "^" ++ E ++ "$"} end, BranchList),
    BranchMatch = elgit_shared:list_match(BranchMatchREs, "refs/heads/" ++ TreeOid),
    case BranchMatch of
        valid ->
            CommitOid = gert:get_commit_oid(Repo#elgit_repo.path, "refs/heads/" ++ TreeOid);
        nomatch ->
            CommitOid = TreeOid % we should already have a commit oid
    end,
    case TreePath of
        [] ->
            [repo_header(Repo, CommitOid),
             tree_crumb(Repo, TreeOid, TreePath),
             tree_entries(Repo, TreeOid, CommitOid, TreePath)];
        _ ->
            [tree_crumb(Repo, TreeOid, TreePath),
             tree_entries(Repo, TreeOid, CommitOid, TreePath)]
    end.

tree_entries(Repo, TreeOid, CommitOid, TreePath) ->
    Entries = gert:get_tree(Repo#elgit_repo.path, CommitOid, TreePath),
    TreeEntries = [L || {tree, _} = L <- Entries],
    BlobEntries = [L || {blob, _} = L <- Entries],
    SubmoduleEntries = [L || {submodule, _} = L <- Entries],

    {html, [<<"
<div id=\"tree\">
    <table id=\"tree-table\" class=\"well table table-striped\">
        <thead>
            <th></th>
            <th>name</th>
        </thead>
        <tbody>
            ">>,
            tree_folders(Repo, TreeOid, TreePath, lists:sort(lists:merge(TreeEntries, SubmoduleEntries))),
            tree_files(Repo, TreeOid, TreePath, lists:sort(BlobEntries)),
            <<"
        </tbody>
    </table>
</div>
    ">>]}.

tree_folders(_, _, _, []) ->
    [];
tree_folders(Repo, TreeOid, TreePath, [Folder|Folders]) ->
    case Folder of
        {tree, _} ->
            FolderType = "tree",
            FolderIcon = "icon-folder-open";
        {submodule, _} ->
            FolderType = "submodule",
            FolderIcon = "icon-folder-close"
    end,
    FolderName = element(2, Folder),
    [<<"
<tr class=\"">>, FolderType, <<"\">
    <td class=\"icon\"><span class=\"">>, FolderIcon, <<"\"></span></td>
    <td class=\"name\">
        <a href=\"/">>, Repo#elgit_repo.slug, <<"/tree/">>,
                        TreeOid, <<"/">>,
                        TreePath, FolderName, <<"/\">">>,
                        FolderName, <<"</a>
    </dt>
</tr>
    ">>] ++ tree_folders(Repo, TreeOid, TreePath, Folders).

tree_files(_, _, _, []) ->
    [];
tree_files(Repo, TreeOid, TreePath, [File|Files]) ->
    FileName = element(2, File),
    [<<"
<tr class=\"blob\">
    <td class=\"icon\"><span class=\"icon-file\"></span></td>
    <td class=\"name\">
        <a href=\"/">>, Repo#elgit_repo.slug, <<"/blob/">>,
                        TreeOid, <<"/">>,
                        TreePath, FileName, <<"\">">>,
                        FileName, <<"</a>
    </td>
</tr>
    ">>] ++ tree_files(Repo, TreeOid, TreePath, Files).