-module(elgit_repo).
-export([out/1]).

-include_lib("yaws_api.hrl").

out(Arg) ->
    case elgit_shared:is_partial(Arg) of
        false ->
            [elgit_www:header(Arg),
             out_partial(Arg),
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
            index(Repo);
        _ ->
            {redirect_local, "/"}
    end.

index(Repo) ->
    [{html, [<<"
<script data-main=\"/js/elgit.js\" src=\"/js/lib/require.js\"></script>
">>]}].

tree(Arg, ActionPath, Repo) ->
    ActionParts = re:run(ActionPath, "^/tree/([[:alnum:]]+)/(.*)", [{capture, [1,2], list}]),
    case ActionParts of
        {match, Matches} ->
            TreeOid = lists:nth(1, Matches),
            TreePath = lists:nth(2, Matches),
            TreeEntries = gert:get_tree(Arg#arg.docroot ++ element(3, Repo), TreeOid, TreePath),
            tree_entries(Repo, TreeOid, TreePath, TreeEntries);
        nomatch ->
            {redirect_local, "/"}
    end.

tree_entries(Repo, TreeOid, TreePath, Entries) ->
    TreeEntries = [L || {tree, _} = L <- Entries],
    BlobEntries = [L || {blob, _} = L <- Entries],
    SubmoduleEntries = [L || {submodule, _} = L <- Entries],
    [{html, [<<"<ul>">>]},
     {html, tree_folders(Repo, TreeOid, TreePath, lists:sort(lists:merge(TreeEntries, SubmoduleEntries)))},
     {html, tree_files(Repo, TreeOid, TreePath, lists:sort(BlobEntries))},
     {html, [<<"</ul>">>]}].

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
    [<<"<li class=\"">>, FolderType, <<"\"><a href=\"/">>,
         RepoSlug, <<"/tree/">>,
         TreeOid, <<"/">>,
         FolderName, <<"/\">">>,
         FolderName, <<"</a></li>">>
    ] ++ tree_folders(Repo, TreeOid, TreePath, Folders).

tree_files(_, _, _, []) ->
    [];
tree_files(Repo, TreeOid, TreePath, [File|Files]) ->
    RepoSlug = element(1, Repo),
    FileName = element(2, File),
    [<<"<li class=\"blob\"><a href=\"/">>,
         RepoSlug, <<"/blob/">>,
         TreeOid, <<"/">>,
         FileName, <<"/\">">>,
         FileName, <<"</a></li>">>
    ] ++ tree_files(Repo, TreeOid, TreePath, Files).