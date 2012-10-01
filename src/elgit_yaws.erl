-module(elgit_yaws).
-export([out/1]).

-include_lib("elgit_records.hrl").
-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

%%%
%   request methods
%%%
out(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    ReqTypes = [{index, "^/$"},
                {setup, "^/setup/$"},
                {xhr, "^/xhr/.+"},
                {repo, "^/[[:alnum:]-]+/.*"}],
    ReqType = elgit_shared:list_match(ReqTypes, Path),
    case ReqType of
        index -> out_index(Arg);
        setup -> elgit_setup:out(Arg);
        xhr -> elgit_xhr:out(Arg);
        repo -> elgit_repo:out(Arg);
        _ -> {redirect_local, "/"}
    end.

out_index(Arg) ->
    [elgit_www:header(Arg),
     {html, [get_repo_links()]},
     elgit_www:footer(Arg)].

get_repo_links() ->
    case elgit_shared:get_repo_list() of
        [] ->
            get_setup_link();
        RepoList ->
            get_repo_link(RepoList)
    end.

get_setup_link() ->
    [<<"<a href=\"/setup/\" class=\"btn\">Setup (no repos defined, yet)</a>">>].

get_repo_link([]) ->
    [];
get_repo_link([Repo|RepoList]) ->
    [<<"
<a href=\"/">>, Repo#elgit_repo.slug, <<"/\" class=\"btn\">
     ">>, Repo#elgit_repo.name, <<"
</a>
    ">>] ++ get_repo_link(RepoList).