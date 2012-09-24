-module(elgit_yaws).
-export([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

%%%
%   request methods
%%%
out(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    ReqTypes = [{index, "^/$"},
                {xhr, "^/xhr/.+"},
                {repo, "^/[[:alnum:]-]+/.*"}],
    ReqType = elgit_shared:list_match(ReqTypes, Path),
    case ReqType of
        index -> out_index(Arg);
        xhr -> elgit_xhr:out(Arg);
        repo -> elgit_repo:out(Arg);
        _ -> {redirect_local, "/"}
    end.

out_index(Arg) ->
    [elgit_www:header(Arg),
     {html, [get_repo_links(elgit_shared:get_repo_list())]},
     elgit_www:footer(Arg)].

get_repo_links([]) ->
    [];
get_repo_links([Repo|RepoList]) ->
    [<<"<a href=\"/">>, element(1, Repo), <<"/\" class=\"btn\">">>,
     element(2, Repo), <<"</a>">>] ++ get_repo_links(RepoList).