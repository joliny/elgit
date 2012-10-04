-module(elgit_setup).
-export([out/1]).

-include_lib("elgit_records.hrl").
-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

out(Arg) ->
    Path = (yaws_api:request_url(Arg))#url.path,
    ReqTypes = [{index, "^/setup/$"},
                {defaults, "^/setup/repo_default/$"},
                {custom, "^/setup/repo_custom/$"}],
    ReqType = elgit_shared:list_match(ReqTypes, Path),
    case ReqType of
        index -> out_index(Arg);
        defaults -> setup_defaults(Arg);
        custom -> setup_custom(Arg);
        _ -> {redirect_local, "/setup/"}
    end.

out_index(Arg) ->
    [elgit_www:header(Arg),
     {html, [<<"
<form method=\"POST\" action=\"/setup/repo_default/\">
    <button type=\"submit\">Setup</button>
</form>
<form method=\"POST\" action=\"/setup/repo_custom/\">
    <input name=\"slug\"/>
    <input name=\"name\"/>
    <input name=\"path\"/>
    <button type=\"submit\">Setup</button>
</form>
     ">>]},
     elgit_www:footer(Arg)].

setup_defaults(Arg) ->
    Req = Arg#arg.req,
    case Req#http_request.method of
        'POST' ->
            RepoList = [#elgit_repo{slug = "elgit",
                                    name = "El Git",
                                    path = Arg#arg.docroot ++ "/../.git/"},
                        #elgit_repo{slug = "gert",
                                    name = "Gert",
                                    path = Arg#arg.docroot ++ "/../deps/gert/.git/"}],
            lists:foreach(fun(Repo) -> elgit_db:repo_add(Repo) end, RepoList);
        _ ->
            ok
    end,
    {redirect_local, "/setup/"}.

setup_custom(Arg) ->
    Req = Arg#arg.req,
    case Req#http_request.method of
        'POST' ->
            case {yaws_api:postvar(Arg, "slug"),
                  yaws_api:postvar(Arg, "name"),
                  yaws_api:postvar(Arg, "path")} of
                {{ok, Slug},
                 {ok, Name},
                 {ok, Path}} ->
                    elgit_db:repo_add(#elgit_repo{slug = Slug,
                                                  name = Name,
                                                  path = Path});
                _ ->
                 ok
            end;
        _ ->
            ok
    end,
    {redirect_local, "/setup/"}.