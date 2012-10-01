-module(elgit_setup).
-export([out/1]).

-include_lib("elgit_records.hrl").
-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    case Req#http_request.method of
        'POST' ->
            setup(Arg);
        _ ->
            [elgit_www:header(Arg),
             out_partial(Arg),
             elgit_www:footer(Arg)]
    end.

out_partial(_Arg) ->
    {html, [<<"
<form method=\"POST\" action=\"/setup/\">
    <button type=\"submit\">Setup</button>
</form>
    ">>]}.

setup(Arg) ->
    RepoList = [#elgit_repo{slug = "elgit",
                            name = "El Git",
                            path = Arg#arg.docroot ++ "/../.git/"},
                #elgit_repo{slug = "gert",
                            name = "Gert",
                            path = Arg#arg.docroot ++ "/../deps/gert/.git/"}],
    lists:foreach(fun(Repo) -> elgit_db:repo_add(Repo) end, RepoList),
    {redirect_local, "/"}.