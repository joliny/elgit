-module(elgit_repo).
-export([out/1]).

-include_lib("yaws_api.hrl").

out(Arg) ->
    case yaws_api:queryvar(Arg, "partial") of
        {ok, _} ->
            out_partial(Arg);
        undefined ->
            [elgit_www:header(Arg),
             out_partial(Arg),
             elgit_www:footer(Arg)]
    end.

out_partial(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    Repo = elgit_shared:get_repo(Arg, string:substr(Path, 2)),
    {html, [<<"
<script data-main=\"/js/elgit.js\" src=\"/js/lib/require.js\"></script>
">>]}.