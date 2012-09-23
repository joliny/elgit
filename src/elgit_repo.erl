-module(elgit_repo).
-export([out/1]).

-include_lib("yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    Repo = elgit_shared:get_repo(Arg, string:substr(Path, 2)),
    [elgit_www:header(Arg),
     {html, [<<"
<script data-main=\"/js/elgit.js\" src=\"/js/lib/require.js\"></script>
">>]},
     elgit_www:footer(Arg)].