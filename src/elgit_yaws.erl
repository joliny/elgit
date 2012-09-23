-module(elgit_yaws).
-export([out/1]).

-include_lib("gert.hrl").
-include_lib("yaws_api.hrl").

%%%
%   request methods
%%%
out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
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
     {html, [<<"
<ul>
    <li><a href=\"/gert/\">Gert</a></li>
    <li><a href=\"/elgit/\">Elgit</a></li>
</ul>
">>]},
     elgit_www:footer(Arg)].