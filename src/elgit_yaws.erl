-module(elgit_yaws).
-export ([out/1]).

-include_lib("yaws/include/yaws_api.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    if
        Path /= "/" ->
            {redirect_local, "/"};
        true ->
            [{ehtml,
              [{h1, [], "Hello World!"},
               {h2, [], ["HEAD: ", gert:get_head_sha(Arg#arg.docroot ++ "/.git")]}]}]
    end.