-module(elgit_yaws).

-export ([out/1]).

out(Arg) ->
    [{ehtml,
      [{h1, [], "Hello World!"}]}].