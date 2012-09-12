-module(elgit_app).

-export([start/0]).

start() ->
    application:start(elgit).