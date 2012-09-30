-module(elgit_app).
-export([start/0]).

-include("elgit_records.hrl").

start() ->
    ElgitAppDir = code:which(elgit_app),
    ElgitBaseDir = string:substr(ElgitAppDir, 1, string:len(ElgitAppDir) - 19),
    ElgitDbDir = ElgitBaseDir ++ "db/",
    application:set_env(mnesia, dir, ElgitDbDir),
    application:load(mnesia),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(elgit_repo,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, elgit_repo)}]),
    application:start(elgit).