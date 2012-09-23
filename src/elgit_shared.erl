-module(elgit_shared).
-export([
    join/2,
    get_repo/1,
    get_repo_list/0,
    list_match/2,
    list_replace/2
]).

-include_lib("yaws_api.hrl").

%%%
%   repository list
%%%
get_repo_list() ->
    % 1. slug
    % 2. Name
    % 3. path relative to docroot
    [{"elgit", "El Git", "/../.git"},
     {"gert", "Gert", "/../deps/gert/.git"}].

get_repo(XhrAction) ->
    get_repo_match(get_repo_list(), XhrAction).

get_repo_match([], _) ->
    nomatch;
get_repo_match([Repo|RepoList], XhrAction) ->
    RERepo = "^" ++ element(1, Repo) ++ "/.*",
    case re:run(XhrAction, RERepo, [{capture, none}]) of
        match ->
            Repo;
        nomatch ->
            get_repo_match(RepoList, XhrAction)
    end.

%%%
%   helper methods
%%%
join([], _) ->
    "";
join([First|Rest], JoinWith) ->
    lists:flatten([First] ++ [JoinWith ++ X || X <- Rest]).

list_match([], _) ->
    nomatch;
list_match([RE|REs], String) ->
    {REAtom, REExp} = RE,
    case re:run(String, REExp, [{capture, none}]) of
        match ->
            REAtom;
        nomatch ->
            list_match(REs, String)
    end.

list_replace([], String) ->
    String;
list_replace([RE|REs], String) ->
    Match = element(1, RE),
    Replace = element(2, RE),
    Options = element(3, RE),
    re:replace(list_replace(REs, String), Match, Replace, Options).