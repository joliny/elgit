-module(elgit_db).
-export([
    repo_add/1,
    repo_by_slug/1
]).

-include_lib("elgit_records.hrl").

repo_add(Repo) ->
    case repo_by_slug(Repo#elgit_repo.slug) of
        undefined ->
            F = fun() ->
                mnesia:write(Repo)
            end,
            mnesia:activity(transaction, F);
        _ ->
            ok
    end.

repo_by_slug(Slug) ->
    F = fun() ->
        case mnesia:read({elgit_repo, Slug}) of
            [#elgit_repo{name = N, path = P}] ->
                {Slug, N, P};
            [] ->
                undefined
        end
    end,
    mnesia:activity(transaction, F).