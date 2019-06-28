
-module(icu_nif).

-export([icu_version/0, unicode_version/0]).

-on_load(init/0).

init() ->
  PrivDir = code:priv_dir(icu),
  LibPath = filename:join([PrivDir, "icu_nif"]),
  erlang:load_nif(LibPath, []).

%%% Versioning

-spec icu_version() -> string().
icu_version() ->
  erlang:nif_error(nif_not_loaded).

-spec unicode_version() -> string().
unicode_version() ->
  erlang:nif_error(nif_not_loaded).
