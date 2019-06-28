
-module(icu_nif).

-export([icu_version/0, unicode_version/0]).
-export([str_from_utf8/1, str_to_utf8/1]).

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

%%% Strings

-spec str_from_utf8(binary()) -> icu:ustring().
str_from_utf8(_BinaryString) ->
  erlang:nif_error(nif_not_loaded).

-spec str_to_utf8(icu:ustring()) -> binary().
str_to_utf8(_UString) ->
  erlang:nif_error(nif_not_loaded).
