
-module(icu_nif).

-export([icu_version/0, unicode_version/0]).
-export([str_from_utf8/1, str_to_utf8/1,
         str_to_lower/1, str_to_lower/2, str_to_upper/1, str_to_upper/2]).
-export([unorm2_get_instance/2, unorm2_normalize/2]).
-export([utrans_open_ids/0, utrans_open_u/2, utrans_uchars/2]).

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

-spec str_to_lower(icu:ustring()) -> icu:ustring().
str_to_lower(_UString) ->
  erlang:nif_error(nif_not_loaded).

-spec str_to_lower(icu:ustring(), string()) -> icu:ustring().
str_to_lower(_UString, _Locale) ->
  erlang:nif_error(nif_not_loaded).

-spec str_to_upper(icu:ustring()) -> icu:ustring().
str_to_upper(_UString) ->
  erlang:nif_error(nif_not_loaded).

-spec str_to_upper(icu:ustring(), string()) -> icu:ustring().
str_to_upper(_UString, _Locale) ->
  erlang:nif_error(nif_not_loaded).

%%% Normalization

-type normalizer() :: reference().

-spec unorm2_get_instance(string(), Mode) -> normalizer() when
    Mode :: compose | decompose | fcd | compose_contiguous.
unorm2_get_instance(_Name, _Mode) ->
  erlang:nif_error(nif_not_loaded).

-spec unorm2_normalize(normalizer(), icu:ustring()) -> icu:ustring().
unorm2_normalize(_Normalizer, _UString) ->
  erlang:nif_error(nif_not_loaded).

%%% Transliteration

-type transliterator() :: reference().

-spec utrans_open_ids() -> [icu:ustring()].
utrans_open_ids() ->
  erlang:nif_error(nif_not_loaded).

-spec utrans_open_u(icu:ustring(), Direction) -> transliterator() when
    Direction :: forward | reverse.
utrans_open_u(_Id, _Direction) ->
  erlang:nif_error(nif_not_loaded).

-spec utrans_uchars(transliterator(), icu:ustring()) -> icu:ustring().
utrans_uchars(_Transliterator, _UString) ->
  erlang:nif_error(nif_not_loaded).
