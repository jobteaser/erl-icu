
-module(icu_string).

-export([from_utf8/1, to_utf8/1,
         transform_utf8/2, transform/2,
         to_lower/1, to_lower/2, to_upper/1, to_upper/2,
         normalize/2]).

%% @doc Convert an UTF-8 encoded binary string to an ICU string.
-spec from_utf8(binary()) -> icu:ustring().
from_utf8(BinaryString) ->
  icu_nif:str_from_utf8(BinaryString).

%% @doc Convert an ICU string to an UTF-8 encoded binary string.
-spec to_utf8(icu:ustring()) -> binary().
to_utf8(UString) ->
  icu_nif:str_to_utf8(UString).

%% @doc Apply a list of transformation functions to an UTF-8 binary string.
%%
%% The function simply wraps `transform/2', adding the necessary input and
%% output format conversions.
%%
%% @see transform/2
-spec transform_utf8(binary(), [Fun]) -> binary() when
    Fun :: fun((icu:ustring()) -> icu:ustring()).
transform_utf8(BinaryString, Funs) ->
  UString = from_utf8(BinaryString),
  UString2 = transform(UString, Funs),
  to_utf8(UString2).

%% @doc Apply a list of transformation functions to an ICU string.
%%
%% Each transformation is applied to the output of the previous one. The
%% function returns the output of the last transformation.
-spec transform(icu:ustring(), [Fun]) -> icu:ustring() when
    Fun :: fun((icu:ustring()) -> icu:ustring()).
transform(UString, []) ->
  UString;
transform(UString, [Fun | Rest]) ->
  transform(Fun(UString), Rest).

%% @doc Convert an ICU string to lower case using the default locale.
-spec to_lower(icu:ustring()) -> icu:ustring().
to_lower(UString) ->
  icu_nif:str_to_lower(UString).

%% @doc Convert an ICU string to lower case.
-spec to_lower(icu:ustring(), string()) -> icu:ustring().
to_lower(UString, Locale) ->
  icu_nif:str_to_lower(UString, Locale).

%% @doc Convert an ICU string to upper case using the default locale.
-spec to_upper(icu:ustring()) -> icu:ustring().
to_upper(UString) ->
  icu_nif:str_to_upper(UString).

%% @doc Convert an ICU string to upper case.
-spec to_upper(icu:ustring(), string()) -> icu:ustring().
to_upper(UString, Locale) ->
  icu_nif:str_to_upper(UString, Locale).

%% @doc Normalize an ICU string.
-spec normalize(icu:ustring(), icu:normalization_mode()) -> icu:ustring().
normalize(UString, Mode) ->
  Normalizer = normalizer(Mode),
  icu_nif:unorm2_normalize(Normalizer, UString).

%% @doc Return the normalizer associated with a normalization mode.
-spec normalizer(icu:normalization_mode()) -> icu_nif:normalizer().
normalizer(nfc) ->
  icu_nif:unorm2_get_instance("nfc", compose);
normalizer(nfkc) ->
  icu_nif:unorm2_get_instance("nfkc", compose);
normalizer(nfkc_cf) ->
  icu_nif:unorm2_get_instance("nfkc_cf", compose);
normalizer(nfd) ->
  icu_nif:unorm2_get_instance("nfc", decompose);
normalizer(nfkd) ->
  icu_nif:unorm2_get_instance("nfkc", decompose).
