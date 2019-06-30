
-module(icu_string).

-export([from_utf8/1, to_utf8/1,
         transform_utf8/2, transform/2,
         to_lower/1, to_lower/2, to_upper/1, to_upper/2]).

-spec from_utf8(binary()) -> icu:ustring().
from_utf8(BinaryString) ->
  icu_nif:str_from_utf8(BinaryString).

-spec to_utf8(icu:ustring()) -> binary().
to_utf8(UString) ->
  icu_nif:str_to_utf8(UString).

-spec transform_utf8(binary(), [Fun]) -> binary() when
    Fun :: fun((icu:ustring()) -> icu:ustring()).
transform_utf8(BinaryString, Funs) ->
  UString = from_utf8(BinaryString),
  UString2 = transform(UString, Funs),
  to_utf8(UString2).

-spec transform(icu:ustring(), [Fun]) -> icu:ustring() when
    Fun :: fun((icu:ustring()) -> icu:ustring()).
transform(UString, []) ->
  UString;
transform(UString, [Fun | Rest]) ->
  transform(Fun(UString), Rest).

-spec to_lower(icu:ustring()) -> icu:ustring().
to_lower(UString) ->
  icu_nif:str_to_lower(UString).

-spec to_lower(icu:ustring(), string()) -> icu:ustring().
to_lower(UString, Locale) ->
  icu_nif:str_to_lower(UString, Locale).

-spec to_upper(icu:ustring()) -> icu:ustring().
to_upper(UString) ->
  icu_nif:str_to_upper(UString).

-spec to_upper(icu:ustring(), string()) -> icu:ustring().
to_upper(UString, Locale) ->
  icu_nif:str_to_upper(UString, Locale).
