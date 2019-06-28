
-module(icu_string).

-export([from_utf8/1, to_utf8/1]).

-spec from_utf8(binary()) -> icu:ustring().
from_utf8(BinaryString) ->
  icu_nif:str_from_utf8(BinaryString).

-spec to_utf8(icu:ustring()) -> binary().
to_utf8(UString) ->
  icu_nif:str_to_utf8(UString).
