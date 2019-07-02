
-module(icu).

-export([transliterator_ids/0]).

-export_type([ustring/0, normalization_mode/0, transliteration_direction/0]).

-type ustring() :: binary().
%% An UTF-16 binary string.
%%
%% We use the term `ustring' to match the C type `UChar' used by ICU, making
%% it obvious that this is the type used by all ICU functions. In the
%% documentation, we also use the term "ICU string".

-type normalization_mode() :: nfc | nfkc | nfkc_cf | nfd | nfkd.
%% A normalization mode as defined by Unicode Standard Annex #15.
%%
%% @reference <a href="http://www.unicode.org/reports/tr15">UAX #15</a>.

-type transliteration_direction() :: forward | reverse.
%% The direction used when applying a transliteration rule.

%% @doc Return a list containing the identifier of available transliterators.
-spec transliterator_ids() -> [binary()].
transliterator_ids() ->
  lists:map(fun icu_string:to_utf8/1, icu_nif:utrans_open_ids()).
