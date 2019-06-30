
#ifndef ICU_NIF_H
#define ICU_NIF_H

#include <string.h>
#include <ctype.h>

#include <erl_nif.h>

#include <unicode/uchar.h>
#include <unicode/ustring.h>
#include <unicode/utypes.h>
#include <unicode/uvernum.h>

#define ICU_STRINGIFY(token_) #token_

#define ICU_EXPORT(name_) \
        ERL_NIF_TERM name_(ErlNifEnv *, int, const ERL_NIF_TERM [])

#define ICU_LOCALE_BUFSZ 1024

// Versioning
ICU_EXPORT(icu_icu_version);
ICU_EXPORT(icu_unicode_version);

// Strings
ICU_EXPORT(icu_str_from_utf8);
ICU_EXPORT(icu_str_to_utf8);

ICU_EXPORT(icu_str_to_lower);
ICU_EXPORT(icu_str_to_upper);

// Internal utils
ERL_NIF_TERM icu_ok_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM icu_error_tuple(ErlNifEnv *, ERL_NIF_TERM);

ERL_NIF_TERM icu_memory_allocation_exception(ErlNifEnv *);
ERL_NIF_TERM icu_error_code_atom(ErlNifEnv *, UErrorCode);
ERL_NIF_TERM icu_error_code_exception(ErlNifEnv *, UErrorCode);

#endif
