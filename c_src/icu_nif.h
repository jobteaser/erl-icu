
#ifndef ICU_NIF_H
#define ICU_NIF_H

#include <erl_nif.h>

#include <unicode/uchar.h>
#include <unicode/uvernum.h>

#define ICU_STRINGIFY(token_) #token_

#define ICU_EXPORT(name_) \
        ERL_NIF_TERM name_(ErlNifEnv *, int, const ERL_NIF_TERM [])

// Versioning
ICU_EXPORT(icu_icu_version);
ICU_EXPORT(icu_unicode_version);

#endif
