
#ifndef ICU_NIF_H
#define ICU_NIF_H

#include <errno.h>
#include <string.h>
#include <ctype.h>

#include <ei.h>
#include <erl_nif.h>

#include <unicode/uchar.h>
#include <unicode/ustring.h>
#include <unicode/utypes.h>
#include <unicode/uvernum.h>
#include <unicode/unorm2.h>
#include <unicode/uenum.h>
#include <unicode/utrans.h>

#define ICU_STRINGIFY(token_) #token_

#define ICU_EXPORT(name_) \
        ERL_NIF_TERM name_(ErlNifEnv *, int, const ERL_NIF_TERM [])

#define ICU_LOCALE_BUFSZ 1024
#define ICU_NORMALIZER_NAME_BUFSZ 128
#define ICU_TRANSLITERATOR_ID_BUFSZ 128

// Versioning
ICU_EXPORT(icu_icu_version);
ICU_EXPORT(icu_unicode_version);

// Strings
ICU_EXPORT(icu_str_from_utf8);
ICU_EXPORT(icu_str_to_utf8);

ICU_EXPORT(icu_str_to_lower);
ICU_EXPORT(icu_str_to_upper);

// Normalization
ICU_EXPORT(icu_unorm2_get_instance);
ICU_EXPORT(icu_unorm2_normalize);

// Transliteration
ICU_EXPORT(icu_utrans_open_ids);
ICU_EXPORT(icu_utrans_open_u);
ICU_EXPORT(icu_utrans_uchars);

// NIF private data
struct icu_nif_data {
        ErlNifResourceType *static_normalizer_rc_type;
        ErlNifResourceType *transliterator_rc_type;
};

struct icu_nif_data *icu_nif_data_new(void);
void icu_nif_data_delete(struct icu_nif_data *);

int icu_nif_data_open_resources(struct icu_nif_data *, ErlNifEnv *);

void icu_transliterator_dtor(ErlNifEnv *, void *);

// Internal utils
ErlNifResourceType *icu_nif_open_rc_type(ErlNifEnv *, const char *,
                                         ErlNifResourceDtor *);
int icu_nif_create_rc(ErlNifEnv *, ErlNifResourceType *,
                      const void *, size_t, ERL_NIF_TERM *);

ERL_NIF_TERM icu_ok_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM icu_error_tuple(ErlNifEnv *, ERL_NIF_TERM);

ERL_NIF_TERM icu_error_string_exception(ErlNifEnv *, const char *, ...);
ERL_NIF_TERM icu_memory_allocation_exception(ErlNifEnv *);
ERL_NIF_TERM icu_error_code_atom(ErlNifEnv *, UErrorCode);
ERL_NIF_TERM icu_error_code_exception(ErlNifEnv *, UErrorCode);

#endif
