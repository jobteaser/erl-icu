
#include "icu_nif.h"

ERL_NIF_TERM
icu_icu_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        return enif_make_string(env, U_ICU_VERSION, ERL_NIF_LATIN1);
}

ERL_NIF_TERM
icu_unicode_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        return enif_make_string(env, U_UNICODE_VERSION, ERL_NIF_LATIN1);
}

#define ICU_NIF_FUNC(name_, arity_)                         \
        {ICU_STRINGIFY(name_), arity_, icu_ ## name_, 0}

static ErlNifFunc icu_nif_functions[] = {
        // Versioning
        ICU_NIF_FUNC(icu_version, 0),
        ICU_NIF_FUNC(unicode_version, 0),

        // Strings
        ICU_NIF_FUNC(str_from_utf8, 1),
        ICU_NIF_FUNC(str_to_utf8, 1),
};

#undef ICU_NIF_FUNC

ERL_NIF_INIT(icu_nif, icu_nif_functions, NULL, NULL, NULL, NULL)
