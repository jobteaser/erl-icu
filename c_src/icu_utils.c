
#include "icu_nif.h"

ERL_NIF_TERM
icu_ok_tuple(ErlNifEnv *env, ERL_NIF_TERM term) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}

ERL_NIF_TERM
icu_error_tuple(ErlNifEnv *env, ERL_NIF_TERM term) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), term);
}

ERL_NIF_TERM
icu_memory_allocation_exception(ErlNifEnv *env) {
        ERL_NIF_TERM reason;

        reason = enif_make_atom(env, "memory_allocation_error");
        return enif_raise_exception(env, reason);
}

ERL_NIF_TERM
icu_error_code_atom(ErlNifEnv *env, UErrorCode code) {
        const char *cstr;

        cstr = u_errorName(code);
        if (strlen(cstr) > 2 && cstr[0] == 'U' && cstr[1] == '_')
                cstr += 2;

        char str[strlen(cstr) + 1];
        for (size_t i = 0; i < strlen(cstr); i++)
                str[i] = tolower((unsigned char)cstr[i]);

        return enif_make_atom(env, str);
}

ERL_NIF_TERM
icu_error_code_exception(ErlNifEnv *env, UErrorCode code) {
        ERL_NIF_TERM reason;

        reason = enif_make_tuple2(env, enif_make_atom(env, "icu_error"),
                                  icu_error_code_atom(env, code));
        return enif_raise_exception(env, reason);
}
