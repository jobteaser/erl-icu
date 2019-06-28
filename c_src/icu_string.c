
#include "icu_nif.h"

ERL_NIF_TERM
icu_str_from_utf8(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary in, out;
        UErrorCode status;
        int32_t ulen;

        if (argc != 1)
                return enif_make_badarg(env);

        if (!enif_inspect_binary(env, argv[0], &in))
                return enif_make_badarg(env);

        status = U_ZERO_ERROR;
        u_strFromUTF8(NULL, 0, &ulen,
                      (const char *)in.data, in.size, &status);
        if (U_FAILURE(status) && status != U_BUFFER_OVERFLOW_ERROR)
                return icu_error_code_exception(env, status);

        if (!enif_alloc_binary((size_t)ulen * sizeof(UChar), &out))
                return icu_memory_allocation_exception(env);

        status = U_ZERO_ERROR;
        u_strFromUTF8((UChar *)out.data, ulen, NULL,
                      (const char *)in.data, in.size, &status);
        if (U_FAILURE(status)) {
                enif_release_binary(&out);
                return icu_error_code_exception(env, status);
        }

        return enif_make_binary(env, &out);
}

ERL_NIF_TERM
icu_str_to_utf8(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary in, out;
        UErrorCode status;
        int32_t len;

        if (argc != 1)
                return enif_make_badarg(env);

        if (!enif_inspect_binary(env, argv[0], &in))
                return enif_make_badarg(env);

        status = U_ZERO_ERROR;
        u_strToUTF8(NULL, 0, &len,
                    (const UChar *)in.data, in.size / sizeof(UChar), &status);
        if (U_FAILURE(status) && status != U_BUFFER_OVERFLOW_ERROR)
                return icu_error_code_exception(env, status);

        if (!enif_alloc_binary((size_t)len, &out))
                return icu_memory_allocation_exception(env);

        status = U_ZERO_ERROR;
        u_strToUTF8((char *)out.data, len, NULL,
                    (const UChar *)in.data, in.size / sizeof(UChar), &status);
        if (U_FAILURE(status)) {
                enif_release_binary(&out);
                return icu_error_code_exception(env, status);
        }

        return enif_make_binary(env, &out);
}
