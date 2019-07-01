
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

ERL_NIF_TERM
icu_str_to_lower(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary in, out;
        char locale_buf[ICU_LOCALE_BUFSZ];
        const char *locale;
        UErrorCode status;
        int32_t ulen;

        if (argc != 1 && argc != 2)
                return enif_make_badarg(env);

        if (!enif_inspect_binary(env, argv[0], &in))
                return enif_make_badarg(env);

        if (argc > 2) {
                int ret;

                ret = enif_get_string(env, argv[1], locale_buf, ICU_LOCALE_BUFSZ,
                                      ERL_NIF_LATIN1);
                if (ret <= 0) {
                        return enif_make_badarg(env);
                }

                locale = locale_buf;
        } else {
                locale = NULL;
        }

        status = U_ZERO_ERROR;
        ulen = u_strToLower(NULL, 0,
                            (const UChar *)in.data, in.size / sizeof(UChar),
                            locale, &status);
        if (U_FAILURE(status) && status != U_BUFFER_OVERFLOW_ERROR)
                return icu_error_code_exception(env, status);

        if (!enif_alloc_binary((size_t)ulen * sizeof(UChar), &out))
                return icu_memory_allocation_exception(env);

        status = U_ZERO_ERROR;
        u_strToLower((UChar *)out.data, ulen,
                     (const UChar *)in.data, in.size / sizeof(UChar),
                     locale, &status);
        if (U_FAILURE(status)) {
                enif_release_binary(&out);
                return icu_error_code_exception(env, status);
        }

        return enif_make_binary(env, &out);
}

ERL_NIF_TERM
icu_str_to_upper(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        ErlNifBinary in, out;
        char locale_buf[ICU_LOCALE_BUFSZ];
        const char *locale;
        UErrorCode status;
        int32_t ulen;

        if (argc != 1 && argc != 2)
                return enif_make_badarg(env);

        if (!enif_inspect_binary(env, argv[0], &in))
                return enif_make_badarg(env);

        if (argc > 2) {
                int ret;

                ret = enif_get_string(env, argv[1],
                                      locale_buf, sizeof(locale_buf),
                                      ERL_NIF_LATIN1);
                if (ret <= 0) {
                        return enif_make_badarg(env);
                }

                locale = locale_buf;
        } else {
                locale = NULL;
        }

        status = U_ZERO_ERROR;
        ulen = u_strToUpper(NULL, 0,
                            (const UChar *)in.data, in.size / sizeof(UChar),
                            locale, &status);
        if (U_FAILURE(status) && status != U_BUFFER_OVERFLOW_ERROR)
                return icu_error_code_exception(env, status);

        if (!enif_alloc_binary((size_t)ulen * sizeof(UChar), &out))
                return icu_memory_allocation_exception(env);

        status = U_ZERO_ERROR;
        u_strToUpper((UChar *)out.data, ulen,
                     (const UChar *)in.data, in.size / sizeof(UChar),
                     locale, &status);
        if (U_FAILURE(status)) {
                enif_release_binary(&out);
                return icu_error_code_exception(env, status);
        }

        return enif_make_binary(env, &out);
}
