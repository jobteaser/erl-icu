
#include "icu_nif.h"

static int icu_normalization_mode_from_string(const char *,
                                              UNormalization2Mode *);

ERL_NIF_TERM
icu_unorm2_get_instance(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct icu_nif_data *nif_data;

        char name[ICU_NORMALIZER_NAME_BUFSZ];
        char mode_str[MAXATOMLEN + 1];
        UNormalization2Mode mode;

        const UNormalizer2 *normalizer;
        ERL_NIF_TERM normalizer_rc;

        UErrorCode status;
        int ret;

        nif_data = (struct icu_nif_data *)enif_priv_data(env);

        if (argc != 2)
                return enif_make_badarg(env);

        // Normalizer name
        ret = enif_get_string(env, argv[0], name, sizeof(name),
                              ERL_NIF_LATIN1);
        if (ret <= 0)
                return enif_make_badarg(env);

        // Normalization mode
        ret = enif_get_atom(env, argv[1], mode_str, sizeof(mode_str),
                            ERL_NIF_LATIN1);
        if (ret == 0)
                return enif_make_badarg(env);

        if (icu_normalization_mode_from_string(mode_str, &mode) == -1)
                return enif_make_badarg(env);

        // Normalizer
        status = U_ZERO_ERROR;
        normalizer = unorm2_getInstance(NULL, name, mode, &status);
        if (U_FAILURE(status))
                return icu_error_code_exception(env, status);
        if (normalizer == NULL) {
                return icu_error_string_exception(
                        env, "no normalizer found with name '%s' and mode '%s'",
                        name, mode_str);
        }

        // Normalizer resource
        ret = icu_nif_create_rc(env, nif_data->rc_static_normalizer,
                                &normalizer, sizeof(normalizer),
                                &normalizer_rc);
        if (ret == -1) {
                return icu_error_string_exception(
                        env, "cannot create normalizer resource");
        }

        return normalizer_rc;
}

ERL_NIF_TERM
icu_unorm2_normalize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct icu_nif_data *nif_data;

        UNormalizer2 *normalizer, **pnormalizer;
        ErlNifBinary in, out;
        UErrorCode status;
        int32_t ulen;
        int ret;

        nif_data = (struct icu_nif_data *)enif_priv_data(env);

        if (argc != 2)
                return enif_make_badarg(env);

        ret = enif_get_resource(env, argv[0], nif_data->rc_static_normalizer,
                                (void **)&pnormalizer);
        if (!ret)
                return enif_make_badarg(env);
        normalizer = *pnormalizer;

        if (!enif_inspect_binary(env, argv[1], &in))
                return enif_make_badarg(env);

        status = U_ZERO_ERROR;
        ulen = unorm2_normalize(normalizer,
                                (const UChar *)in.data, in.size / sizeof(UChar),
                                NULL, 0, &status);
        if (U_FAILURE(status) && status != U_BUFFER_OVERFLOW_ERROR)
                return icu_error_code_exception(env, status);

        if (!enif_alloc_binary((size_t)ulen * sizeof(UChar), &out))
                return icu_memory_allocation_exception(env);

        status = U_ZERO_ERROR;
        ulen = unorm2_normalize(normalizer,
                                (const UChar *)in.data, in.size / sizeof(UChar),
                                (UChar *)out.data, ulen, &status);
        if (U_FAILURE(status)) {
                enif_release_binary(&out);
                return icu_error_code_exception(env, status);
        }

        return enif_make_binary(env, &out);
}

static int
icu_normalization_mode_from_string(const char *s, UNormalization2Mode *pmode) {
        if (strcmp(s, "compose") == 0) {
                *pmode = UNORM2_COMPOSE;
        } else if (strcmp(s, "decompose") == 0) {
                *pmode = UNORM2_DECOMPOSE;
        } else if (strcmp(s, "fcd") == 0) {
                *pmode = UNORM2_FCD;
        } else if (strcmp(s, "compose_contiguous") == 0) {
                *pmode = UNORM2_COMPOSE_CONTIGUOUS;
        } else {
                return -1;
        }

        return 0;
}
