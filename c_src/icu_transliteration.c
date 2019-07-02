
#include "icu_nif.h"

static int icu_transliteration_direction_from_string(const char *,
                                                     UTransDirection *);

ERL_NIF_TERM
icu_utrans_open_ids(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        UErrorCode status;
        ERL_NIF_TERM id_list;
        UEnumeration *enumeration;
        ERL_NIF_TERM *ids;
        int32_t nb_ids;

        status = U_ZERO_ERROR;
        enumeration = utrans_openIDs(&status);
        if (U_FAILURE(status))
                return icu_error_code_exception(env, status);

        status = U_ZERO_ERROR;
        nb_ids = uenum_count(enumeration, &status);
        if (U_FAILURE(status))
                return icu_error_code_exception(env, status);

        ids = enif_alloc((size_t)nb_ids * sizeof(*ids));
        if (ids == NULL) {
                uenum_close(enumeration);
                return icu_memory_allocation_exception(env);
        }

        for (int32_t i = 0; i < nb_ids; i++) {
                const UChar *id;
                int32_t id_len;
                size_t bin_size;
                ErlNifBinary bin;

                status = U_ZERO_ERROR;
                id = uenum_unext(enumeration, &id_len, &status);
                if (U_FAILURE(status)) {
                        enif_free(ids);
                        uenum_close(enumeration);
                        return icu_error_code_exception(env, status);
                }

                bin_size = (size_t)id_len * sizeof(UChar);
                if (!enif_alloc_binary(bin_size, &bin)) {
                        enif_free(ids);
                        return icu_memory_allocation_exception(env);
                }
                memcpy(bin.data, id, bin_size);

                ids[i] = enif_make_binary(env, &bin);
        }

        id_list = enif_make_list_from_array(env, ids, (size_t)nb_ids);

        enif_free(ids);
        uenum_close(enumeration);

        return id_list;
}

ERL_NIF_TERM
icu_utrans_open_u(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct icu_nif_data *nif_data;

        char dir_str[MAXATOMLEN + 1];
        ErlNifBinary id;
        UTransDirection dir;
        UErrorCode status;
        int ret;

        UTransliterator *transliterator;
        ERL_NIF_TERM transliterator_rc;

        nif_data = (struct icu_nif_data *)enif_priv_data(env);

        if (argc != 2)
                return enif_make_badarg(env);

        // Transliterator identifier
        if (!enif_inspect_binary(env, argv[0], &id))
                return enif_make_badarg(env);

        // Transliteration direction
        ret = enif_get_atom(env, argv[1], dir_str, sizeof(dir_str),
                            ERL_NIF_LATIN1);
        if (ret == 0)
                return enif_make_badarg(env);

        if (icu_transliteration_direction_from_string(dir_str, &dir) == -1)
                return enif_make_badarg(env);

        // Transliterator
        status = U_ZERO_ERROR;
        transliterator = utrans_openU((const UChar *)id.data,
                                      id.size / sizeof(UChar),
                                      dir, NULL, -1, NULL, &status);
        if (U_FAILURE(status))
                return icu_error_code_exception(env, status);
        if (transliterator == NULL) {
                return icu_error_string_exception(
                        env, "no transliterator found with id '%s", id);
        }

        // Transliterator resource
        ret = icu_nif_create_rc(env, nif_data->transliterator_rc_type,
                                &transliterator, sizeof(transliterator),
                                &transliterator_rc);
        if (ret == -1) {
                return icu_error_string_exception(
                        env, "cannot create transliterator resource");
        }

        return transliterator_rc;
}

ERL_NIF_TERM
icu_utrans_uchars(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct icu_nif_data *nif_data;

        UTransliterator *transliterator, **ptransliterator;
        ErlNifBinary in, out;
        UErrorCode status;
        UChar *buf;
        int32_t buf_len, buf_cap, limit;
        int ret;

        nif_data = (struct icu_nif_data *)enif_priv_data(env);

        if (argc != 2)
                return enif_make_badarg(env);

        // Transliterator
        ret = enif_get_resource(env, argv[0],
                                nif_data->transliterator_rc_type,
                                (void **)&ptransliterator);
        if (!ret)
                return enif_make_badarg(env);
        transliterator = *ptransliterator;

        // Input ustring
        if (!enif_inspect_binary(env, argv[1], &in))
                return enif_make_badarg(env);

        // Allocate a buffer since utrans_transUChars mutates the input string
        buf_len = in.size / sizeof(UChar);
        buf_cap = buf_len;
        buf = enif_alloc((size_t)buf_cap * sizeof(UChar) + 1);
        if (buf == NULL)
                return icu_memory_allocation_exception(env);

        memcpy(buf, in.data, in.size);
        buf[in.size] = '\0';

        limit = buf_len;

        // Try to transliterate a first time. If the output, does not need
        // more space than the input, it will succeed. If it requires more
        // space, it will fail with U_BUFFER_OVERFLOW_ERROR.
        status = U_ZERO_ERROR;
        utrans_transUChars(transliterator, buf, &buf_len, buf_cap,
                           0, &limit, &status);
        if (U_FAILURE(status) && status != U_BUFFER_OVERFLOW_ERROR) {
                enif_free(buf);
                return icu_error_code_exception(env, status);
        }

        if (status == U_BUFFER_OVERFLOW_ERROR) {
                UChar *nbuf;

                // Since buf_len now contains the required buffer length, we
                // can extend the buffer.
                buf_cap = buf_len;

                nbuf = enif_realloc(buf, (size_t)buf_cap * sizeof(UChar) + 1);
                if (nbuf == NULL) {
                        enif_free(buf);
                        return icu_error_code_exception(env, status);
                }
                buf = nbuf;

                // Restore the original input string.
                buf_len = in.size / sizeof(UChar);
                limit = buf_len;

                memcpy(buf, in.data, in.size);
                buf[in.size] = '\0';

                // Retry transliteration now that the buffer is large enough.
                status = U_ZERO_ERROR;
                utrans_transUChars(transliterator, buf, &buf_len, buf_cap,
                                   0, &limit, &status);
                if (U_FAILURE(status)) {
                        enif_free(buf);
                        return icu_error_code_exception(env, status);
                }
        }

        // Copy the buffer to the output binary
        if (!enif_alloc_binary((size_t)buf_len * sizeof(UChar), &out)) {
                enif_free(buf);
                return icu_memory_allocation_exception(env);
        }
        memcpy(out.data, buf, out.size);

        enif_free(buf);

        return enif_make_binary(env, &out);
}

static int
icu_transliteration_direction_from_string(const char *s, UTransDirection *pdir) {
        if (strcmp(s, "forward") == 0) {
                *pdir = UTRANS_FORWARD;
        } else if (strcmp(s, "reverse") == 0) {
                *pdir = UTRANS_REVERSE;
        } else {
                return -1;
        }

        return 0;
}
