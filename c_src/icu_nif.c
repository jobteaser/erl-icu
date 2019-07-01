
#include "icu_nif.h"

ERL_NIF_TERM
icu_icu_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        return enif_make_string(env, U_ICU_VERSION, ERL_NIF_LATIN1);
}

ERL_NIF_TERM
icu_unicode_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        return enif_make_string(env, U_UNICODE_VERSION, ERL_NIF_LATIN1);
}

static ErlNifFunc icu_nif_functions[] = {
        // Versioning
        {"icu_version", 0, icu_icu_version, 0},
        {"unicode_version", 0, icu_unicode_version, 0},

        // Strings
        {"str_from_utf8", 1, icu_str_from_utf8, 0},
        {"str_to_utf8", 1, icu_str_to_utf8, 0},

        {"str_to_lower", 1, icu_str_to_lower, 0},
        {"str_to_lower", 2, icu_str_to_lower, 0},
        {"str_to_upper", 1, icu_str_to_upper, 0},
        {"str_to_upper", 2, icu_str_to_upper, 0},

        // Normalization
        {"unorm2_get_instance", 2, icu_unorm2_get_instance, 0},
        {"unorm2_normalize", 2, icu_unorm2_normalize, 0},
};

static int
icu_nif_load(ErlNifEnv *env, void **p_data_ptr, ERL_NIF_TERM load_info) {
        struct icu_nif_data *data;

        data = icu_nif_data_new();
        if (data == NULL)
                return -1;

        if (icu_nif_data_open_resources(data, env) == -1)
                return -1;

        *p_data_ptr = data;
        return 0;
}

static int
icu_nif_upgrade(ErlNifEnv *env, void **p_data_ptr, void **p_old_data_ptr,
                ERL_NIF_TERM load_info) {
        return 0;
}

static void
icu_nif_unload(ErlNifEnv *env, void *data_ptr) {
        struct icu_nif_data *data;

        data = data_ptr;

        icu_nif_data_delete(data);
}

ERL_NIF_INIT(icu_nif, icu_nif_functions,
             icu_nif_load, NULL, icu_nif_upgrade, icu_nif_unload)
