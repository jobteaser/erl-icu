
#include "icu_nif.h"

struct icu_nif_data *
icu_nif_data_new(void) {
        struct icu_nif_data *data;

        data = enif_alloc(sizeof(*data));
        if (data == NULL) {
                enif_fprintf(stderr, "cannot allocate nif data: %s\n",
                             strerror(errno));
                return NULL;
        }

        return data;
}

void
icu_nif_data_delete(struct icu_nif_data *data) {
        if (!data)
                return;

        enif_free(data);
}

int
icu_nif_data_open_resources(struct icu_nif_data *data, ErlNifEnv *env) {
        ErlNifResourceType *rc;

        rc = icu_nif_open_rc_type(env, "static_normalizer", NULL);
        if (rc == NULL)
                return -1;
        data->static_normalizer_rc_type = rc;

        rc = icu_nif_open_rc_type(env, "transliterator",
                                  icu_transliterator_dtor);
        if (rc == NULL)
                return -1;
        data->transliterator_rc_type = rc;

        return 0;
}
