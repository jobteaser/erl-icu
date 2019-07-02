
#include "icu_nif.h"

ErlNifResourceType *
icu_nif_open_rc_type(ErlNifEnv *env, const char *name, ErlNifResourceDtor *dtor) {
        ErlNifResourceFlags flags;
        ErlNifResourceType *rc;

        flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

        rc = enif_open_resource_type(env, NULL, name, NULL, flags, NULL);
        if (rc == NULL) {
                enif_fprintf(stderr, "cannot open resource '%s'");
                return NULL;
        }

        return rc;
}

int
icu_nif_create_rc(ErlNifEnv *env, ErlNifResourceType *type,
                  const void *data, size_t size, ERL_NIF_TERM *pterm) {
        ERL_NIF_TERM term;
        void *rc_data;

        if ((rc_data = enif_alloc_resource(type, size)) == NULL) {
                enif_fprintf(stderr, "cannot allocate resource: %s",
                             strerror(errno));
                return -1;
        }

        memcpy(rc_data, data, size);

        term = enif_make_resource(env, rc_data);
        enif_release_resource(rc_data);

        *pterm = term;
        return 0;
}

void
icu_transliterator_dtor(ErlNifEnv *env, void *ptr) {
        if (ptr == NULL)
                return;

        utrans_close(ptr);
}
