#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "erl_nif.h"

#define OPT_SIGNED 0b00000001

static ERL_NIF_TERM atom_ok, atom_max, atom_min, atom_size, atom_memory;

typedef struct {
	uint32_t opts;
	uint32_t arity;
	union {
		int64_t s[0];
		uint64_t u[0];
	} array;
} atomics_handle_t;

static ErlNifResourceType *atomics_handle_resource = NULL;

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int arity;
	unsigned int opts;

	if (enif_get_uint(env, argv[0], &arity) && arity && enif_get_uint(env, argv[1], &opts)) {
		ERL_NIF_TERM r;
		size_t size = arity * (opts & OPT_SIGNED ? sizeof(int64_t) : sizeof(uint64_t));
		atomics_handle_t* h = enif_alloc_resource(atomics_handle_resource, sizeof(atomics_handle_t) + size);

		h->opts = opts;
		h->arity = arity;
		memset(&h->array, 0, size);
		r = enif_make_resource(env, h);
		enif_release_resource(h);
		return r;
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		return h->opts & OPT_SIGNED
		       ? enif_make_int64(env, __atomic_load_n(h->array.s + i, __ATOMIC_RELAXED))
		       : enif_make_uint64(env, __atomic_load_n(h->array.u + i, __ATOMIC_RELAXED));
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		if (h->opts & OPT_SIGNED) {
			int64_t v;

			if (enif_get_int64(env, argv[2], &v)) {
				__atomic_store_n(h->array.s + i, v, __ATOMIC_RELAXED);
				return atom_ok;
			}
		} else {
			uint64_t v;

			if (enif_get_uint64(env, argv[2], &v)) {
				__atomic_store_n(h->array.u + i, v, __ATOMIC_RELAXED);
				return atom_ok;
			}
		}
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		union {
			int64_t s;
			uint64_t u;
		} v;

		if (enif_get_int64(env, argv[2], &v.s) || enif_get_uint64(env, argv[2], &v.u)) {
			if (h->opts & OPT_SIGNED)
				__atomic_add_fetch(h->array.s + i, v.s, __ATOMIC_RELAXED);
			else
				__atomic_add_fetch(h->array.u + i, v.u, __ATOMIC_RELAXED);
			return atom_ok;
		}
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM sub(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		union {
			int64_t s;
			uint64_t u;
		} v;

		if (enif_get_int64(env, argv[2], &v.s) || enif_get_uint64(env, argv[2], &v.u)) {
			if (h->opts & OPT_SIGNED)
				__atomic_sub_fetch(h->array.s + i, v.s, __ATOMIC_RELAXED);
			else
				__atomic_sub_fetch(h->array.u + i, v.u, __ATOMIC_RELAXED);
			return atom_ok;
		}
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM add_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		union {
			int64_t s;
			uint64_t u;
		} v;

		if (enif_get_int64(env, argv[2], &v.s) || enif_get_uint64(env, argv[2], &v.u)) {
			if (h->opts & OPT_SIGNED)
				return enif_make_int64(env, __atomic_add_fetch(h->array.s + i, v.s, __ATOMIC_RELAXED));
			else
				return enif_make_uint64(env, __atomic_add_fetch(h->array.u + i, v.u, __ATOMIC_RELAXED));
		}
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM sub_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		union {
			int64_t s;
			uint64_t u;
		} v;

		if (enif_get_int64(env, argv[2], &v.s) || enif_get_uint64(env, argv[2], &v.u)) {
			if (h->opts & OPT_SIGNED)
				return enif_make_int64(env, __atomic_sub_fetch(h->array.s + i, v.s, __ATOMIC_RELAXED));
			else
				return enif_make_uint64(env, __atomic_sub_fetch(h->array.u + i, v.u, __ATOMIC_RELAXED));
		}
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM exchange(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		if (h->opts & OPT_SIGNED) {
			int64_t v;

			if (enif_get_int64(env, argv[2], &v))
				return enif_make_int64(env, __atomic_exchange_n(h->array.s + i, v, __ATOMIC_RELAXED));
		} else {
			uint64_t v;

			if (enif_get_uint64(env, argv[2], &v))
				return enif_make_uint64(env, __atomic_exchange_n(h->array.u + i, v, __ATOMIC_RELAXED));
		}
	}
	return enif_make_badarg(env);
}

static ERL_NIF_TERM compare_exchange(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;
	unsigned int i;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h) &&
	    enif_get_uint(env, argv[1], &i) && --i < h->arity) {
		if (h->opts & OPT_SIGNED) {
			int64_t e, d;

			if (enif_get_int64(env, argv[2], &e) && enif_get_int64(env, argv[3], &d))
				return __atomic_compare_exchange_n(h->array.s + i, &e, d, false,
								   __ATOMIC_RELAXED, __ATOMIC_RELAXED)
				       ? atom_ok
				       : enif_make_int64(env, e);
		} else {
			uint64_t e, d;

			if (enif_get_uint64(env, argv[2], &e) && enif_get_uint64(env, argv[3], &d))
				return __atomic_compare_exchange_n(h->array.u + i, &e, d, false,
								   __ATOMIC_RELAXED, __ATOMIC_RELAXED)
				       ? atom_ok
				       : enif_make_uint64(env, e);
		}
	}
	return enif_make_badarg(env);

}

static ERL_NIF_TERM info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	atomics_handle_t* h;

	if (enif_get_resource(env, argv[0], atomics_handle_resource, (void*)&h)) {
		ERL_NIF_TERM max, min;
		uint64_t memory = (uint64_t)h->arity * ((h->opts & OPT_SIGNED) ? sizeof(int64_t) : sizeof(uint64_t)) + 40;

		if (h->opts & OPT_SIGNED) {
			max = enif_make_int64(env, INT64_MAX);
			min = enif_make_int64(env, INT64_MIN);
		} else {
			max = enif_make_uint64(env, UINT64_MAX);
			min = enif_make_uint64(env, 0);
		}
#if ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 6
		ERL_NIF_TERM m = enif_make_new_map(env);
		enif_make_map_put(env, m, atom_size, enif_make_uint(env, h->arity), &m);
		enif_make_map_put(env, m, atom_max, max, &m);
		enif_make_map_put(env, m, atom_min, min, &m);
		enif_make_map_put(env, m, atom_memory, enif_make_uint64(env, memory), &m);
		return m;
#else
		return enif_make_list4(env,
				       enif_make_tuple2(env, atom_size, enif_make_uint(env, h->arity)),
				       enif_make_tuple2(env, atom_max, max),
				       enif_make_tuple2(env, atom_min, min),
				       enif_make_tuple2(env, atom_memory, enif_make_uint64(env, memory)));
#endif
	}
	return enif_make_badarg(env);
}

static void atomics_handle_dtor(ErlNifEnv *env, void *r)
{
	enif_release_resource(r);
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
	atomics_handle_resource = enif_open_resource_type(env, NULL, "atomics_handle", &atomics_handle_dtor,
							  ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
	if (atomics_handle_resource == NULL)
		return -1;
	atom_ok = enif_make_atom(env, "ok");
	atom_max = enif_make_atom(env, "max");
	atom_min = enif_make_atom(env, "min");
	atom_size = enif_make_atom(env, "size");
	atom_memory = enif_make_atom(env, "memory");
	return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
	return 0;
}

static ErlNifFunc nif_functions[] = {
	{"new_2", 2, new},
#if ERL_NIF_MAJOR_VERSION >= 2 && ERL_NIF_MINOR_VERSION >= 6
	{"info", 1, info},
#else
	{"info_1", 1, info},
#endif
	{"get", 2, get},
	{"put", 3, put},
	{"add", 3, add},
	{"sub", 3, sub},
	{"add_get", 3, add_get},
	{"sub_get", 3, sub_get},
	{"exchange", 3, exchange},
	{"compare_exchange", 4, compare_exchange}
};

ERL_NIF_INIT(otpbp_atomics, nif_functions, &load, NULL, &upgrade, NULL);
