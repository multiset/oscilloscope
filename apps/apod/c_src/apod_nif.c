#include <math.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "apod.h"
#include "debug.h"

typedef struct
{
  ErlNifPid p;
  ApodClass class;
  ApodData *data;
} ApodWrap;

typedef struct
{
  ERL_NIF_TERM atom_ok;
  ERL_NIF_TERM atom_undefined;
  ErlNifResourceType* res_apod;
} ApodPriv;

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
  ERL_NIF_TERM ret;
  if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
    return ret;
  }
  return enif_make_atom(env, name);
}

static int check_pid(ErlNifEnv* env, ApodWrap* wrap)
{
  ErlNifPid pid;
  enif_self(env, &pid);
  if (enif_compare(pid.pid, wrap->p.pid) == 0) {
    return 1;
  }
  return 0;
}

static int
get_class(char *agg_str, ApodClass *class)
{
  if (!strcmp(agg_str, "rectangular")) {
    *class = RECT;
  } else {
    return 0;
  }

  return 1;
}


static int
get_aggregation(char *agg_str, ApodAggregation *aggregation)
{
  if (!strcmp(agg_str, "average")) {
    *aggregation = AVERAGE;
  } else if (!strcmp(agg_str, "sum")) {
    *aggregation = SUM;
  } else if (!strcmp(agg_str, "min")) {
    *aggregation = MIN;
  } else if (!strcmp(agg_str, "max")) {
    *aggregation = MAX;
  } else if (!strcmp(agg_str, "last")) {
    *aggregation = LAST;
  } else {
    return 0;
  }

  return 1;
}

static ERL_NIF_TERM
apod_nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  ERL_NIF_TERM ret;
  ApodWrap* wrap = NULL;
  ApodPriv* priv = enif_priv_data(env);

  char class_str[16];
  ApodClass class;
  char agg_str[16];
  ApodAggregation aggregation;

  int interval;
  int count;
  ErlNifSInt64 floor;

  if (!enif_get_atom(env, argv[0], class_str, sizeof(class_str), ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  if (!get_class(class_str, &class)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_atom(env, argv[1], agg_str, sizeof(agg_str), ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  if (!get_aggregation(agg_str, &aggregation)) {
    return enif_make_badarg(env);
  }

  wrap = (ApodWrap*)enif_alloc_resource(priv->res_apod, sizeof(ApodWrap));
  memset(wrap, '\0', sizeof(ApodWrap));
  // Keep track of the owner pid in apod->p
  enif_self(env, &(wrap->p));
  wrap->class = class;

  switch (class) {
  case RECT:
    if (argc != 5) {
      return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[2], &interval)) {
      return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &count)) {
      return enif_make_badarg(env);
    }

    if (!enif_get_int64(env, argv[4], &floor)) {
      return enif_make_badarg(env);
    }

    wrap->data = apod_new(aggregation, interval, count, (int64_t)floor);
    break;
  }

  ret = enif_make_resource(env, wrap);
  enif_release_resource(wrap);

  return enif_make_tuple2(env, priv->atom_ok, ret);
}

static ERL_NIF_TERM
apod_nif_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;
  ErlNifSInt64 t;
  double v;

  if (argc != 3) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  if (!check_pid(env, wrap)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int64(env, argv[1], &t)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_double(env, argv[2], &v)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    apod_update(wrap->data, t, v);
    break;
  }

  return priv->atom_ok;
}

static ERL_NIF_TERM
apod_nif_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;
  ApodRead* read = NULL;
  ERL_NIF_TERM* points = NULL;
  ERL_NIF_TERM ret;

  ErlNifSInt64 from;
  ErlNifSInt64 until;

  int i;

  if (argc != 3) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  if (!check_pid(env, wrap)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int64(env, argv[1], &from)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int64(env, argv[2], &until)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    read = apod_read(wrap->data, from, until);
    if (read == NULL) {
      // TODO: Return error, not undefined?
      return priv->atom_undefined;
    }

    points = malloc(read->size * sizeof(ERL_NIF_TERM));
    if (points == NULL) {
      // TODO: return error, not undefined
      apod_read_dealloc(read);
      return priv->atom_undefined;
    }

    for (i = 0; i < read->size; i++) {
      if (isnan(read->arr[i])) {
        points[i] = priv->atom_undefined;
      } else {
        points[i] = enif_make_double(env, read->arr[i]);
      }
    }
    ret = enif_make_tuple3(
      env,
      enif_make_int64(env, read->from),
      enif_make_int64(env, read->until),
      enif_make_list_from_array(env, points, read->size)
      );
    free(points);
    apod_read_dealloc(read);
    return ret;
  }
}

static ERL_NIF_TERM
apod_nif_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;
  ApodRead* read = NULL;
  ERL_NIF_TERM* points = NULL;
  ERL_NIF_TERM ret;
  int i;

  if (argc != 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  if (!check_pid(env, wrap)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    read = apod_read(wrap->data, 0, INT64_MAX);
    if (read == NULL) {
      // TODO: return error, not undefined?
      return priv->atom_undefined;
    }

    points = malloc(read->size * sizeof(ERL_NIF_TERM));
    if (points == NULL) {
      // TODO: return error, not undefined?
      apod_read_dealloc(read);
      return priv->atom_undefined;
    }

    for (i = 0; i < read->size; i++) {
      if (isnan(read->arr[i])) {
        points[i] = priv->atom_undefined;
      } else {
        points[i] = enif_make_double(env, read->arr[i]);
      }
    }

    ret = enif_make_list_from_array(env, points, read->size);
    free(points);
    apod_read_dealloc(read);
    return ret;
  }

}

static ERL_NIF_TERM
apod_nif_truncate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;
  ErlNifSInt64 floor;

  if (argc != 2) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  if (!check_pid(env, wrap)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int64(env, argv[1], &floor)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    apod_truncate(wrap->data, floor);
    break;
  }

  return priv->atom_ok;
}

static ERL_NIF_TERM
apod_nif_earliest_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;

  int64_t timestamp;

  if (argc != 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    timestamp = wrap->data->t;
    break;
  }
  if (timestamp < 0) {
    return priv->atom_undefined;
  } else {
    return enif_make_int64(env, timestamp);
  }
}

static ERL_NIF_TERM
apod_nif_latest_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;

  int64_t timestamp;

  if (argc != 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    timestamp = apod_latest_time(wrap->data);
    break;
  }
  if (timestamp < 0) {
    return priv->atom_undefined;
  } else {
    return enif_make_int64(env, timestamp);
  }
}

static ERL_NIF_TERM
apod_nif_interval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ApodPriv* priv = enif_priv_data(env);
  ApodWrap* wrap = NULL;

  int64_t interval;

  if (argc != 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_resource(env, argv[0], priv->res_apod, (void**) &wrap)) {
    return enif_make_badarg(env);
  }

  switch (wrap->class) {
  case RECT:
    interval = wrap->data->interval;
    break;
  }

  return enif_make_int64(env, interval);
}

static ERL_NIF_TERM
apod_nif_chunkifyability(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  // TODO
  return enif_make_double(env, 0.0);
}

static void apod_nif_destroy(ErlNifEnv* env, void* obj)
{
  ApodWrap* wrap = (ApodWrap*) obj;
  switch (wrap->class) {
  case RECT:
    apod_dealloc(wrap->data);
  }
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
  ErlNifResourceType* res;

  ApodPriv* new_priv = (ApodPriv*)enif_alloc(sizeof(ApodPriv));
  if (new_priv == NULL) {
    return 1;
  }

  res = enif_open_resource_type(
    env,
    NULL,
    "apod",
    apod_nif_destroy,
    ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
    NULL
    );

  if (res == NULL) {
    return 1;
  }

  new_priv->atom_ok = make_atom(env, "ok");
  new_priv->atom_undefined = make_atom(env, "undefined");
  new_priv->res_apod = res;
  *priv = (void*)new_priv;
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
  enif_free(priv);
  return;
}

static ErlNifFunc funcs[] = {
  {"new", 5, apod_nif_new},
  {"update", 3, apod_nif_update},
  {"read", 3, apod_nif_read},
  {"to_list", 1, apod_nif_to_list},
  {"truncate", 2, apod_nif_truncate},
  {"earliest_time", 1, apod_nif_earliest_time},
  {"latest_time", 1, apod_nif_latest_time},
  {"interval", 1, apod_nif_interval},
  {"chunkifyability", 1, apod_nif_chunkifyability}
};

ERL_NIF_INIT(apod, funcs, &load, NULL, NULL, &unload);
