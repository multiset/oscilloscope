#ifndef APOD_H
#define APOD_H

#include <Judy.h>
#include "erl_nif.h"

typedef enum
{
    AVERAGE,
    SUM,
    MIN,
    MAX,
    LAST
} ApodAggregation;

typedef enum
{
    RECT
} ApodClass;

typedef struct
{
    int64_t from;
    int64_t until;
    int size;
    int interval;
    double *arr;
} ApodRead;

typedef struct
{
    Pvoid_t values;
    Pvoid_t counters;
    ApodAggregation aggregation;
    int bucket_size;
    int64_t t;
    int64_t floor;
    int interval;
    int count;
} ApodData;

ApodData *apod_new(ApodAggregation, int, int, int64_t);
int apod_update(ApodData*, int64_t, double);
ApodRead *apod_read(ApodData*, int64_t, int64_t);
void apod_truncate(ApodData*, int64_t);
int64_t apod_earliest_time(ApodData*);
int64_t apod_latest_time(ApodData*);
void apod_read_dealloc(ApodRead*);
void apod_dealloc(ApodData*);

#endif
