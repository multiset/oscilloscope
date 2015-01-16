#include <math.h>
#include <Judy.h>
#include "apod.h"
#include "debug.h"

static const int bucket_size = 20;

static void apod_judy_truncate(Pvoid_t *judy, int64_t floor)
{
    PWord_t pvalue;
    Word_t pindex;
    int rc;

    pindex = (Word_t) floor;
    for (;;) {
        JLL(pvalue, *judy, pindex);
        if (pvalue == NULL) {
            break;
        }
        free((double*) *pvalue);
        JLD(rc, *judy, pindex);
    }
}

static int apod_scan_judy_double(Pvoid_t judy, int interval, int64_t from, int size, double* arr)
{
    PWord_t pvalue;
    Word_t pindex;
    int64_t index;
    double* bucket;
    int i, offset;

    pindex = (Word_t) from;
    JLL(pvalue, judy, pindex);
    if (pvalue == NULL) {
        return 0;
    }

    index = (int64_t) pindex;
    bucket = (double*) *pvalue;
    i = (from - index) / interval;
    offset = 0;

    for (;;) {
        for (;;) {
            if (offset >= size || i >= bucket_size) {
                break;
            }
            arr[offset] = bucket[i];
            offset++;
            i++;
        }

        if (offset >= size) {
            break;
        }

        pvalue = NULL;
        /*
         * Find the next suitable index in the judy. JLN can re-use pindex,
         * because the previous invocation (of either JLI or JLN, depending on
         * which loop iteration we're on) mutated it.
         */
        JLN(pvalue, judy, pindex);
        if (pvalue == NULL) {
            /*
             * The judy didn't contain all of the data we expected it to.
             * TODO: log.
             */
            return 0;
        }
        index = (int64_t) pindex;
        bucket = (double*) *pvalue;
        i = 0;
        offset = (index - from) / interval;
    }

    return 1;
}

static double *apod_get_judy_double(Pvoid_t *judy, int64_t index)
{
    PWord_t pvalue;
    Word_t pindex;
    double *bucket = NULL;
    int i;

    pindex = (Word_t) index;
    JLG(pvalue, *judy, pindex);
    if (pvalue == PJERR) {
        return NULL;
    }

    if (pvalue == NULL) {
        JLI(pvalue, *judy, pindex);
        if (pvalue == PJERR) {
            return NULL;
        }
        bucket = malloc(bucket_size * sizeof(bucket));
        if (bucket == NULL) {
            return NULL;
        }

        for (i = 0; i < bucket_size; i++) {
            bucket[i] = NAN;
        }

        *pvalue = (Word_t) bucket;
    }

    return (double*) *pvalue;
}

ApodData *apod_new(ApodAggregation aggregation, int interval, int count, int64_t floor)
{
    ApodData *data = NULL;
    data = malloc(sizeof(ApodData));
    if (data == NULL) {
        return NULL;
    }

    data->values = NULL;
    data->counters = NULL;
    data->t = -1;
    data->floor = floor;
    data->interval = interval;
    data->count = count;
    data->aggregation = aggregation;
    return data;
}

void apod_dealloc(ApodData *data)
{
    apod_judy_truncate(&data->values, -1);
    switch (data->aggregation) {
    case AVERAGE:
        apod_judy_truncate(&data->counters, -1);
        break;
    case SUM: break;
    case MIN: break;
    case MAX: break;
    case LAST: break;
    }
    free(data);
}

void apod_read_dealloc(ApodRead *read)
{
    free(read->arr);
    free(read);
}

int apod_update(ApodData *data, int64_t t, double v)
{
    int64_t floor, latest_time, bucket_key;
    double *bucket = NULL;
    double existing;

    t = t - t % data->interval;
    latest_time = apod_latest_time(data);
    floor = latest_time - data->interval * data->count;
    if (floor < data->floor) {
        floor = data->floor;
    }

    if (t <= floor) {
        // This datapoint is too old
        return 1;
    }

    if (data->t < 0 || t < data->t) {
        data->t = t;
    }

    bucket_key = t - t % (bucket_size * data->interval);
    // Offset t by the bucket key to find the index within the bucket
    t = (t - bucket_key) / data->interval;
    bucket = apod_get_judy_double(&data->values, bucket_key);
    if (bucket == NULL) {
        // TODO: log
        return 0;
    }

    existing = bucket[t];
    if (isnan(existing)) {
        bucket[t] = v;
    } else {
        switch (data->aggregation) {
        case AVERAGE:
            bucket[t] = v + existing;
            break;
        case SUM:
            bucket[t] = v + existing;
            break;
        case MIN:
            if (existing > v) {
                bucket[t] = v;
            }
            break;
        case MAX:
            if (v > existing) {
                bucket[t] = v;
            }
            break;
        case LAST:
            bucket[t] = v;
            break;
        }
    }

    switch (data->aggregation) {
    case AVERAGE:
        bucket = apod_get_judy_double(&data->counters, bucket_key);
        if (bucket == NULL) {
            // TODO: log
            return 0;
        }
        existing = bucket[t];
        if (isnan(existing)) {
            bucket[t] = 1;
        } else {
            bucket[t] = bucket[t] + 1;
        }
        break;
    case SUM: break;
    case MIN: break;
    case MAX: break;
    case LAST: break;
    }

    latest_time = apod_latest_time(data);
    floor = latest_time - data->interval * data->count;
    apod_truncate(data, floor);

    return 1;
}

ApodRead *apod_read(ApodData *data, int64_t from, int64_t until)
{
    int rem, i, ok;
    int64_t latest_time;
    ApodRead *read = NULL;
    double *counters;

    latest_time = apod_latest_time(data);
    read = malloc(sizeof(ApodRead));
    if (data->t == -1 || latest_time == -1 || read == NULL) {
        free(read);
        return NULL;
    }

    read->from = from;
    read->until = until;

    read->from = read->from - read->from % data->interval;
    rem = read->until % data->interval;
    if (rem != 0) {
        // Ceil the until value, since it's inclusive
        read->until = read->until - data->interval - rem;
    }

    if (read->from < data->t) {
        read->from = data->t;
    }

    if (read->until > latest_time) {
        read->until = latest_time;
    }

    if (read->from > latest_time || read->from > read->until) {
        free(read);
        return NULL;
    }

    // Increment size by one because until is inclusive
    read->size = ((read->until - read->from) / data->interval) + 1;
    read->arr = malloc(read->size * sizeof(double));
    if (read->arr == NULL) {
        // TODO: Log - this is a complete failure, not just "no data"
        apod_read_dealloc(read);
        return NULL;
    }

    for (i = 0; i < read->size; i++) {
        read->arr[i] = NAN;
    }

    ok = apod_scan_judy_double(
        data->values,
        data->interval,
        read->from,
        read->size,
        read->arr
        );

    if (!ok) {
        apod_read_dealloc(read);
        return NULL;
    }

    switch (data->aggregation) {
    case AVERAGE:
        counters = malloc(read->size * sizeof(double));
        if (counters == NULL) {
            // TODO: log
            apod_read_dealloc(read);
            return NULL;
        }

        ok = apod_scan_judy_double(
            data->counters,
            data->interval,
            read->from,
            read->size,
            counters
            );
        if (!ok) {
            // TODO: log
            free(counters);
            apod_read_dealloc(read);
            return NULL;
        }

        for (i = 0; i < read->size; i++) {
            read->arr[i] = read->arr[i] / counters[i];
        }

        free(counters);
        break;
    case SUM: break;
    case MIN: break;
    case MAX: break;
    case LAST: break;
    }

    return read;
}

int64_t apod_earliest_time(ApodData *data)
{
    PWord_t pvalue;
    Word_t pindex = 0;
    double *bucket = NULL;
    int64_t t;
    int i;

    JLF(pvalue, data->values, pindex);

    for (;;) {
        if (pvalue == NULL) {
            return -1;
        }

        bucket = (double*) *pvalue;
        for (i = 0; i < bucket_size; i++) {
            t = (int64_t) pindex + i * data->interval;
            /*
             * We intentionally leave some truncated values in the array (that
             * is, we only lazily truncate) and use data->floor as a source of
             * truth for what's truly "alive". Therefore t here must be greater
             * than data->floor to be valid.
             */
            if (!isnan(bucket[i]) && t > data->floor) {
                return t;
            }
        }

        JLN(pvalue, data->values, pindex);
    }

    return -1;
}

int64_t apod_latest_time(ApodData *data)
{
    PWord_t pvalue;
    Word_t pindex = -1;
    double *bucket = NULL;
    int64_t index;
    int i;
    // Get the latest index in the Judy
    JLL(pvalue, data->values, pindex);
    if (pvalue == NULL) {
        return -1;
    }

    /*
     * pvalue is now a pointer to the last bucket in the judy, and pindex is the
     * index (timestamp) of that bucket. Therefore, the last non-null index in
     * the bucket represents the latest datapoint stored in this window.
     */
    bucket = (double*) *pvalue;
    index = (int64_t) pindex;
    for (i = bucket_size; i > 0; i--) {
        if (!isnan(bucket[i - 1])) {
            return index + (i - 1) * data->interval;
        }
    }
    return -1;
}

void apod_truncate(ApodData *data, int64_t floor)
{
    int64_t truncate_index, bucket_width;

    floor = floor - floor % data->interval;
    if (data->t < 0 && floor > data->floor) {
        // The window is empty, so just reset the floor
        data->floor = floor;
    } else if (floor >= data->t) {
        data->floor = floor;
        /*
         * The bucket that we're looking to truncate is the latest one *before*
         * the bucket containing the provided floor. We can't truncate the
         * bucket with the current floor, because we can't easily guarantee that
         * there aren't points in that bucket *later* than the floor time.
         */
        bucket_width = data->interval * bucket_size;
        truncate_index = floor - bucket_width - floor % bucket_width;
        if (truncate_index >= 0) {
            /*
             * Don't try to truncate negative indices, because that clears the
             * whole array...
             */
            apod_judy_truncate(&data->values, truncate_index);
            switch (data->aggregation) {
            case AVERAGE:
                apod_judy_truncate(&data->counters, truncate_index);
                break;
            case SUM: break;
            case MIN: break;
            case MAX: break;
            case LAST: break;
            }
        }
        data->t = apod_earliest_time(data);
    }
}
