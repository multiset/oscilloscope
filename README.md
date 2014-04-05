Oscilloscope
============

Oscilloscope is a "database" for time-series floating point data. It listens for
incoming data via a variety of ports and protocols and stores it for future
retrieval.

The primary goals of Oscilloscope are:

1. Efficiency
2. Reliability

The first matter is attempted primarily through tightly packing data at rest;
there's much to be done there. The second is addressed by writing the system as
a set of Erlang/OTP applications.

Nomenclature
------------

* Service: a metric name; one-third of the primary method of identifying cache
  groups, along with Host and User.

* Host: a physical machine name; one-third of the primary method of identifying
  cache groups, along with Service and User. In the Graphite protocol, Host is
  omitted.

* Metric: A {User, Service, Host} set.

* Retention: a parameter that specifies how long data for a specific metric is
  stored. Consists of a series of {Interval, Count} pairs (generally "Schemas")
  representing the number of seconds between each data point and how many data
  points to store. For example, the retention {10, 100} would force 100 data
  points to be retained at a 10 second interval.

* Retention Schema: A list of {Interval, Count} pairs. See above.

* Group: a set of caches specified by a single metric and its configured
  retentions.

* Cache: a process that manages a single resolution's data.

Architecture
------------

The primary data pipeline for Oscilloscope is as follows:

1. The system listens to the outside world via TCP/IP for incoming data. At the
time of writing only the Graphite protocol is supported, but others should be
considered in the future. TCP is preferable to UDP due to delivery guarantees
and ease of security.

2. The edge-facing listeners forward received data to the caches for the
specified metric. Each datapoint is sent to _all_ processes in a cache group.
For the purposes of data ingestion, cache groups operate completely
independently, though they are supervised in a one-for-all strategy, so crashes
propagate across a cache group.

3. As caches receive data they shuffle it immediately to Redis. Each data
point's timestamp is floored to match the cache's retention. The floored values
are consistent regardless of cache creation time, i.e., every cache with the
same retention will have synchronous timestamps. Multiple incoming datapoints
for a single time window are aggregated lazily, though this is likely to change
in the future.

4. Each cache periodically and independently inspects the state of its in-memory
data. If the data has reached a certain threshold (generally 1KB) it will be
asynchronously written to DynamoDB. Simultaneous efforts are made to delete
"expired" data from DynamoDB, i.e., data that has fallen out of the cache's
retention range. Writes to DynamoDB are keyed on a unique metric ID and the
timestamp of the first value in the range. The stored value is a compressed
representation of a series of datapoints.

5. Cache reads are handled directly by the "most appropriate" group member,
determined as the highest-resolution (lowest interval) cache that has a count
large enough to satisfy the full query. Caches will not read from DynamoDB
unless necessary. No caching of data read from DynamoDB is done. This may change
in the future.

6. Caches store various bits of metadata in PostgreSQL. This is done primarily
because DynamoDB writes are exceptionally expensive; storing cache metadata in
DynamoDB would ~double total DynamoDB read/write costs. The most interesting bit
of data stored in PostgreSQL is the persistence records. Each record in DynamoDB
has a corresponding record in PostgreSQL that records the {MetricID, Timestamp}
key for that record as well as the number of points in the stored value. This
allows caches to reason about what data to read or delete without making
extraneous queries to DynamoDB.

Applications
------------

### oscilloscope

The `oscilloscope` application is a stub for code shares across the various
other applications. Nothing very exciting.

### oscilloscope_http

`oscilloscope_http` is the human interface to Oscilloscope. It's implemented as
a series of Webmachine resources. This is the only interface through which users
can read data, retrieve metadata, and configure their usage.

### oscilloscope_net

`oscilloscope_net` provides the interfaces through which data is inserted to
Oscilloscope. These interfaces are implemented as Cowboy workers.

### oscilloscope_sql

`oscilloscope_sql` manages metric metadata persistence in PostgreSQL. The DB
schema and common queries are both in `priv/`.

### oscilloscope_cache

`oscilloscope_cache` is the meat of the system.
