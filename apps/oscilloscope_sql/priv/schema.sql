SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TYPE metric_perms AS ENUM ('r', 'w', 'rw');


CREATE TABLE users (
    id serial PRIMARY KEY,
    username bytea UNIQUE NOT NULL,
    port integer UNIQUE NOT NULL,
    email bytea NOT NULL,
    password bytea NOT NULL
);

CREATE TABLE metrics (
    id serial PRIMARY KEY,
    user_id integer NOT NULL REFERENCES users(id),
    name bytea NOT NULL,
    host bytea NOT NULL,
    aggregation bytea NOT NULL,
    UNIQUE(user_id, name, host, aggregation)
);

CREATE TABLE resolutions (
    id serial PRIMARY KEY,
    metric_id integer NOT NULL REFERENCES metrics(id),
    "interval" integer,
    count integer,
    UNIQUE(metric_id, "interval", count)
);

CREATE TABLE persists (
    id serial PRIMARY KEY,
    resolution_id integer NOT NULL REFERENCES resolutions(id),
    "timestamp" integer NOT NULL,
    count integer NOT NULL,
    UNIQUE(resolution_id, "timestamp")
);

CREATE TABLE shared_metrics (
    metric_id serial REFERENCES metrics(id),
    user_id serial REFERENCES users(id),
    perms metric_perms
);


CREATE INDEX sm_metric_id_idx ON shared_metrics (metric_id);

CREATE INDEX sm_user_id_idx ON shared_metrics (user_id);


ALTER TABLE public.metrics OWNER TO oscilloscope;
ALTER TABLE public.resolutions OWNER TO oscilloscope;
ALTER TABLE public.users OWNER TO oscilloscope;
ALTER TABLE public.shared_metrics OWNER TO oscilloscope;
