SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;


CREATE TABLE metrics (
    id serial PRIMARY KEY,
    name bytea NOT NULL,
    host bytea NOT NULL,
    aggregation bytea NOT NULL,
    UNIQUE(name, host)
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


ALTER TABLE public.metrics OWNER TO oscilloscope;
ALTER TABLE public.resolutions OWNER TO oscilloscope;
