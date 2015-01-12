SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

CREATE TABLE owners (
    id serial PRIMARY KEY
);

CREATE TABLE users (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    name text UNIQUE NOT NULL,
    password bytea NOT NULL,
    active boolean DEFAULT TRUE NOT NULL
);

CREATE TABLE emails (
    id serial PRIMARY KEY,
    user_id integer NOT NULL REFERENCES users(id),
    email text NOT NULL
);

CREATE TABLE orgs (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    name text UNIQUE NOT NULL,
    active boolean DEFAULT TRUE NOT NULL
);

CREATE TABLE teams (
    id serial PRIMARY KEY,
    name text NOT NULL,
    org_id integer NOT NULL REFERENCES orgs(id)
);

CREATE TABLE org_members (
    org_id integer NOT NULL REFERENCES orgs(id),
    user_id integer NOT NULL REFERENCES users(id)
);

CREATE TABLE team_members (
    team_id integer NOT NULL REFERENCES teams(id),
    user_id integer NOT NULL REFERENCES users(id)
);

CREATE TABLE metrics (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    hash bytea NOT NULL,
    UNIQUE(owner_id, hash)
);

CREATE TABLE tags (
    key text NOT NULL,
    value text NOT NULL,
    metric_id integer NOT NULL REFERENCES metrics(id)
);

CREATE TABLE windows (
    id serial PRIMARY KEY,
    metric_id integer NOT NULL REFERENCES metrics(id),
    type bytea NOT NULL,
    aggregation bytea,
    "interval" integer,
    count integer
);

CREATE TABLE persists (
    id serial PRIMARY KEY,
    window_id integer NOT NULL REFERENCES windows(id),
    "timestamp" integer NOT NULL,
    count integer NOT NULL,
    vacuumed boolean,
    persist_time timestamp without time zone default (now() at time zone 'utc') NOT NULL,
    vacuum_time timestamp without time zone,
    UNIQUE(window_id, "timestamp")
);

CREATE TABLE window_configuration_groups (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    priority integer NOT NULL,
    tags text[][] NOT NULL
);

CREATE TABLE window_configurations (
    id serial PRIMARY KEY,
    group_id integer NOT NULL REFERENCES window_configuration_groups(id),
    type bytea NOT NULL,
    aggregation bytea,
    "interval" integer,
    count integer
);

CREATE TABLE ports (
    owner_id integer NOT NULL REFERENCES owners(id),
    host bytea,
    "type" bytea NOT NULL,
    port integer NOT NULL
);

CREATE INDEX tag_metric_ids_idx ON tags(metric_id);

ALTER TABLE public.metrics OWNER TO osc;
ALTER TABLE public.users OWNER TO osc;
ALTER TABLE public.owners OWNER TO osc;
ALTER TABLE public.users OWNER TO osc;
ALTER TABLE public.orgs OWNER TO osc;
ALTER TABLE public.teams OWNER TO osc;
ALTER TABLE public.team_members OWNER TO osc;
ALTER TABLE public.metrics OWNER TO osc;
ALTER TABLE public.tags OWNER TO osc;
ALTER TABLE public.windows OWNER TO osc;
ALTER TABLE public.persists OWNER TO osc;
