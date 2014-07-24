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
    name bytea NOT NULL,
    password bytea NOT NULL,
    active boolean DEFAULT TRUE NOT NULL
);

CREATE TABLE emails (
    id serial PRIMARY KEY,
    user_id integer NOT NULL REFERENCES users(id),
    email bytea NOT NULL
);

CREATE TABLE orgs (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    name bytea UNIQUE NOT NULL,
    active boolean DEFAULT TRUE NOT NULL
);

CREATE TABLE teams (
    id serial PRIMARY KEY,
    name bytea NOT NULL,
    org_id integer NOT NULL REFERENCES orgs(id)
);

CREATE TABLE org_members (
    org_id integer NOT NULL REFERENCES orgs(id),
    user_id integer NOT NULL REFERENCES users(id)
);

CREATE TABLE team_members (
    org_id integer NOT NULL REFERENCES orgs(id),
    team_id integer NOT NULL REFERENCES teams(id),
    user_id integer NOT NULL REFERENCES users(id)
);

CREATE TABLE metrics (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    hash bytea NOT NULL,
    aggregation bytea NOT NULL,
    UNIQUE(owner_id, hash, aggregation)
);

CREATE TABLE tags (
    owner_id integer NOT NULL REFERENCES owners(id),
    name bytea NOT NULL,
    value bytea NOT NULL,
    metric_id integer NOT NULL REFERENCES metrics(id)
);

CREATE TABLE certs (
    id serial PRIMARY KEY,
    owner_id integer NOT NULL REFERENCES owners(id),
    cert bytea NOT NULL,
    active boolean NOT NULL
);

CREATE TABLE ports (
    id serial PRIMARY KEY,
    port integer NOT NULL,
    host integer NOT NULL,
    owner_id integer NOT NULL REFERENCES owners(id),
    cert_id integer REFERENCES certs(id),
    UNIQUE(port, host)
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
ALTER TABLE public.users OWNER TO oscilloscope;
ALTER TABLE public.owners OWNER TO oscilloscope;
ALTER TABLE public.users OWNER TO oscilloscope;
ALTER TABLE public.orgs OWNER TO oscilloscope;
ALTER TABLE public.teams OWNER TO oscilloscope;
ALTER TABLE public.team_members OWNER TO oscilloscope;
ALTER TABLE public.metrics OWNER TO oscilloscope;
ALTER TABLE public.tags OWNER TO oscilloscope;
ALTER TABLE public.certs OWNER TO oscilloscope;
ALTER TABLE public.ports OWNER TO oscilloscope;
ALTER TABLE public.resolutions OWNER TO oscilloscope;
ALTER TABLE public.persists OWNER TO oscilloscope;
