--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: metrics; Type: TABLE; Schema: public; Owner: benjamin; Tablespace: 
--

CREATE TABLE metrics (
    id integer NOT NULL,
    username integer,
    name bytea NOT NULL,
    host bytea NOT NULL,
    aggregation bytea NOT NULL
);


ALTER TABLE public.metrics OWNER TO benjamin;

--
-- Name: metrics_id_seq; Type: SEQUENCE; Schema: public; Owner: benjamin
--

CREATE SEQUENCE metrics_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.metrics_id_seq OWNER TO benjamin;

--
-- Name: metrics_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: benjamin
--

ALTER SEQUENCE metrics_id_seq OWNED BY metrics.id;


--
-- Name: metrics_id_seq; Type: SEQUENCE SET; Schema: public; Owner: benjamin
--

SELECT pg_catalog.setval('metrics_id_seq', 1, false);


--
-- Name: resolutions; Type: TABLE; Schema: public; Owner: benjamin; Tablespace: 
--

CREATE TABLE resolutions (
    id integer NOT NULL,
    metric_id integer,
    "interval" integer,
    count integer,
    persisted integer[]
);


ALTER TABLE public.resolutions OWNER TO benjamin;

--
-- Name: resolutions_id_seq; Type: SEQUENCE; Schema: public; Owner: benjamin
--

CREATE SEQUENCE resolutions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.resolutions_id_seq OWNER TO benjamin;

--
-- Name: resolutions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: benjamin
--

ALTER SEQUENCE resolutions_id_seq OWNED BY resolutions.id;


--
-- Name: resolutions_id_seq; Type: SEQUENCE SET; Schema: public; Owner: benjamin
--

SELECT pg_catalog.setval('resolutions_id_seq', 1, false);


--
-- Name: users; Type: TABLE; Schema: public; Owner: benjamin; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    username bytea NOT NULL
);


ALTER TABLE public.users OWNER TO benjamin;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: benjamin
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO benjamin;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: benjamin
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: benjamin
--

SELECT pg_catalog.setval('users_id_seq', 1, true);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: benjamin
--

ALTER TABLE ONLY metrics ALTER COLUMN id SET DEFAULT nextval('metrics_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: benjamin
--

ALTER TABLE ONLY resolutions ALTER COLUMN id SET DEFAULT nextval('resolutions_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: benjamin
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Data for Name: metrics; Type: TABLE DATA; Schema: public; Owner: benjamin
--

COPY metrics (id, username, name, host, aggregation) FROM stdin;
\.


--
-- Data for Name: resolutions; Type: TABLE DATA; Schema: public; Owner: benjamin
--

COPY resolutions (id, metric_id, "interval", count, persisted) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: benjamin
--

COPY users (id, username) FROM stdin;
1	\\x62616e6a696577656e
\.


--
-- Name: metrics_pkey; Type: CONSTRAINT; Schema: public; Owner: benjamin; Tablespace: 
--

ALTER TABLE ONLY metrics
    ADD CONSTRAINT metrics_pkey PRIMARY KEY (id);


--
-- Name: metrics_username_name_host_key; Type: CONSTRAINT; Schema: public; Owner: benjamin; Tablespace: 
--

ALTER TABLE ONLY metrics
    ADD CONSTRAINT metrics_username_name_host_key UNIQUE (username, name, host);


--
-- Name: resolutions_pkey; Type: CONSTRAINT; Schema: public; Owner: benjamin; Tablespace: 
--

ALTER TABLE ONLY resolutions
    ADD CONSTRAINT resolutions_pkey PRIMARY KEY (id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: benjamin; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: metrics_username_fkey; Type: FK CONSTRAINT; Schema: public; Owner: benjamin
--

ALTER TABLE ONLY metrics
    ADD CONSTRAINT metrics_username_fkey FOREIGN KEY (username) REFERENCES users(id);


--
-- Name: resolutions_metric_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: benjamin
--

ALTER TABLE ONLY resolutions
    ADD CONSTRAINT resolutions_metric_id_fkey FOREIGN KEY (metric_id) REFERENCES metrics(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: benjamin
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM benjamin;
GRANT ALL ON SCHEMA public TO benjamin;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

