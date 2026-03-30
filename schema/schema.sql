-- CREATE EXTENSION pgcrypto WITH SCHEMA public;

-- CREATE TYPE public.account_ownership AS ENUM (
--     'owned',
--     'other_party'
-- );


CREATE TABLE folio_user (
    id character varying PRIMARY KEY NOT NULL,
    name character varying NOT NULL
);

CREATE TABLE uploaded_files (
    id uuid DEFAULT uuidv7() PRIMARY KEY NOT NULL,
    name text NOT NULL,
    filetype text NOT NULL,
    contents bytea NOT NULL,
    process_state text NOT NULL DEFAULT 'New',

    CONSTRAINT filetype_check_1 CHECK (filetype = ANY('{csv}'::TEXT[])),
    CONSTRAINT process_state_check_2 CHECK (process_state = ANY('{New,Labelled,Done}'::TEXT[]))
)
