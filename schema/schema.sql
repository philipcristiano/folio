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

    CONSTRAINT filetype_check_1 CHECK (filetype = ANY('{csv}'::TEXT[]))
)
