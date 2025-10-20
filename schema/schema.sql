-- CREATE EXTENSION pgcrypto WITH SCHEMA public;

-- CREATE TYPE public.account_ownership AS ENUM (
--     'owned',
--     'other_party'
-- );

CREATE TABLE external_connections (
    id uuid NOT NULL,
    name character varying NOT NULL,
    integration character varying NOT NULL,
    CONSTRAINT external_connections_pkey PRIMARY KEY (id)
);

CREATE TABLE accounts (
    id uuid NOT NULL,
    external_connection_id uuid NOT NULL,
    external_id character varying NOT NULL,
    ownership character varying NOT NULL,
    CONSTRAINT accounts_pkey PRIMARY KEY (id),
    CONSTRAINT external_connections FOREIGN KEY (external_connection_id) REFERENCES external_connections(id) ON DELETE CASCADE,
    CONSTRAINT external_connection_unique_external UNIQUE (external_connection_id, external_id),
    CONSTRAINT ownership_value CHECK (ownership IN ('owned', 'other_party'))
);

CREATE TABLE external_connection_credentials (
    external_connection_id uuid NOT NULL,
    data bytea NOT NULL,
    CONSTRAINT external_connection_credentials_pkey PRIMARY KEY (external_connection_id),
    CONSTRAINT external_connections FOREIGN KEY (external_connection_id) REFERENCES external_connections(id) ON DELETE CASCADE,
);


CREATE TABLE folio_user (
    id character varying NOT NULL,
    name character varying NOT NULL,
    CONSTRAINT folio_user_pkey PRIMARY KEY (id)
);



CREATE TABLE items (
    id uuid NOT NULL,
    name text,
    CONSTRAINT items_pkey PRIMARY KEY (id)
);

CREATE TABLE transactions (
    id uuid NOT NULL,
    account_id uuid NOT NULL,
    external_transaction_id character varying NOT NULL,
    CONSTRAINT transactions_pkey PRIMARY KEY (id),
    CONSTRAINT account_id FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE,
    CONSTRAINT external_tx_id_for_account UNIQUE (account_id, external_transaction_id),
);

CREATE TABLE transaction_legs (
    transaction_id uuid NOT NULL,
    leg_id character varying NOT NULL,
    type character varying NOT NULL,
    currency uuid NOT NULL,
    value character varying NOT NULL,
    CONSTRAINT transaction_legs_pkey PRIMARY KEY (transaction_id, leg_id),
    CONSTRAINT transaction_id FOREIGN KEY (transaction_id) REFERENCES transactions(id) ON DELETE CASCADE
);

