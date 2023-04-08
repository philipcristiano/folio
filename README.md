# folio
Self host crypto portfolio application.

Currently in development to keep myself busy. You likely shouldn't use.

## DB

```
rm -rf $PGDATA
pg_ctl init
make services
```

```
sh ./db_init.sh
```

## Model

`provider` - A source of information, the implementation for integrations but not setup. Something like "coinbase" or "bitcoin chain".

`integration` - An installed instance of a provider. A particular Coinbase account or Bitcoin address

`account` - a collection of balances and transactions. Multiple accounts may come from an integration.


## Providers

* Coinbase
* Coinbase Pro
* Gemini
* Bitcoin addresses, x/y pub (no bech3/bc1 addresses yet).
* Ethereum and ERC20 tokens
  * Up to 1000 ERC20 token transactions
  * Up to 1000 ETH transactions
* Loopring
  * Balances
  * Transfers in the last ~90 days.


## Installation

### Configuration

#### Postgres

`PGHOST` - (`localhost`) - Hostname of the database
`PGPORT` - (`5432`) - Port of the database
`PGDATABASE` - (`folio`) - Postgres database name
`PGUSER` - (`folio_admin`) - Postgres username
`PGPASSWORD` - (`pass`) - Postgres user password

`PG_POOL_SIZE` - (`10`) - Number of concurrent connections to Postgres
