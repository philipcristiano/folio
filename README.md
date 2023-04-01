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
* Ethereum and ERC20 tokens (no transaction support yet, only balances).
