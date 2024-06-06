createdb folio
createuser --superuser folio

psql -U folio folio -c "CREATE EXTENSION IF NOT EXISTS pgcrypto;"
