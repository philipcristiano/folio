createdb -U $USER folio
createuser -U $USER --superuser folio

psql -U folio folio -c "CREATE EXTENSION IF NOT EXISTS pgcrypto;"
