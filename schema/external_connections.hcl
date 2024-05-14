table "external_connections" {
  schema = schema.public
  column "id" {
    type = uuid
  }
  column "integration" {
    type = varchar
  }
  column "name" {
    type = varchar
  }
  primary_key {
    columns = [
      column.id
    ]
  }
}

table "external_connection_credentials" {
  schema = schema.public

  column "external_connection_id" {
    type = uuid
  }
  column "data" {
    type = bytea
  }
  primary_key {
    columns = [
      column.external_connection_id,
    ]
  }

  foreign_key "external_connections" {
    columns = [column.external_connection_id]
    ref_columns = [table.external_connections.column.id]
    on_delete = CASCADE
    on_update = NO_ACTION
  }
}
