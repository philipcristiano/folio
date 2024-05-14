table "folio_user" {
  schema = schema.public
  column "id" {
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

