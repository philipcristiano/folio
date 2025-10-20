variable "destructive" {
  type    = bool
  default = false
}

variable "database_url" {
  type    = string
  default = getenv("DATABASE_URL")
}

env "local" {
  src = [
    "file://schema/"
  ]

  url = var.database_url

  diff {
    skip {
      drop_schema = !var.destructive
      drop_table  = !var.destructive
    }
  }
}
