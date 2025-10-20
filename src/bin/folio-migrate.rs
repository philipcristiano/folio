use clap::Parser;
use serde::{Deserialize, Serialize};
use std::fs;

#[derive(Parser, Debug)]
pub struct Args {
    #[arg(short, long, default_value = "folio.toml")]
    config_file: String,
}

#[serde_with::serde_as]
#[derive(Clone, Debug, Deserialize)]
struct AppConfig {
    database_url: String,
}

fn read_app_config(path: String) -> AppConfig {
    let config_file_error_msg = format!("Could not read config file {}", path);
    let config_file_contents = fs::read_to_string(path).expect(&config_file_error_msg);
    let app_config: AppConfig =
        toml::from_str(&config_file_contents).expect("Problems parsing config file");

    app_config
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    println!("Args {args:?}");
    let config = read_app_config(args.config_file);
    let pool = sqlx::PgPool::connect(&config.database_url).await.expect("Connection attempt");
    let target_schema = include_str!("../../schema/schema.sql").to_string();
    declare_schema::migrate_from_string(&target_schema, &pool).await?;
    Ok(())
}
