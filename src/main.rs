use clap::Parser;
use futures::try_join;
use serde::{Deserialize, Serialize};
use std::fs;
use std::ops::Deref;
use std::str;

use axum::{
    extract::{FromRef, Path, State},
    http::StatusCode,
    response::{IntoResponse, Redirect, Response},
    routing::{get, post},
    Form, Router,
};
use axum_extra::extract::Query;
use std::net::SocketAddr;

use maud::html;
use tower_cookies::CookieManagerLayer;

mod dates;
mod html;
mod svg_icon;
mod sync_manager;
use rust_embed::RustEmbed;

#[derive(RustEmbed, Clone)]
#[folder = "static/"]
struct StaticAssets;

mod external;

#[derive(Parser, Debug)]
pub struct Args {
    #[arg(short, long, default_value = "127.0.0.1:3002")]
    bind_addr: String,
    #[arg(short, long, default_value = "folio.toml")]
    config_file: String,
    #[arg(short, long, value_enum, default_value = "DEBUG")]
    log_level: tracing::Level,
    #[arg(long, action)]
    log_json: bool,
}

#[serde_with::serde_as]
#[derive(Clone, Debug, Deserialize)]
struct AppConfig {
    database_url: String,
    auth: service_conventions::oidc::OIDCConfig,
    #[serde_as(as = "serde_with::base64::Base64")]
    database_encryption_key: [u8; 32],
}

#[derive(FromRef, Clone, Debug)]
struct AppState {
    auth: service_conventions::oidc::AuthConfig,
    db: PgPool,
    database_encryption_key: [u8; 32],
    #[from_ref(skip)]
    db_spike: PgPool,
}

impl AppState {
    fn from_config(item: AppConfig, db: PgPool, db_spike: PgPool) -> Self {
        let auth_config = service_conventions::oidc::AuthConfig {
            oidc_config: item.auth,
            post_auth_path: "/logged_in".to_string(),
            scopes: vec!["profile".to_string(), "email".to_string()],
        };
        AppState {
            auth: auth_config,
            db,
            database_encryption_key: item.database_encryption_key,
            db_spike,
        }
    }
}

use sqlx::postgres::PgPool;
use sqlx::postgres::PgPoolOptions;

#[derive(Clone, Debug)]
pub struct FolioUser {
    pub id: String,
    pub name: String,
}

impl FolioUser {
    #[tracing::instrument]
    async fn ensure_in_db(&self, pool: &PgPool) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    INSERT INTO folio_user ( id, name )
        VALUES ( $1, $2 )
    ON CONFLICT (id) DO UPDATE
        SET name = EXCLUDED.name;
            "#,
            self.id,
            self.name,
        )
        .execute(pool)
        .await?;

        Ok(())
    }
}
impl From<service_conventions::oidc::OIDCUser> for FolioUser {
    fn from(item: service_conventions::oidc::OIDCUser) -> Self {
        FolioUser {
            id: item.id,
            name: item.name.unwrap_or("".to_string()),
        }
    }
}

use tower_http::trace::{self, TraceLayer};
use tracing::Level;

#[tokio::main]
async fn main() {
    let args = Args::parse();
    service_conventions::tracing::setup(args.log_level);

    let app_config = read_app_config(args.config_file);

    // Start by making a database connection.
    tracing::info!("connecting to database");
    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&app_config.database_url)
        .await
        .expect("Cannot connect to DB");
    let pool_spike = PgPoolOptions::new()
        .max_connections(5)
        .connect(&app_config.database_url)
        .await
        .expect("Cannot connect to DB");

    let app_state = AppState::from_config(app_config, pool, pool_spike);

    let app_state2 = app_state.clone();
    tokio::spawn(async move {
        sync_manager::sync_all(app_state2).await;
    });

    let oidc_router = service_conventions::oidc::router(app_state.auth.clone());
    let serve_assets = axum_embed::ServeEmbed::<StaticAssets>::new();
    let app = Router::new()
        // `GET /` goes to `root`
        .route("/", get(root))
        .route("/logged_in", get(handle_logged_in))
        .nest("/oidc", oidc_router.with_state(app_state.auth.clone()))
        .nest_service("/static", serve_assets)
        .nest("/external", external::setup())
        .with_state(app_state.clone())
        .layer(CookieManagerLayer::new())
        .layer(tower_http::compression::CompressionLayer::new())
        .layer(
            TraceLayer::new_for_http()
                .make_span_with(trace::DefaultMakeSpan::new().level(Level::INFO))
                .on_response(trace::DefaultOnResponse::new().level(Level::INFO)),
        )
        .route("/_health", get(health));

    let addr: SocketAddr = args.bind_addr.parse().expect("Expected bind addr");
    tracing::info!("listening on {}", addr);
    let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

fn read_app_config(path: String) -> AppConfig {
    let config_file_error_msg = format!("Could not read config file {}", path);
    let config_file_contents = fs::read_to_string(path).expect(&config_file_error_msg);
    let app_config: AppConfig =
        toml::from_str(&config_file_contents).expect("Problems parsing config file");

    app_config
}

async fn health() -> Response {
    "OK".into_response()
}

async fn root(
    State(app_state): State<AppState>,
    user: Option<service_conventions::oidc::OIDCUser>,
) -> Result<Response, AppError> {
    if let Some(_user) = user {
        let mut tx = app_state.db.begin().await?;
        let ecs = external::services::ExternalConnection::get_all(&mut tx).await?;
        tx.commit().await?;
        Ok(html::maud_page(html! {
              div class="flex flex-col lg:flex-row"{
              (html::sidebar())
              div #main class="main" {

                    @for ec in ecs {

                        div
                            hx-get={"/external/services/" (ec.integration) "/f/get/" (ec.id)}
                            hx-target="this"
                            hx-swap="innerHTML"
                            hx-trigger="load"
                            {}

                    }

                    form
                      method="post"
                      action="/external/services/bitcoin/f/add" {
                        div
                            hx-get={"/external/services/bitcoin/f/add"}
                            hx-target="this"
                            hx-swap="innerHTML"
                            hx-trigger="load"
                            {}
                        input type="submit" {}
                      }
                  }

        }})
        .into_response())
    } else {
        Ok(html::maud_page(html! {
            p { "Welcome! You need to login" }
            a href="/oidc/login" { "Login" }
        })
        .into_response())
    }
}

async fn handle_logged_in(
    State(app_state): State<AppState>,
    user: service_conventions::oidc::OIDCUser,
) -> Result<Response, AppError> {
    let user = FolioUser::from(user);
    user.ensure_in_db(&app_state.db).await?;
    Ok(Redirect::to("/").into_response())
}

// Make our own error that wraps `anyhow::Error`.
struct AppError(anyhow::Error);

// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            format!("Something went wrong: {}", self.0),
        )
            .into_response()
    }
}

// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
// `Result<_, AppError>`. That way you don't need to do that manually.
impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self(err.into())
    }
}
