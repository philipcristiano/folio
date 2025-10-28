use clap::Parser;
use std::str;

use axum::{
    extract::{State},
    response::{IntoResponse, Redirect, Response},
    routing::{get}, Router,
};
use std::net::SocketAddr;

use maud::html;
use tower_cookies::CookieManagerLayer;

mod config;
mod files;
mod html;
mod svg_icon;
use rust_embed::RustEmbed;

#[derive(RustEmbed, Clone)]
#[folder = "static/"]
struct StaticAssets;

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

use folio::{AppState, AppConfig, AppError};

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

    let app_config = crate::config::read_app_config(args.config_file);

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

    let oidc_router = service_conventions::oidc::router(app_state.auth.clone());
    let serve_assets = axum_embed::ServeEmbed::<StaticAssets>::new();
    let app = Router::new()
        // `GET /` goes to `root`
        .route("/", get(root))
        .route("/logged_in", get(handle_logged_in))
        .nest("/oidc", oidc_router.with_state(app_state.auth.clone()))
        .nest("/files", files::router())
        .nest_service("/static", serve_assets)
        .with_state(app_state.clone())
        .layer(CookieManagerLayer::new())
        .layer(tower_http::compression::CompressionLayer::new())
        .layer(service_conventions::tracing_http::trace_layer(
            tracing::Level::INFO,
        ))
        .route("/_health", get(health));

    let addr: SocketAddr = args.bind_addr.parse().expect("Expected bind addr");
    tracing::info!("listening on {}", addr);
    let listener = tokio::net::TcpListener::bind(&addr).await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

async fn health() -> Response {
    "OK".into_response()
}

async fn root(
    State(_app_state): State<AppState>,
    user: Result<
        Option<service_conventions::oidc::OIDCUser>,
        service_conventions::oidc::OIDCUserError,
    >,
) -> Result<Response, AppError> {
    if let Ok(Some(_user)) = user {
        Ok(html::maud_page(html! {
              (html::sidebar())
              div #main class="main" {

                    // form
                    //   method="post"
                    //   action="/external/services/bitcoin/f/add" {
                    //     div
                    //         hx-get={"/external/services/bitcoin/f/add"}
                    //         hx-target="this"
                    //         hx-swap="innerHTML"
                    //         hx-trigger="load"
                    //         {}
                    //     input type="submit" {}
                    //   }
                  }

        })
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
