pub mod html;
pub mod model_types;
pub mod svg_icon;
use serde::{Deserialize, Serialize};

use axum::{
    Form, Router,
    extract::{FromRef, Path, State},
    http::StatusCode,
    response::{IntoResponse, Redirect, Response},
    routing::{get, post},
};
// Make our own error that wraps `anyhow::Error`.
pub struct AppError(anyhow::Error);

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

#[serde_with::serde_as]
#[derive(Clone, Debug, Deserialize)]
pub struct AppConfig {
    pub database_url: String,
    pub auth: service_conventions::oidc::OIDCConfig,
    #[serde_as(as = "serde_with::base64::Base64")]
    pub database_encryption_key: [u8; 32],
}

use sqlx::postgres::PgPool;
use sqlx::postgres::PgPoolOptions;

#[derive(FromRef, Clone, Debug)]
pub struct AppState {
    pub auth: service_conventions::oidc::AuthConfig,
    pub db: PgPool,
    pub database_encryption_key: [u8; 32],
    #[from_ref(skip)]
    pub db_spike: PgPool,
}

impl AppState {
    pub fn from_config(item: AppConfig, db: PgPool, db_spike: PgPool) -> Self {
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
