pub mod init;
pub mod services;

use crate::{AppError, AppState};
use axum::Router;

pub fn setup() -> Router<AppState> {
    let r = Router::new();
    r.nest("/services", services::setup())
}
