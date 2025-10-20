use crate::AppState;
use std::time::Duration;
use tokio::time::sleep;
use tracing::Level;

use folio::external::services::{ExternalConnection, sync_connection};

pub async fn sync_all(app_state: AppState) -> () {
    loop {
        let res = try_sync_all(&app_state).await;
        if let Err(e) = res {
            tracing::event!(Level::ERROR, error = e.to_string(), "Could not sync");
        }
        sleep(Duration::from_secs(60 * 60)).await;
    }
}

struct Lock {
    pg_try_advisory_lock: Option<bool>,
}

impl Lock {
    fn held(&self) -> bool {
        if let Some(b) = self.pg_try_advisory_lock {
            return b;
        }
        return false;
    }
}

#[tracing::instrument(name = "sync_connections", skip_all)]
async fn try_sync_all(app_state: &AppState) -> anyhow::Result<()> {
    let k = sqlx::postgres::PgAdvisoryLock::new("Sync connections")
        .key()
        .as_bigint();
    let mut c = app_state.db_spike.begin().await?;
    let lock = sqlx::query_as!(Lock, "SELECT pg_try_advisory_lock($1)", k)
        .fetch_one(c.as_mut())
        .await?;
    if lock.held() {
        tracing::event!(Level::DEBUG, "Holding PG Advisory lock");
        sync_each_connection(app_state).await?;
    } else {
        tracing::event!(Level::INFO, "Could not get PG Advisory lock");
    }
    c.rollback().await?;
    Ok(())
}


#[tracing::instrument(skip_all)]
async fn sync_each_connection(app_state: &AppState) -> anyhow::Result<()> {

    let mut c = app_state.db_spike.begin().await?;
    let connections = ExternalConnection::get_all(&mut c).await?;
    c.rollback().await?;

    for connection in connections {
        sync_connection(&connection, app_state).await?;
    }

    Ok(())

}
