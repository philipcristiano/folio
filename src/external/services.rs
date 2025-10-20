mod bitcoin;

use sqlx::Transaction;

use crate::{AppError, AppState};
use axum::Router;
pub fn setup() -> Router<AppState> {
    let r = Router::new();
    r.nest("/bitcoin", bitcoin::setup())
}

#[derive(Debug, Clone)]
pub struct ExternalConnection {
    pub id: uuid::Uuid,
    pub integration: String,
    pub name: String,
}

impl ExternalConnection {
    #[tracing::instrument]
    async fn insert(&self, tx: &mut sqlx::Transaction<'_, sqlx::Postgres>) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    INSERT INTO external_connections ( id, integration, name )
    VALUES ( $1, $2, $3 )
    ON CONFLICT (id) DO NOTHING
            "#,
            self.id,
            self.integration,
            self.name,
        )
        .execute(&mut **tx)
        .await?;

        Ok(())
    }

    #[tracing::instrument]
    pub async fn get_all(
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<Vec<ExternalConnection>> {
        let r = sqlx::query_as!(
            ExternalConnection,
            r#"
    SELECT
        id,
        integration,
        name
    FROM external_connections
            "#
        )
        .fetch_all(&mut **tx)
        .await?;

        Ok(r)
    }
    #[tracing::instrument]
    pub async fn get_one(
        id: uuid::Uuid,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<ExternalConnection> {
        let r = sqlx::query_as!(
            ExternalConnection,
            r#"
    SELECT
        id,
        integration,
        name
    FROM external_connections
    WHERE id = $1
            "#,
            id
        )
        .fetch_one(&mut **tx)
        .await?;

        Ok(r)
    }

}

#[derive(Debug, Clone)]
struct ExternalConnectionCredentials {
    external_connection_id: uuid::Uuid,
    data: Vec<u8>,
}
use aes_gcm_siv::{
    aead::{Aead, KeyInit},
    Aes256GcmSiv,
    Nonce, // Or `Aes128GcmSiv`
};

impl ExternalConnectionCredentials {
    fn new(
        id: uuid::Uuid,
        obj: impl serde::Serialize,
        keybytes: &[u8; 32],
    ) -> anyhow::Result<ExternalConnectionCredentials> {
        let cipher = Aes256GcmSiv::new(keybytes.into());
        let nonce = Nonce::from_slice(b"unique nonce"); // 96-bits; unique per message
        let plaintext_data = serde_json::to_string(&obj)?;
        let payload = aead::Payload {
            msg: plaintext_data.as_bytes(),
            aad: &id.into_bytes(),
        };
        let maybe_ciphertext = cipher.encrypt(nonce, payload);
        if let Ok(ciphertext) = maybe_ciphertext {
            Ok(ExternalConnectionCredentials {
                external_connection_id: id,
                data: ciphertext,
            })
        } else {
            return Err(anyhow::anyhow!("Could not encrypt"));
        }
    }

    fn decrypt_credentials<T: for<'a> serde::Deserialize<'a>>(
        self,
        keybytes: &[u8; 32],
    ) -> anyhow::Result<T> {
        let cipher = Aes256GcmSiv::new(keybytes.into());
        let nonce = Nonce::from_slice(b"unique nonce"); // 96-bits; unique per message
        let payload = aead::Payload {
            msg: &self.data,
            aad: &self.external_connection_id.into_bytes(),
        };
        let maybe_plaintext = cipher.decrypt(nonce, payload);
        if let Ok(plaintext) = maybe_plaintext {
            let pt_string = std::str::from_utf8(&plaintext.as_slice())?;
            if let Ok(r) = serde_json::from_str(pt_string) {
                Ok(r)
            } else {
                return Err(anyhow::anyhow!("Could not deserialize"));
            }
        } else {
            return Err(anyhow::anyhow!("Could not decrypt"));
        }
    }
    #[tracing::instrument]
    async fn insert(&self, tx: &mut sqlx::Transaction<'_, sqlx::Postgres>) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    INSERT INTO external_connection_credentials ( external_connection_id, data )
    VALUES ( $1, $2 )
            "#,
            self.external_connection_id,
            self.data,
        )
        .execute(&mut **tx)
        .await?;

        Ok(())
    }

    #[tracing::instrument]
    pub async fn get_for(
        ec: &ExternalConnection,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<ExternalConnectionCredentials> {
        let r = sqlx::query_as!(
            ExternalConnectionCredentials,
            r#"
    SELECT
        external_connection_id,
        data
    FROM external_connection_credentials
    WHERE external_connection_id = $1
            "#,
            ec.id
        )
        .fetch_one(&mut **tx)
        .await?;

        Ok(r)
    }
}

pub async fn sync_connection(c: &ExternalConnection, app_state: &crate::AppState) -> anyhow::Result<()> {

    let integration = c.integration.as_str();
    match integration {
        "bitcoin" => bitcoin::sync_connection(c, app_state).await?,
        _ =>  panic!("Integration not defined"),

    }

    Ok(())

}
