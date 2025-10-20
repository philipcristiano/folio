
use crate::external::services::{ExternalConnection};
use sqlx::Transaction;
use rust_decimal::Decimal;


#[derive(Debug, Clone)]
pub struct FTransaction {
    pub id: uuid::Uuid,
    pub account_id: uuid::Uuid,
    pub external_transaction_id: String,
}

impl FTransaction {

    #[tracing::instrument]
    async fn insert(&self, tx: &mut sqlx::Transaction<'_, sqlx::Postgres>) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    INSERT INTO transactions ( id, account_id, external_transaction_id )
    VALUES ( $1, $2, $3 )
            "#,
            self.id,
            self.account_id,
            self.external_transaction_id,
        )
        .execute(&mut **tx)
        .await?;

        Ok(())
    }


    #[tracing::instrument]
    pub async fn get_one(
        id: uuid::Uuid,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<FTransaction> {
        let r = sqlx::query_as!(
            FTransaction,
            r#"
    SELECT
        id,
        account_id,
        external_transaction_id
    FROM transactions
    WHERE id = $1
            "#,
            id
        )
        .fetch_one(&mut **tx)
        .await?;

        Ok(r)
    }


}
