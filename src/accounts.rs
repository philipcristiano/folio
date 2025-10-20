
use crate::external::services::{ExternalConnection};
use sqlx::Transaction;
use rust_decimal::Decimal;

#[derive(sqlx::Type, Debug, Clone)]
#[sqlx(type_name = "account_ownership", rename_all = "snake_case")]
pub enum AccountOwnership{
    OtherParty,
    Owned,
}


#[derive(Debug, Clone)]
pub struct Account {
    pub id: uuid::Uuid,
    pub external_connection_id: uuid::Uuid,
    pub external_id: String,
    pub ownership: String,
}

impl Account {

    fn ownership(&self) -> AccountOwnership {
        if self.ownership == "owned" {
            AccountOwnership::Owned

        } else {
            AccountOwnership::OtherParty
        }

    }
    #[tracing::instrument]
    async fn insert(&self, tx: &mut sqlx::Transaction<'_, sqlx::Postgres>) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    INSERT INTO accounts ( id, external_connection_id, external_id, ownership  )
    VALUES ( $1, $2, $3, $4 )
            "#,
            self.id,
            self.external_connection_id,
            self.external_id,
            self.ownership as _
        )
        .execute(&mut **tx)
        .await?;

        Ok(())
    }

    async fn set_owned(&self, tx: &mut sqlx::Transaction<'_, sqlx::Postgres>) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    UPDATE accounts
    SET ownership = $1
    WHERE id = $2
            "#,
            "owned",
            self.id
        )
        .execute(&mut **tx)
        .await?;

        Ok(())
    }


    #[tracing::instrument]
    pub async fn get_one(
        id: uuid::Uuid,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<Account> {
        let r = sqlx::query_as!(
            Account,
            r#"
    SELECT
        id,
        external_connection_id,
        external_id,
        ownership as "ownership: _"
    FROM accounts
    WHERE id = $1
            "#,
            id
        )
        .fetch_one(&mut **tx)
        .await?;

        Ok(r)
    }

    #[tracing::instrument]
    pub async fn get_all_for_external_connection(
        external_connection_id: &uuid::Uuid,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<Vec<Account>> {
        let r = sqlx::query_as!(
            Account,
            r#"
    SELECT
        id,
        external_connection_id,
        external_id,
        ownership as "ownership: _"

    FROM accounts
    WHERE external_connection_id = $1
            "#,
            external_connection_id,
        )
        .fetch_all(&mut **tx)
        .await?;

        Ok(r)
    }

    #[tracing::instrument]
    pub async fn get_by_external_connection(
        external_connection_id: &uuid::Uuid,
        external_id: &String,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<Option<Account>> {
        let r = sqlx::query_as!(
            Account,
            r#"
    SELECT
        id,
        external_connection_id,
        external_id,
        ownership as "ownership: _"

    FROM accounts
    WHERE external_connection_id = $1
    AND external_id = $2
            "#,
            external_connection_id,
            external_id
        )
        .fetch_optional(&mut **tx)
        .await?;

        Ok(r)
    }

}

#[tracing::instrument(skip(tx))]
pub async fn insert_or_update_external_account_info(
        ec: &ExternalConnection,
        ecid: &String,
        currency: &uuid::Uuid,
        balance: Decimal,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
        ) -> anyhow::Result<()> {
    let maybe_account = Account::get_by_external_connection(&ec.id, ecid, tx).await?;
    if let Some(account) = maybe_account {
        tracing::debug!("Account already exists");
        match account.ownership() {
            AccountOwnership::Owned => (),
            _ => account.set_owned(tx).await?


        };

    } else {
        let account = Account{
            id: uuid::Uuid::new_v4(),
            external_connection_id: ec.id,
            external_id: ecid.to_string(),
            ownership: "owned".to_string(),
        };
        account.insert(tx).await?;

    };

    Ok(())
}
