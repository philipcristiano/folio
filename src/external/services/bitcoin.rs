use axum::{
    extract::{Path, State},
    response::{IntoResponse, Redirect, Response},
    routing::{get, post},
    Form, Router,
};
use rust_decimal::Decimal;
use crate::external::services::{ExternalConnection, ExternalConnectionCredentials};
use crate::html;

use bitcoin::bip32::{DerivationPath, Xpub};
use bitcoin::secp256k1::Secp256k1;
use bitcoin::{Address, Network, PublicKey};

use crate::{AppError,AppState};
use crate::accounts::insert_or_update_external_account_info;

pub fn setup() -> Router<AppState> {
    let r = Router::new();
    r.route("/f/add", get(handle_f_add).post(handle_f_add_post))
        .route("/f/get/:id", get(handle_f_get))
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct Credentials {
    pubkey: Xpub,
    address_format: AddressFormat,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
enum SaveableState {
    Pub(Xpub, AddressFormat),
}

impl SaveableState {
    fn derive_address(&self, dp: &DerivationPath) -> Address {
        let secp = Secp256k1::verification_only();
        match self {
            SaveableState::Pub(xpub, format) => {
                let addr = xpub.derive_pub(&secp, &dp).expect("render");
                let address = match format {
                    AddressFormat::P2shwpkh => {
                        let ck = addr.to_pub();
                        Address::p2shwpkh(&ck, Network::Bitcoin).expect("foo")
                    }
                    AddressFormat::P2sh => {
                        panic!("Not yet supported")
                    }
                };
                address
            }
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
enum AddressFormat {
    #[serde(alias = "p2shwpkh")]
    P2shwpkh,
    #[serde(alias = "p2sh")]
    P2sh,
}

impl Render for AddressFormat {
    fn render(&self) -> Markup {
        match self {
            Self::P2shwpkh => maud::html!{"P2shwpkh"},
            Self::P2sh => maud::html!{"P2sh"},
        }
    }
}
impl AddressFormat {
    fn render_choice() -> Markup {
        maud::html! {
          input type="radio" name="address_format" value={(AddressFormat::P2shwpkh)} {}
          label for="P2shwpkh" {"p2shwpkh - Addresses starting with 3"}
          br {}

        }
    }
}

use maud::{Markup, Render};
use std::str::FromStr;
impl Render for SaveableState {
    fn render(&self) -> Markup {
        let p = DerivationPath::from_str("m/0/0").unwrap();
        let addr = self.derive_address(&p);
        maud::html! {
             "xpub: " (addr )
        }
    }
}

async fn handle_f_add(
    State(_app_state): State<AppState>,
    _user: service_conventions::oidc::OIDCUser,
) -> Result<Response, AppError> {
    Ok(maud::html! {

       p { "Bitcoin"}
       {
           input type="text" name="pubkey" {}
           (AddressFormat::render_choice())
       }
    }
    .into_response())
}

async fn handle_f_add_post(
    State(app_state): State<AppState>,
    _user: service_conventions::oidc::OIDCUser,
    Form(form): Form<Credentials>,
) -> Result<Response, AppError> {

    let p = DerivationPath::from_str("m/0/0").unwrap();
    let state = SaveableState::Pub(form.pubkey, form.address_format);
    let addr = state.derive_address(&p);
    let id = uuid::Uuid::new_v4();
    let ec = ExternalConnection {
        id,
        integration: "bitcoin".to_string(),
        name: "New Bitcoin Account".to_string(),
    };
    let ecc = ExternalConnectionCredentials::new(id, state, &app_state.database_encryption_key)?;
    let mut tx = app_state.db.begin().await?;
    ec.insert(&mut tx).await?;
    ecc.insert(&mut tx).await?;
    tx.commit().await?;

    let script = addr.script_pubkey();

    tracing::info!("Accounts {:?}", addr);
    Ok(maud::html! {

       p { "Add Bitcoin Account"}
       {
           (addr) br {}
       }
    }
    .into_response())
}

pub async fn sync_connection(c: &ExternalConnection, app_state: &AppState) -> anyhow::Result<()> {

    let mut tx = app_state.db_spike.begin().await?;
    let ecc = ExternalConnectionCredentials::get_for(&c, &mut tx).await?;
    let credentials: SaveableState = ecc.decrypt_credentials(&app_state.database_encryption_key)?;
    tx.rollback().await?;
    let currency = uuid::Uuid::from_str("049c1057-82a0-4a3b-b365-10acb3da9862")?;

    let p = DerivationPath::from_str("m/0").unwrap();
    for child in p.normal_children() {
        let addr = credentials.derive_address(&child);
        let bal = blockstream_balance(&addr).await?;

        tracing::debug!(tx_count=bal.chain_stats.funded_txo_count, funded=bal.chain_stats.funded_txo_sum, spent=bal.chain_stats.spent_txo_sum, addr=addr.to_string(), "Account balance" );
        if bal.chain_stats.funded_txo_count > 0 {

            let balance64 = bal.chain_stats.funded_txo_sum - bal.chain_stats.spent_txo_sum;
            let balance128: i128 = balance64.into();
            let balance = Decimal::from_i128_with_scale(balance128, 0);
            let mut tx = app_state.db_spike.begin().await?;
            insert_or_update_external_account_info(
                c,
                &addr.to_string(),
                &currency,
                balance,
                &mut tx).await?;
            tx.commit().await?;

            sync_transactions(c, &addr, app_state).await?;

        } else {
            break;
        }
    }
    Ok(())
}

pub async fn sync_transactions(c: &ExternalConnection, addr: &Address, app_state: &AppState) -> anyhow::Result<()> {
    let transactions = blockstream_transactions(addr, &"0".to_string()).await?;
    tracing::debug!("transactions {transactions:?}");
    Ok(())

}

#[derive(serde::Deserialize, Clone, Debug)]
pub struct IDFilter {
    pub id: uuid::Uuid,
}
async fn handle_f_get(
    State(app_state): State<AppState>,
    Path(params): Path<IDFilter>,
    _user: service_conventions::oidc::OIDCUser,
) -> Result<Response, AppError> {
    let mut tx = app_state.db.begin().await?;
    let ec = ExternalConnection::get_one(params.id, &mut tx).await?;
    let accounts = crate::accounts::Account::get_all_for_external_connection(&ec.id, &mut tx).await?;
    tx.rollback().await?;



    Ok(maud::html! {

       @for account in accounts {
       p { "Bitcoin Account"}
       {
         (account.external_id) " - "
       }

       }
    }
    .into_response())
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct Balance {

    chain_stats: BalanceStats
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct BalanceStats {
    funded_txo_count: u32,
    funded_txo_sum:  u64,
    spent_txo_sum:  u64,
    spent_txo_count: u32,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct BlockstreamTransaction {
    txid: String,
    vin: Vec<Vin>,
    vout: Vec<Out>,
    size: u32,
    weight: u32,
    status: BlockStreamTransactionStatus,

}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct Vin {
    txid: String,
    prevout: Out,
}
#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct Out {
    scriptpubkey_address: String, // Maybe Address?
    scriptpubkey_type: String, // todo: AddressFormat,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
struct BlockStreamTransactionStatus {
    confirmed: bool,
    block_height: u32,
    block_hash: String,
    block_time: u32,
}

pub async fn blockstream_balance(addr: &Address) -> anyhow::Result<Balance> {
    let client = reqwest::Client::new();
    let url = format!("https://blockstream.info/api/address/{addr}");
    println!("URL {url}");
    let account_set_text = client.get(&url).send().await?.text().await?;
    tracing::debug!(url=&url, text=account_set_text, "Blockstream response");

    let balance: Balance = serde_json::from_str(&account_set_text)?;
    Ok(balance)
}

pub async fn blockstream_transactions(addr: &Address, last_seen_id: &String) -> anyhow::Result<Vec<BlockstreamTransaction>> {
    let client = reqwest::Client::new();
    let url = format!("https://blockstream.info/api/address/{addr}/txs/chain/{last_seen_id}");
    println!("URL {url}");
    let account_set_text = client.get(&url).send().await?.text().await?;
    tracing::debug!(url=&url, text=account_set_text, "Blockstream response");

    let obj: Vec<BlockstreamTransaction> = serde_json::from_str(&account_set_text)?;
    Ok(obj)
}
