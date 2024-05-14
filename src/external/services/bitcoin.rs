use axum::{
    extract::{Path, State},
    response::{IntoResponse, Redirect, Response},
    routing::{get, post},
    Form, Router,
};

use crate::external::services::{ExternalConnection, ExternalConnectionCredentials};
use crate::html;

use bitcoin::bip32::{DerivationPath, Xpub};
use bitcoin::secp256k1::Secp256k1;
use bitcoin::{Address, Network, PublicKey};

use crate::{AppError, AppState};

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
                let ck = xpub.derive_pub(&secp, &dp).expect("render").to_pub();
                let addr = match format {
                    AddressFormat::P2shwpkh => {
                        Address::p2shwpkh(&ck, Network::Bitcoin).expect("foo")
                    }
                };
                addr
            }
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone)]
enum AddressFormat {
    P2shwpkh,
}
impl AddressFormat {
    fn render_choice() -> Markup {
        maud::html! {
          input type="radio" name="address_format" value="P2shwpkh" {}
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
    let secp = Secp256k1::verification_only();

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
    //let ck = form.pubkey.to_pub();
    //let addr = Address::p2wpkh(&ck, Network::Bitcoin);
    use electrum_client::{Client, ElectrumApi};

    let mut client = Client::new("tcp://electrum.blockstream.info:50001")?;
    let script = addr.script_pubkey();
    let bal = client.script_get_balance(&script)?;
    let history = client.script_get_history(&script)?;
    let txs: Vec<Result<bitcoin::Transaction, electrum_client::Error>> = history
        .into_iter()
        .map(|h| {
            let hash = h.tx_hash;
            client.transaction_get(&hash)
        })
        .collect();

    tracing::info!("Accounts {:?}", addr);
    Ok(maud::html! {

       p { "Add Bitcoin Account"}
       {
           (addr) br {}
           "confirmed: " (bal.confirmed) br {}
           "unconfirmed: " (bal.unconfirmed) br {}
           @for tx in txs {
               "tx:" br{}
               @if let Ok(oktx) = tx {
                    @for output in oktx.output {
                        (output.value)
                        br {}
                    }
               }

           }
           //@for a in accounts {
           //    p { (a.name) " " (a.currency)}
           //}
       }
    }
    .into_response())
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
    let ecc = ExternalConnectionCredentials::get_for(&ec, &mut tx).await?;
    let credentials: SaveableState = ecc.decrypt_credentials(&app_state.database_encryption_key)?;

    tx.rollback().await?;

    Ok(maud::html! {

       p { "Bitcoin Account"}
       {
         (ec.id) " - "
         (ec.integration) " - "
         (ec.name) " - "
         (credentials) " - "
       }
    }
    .into_response())
}
