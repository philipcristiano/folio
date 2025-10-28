use axum::{
    Router,
    extract::State,
    response::{IntoResponse, Redirect, Response},
    routing::get,
};

use std::marker::PhantomData;
use uuid::Uuid;

// Typestate markers
#[derive(Debug)]
pub struct New;
#[derive(Debug)]
pub struct Saved;

#[derive(Clone, Debug, sqlx::Type)]
#[sqlx(transparent)]
pub struct FileID(uuid::Uuid);
// Encode implementation for sending data to Postgres
impl From<uuid::Uuid> for FileID {
    fn from(id: uuid::Uuid) -> Self {
        Self(id)
    }
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route(
            "/upload",
            get(handle_file_upload_get)
                .post(handle_file_upload_post)
                .layer(axum::extract::DefaultBodyLimit::max(1024 * 1024 * 100)),
        )
        .route("/", get(handle_file_list))
    //.route("/list", get(handle_file_list)))
}

use folio::{AppError, AppState};
use maud::html;
async fn handle_file_upload_get(
    State(_app_state): State<AppState>,
    user: Result<
        Option<service_conventions::oidc::OIDCUser>,
        service_conventions::oidc::OIDCUserError,
    >,
) -> Result<Response, AppError> {
    if let Ok(Some(_user)) = user {
        Ok(crate::html::maud_page(html! {
              (crate::html::sidebar())
              div #main class="main" {

                form id="form" method="post" enctype="multipart/form-data" action="/files/upload" {
                    input type="file" name="file" {}
                    input type="submit" {}
                }
              }

        })
        .into_response())
    } else {
        Ok(Redirect::to("/").into_response())
    }
}

async fn handle_file_upload_post(
    State(app_state): State<AppState>,
    user: Result<
        Option<service_conventions::oidc::OIDCUser>,
        service_conventions::oidc::OIDCUserError,
    >,
    mut multipart: axum::extract::Multipart,
) -> Result<Response, AppError> {
    tracing::info!("Post!");
    if let Ok(Some(_user)) = user {
        while let Some(field) = multipart.next_field().await.unwrap() {
            let name = field.name().unwrap().to_string();
            let filename = field.file_name().unwrap().to_string();

            let data = field.bytes().await.unwrap();
            let new_file = UploadedFile::new(filename.clone(), "csv".to_string(), data.to_vec());

            let mut c = app_state.db.begin().await?;
            new_file.insert(&mut c).await?;
            c.commit().await?;

            tracing::info!(
                name = &name,
                filename = filename,
                length = data.len(),
                "file upload "
            );
        }
        Ok(Redirect::to("/files").into_response())
    } else {
        Ok(Redirect::to("/").into_response())
    }
}

async fn handle_file_list(
    State(app_state): State<AppState>,
    user: Result<
        Option<service_conventions::oidc::OIDCUser>,
        service_conventions::oidc::OIDCUserError,
    >,
) -> Result<Response, AppError> {
    if let Ok(Some(_user)) = user {
        let mut c = app_state.db.begin().await?;
        let files = UploadedFileMetadata::get_all(&mut c).await?;
        c.rollback().await?;
        Ok(crate::html::maud_page(html! {
              (crate::html::sidebar())
              div #main class="main" {

                ul {
                    @for file in files {
                        li { (file.name) }
                    }
                }


              }

        })
        .into_response())
    } else {
        Ok(Redirect::to("/").into_response())
    }
}

#[derive(Debug)]
pub struct UploadedFile<State = Saved> {
    // Option<Uuid> allows us to have a flat struct for SQLx
    // while enforcing id presence/absence through the type system
    id: Option<FileID>,
    name: String,
    filetype: String,
    contents: Vec<u8>,
    // Zero-sized marker that doesn't affect memory layout
    _state: PhantomData<State>,
}

// Need this to pull from SQLX with query_as! checking. The `From` implementation will then turn it
// into the right type
#[derive(Debug)]
pub struct UploadedFileRow {
    // Option<Uuid> allows us to have a flat struct for SQLx
    // while enforcing id presence/absence through the type system
    id: FileID,
    name: String,
    filetype: String,
    contents: Vec<u8>,
    // Zero-sized marker that doesn't affect memory layout
}
impl From<UploadedFileRow> for UploadedFile<Saved> {
    fn from(row: UploadedFileRow) -> Self {
        Self {
            id: Some(row.id),
            name: row.name,
            filetype: row.filetype,
            contents: row.contents,
            _state: PhantomData,
        }
    }
}

// Methods available for both states
impl<State> UploadedFile<State> {
    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_filetype(&self) -> &str {
        &self.filetype
    }

    pub fn get_contents(&self) -> &[u8] {
        &self.contents
    }
}

// Constructor for new files (no id yet)
impl UploadedFile<New> {
    pub fn new(name: String, filetype: String, contents: Vec<u8>) -> Self {
        Self {
            id: None,
            name,
            filetype,
            contents,
            _state: PhantomData,
        }
    }

    #[tracing::instrument]
    async fn insert(&self, tx: &mut sqlx::Transaction<'_, sqlx::Postgres>) -> anyhow::Result<()> {
        sqlx::query!(
            r#"
    INSERT INTO uploaded_files ( name, filetype, contents )
    VALUES ( $1, $2, $3 )
            "#,
            self.name,
            self.filetype,
            self.contents,
        )
        .execute(&mut **tx)
        .await?;

        Ok(())
    }
}

// Methods available for both states
impl<State> UploadedFile<State> {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn filetype(&self) -> &str {
        &self.filetype
    }

    pub fn contents(&self) -> &[u8] {
        &self.contents
    }
}

impl UploadedFile<Saved> {
    /// Get the id (guaranteed to exist for Saved state)
    pub fn id(&self) -> FileID {
        self.id.clone().expect("Saved UploadedFile must have an id")
    }

    #[tracing::instrument]
    pub async fn get_one(
        id: FileID,
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<Self> {
        let r = sqlx::query_as!(
            UploadedFileRow,
            r#"
    SELECT
        id,
        name,
        filetype,
        contents
    FROM uploaded_files
    WHERE id = $1
            "#,
            id as FileID
        )
        .fetch_one(&mut **tx)
        .await?;

        Ok(r.into())
    }
}

#[derive(Debug)]
pub struct UploadedFileMetadata {
    // Option<Uuid> allows us to have a flat struct for SQLx
    // while enforcing id presence/absence through the type system
    id: FileID,
    name: String,
    filetype: String,
}
impl UploadedFileMetadata {
    pub fn get_id(&self) -> &FileID {
        &self.id
    }

    #[tracing::instrument]
    pub async fn get_all(
        tx: &mut sqlx::Transaction<'_, sqlx::Postgres>,
    ) -> anyhow::Result<Vec<Self>> {
        let r = sqlx::query_as!(
            UploadedFileMetadata,
            r#"
    SELECT
        id,
        name,
        filetype
    FROM uploaded_files
            "#,
        )
        .fetch_all(&mut **tx)
        .await?;

        Ok(r)
    }
}
