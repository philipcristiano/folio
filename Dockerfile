FROM lukemathwalker/cargo-chef:latest-rust-1.90-bookworm AS chef
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder
COPY --from=planner /app/recipe.json recipe.json
# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --recipe-path recipe.json
# Build application
COPY . .
COPY --from=d3fk/tailwindcss:stable /tailwindcss /usr/local/bin/tailwindcss
ENV SQLX_OFFLINE=true
RUN cargo build --release

# We do not need the Rust toolchain to run the binary!
FROM debian:bookworm-slim
WORKDIR /app

RUN apt-get update && apt-get install openssl ca-certificates -y && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/folio-migrate /usr/local/bin/folio-migrate
COPY --from=builder /app/target/release/folio /usr/local/bin/folio

ENTRYPOINT ["/usr/local/bin/folio"]
