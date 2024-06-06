FROM rust:1.77-bookworm as builder
WORKDIR /usr/src/app

COPY --from=d3fk/tailwindcss:stable /tailwindcss /usr/local/bin/tailwindcss
COPY . .
RUN cargo install --path .

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y procps ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/cargo/bin/folio-migrate /usr/local/bin/folio-migrate
COPY --from=builder /usr/local/cargo/bin/folio /usr/local/bin/folio

ENTRYPOINT ["/usr/local/bin/folio"]
