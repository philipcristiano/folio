FROM erlang:24 AS builder
WORKDIR /app/src
ADD . /app/src
RUN rm -rf /app/src/deps /app/src/_rel

RUN make deps app
RUN make rel
RUN mv /app/src/_rel/folio_release/folio_*.tar.gz /app.tar.gz

FROM debian:buster

ENV LOG_LEVEL=info

RUN apt-get update && apt-get install -y openssl && apt-get clean

COPY --from=builder /app.tar.gz /app.tar.gz

WORKDIR /app

RUN tar -xzf /app.tar.gz

CMD ["/app/bin/folio_release", "foreground"]
