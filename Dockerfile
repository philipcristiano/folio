
FROM erlang:25 AS BUILDER
RUN mkdir -p /app/folio
ADD Makefile rebar3 rebar.* /app/folio
WORKDIR /app/folio
RUN make compile

ADD . /app/folio
RUN make compile
RUN make tar && mv /app/folio/_build/default/rel/folio_release/folio_release-*.tar.gz /app.tar.gz


FROM debian:bullseye

ENV LOG_LEVEL=info
RUN apt-get update && apt-get install -y openssl && apt-get clean
COPY --from=BUILDER /app.tar.gz /app.tar.gz

WORKDIR /app
EXPOSE 8000

RUN tar -xzf /app.tar.gz

CMD ["/app/bin/folio_release", "foreground"]
