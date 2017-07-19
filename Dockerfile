FROM tsloughter/erlang-alpine:20.0.1 as builder

WORKDIR /usr/src/app
COPY . /usr/src/app

RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /usr/src/app/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.6

RUN apk add --no-cache openssl-dev ncurses

WORKDIR /opt/presence

ENV RELX_REPLACE_OS_VARS true
ENV NODE 127.0.0.1
ENV PORT 8080
ENV DISCOVERY_DOMAIN localhost
ENV POSTGRES_HOST localhost
ENV POSTGRES_PORT 5432
ENV POSTGRES_DATABASE test
ENV POSTGRES_USER test
ENV POSTGRES_PASSWORD test

COPY --from=builder /opt/rel /opt/presence

EXPOSE 8080 8080

ENTRYPOINT ["/opt/presence/bin/presence"]

CMD ["foreground"]
