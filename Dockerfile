# syntax = docker/dockerfile:experimental
FROM erlang:22.2-alpine as builder


# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update git curl

RUN curl -fSL -o /tmp/rebar3 https://s3.amazonaws.com/rebar3/rebar3 && \
    mv /tmp/rebar3 /usr/local/bin/ && \
    chmod +x /usr/local/bin/rebar3

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock .
RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 compile

FROM builder as prod_compiled

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as prod compile

FROM prod_compiled as releaser

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# tar for unpacking the target system
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    apk add --update tar

RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.11 as runner

WORKDIR /opt/presence

ENV COOKIE=presence \
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # presence specific env variables to act as defaults
    DB_HOST=127.0.0.1 \
    LOGGER_LEVEL=debug \
    SCHEDULERS=1 \
    PORT=8080 \
    DISCOVERY_DOMAIN=localhost \
    POSTGRES_HOST=localhost \
    POSTGRES_PORT=5432 \
    POSTGRES_DATABASE=test \
    POSTGRES_USER=test \
    POSTGRES_PASSWORD=test

# openssl needed by the crypto app
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update openssl ncurses

COPY --from=releaser /opt/rel .

ENTRYPOINT ["/opt/presence/bin/presence"]
CMD ["foreground"]

# image to use in tilt when running the release
FROM builder as dev_release

COPY . .
RUN rebar3 as tilt release

ENTRYPOINT ["/app/_build/tilt/rel/presence/bin/presence"]
CMD ["foreground"]

# FROM busybox as dev_sql

# COPY _build/default/lib/erleans_provider_pgo/priv/* /app/sql/
