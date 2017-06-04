FROM erlang:19.3.3 as builder

WORKDIR /usr/src/app
COPY . /usr/src/app
RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /usr/src/app/_build/prod/rel/*/*.tar.gz -C /opt/rel

FROM ubuntu:16.04

RUN apt-get update && \
    apt-get install --no-install-recommends -y libssl-dev && \
    rm -rf /var/lib/apt/lists/*

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
