allow_k8s_contexts("microk8s")
default_registry('127.0.0.1:32000')

custom_build(
    'presence_sql',
    'docker buildx build -o type=docker --target releaser --tag $EXPECTED_REF .',
    ['_build/default/lib/erleans_provider_pgo/priv/migrations'],
    entrypoint="cp /opt/rel/sql/* /flyway/sql"
)

custom_build(
    'presence',
    'docker buildx build -o type=docker --target dev_release --tag $EXPECTED_REF .',
    ['.'],
    live_update=[
        sync('rebar.config', '/app/src/rebar.config'),
        sync('src', '/app/src/'),
        run('rebar3 as tilt compile'),
        run('/app/_build/tilt/rel/presence/bin/presence restart')
    ],
    ignore=["rebar.lock"]
)

k8s_yaml(kustomize('deployment/overlays/dev'))

watch_file('deployment/')
