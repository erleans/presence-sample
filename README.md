# Presence Erleans Example

## Run in Kubernetes

The simplest way to get started with Kubernetes locally is to use something like [microk8s](https://microk8s.io/).

With `microk8s` running and the `dns` and `registry` enabled use [Tilt](https://tilt.dev/) to boot up the project. The Kubernetes resources are generated from kustomize configs under `deployment/`:

``` shell
$ tilt up
```
