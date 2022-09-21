#!/bin/sh
IMAGE=org.Galois/der:latest
exec docker run --rm -i --user="$(id -u):$(id -g)" --net=none -v "$PWD":/data "$IMAGE" -s /data -t /data "$@"