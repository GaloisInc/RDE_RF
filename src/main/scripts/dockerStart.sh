#!/bin/sh
IMAGE=org.Galois/der:latest
exec docker run --rm -i --user="$(id -u):$(id -g)" -v /Users/shansen/Desktop/Source:/data "$IMAGE"