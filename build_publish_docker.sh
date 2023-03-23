#!/bin/bash
# Build the docker container using sbt
sbt docker
# Tag the docker container with the docker hub username
docker tag org.Galois/der:latest simonthrane/document_enricher:latest
# Publish the docker container to docker hub - this requires a login to docker hub. Ask Simon to do this.
docker push simonthrane/document_enricher:latest
# Remove the docker container from the local machine
docker rmi simonthrane/document_enricher:latest