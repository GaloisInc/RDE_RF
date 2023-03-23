#!/bin/bash

# Path: run_docker.sh
docker run -it --rm -v $(pwd):/data --name refinement_finder org.Galois/der:latest -i /data -o /data/report -g -d=a4