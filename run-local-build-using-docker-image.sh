#!/bin/bash
docker build --build-arg NON_ROOT_USER=true --build-arg USERNAME=myuser --build-arg USERID=1000 -t asn1scc .
docker volume rm asn1scc_workdir
docker volume create asn1scc_workdir
docker run -ti --rm -v .:/app -v asn1scc_workdir:/workdir asn1scc bash -c "./local-build.sh $(git rev-parse --abbrev-ref HEAD)"
