#!/bin/bash
#docker build -f Dockerfile.local.wsl --build-arg NON_ROOT_USER=true --build-arg USERNAME=myuser --build-arg USERID=1000 -t myimage .
docker run -ti --rm -v .:/app -v asn1scc_workdir:/workdir asn1scc bash -c "./local-build.sh $(git rev-parse --abbrev-ref HEAD)"
