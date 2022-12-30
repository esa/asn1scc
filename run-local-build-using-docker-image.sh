#!/bin/bash
docker build -t asn1scc .
docker run -ti --rm -v $(pwd):/app -v asn1scc_workdir:/workdir asn1scc /bin/bash -c './local-build.sh'
