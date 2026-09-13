# syntax=docker/dockerfile:1
ARG SDK_IMAGE=mcr.microsoft.com/dotnet/sdk:10.0-noble@sha256:e1ffd2a92ae84c1291bc1b6887501f8af98e6331e7af6d4c8d37168c5e87a64c
FROM ${SDK_IMAGE} AS toolchain
ARG SDK_IMAGE
ENV ASN1SCC_COVERAGE_BASE_IMAGE=${SDK_IMAGE}
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
        gcc gcc-13 gnat gnat-13 gprbuild make python3 ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 coverage \
    && useradd --uid 10001 --gid 10001 --create-home coverage \
    && mkdir -p /results /opt/asn1scc /opt/coverage /src \
    && chown 10001:10001 /results /opt/asn1scc /opt/coverage /src
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1 DOTNET_NOLOGO=1 \
    PYTHONDONTWRITEBYTECODE=1 PYTHONUNBUFFERED=1

FROM toolchain AS build
ARG SOURCE_REVISION=unspecified
WORKDIR /src
COPY --chown=10001:10001 . .
USER 10001:10001
RUN python3 v4Tests/scripts/coverageBaseline.py \
        --write-build-manifest /opt/coverage/build.json \
        --source-root /src --source-revision "$SOURCE_REVISION" \
    && dotnet publish asn1scc/asn1scc.fsproj -c Release -o /opt/asn1scc -m:1

FROM toolchain AS runner
COPY --from=build /opt/asn1scc/ /opt/asn1scc/
COPY --from=build /opt/coverage/build.json /opt/coverage/build.json
COPY v4Tests/scripts/coverageBaseline.py /opt/coverage/coverageBaseline.py
COPY v4Tests/coverage/ /opt/coverage/
COPY v4Tests/test-cases/acn/ /opt/test-cases/acn/
ENV ASN1SCC_COMPILER=/opt/asn1scc/asn1scc \
    ASN1SCC_TESTROOT=/opt/test-cases/acn \
    ASN1SCC_COVERAGE_REFERENCE=/opt/coverage/phase0-reference.json \
    ASN1SCC_BUILD_MANIFEST=/opt/coverage/build.json
USER 10001:10001
WORKDIR /results
ENTRYPOINT ["python3", "/opt/coverage/coverageBaseline.py"]
CMD ["--outdir", "/results"]
