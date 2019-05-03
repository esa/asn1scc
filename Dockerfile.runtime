# syntax = docker/dockerfile:experimental
FROM debian:stretch as builder

RUN rm -f /etc/apt/apt.conf.d/docker-clean; echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache
RUN --mount=type=cache,target=/var/cache/apt --mount=type=cache,target=/var/lib/apt \
    set -ex ;\
	apt update ;\
    apt-get install -y mono-devel mono-complete fsharp mono-xbuild python3 \
    gnat-6 gcc g++ make openjdk-8-jre nuget libgit2-dev libssl-dev

RUN --mount=type=cache,target=/var/cache/apt --mount=type=cache,target=/var/lib/apt \
    set -ex ;\
    apt-get install -y git

RUN set -ex ;\
    cd /tmp ;\
    git clone https://github.com/ttsiodras/asn1scc.git ;\
    cd asn1scc/ ;\
    nuget restore ;\
    xbuild /p:TargetFrameworkVersion="v4.5" ;\
    xbuild /p:Configuration=Release /p:TargetFrameworkVersion="v4.5" ;\
    cd /tmp ;\
    mkdir asn1install ;\
    cd asn1install ;\
    cp ../asn1scc/Asn1f4/bin/Release/*.dll . ;\
    cp ../asn1scc/Asn1f4/bin/Release/*.exe . ;\
    mv Asn1f4.exe asn1.exe ;\
    chmod +x asn1.exe ;\
    cp ../asn1scc/Asn1f4/bin/Release/*.stg .

#################################################################################################
#################################################################################################
FROM alpine:edge

COPY --from=builder /tmp/asn1install /opt/asn1scc

RUN apk add --no-cache mono --repository http://dl-cdn.alpinelinux.org/alpine/edge/testing
