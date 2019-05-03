FROM debian:stretch

RUN apt-get update
RUN apt-get install -y mono-devel mono-complete fsharp mono-xbuild python3 \
    gnat-6 gcc g++ make openjdk-8-jre nuget libgit2-dev libssl-dev