FROM debian:buster

RUN apt update
RUN apt install -y mono-devel mono-complete mono-xbuild python3 \
    gnat gcc g++ make openjdk-11-jre nuget libgit2-dev libssl-dev gprbuild ; apt clean
RUN echo "deb http://deb.debian.org/debian stretch main" >> /etc/apt/sources.list
RUN apt update
RUN apt install -y --no-install-recommends fsharp
