FROM debian:stretch

RUN apt-get update
RUN apt-get install -y mono-devel mono-complete fsharp mono-xbuild python3 \
    gnat-6 gcc g++ make openjdk-8-jre nuget libgit2-dev libssl-dev gprbuild ; apt clean
RUN echo "deb http://deb.debian.org/debian sid main" >> /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y mono-complete
