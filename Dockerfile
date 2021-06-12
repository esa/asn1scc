FROM  mcr.microsoft.com/dotnet/sdk AS build

RUN apt update
RUN mkdir /usr/share/man/man1
RUN apt install -y python3 python3-distutils \
    gnat gcc g++ make openjdk-11-jre nuget libgit2-dev libssl-dev gprbuild ; apt clean


WORKDIR /src
COPY . .
RUN dotnet build Antlr/
RUN dotnet build "asn1scc.sln"

