#FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
FROM ubuntu:20.04

#install .net
RUN apt-get update && apt-get install -y wget apt-transport-https && \
    wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    apt-get update && apt-get install -y dotnet-sdk-9.0

# Install system dependencies
RUN set -xe \
    && DEBIAN_FRONTEND=noninteractive apt-get update -y \
    && apt-get install -y libfontconfig libdbus-1-3 libx11-6 libx11-xcb-dev cppcheck htop \
        python3 python3-distutils gcc g++ make nuget libgit2-dev libssl-dev curl wget git unzip zip \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get purge --auto-remove \
    && apt-get clean 

# Create a non-root user
RUN adduser --disabled-password --gecos '' --uid 1000 myuser

# Adjust permissions for volumes
RUN mkdir -p /workdir /app && chown -R myuser:myuser /workdir /app

# Switch to the non-root user
USER myuser

# Install SDKMAN
RUN curl -s "https://get.sdkman.io" | bash && \
    echo "source $HOME/.sdkman/bin/sdkman-init.sh" >> $HOME/.bashrc && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk install java 17.0.9-oracle && sdk install scala 3.3.0 && sdk install sbt 1.9.0"

# Install GNAT and SPARK from AdaCore (still as root since no SDKMAN required here)
USER root
WORKDIR /gnat_tmp/
RUN wget -O gnat-2021-x86_64-linux-bin https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin \
    && git clone https://github.com/AdaCore/gnat_community_install_script.git \
    && chmod +x gnat_community_install_script/install_package.sh \
    && chmod +x gnat-2021-x86_64-linux-bin \
    && gnat_community_install_script/install_package.sh ./gnat-2021-x86_64-linux-bin /opt/GNAT/gnat-x86-2021 \
    && rm -rf /gnat_tmp/

# Set back to the non-root user for remaining tasks
USER myuser
WORKDIR /app/
ENV PATH="/opt/GNAT/gnat-x86-2021/bin:${PATH}"
