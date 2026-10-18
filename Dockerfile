FROM ubuntu:24.04

# Build arguments that control user creation. They must be declared after FROM:
# an ARG declared before FROM is visible only to the FROM line itself.
ARG NON_ROOT_USER=false
ARG USERNAME=root
ARG USERID=0

ENV DEBIAN_FRONTEND=noninteractive

# Install .NET 10 SDK via the official dotnet-install.sh script
# (latest 10.0 SDK, the same channel as before the move to 24.04).
RUN apt-get update && apt-get install -y --no-install-recommends \
        wget ca-certificates libicu74 libssl3t64 libstdc++6 zlib1g \
        libgssapi-krb5-2 \
    && wget -q https://dot.net/v1/dotnet-install.sh -O /tmp/dotnet-install.sh \
    && chmod +x /tmp/dotnet-install.sh \
    && /tmp/dotnet-install.sh --channel 10.0 --install-dir /usr/share/dotnet \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet \
    && rm /tmp/dotnet-install.sh
ENV DOTNET_ROOT=/usr/share/dotnet

# Install system dependencies.
# GCC and GNAT come from Ubuntu (gcc-13 / gnat-13 + gprbuild): the same compiler
# for C and Ada, and the same versions as the Ubuntu 24.04 development hosts and
# the coverage image (v4Tests/coverage/Dockerfile).
RUN set -xe \
    && apt-get update -y \
    && apt-get install -y libfontconfig1 libdbus-1-3 libx11-6 libx11-xcb-dev cppcheck htop \
        python3 python3-pytest gcc g++ make libgit2-dev libssl-dev curl wget git unzip zip \
        gcc-13 gnat gnat-13 gprbuild \
    && gnatmake --version | head -1 && gprbuild --version | head -1 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get purge --auto-remove \
    && apt-get clean

# Install a pinned Rust toolchain (bump the version deliberately; "stable" would change on every image build) for compiling and running generated Rust code
# during the test suite (./scripts/runTests.py -l Rust -s)
ENV RUSTUP_HOME="/usr/local/rustup"
ENV CARGO_HOME="/usr/local/cargo"
ENV PATH="/usr/local/cargo/bin:${PATH}"
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
        sh -s -- -y --default-toolchain 1.98.1 --profile minimal \
    && rustc --version && cargo --version

# Conditionally create non-root user and set permissions
# (ubuntu:24.04 already has user "ubuntu" with uid 1000: remove whatever user holds USERID)
RUN if [ "$NON_ROOT_USER" = "true" ]; then \
    if getent passwd "$USERID" >/dev/null; then userdel -r "$(getent passwd "$USERID" | cut -d: -f1)"; fi && \
    adduser --disabled-password --gecos '' --uid $USERID $USERNAME && \
    mkdir -p /workdir /app && \
    chown -R $USERNAME:$USERNAME /workdir /app /usr/local/cargo /usr/local/rustup; \
fi

# Switch to the appropriate user
USER $USERNAME

# Install SDKMAN
RUN curl -s "https://get.sdkman.io" | bash && \
    echo "source $HOME/.sdkman/bin/sdkman-init.sh" >> $HOME/.bashrc && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk install java 17.0.9-oracle && sdk install scala 3.3.0 && sdk install sbt 1.9.0"

# Install GNAT Community 2021 ONLY for its SPARK prover: the regression runs
# gnatprove on the test files marked RUN_SPARK (regression/Program.fs).
# Its compiler is not used (its back end hangs on some generated test files,
# and its `ld` cannot link against glibc 2.36+), so it stays out of PATH; the
# gnatprove wrapper puts it first in PATH only for gnatprove itself.
# (temporarily switch back to root)
USER root
WORKDIR /gnat_tmp/
RUN wget -O gnat-2021-x86_64-linux-bin https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin \
    && git clone https://github.com/AdaCore/gnat_community_install_script.git \
    && chmod +x gnat_community_install_script/install_package.sh \
    && chmod +x gnat-2021-x86_64-linux-bin \
    && gnat_community_install_script/install_package.sh ./gnat-2021-x86_64-linux-bin /opt/GNAT/gnat-x86-2021 \
    && rm -rf /gnat_tmp/ \
    && printf '#!/bin/sh\nPATH=/opt/GNAT/gnat-x86-2021/bin:$PATH exec /opt/GNAT/gnat-x86-2021/bin/gnatprove "$@"\n' \
        > /usr/local/bin/gnatprove \
    && chmod +x /usr/local/bin/gnatprove \
    && gnatprove --version

# Add UV for efficient python version management
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
ENV UV_HOME=/app/.uv

# Set back to the appropriate user
USER $USERNAME
WORKDIR /app/
