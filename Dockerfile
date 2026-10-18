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

# Conditionally create non-root user and set permissions
# (ubuntu:24.04 already has user "ubuntu" with uid 1000: remove whatever user holds USERID)
RUN if [ "$NON_ROOT_USER" = "true" ]; then \
    if getent passwd "$USERID" >/dev/null; then userdel -r "$(getent passwd "$USERID" | cut -d: -f1)"; fi && \
    adduser --disabled-password --gecos '' --uid $USERID $USERNAME && \
    mkdir -p /workdir /app && \
    chown -R $USERNAME:$USERNAME /workdir /app; \
fi

# Install a pinned Rust toolchain (bump the version deliberately; "stable" would change on every image build) for compiling and running generated Rust code
# during the test suite (./scripts/runTests.py -l Rust -s)
ENV RUSTUP_HOME="/usr/local/rustup"
ENV CARGO_HOME="/usr/local/cargo"
ENV PATH="/usr/local/cargo/bin:${PATH}"
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | \
        sh -s -- -y --default-toolchain 1.98.1 --profile minimal \
    && rustc --version && cargo --version \
    && if [ "$NON_ROOT_USER" = "true" ]; then chown -R $USERNAME:$USERNAME $RUSTUP_HOME $CARGO_HOME; fi

# Switch to the appropriate user
USER $USERNAME

# Install SDKMAN
RUN curl -s "https://get.sdkman.io" | bash && \
    echo "source $HOME/.sdkman/bin/sdkman-init.sh" >> $HOME/.bashrc && \
    bash -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk install java 17.0.9-oracle && sdk install scala 3.3.0 && sdk install sbt 1.9.0"

# SPARK prover for the test files marked RUN_SPARK (regression/Program.fs):
# the FSF build of gnatprove from alire-project/GNAT-FSF-builds, pinned by
# checksum. It is self-contained (its own front end, Ada runtime, gprbuild and
# provers) and independent of the GNAT 13 compiler above; its bin/ holds only
# gnatprove. (temporarily switch back to root)
USER root
ARG GNATPROVE_VERSION=16.1.0-1
ARG GNATPROVE_SHA256=82528bef29857e239373fcc36732837d743f5a00805d502c70455422ffc97191
RUN wget -q -O /tmp/gnatprove.tar.gz \
        https://github.com/alire-project/GNAT-FSF-builds/releases/download/gnatprove-${GNATPROVE_VERSION}/gnatprove-x86_64-linux-${GNATPROVE_VERSION}.tar.gz \
    && echo "${GNATPROVE_SHA256}  /tmp/gnatprove.tar.gz" | sha256sum -c - \
    && mkdir -p /opt/gnatprove \
    && tar -xzf /tmp/gnatprove.tar.gz -C /opt/gnatprove --strip-components=1 \
    && rm -rf /tmp/gnatprove.tar.gz /opt/gnatprove/share/examples \
    && /opt/gnatprove/bin/gnatprove --version
ENV PATH="/opt/gnatprove/bin:${PATH}"

# Add UV for efficient python version management
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
ENV UV_HOME=/app/.uv

# Set back to the appropriate user
USER $USERNAME
WORKDIR /app/
