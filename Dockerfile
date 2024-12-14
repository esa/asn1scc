FROM  mcr.microsoft.com/dotnet/sdk:9.0 AS build

RUN set -xe \
    && DEBIAN_FRONTEND=noninteractive apt-get update -y \
	&& apt-get install -y libfontconfig libdbus-1-3 libx11-6 libx11-xcb-dev cppcheck htop \
	    python3 python3-distutils gcc g++ make nuget libgit2-dev libssl-dev \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get purge --auto-remove \
    && apt-get clean 

# this SHELL command is needed to allow using source
SHELL ["/bin/bash", "-c"]  
# Install dependencies for scala backend
RUN apt-get update -y \
	&& apt-get install -y curl wget unzip zip \	
	&& curl -s "https://get.sdkman.io" | bash \
	&& chmod a+x "$HOME/.sdkman/bin/sdkman-init.sh" \
 	&& source "$HOME/.sdkman/bin/sdkman-init.sh" \	
	&& sdk install java 17.0.9-oracle \				
	&& sdk install scala 3.3.0 \					
	&& sdk install sbt 1.9.0

# Install GNAT AND SPARK from AdaCore
WORKDIR /gnat_tmp/

# The ADD instruction will always download the file and the cache will be invalidated if the checksum of the file no longer matches
# On the other hand, the RUN instruction will not invalidate the cache unless its text changes. 
# So if the remote file is updated, you won't get it. Docker will use the cached layer.
# In our case, the gnat-2021-20210519-x86_64-linux-bin will not change. So, it is preferable to ADD
#ADD https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin  ./gnat-2021-20210519-x86_64-linux-bin

RUN wget -O gnat-2021-20210519-x86_64-linux-bin https://community.download.adacore.com/v1/f3a99d283f7b3d07293b2e1d07de00e31e332325?filename=gnat-2021-20210519-x86_64-linux-bin \
	&& git clone https://github.com/AdaCore/gnat_community_install_script.git \
	&& chmod +x gnat_community_install_script/install_package.sh \
	&& chmod +x gnat-2021-20210519-x86_64-linux-bin \
	&& gnat_community_install_script/install_package.sh ./gnat-2021-20210519-x86_64-linux-bin /opt/GNAT/gnat-x86-2021 \
	&& cd \
	&& rm -rf /gnat_tmp/ \
	&& sed -i 's/# alias l=/alias l=/' ~/.bashrc \
	&& sed -i 's/# export LS_OPTIONS/export LS_OPTIONS/' ~/.bashrc

WORKDIR /app/

ENV PATH="/opt/GNAT/gnat-x86-2021/bin:${PATH}"
#ENTRYPOINT ["/bin/bash"]
