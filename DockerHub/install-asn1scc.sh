#!/bin/bash
apt-get update 
apt-get install -y wget bzip2 libicu63 make gcc
mkdir -p /usr/local/share/ 
cd /usr/local/share/  || exit 1
wget -q -O - https://github.com/maxime-esa/asn1scc/releases/download/4.2.4.7f/asn1scc-bin-4.2.4.7f.tar.bz2 | tar jxvf -
apt clean
