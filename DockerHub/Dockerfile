FROM debian:buster

# Install example code and instructional message upon login
COPY .bashrc /root/
RUN mkdir -p /root/asn1scc-example
COPY asn1scc-example/* /root/asn1scc-example/

# Install asn1scc itself
COPY  ./install-asn1scc.sh /root/
RUN /root/install-asn1scc.sh
