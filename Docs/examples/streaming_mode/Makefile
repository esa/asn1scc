export PATH:=$(realpath ../../../asn1scc/bin/Debug/net9.0/):$(PATH)

$(info ${PATH})

all:	cTest adaTest

cTest:
	asn1scc -c -uPER -sm -equal -o c_out/ a.asn && (cd c_out; make coverage; cd ..)

adaTest:
	asn1scc -Ada -uPER -sm -equal -o ada_out/ a.asn && (cd ada_out; make coverage; cd ..)
