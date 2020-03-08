export PATH:=$(realpath ../../../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

all:	cTest adaTest

cTest:
	Asn1f4.exe -c -uPER -sm -equal -o c_out/ a.asn && (cd c_out; make coverage; cd ..)

adaTest:
	Asn1f4.exe -Ada -uPER -sm -equal -o ada_out/ a.asn && (cd ada_out; make coverage; cd ..)
