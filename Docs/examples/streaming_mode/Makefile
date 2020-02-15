export PATH:=$(realpath ../../../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

all:	
	Asn1f4.exe -c -uPER -sm -equal -o c_out/ a.asn && (cd c_out; make coverage; cd ..)

