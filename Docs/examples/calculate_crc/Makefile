export PATH:=$(realpath ../../../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

all:	cTest adaTest

cTest:
	Asn1f4.exe -mfm postEncoding -c -ACN -atc -o c_out/ a.a* && (cd c_out/ ; make coverage; cd ..)

adaTest:
	Asn1f4.exe -mfm postEncoding -Ada -ACN -atc -o a_out/ a.a* && (cd a_out/ ; make coverage; cd ..)

