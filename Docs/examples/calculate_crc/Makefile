export PATH:=$(realpath ../../../asn1scc/bin/Debug/net9.0/):$(PATH)

$(info ${PATH})

all:	cTest adaTest

cTest:
	asn1scc -if Acn_Dec_Int_PositiveInteger_ConstSize -if Acn_Enc_Int_PositiveInteger_ConstSize -mfm postEncoding -c -ACN -atc -o c_out/ a.a* && (cd c_out/ ; make coverage; cd ..)

adaTest:
	asn1scc -mfm postEncoding -Ada -ACN -atc -o a_out/ a.a* && (cd a_out/ ; make coverage; cd ..)

