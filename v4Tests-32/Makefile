export PATH:=$(realpath ../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

all:	cTests adaTests

cTests:
	./scripts/runTests.py -l c
	./scripts/runTests.py -l c -s

adaTests:
	./scripts/runTests.py -l Ada

