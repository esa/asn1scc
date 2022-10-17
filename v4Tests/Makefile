export PATH:=$(realpath ../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

all:	cTests adaTests

cTests:
	./scripts/runTests.py -l c -s
	./scripts/runTests.py -l c

adaTests:
	./scripts/runTests.py -l Ada -s
	./scripts/runTests.py -l Ada

