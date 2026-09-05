export PATH:=$(realpath ../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

all:	cTests adaTests scalaTests pythonTests

cTests:
	./scripts/runTests.py -l c -s
	./scripts/runTests.py -l c

adaTests:
	./scripts/runTests.py -l Ada -s
	./scripts/runTests.py -l Ada

scalaTests:
	./scripts/runTests.py -l Scala -s
	./scripts/runTests.py -l Scala

pythonTests:
	./scripts/runTests.py -l python -s
	./scripts/runTests.py -l python

