export PATH:=$(realpath ../Asn1f4/bin/Debug/):$(PATH)

$(info ${PATH})

# Number of parallel jobs for runTests.py (-j option).
# Override with: make -j8  or  make JOBS=8
JOBS ?= 1

all:	cTests adaTests scalaTests pythonTests rustTests

cTests:
	./scripts/runTests.py -l c -s -j $(JOBS)
	./scripts/runTests.py -l c -j $(JOBS)

adaTests:
	./scripts/runTests.py -l Ada -s -j $(JOBS)
	./scripts/runTests.py -l Ada -j $(JOBS)

scalaTests:
	./scripts/runTests.py -l Scala -s -j $(JOBS)
	./scripts/runTests.py -l Scala -j $(JOBS)

pythonTests:
	./scripts/runTests.py -l python -s -j $(JOBS)
	./scripts/runTests.py -l python -j $(JOBS)

rustTests:
	./scripts/runTests.py -l Rust -s -j $(JOBS)
	./scripts/runTests.py -l Rust -j $(JOBS)
