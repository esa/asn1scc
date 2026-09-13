# Optional convenience targets executed INSIDE the image (host needs only Docker).
PYTHON ?= python3
COLLECTOR ?= /opt/coverage/coverageBaseline.py
OUTDIR ?= /results
COHORT ?= pilot
LANGUAGE ?= c
EXTRA ?=

.PHONY: branch-coverage inventory historical test
branch-coverage:
	$(PYTHON) $(COLLECTOR) --outdir $(OUTDIR) --cohort $(COHORT) --language $(LANGUAGE) $(EXTRA)

inventory:
	$(PYTHON) $(COLLECTOR) --outdir $(OUTDIR) --inventory-only

historical:
	$(PYTHON) $(COLLECTOR) --outdir $(OUTDIR) --cohort historical --compare-baseline

test:
	$(PYTHON) /opt/coverage/testCollector.py
