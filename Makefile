# Time Warp Studio -- top-level convenience targets.
#
# The product is the Qt application in native/. The real build, test and run
# logic lives in native/Makefile; this file exists so that plain `make` at the
# repository root does the obvious thing, and so the bridge's own tests and
# checks have one obvious home.

NATIVE := native
# The bridge's tooling (pytest, ruff) lives in the repository virtual
# environment; fall back to whatever `python3` is on PATH.
PYTHON ?= $(shell test -x .venv/bin/python3 && echo .venv/bin/python3 || echo python3)
FORWARDED := build run test test-debug asan clean distclean info test-list \
             screenshot reset-layout

.PHONY: help $(FORWARDED) engine-test smoke test-all lint lint-md

help:
	@echo "Time Warp Studio"
	@echo ""
	@echo "The IDE is the Qt application in $(NATIVE)/. Targets that forward to it:"
	@echo "  build         Build the Qt IDE (Release)"
	@echo "  run           Build it and run it"
	@echo "  test          Run the 17 C++ suites"
	@echo "  test-debug    Run them in the Debug tree"
	@echo "  asan          Run them under AddressSanitizer (ASAN=undefined for UBSan)"
	@echo "  screenshot    Capture a window screenshot"
	@echo "  clean         Remove build output"
	@echo ""
	@echo "The temporary language bridge in $(NATIVE)/bridge:"
	@echo "  engine-test   The bridge engine's own pytest suite"
	@echo "  smoke         Every example program, through the real bridge"
	@echo "  test-all      All of the above"
	@echo "  lint          Ruff over the bridge"
	@echo "  lint-md       Markdownlint over every tracked Markdown file"
	@echo ""
	@echo "See 'make -C $(NATIVE) help' for everything the native tree offers."

$(FORWARDED):
	@$(MAKE) --no-print-directory -C $(NATIVE) $@

engine-test:
	@PYTHONPATH=$(NATIVE)/bridge $(PYTHON) -m pytest -q $(NATIVE)/bridge/time_warp/tests

smoke:
	@$(PYTHON) $(NATIVE)/tools/example_smoke.py

test-all: test engine-test smoke

lint:
	@$(PYTHON) -m ruff check $(NATIVE)/bridge

# Markdown lint over the repository's own Markdown. `git ls-files` is what keeps the
# virtual environment, the build trees and node_modules out of the run without a
# second ignore file, so this reports on the files a reviewer sees and nothing else.
lint-md:
	@npx --yes markdownlint-cli2 --config .markdownlint.json $$(git ls-files '*.md')
