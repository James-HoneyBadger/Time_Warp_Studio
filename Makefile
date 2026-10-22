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

.PHONY: help $(FORWARDED) engine-test smoke command-matrix repl-matrix \
        golden-record golden-check teach-check test-all lint lint-md doc-links

help:
	@echo "Time Warp Studio"
	@echo ""
	@echo "The IDE is the Qt application in $(NATIVE)/. Targets that forward to it:"
	@echo "  build         Build the Qt IDE (Release)"
	@echo "  run           Build it and run it"
	@echo "  test          Run the 21 C++ suites"
	@echo "  test-debug    Run them in the Debug tree"
	@echo "  asan          Run them under AddressSanitizer (ASAN=undefined for UBSan)"
	@echo "  screenshot    Capture a window screenshot"
	@echo "  clean         Remove build output"
	@echo ""
	@echo "The reference engine in $(NATIVE)/bridge (development tooling):"
	@echo "  engine-test   The bridge engine's own pytest suite"
	@echo "  smoke         Every example program, through the real bridge"
	@echo "  golden-check  The ports against the frozen reference corpus (no Python)"
	@echo "  golden-record Freeze the reference's answers again (needs the bridge)"
	@echo "  teach-check   Every lesson solution passes and every starter fails"
	@echo "  test-all      All of the above"
	@echo "  lint          Ruff over the bridge"
	@echo "  lint-md       Markdownlint over every tracked Markdown file
	@echo "  doc-links     Every relative link and #anchor in the guides resolves""
	@echo ""
	@echo "See 'make -C $(NATIVE) help' for everything the native tree offers."

$(FORWARDED):
	@$(MAKE) --no-print-directory -C $(NATIVE) $@

# A missing pytest is a setup problem rather than a failing test, so it is
# reported as one: the alternative is a traceback that reads like the engine
# broke.
engine-test:
	@$(PYTHON) -c "import pytest" 2>/dev/null || { \
		echo "pytest is not installed for $(PYTHON)."; \
		echo "Create the virtual environment: python3 -m venv .venv && .venv/bin/pip install pytest ruff"; \
		exit 1; \
	}
	@PYTHONPATH=$(NATIVE)/bridge $(PYTHON) -m pytest -q $(NATIVE)/bridge/time_warp/tests

smoke:
	@$(PYTHON) $(NATIVE)/tools/example_smoke.py

command-matrix:
	@$(PYTHON) $(NATIVE)/tools/command_matrix.py

repl-matrix:
	@$(PYTHON) $(NATIVE)/tools/repl_matrix.py

# The parity gate's answer, frozen. `--record` asks the Python reference and is
# run only while the reference still exists; `--golden` asks the recorded corpus
# and needs neither Python nor `native/bridge/`, so the ports stay honest after
# the engine they were ported from is deleted.
golden-record:
	@$(PYTHON) $(NATIVE)/tools/lang_differential_check.py --record

golden-check:
	@$(PYTHON) $(NATIVE)/tools/lang_differential_check.py --golden

# The curriculum, checked by the same library the Lessons panel asks: every
# worked answer passes its checkpoint and every starter fails it. `teach-check`
# needs the build; `teach-check-py` is the reference's answer while it exists.
teach-check:
	@$(MAKE) --no-print-directory -C $(NATIVE) teach-check

test-all: test engine-test smoke command-matrix repl-matrix golden-check doc-links

lint:
	@$(PYTHON) -m ruff check $(NATIVE)/bridge

# The guides link to each other; a dead link is how a reader learns the map is wrong.
# Python only, because the documentation job must not need the C++ build.
doc-links:
	@$(PYTHON) $(NATIVE)/tools/check_doc_links.py

# Markdown lint over the repository's own Markdown. `git ls-files` is what keeps the
# virtual environment, the build trees and node_modules out of the run without a
# second ignore file, so this reports on the files a reviewer sees and nothing else.
lint-md:
	@npx --yes markdownlint-cli2 --config .markdownlint.json $$(git ls-files '*.md')
