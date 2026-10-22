# Time Warp Studio -- top-level convenience targets.
#
# The product is the Qt application in native/. The real build, test and run
# logic lives in native/Makefile; this file exists so that plain `make` at the
# repository root does the obvious thing, and so every check has one obvious home.

NATIVE := native
# The harness scripts are the only Python left, and they drive `time_warp_check`
# rather than an engine: they need an interpreter, not a virtual environment. The
# bridge's own pytest/ruff live in .venv while `native/bridge/` still exists.
PYTHON ?= $(shell test -x .venv/bin/python3 && echo .venv/bin/python3 || echo python3)
FORWARDED := build run test test-debug asan clean distclean info test-list \
             screenshot reset-layout

.PHONY: help $(FORWARDED) engine-test smoke command-matrix repl-matrix \
        golden-check teach-check test-all lint lint-md doc-links doc-claims

help:
	@echo "Time Warp Studio"
	@echo ""
	@echo "The IDE is the Qt application in $(NATIVE)/. Targets that forward to it:"
	@echo "  build         Build the Qt IDE (Release)"
	@echo "  run           Build it and run it"
	@echo "  test          Run the C++ suites"
	@echo "  test-debug    Run them in the Debug tree"
	@echo "  asan          Run them under AddressSanitizer (ASAN=undefined for UBSan)"
	@echo "  screenshot    Capture a window screenshot"
	@echo "  clean         Remove build output"
	@echo ""
	@echo "The checks (development tooling):"
	@echo "  smoke         Every example program, through the native engines"
	@echo "  golden-check  The ports against the frozen reference corpus (no Python)"
	@echo "  teach-check   Every lesson solution passes and every starter fails"
	@echo "  doc-links     Every relative link and #anchor in the guides resolves"
	@echo "  test-all      All of the above"
	@echo "  lint          Ruff over the bridge"
	@echo "  lint-md       Markdownlint over every tracked Markdown file"
	@echo "  doc-claims    Every path, name and make target the guides name exists"
	@echo ""
	@echo "See 'make -C $(NATIVE) help' for everything the native tree offers."

$(FORWARDED):
	@$(MAKE) --no-print-directory -C $(NATIVE) $@

# The three harnesses drive what the product drives: `smoke` and `command-matrix`
# spawn `time_warp_check` (the native engines) and `repl-matrix` speaks the API.
smoke: check-built
	@$(PYTHON) $(NATIVE)/tools/example_smoke.py

command-matrix: check-built
	@$(PYTHON) $(NATIVE)/tools/command_matrix.py

repl-matrix: check-built
	@$(PYTHON) $(NATIVE)/tools/repl_matrix.py

# A harness whose runner is missing reports it as a failing program, so the build
# is named here instead: `make smoke` on a fresh checkout should build the tool it
# needs rather than explain that it cannot find it.
check-built:
	@$(MAKE) --no-print-directory -C $(NATIVE) check

# The parity gate. `native/tests/golden/` holds the reference engine's answers --
# 470 language programs and 192 Scheme programs; this compares the ports with those
# and imports no Python to do it. There is no `--record` any more: the engine those
# answers came from is deleted, so a deliberate behaviour change edits the corpus
# entry in the same commit that changes the behaviour.
golden-check:
	@$(PYTHON) $(NATIVE)/tools/golden_check.py --all

# The curriculum, checked by the same library the Lessons panel asks: every
# worked answer passes its checkpoint and every starter fails it. Needs the build,
# because the tool that does it is `time_warp_teach`.
teach-check:
	@$(MAKE) --no-print-directory -C $(NATIVE) teach-check

test-all: test smoke command-matrix repl-matrix teach-check golden-check doc-links \
          doc-claims

# The harness scripts are the only Python left, and they are code: they parse flags,
# walk directories and assert on output. Ruff's configuration moved with them.
lint:
	@$(PYTHON) -m ruff check $(NATIVE)/tools

# The guides link to each other; a dead link is how a reader learns the map is wrong.
# Python only, because the documentation job must not need the C++ build.
doc-links:
	@$(PYTHON) $(NATIVE)/tools/check_doc_links.py

# A link that resolves can still be a false claim: a sentence can name a file that
# moved, a symbol that was deleted, or a `make` target that never existed. Nothing
# else checks that, and three such claims survived a release.
doc-claims:
	@$(PYTHON) $(NATIVE)/tools/check_doc_claims.py --self-test
	@$(PYTHON) $(NATIVE)/tools/check_doc_claims.py

# Markdown lint over the repository's own Markdown. `git ls-files` is what keeps the
# virtual environment, the build trees and node_modules out of the run without a
# second ignore file, so this reports on the files a reviewer sees and nothing else.
lint-md:
	@npx --yes markdownlint-cli2 --config .markdownlint.json $$(git ls-files '*.md')
