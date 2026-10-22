# Time Warp Studio -- top-level convenience targets.
#
# The product is the Qt application in native/. The real build, test and run
# logic lives in native/Makefile; this file exists so that plain `make` at the
# repository root does the obvious thing, and so every check has one obvious home.

NATIVE := native
# The checks are C++ and built from this repository: `time_warp_gates` asks the
# questions about programs, and `time_warp_docs` asks them about the guides. Nothing
# here needs an interpreter, a virtual environment or a package manager.
GATES := $(NATIVE)/build/time_warp_gates
DOCS_TOOL := native/tools/docs/build/time_warp_docs
FORWARDED := build run test test-debug asan clean distclean info test-list \
             screenshot reset-layout

.PHONY: help $(FORWARDED) smoke command-matrix repl-matrix gates-built \
        golden-check teach-check test-all lint lint-md docs-tool doc-links doc-claims \
        tree-check

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
	@echo "  command-matrix Every command row, one program each"
	@echo "  repl-matrix   Every command row, through the native REPL"
	@echo "  golden-check  The ports against the frozen reference corpus"
	@echo "  teach-check   Every lesson solution passes and every starter fails"
	@echo "  doc-links     Every relative link and #anchor in the guides resolves"
	@echo "  test-all      The suites, the program gates and the documentation gates"
	@echo "  lint          clang-tidy-shaped checks: the build is warning-free"
	@echo "  lint-md       Every tracked Markdown file against the house rules"
	@echo "  doc-claims    Every path, name and make target the guides name exists"
	@echo "  tree-check    The tree holds no Python"
	@echo ""
	@echo "See 'make -C $(NATIVE) help' for everything the native tree offers."

$(FORWARDED):
	@$(MAKE) --no-print-directory -C $(NATIVE) $@

# The three harnesses drive what the product drives: `smoke` and `command-matrix`
# spawn `time_warp_check` (the native engines) and `repl-matrix` speaks the API.
smoke: gates-built
	@$(GATES) smoke

command-matrix: gates-built
	@$(GATES) commands

repl-matrix: gates-built
	@$(GATES) repl

# The gates drive `time_warp_check`, `time_warp_api`, `lang_cli` and `scheme_cli`,
# so they are built with them. Naming that here means `make smoke` on a fresh
# checkout builds what it needs rather than explaining that it cannot find it.
gates-built:
	@$(MAKE) --no-print-directory -C $(NATIVE) gates

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
golden-check: gates-built
	@$(GATES) golden --all

# The curriculum, checked by the same library the Lessons panel asks: every
# worked answer passes its checkpoint and every starter fails it. Needs the build,
# because the tool that does it is `time_warp_teach`.
teach-check:
	@$(MAKE) --no-print-directory -C $(NATIVE) teach-check

# Every gate in one target: the suites, the four program gates, the curriculum,
# and the documentation checks -- including the Markdown lint, which is a gate in
# CI and was the one check a local `test-all` did not run.
test-all: test smoke command-matrix repl-matrix teach-check golden-check doc-links \
          doc-claims lint-md tree-check

# Warnings are errors in every target, so a lint pass is the build passing. Kept as
# a target because `make lint` is what a contributor types and because the guides
# name it.
lint: build
	@echo "lint: the build compiles with -Werror; see native/CMakeLists.txt"

# The guides link to each other; a dead link is how a reader learns the map is wrong.
# `time_warp_docs` is its own small CMake project with no Qt and no dependency beyond
# a C++ compiler, which is what lets the documentation job answer in seconds.
docs-tool:
	@cmake -S native/tools/docs -B native/tools/docs/build >/dev/null \
		&& cmake --build native/tools/docs/build -j >/dev/null
	@echo "  built      native/tools/docs/build/time_warp_docs"

doc-links: docs-tool
	@$(DOCS_TOOL) links

# A link that resolves can still be a false claim: a sentence can name a file that
# moved, a symbol that was deleted, or a `make` target that never existed. Nothing
# else checks that, and three such claims survived a release.
doc-claims: docs-tool
	@$(DOCS_TOOL) claims --self-test
	@$(DOCS_TOOL) claims

# The tree holds no Python: the product is built, tested and checked by C++ tools
# that live here, and this is what keeps a script from creeping back.
tree-check: gates-built
	@$(GATES) tree

# Markdown lint over the repository's own Markdown: `time_warp_docs md`, which asks
# `git ls-files` for the file list so the build trees and vendored directories stay
# out of the run without a second ignore file. It replaced `markdownlint-cli2`,
# which needed Node and fetched a package on every run.
lint-md: docs-tool
	@$(DOCS_TOOL) md
