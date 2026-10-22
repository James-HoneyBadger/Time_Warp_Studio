# Time Warp Studio -- native C++/Qt6 IDE
#
#     make                 release build
#     make debug           debug build, in its own tree so you can keep both
#     make test            build, then run all 20 test suites
#     make run             build, then launch the IDE
#     make screenshot      render the window offscreen to a PNG (no display needed)
#     make install         install to PREFIX (default /usr/local)
#     make help            list every target
#
# ---------------------------------------------------------------------------
# Why this drives CMake instead of invoking the compiler itself
# ---------------------------------------------------------------------------
#
# Compiling these sources directly is not a matter of listing them and calling
# $(CXX). The build needs:
#
#   * moc for every Q_OBJECT header -- and the moc output compiled exactly once.
#     A header that is moc'd *and* compiled directly gives duplicate symbols; one
#     that is not moc'd fails at link time with a vtable error. Getting this right
#     by hand means a rule per header plus a dependency scan for `#include`
#     changes, which is a small build system in its own right.
#   * Qt6 include paths and -fPIC for Core, Gui and Widgets, discovered with
#     find_package rather than hard-coded, so the tree builds on a machine where
#     Qt lives somewhere else.
#   * eight libraries in dependency order (expr, scheme, lang, canvas, canvas_qt,
#     debugger, bridge, ui), four of them optional depending on which Qt6
#     components exist.
#   * a compile definition generated from ../VERSION, and the re-configure that
#     has to happen when that file changes.
#   * seventeen test binaries registered with CTest, each with its own include
#     path.
#
# All of that lives in CMakeLists.txt and is what CI exercises. Duplicating it
# here would create a second build system to keep in sync, and it would be the
# copy that rots. So this file owns the *interface* -- targets, defaults, jobs,
# out-of-tree layout, exit codes -- and CMake owns the compilation.
#
# The practical effect: `make` and `make test` do the same thing as the two
# `cmake` commands in native/README.md, and there is only one place where the
# flags are defined.
# ---------------------------------------------------------------------------

.DEFAULT_GOAL := all

# Suppresses "Entering directory"/"Leaving directory" for this file and for the
# recursive calls a few targets make, so the output is only what is useful.
MAKEFLAGS += --no-print-directory

# -- Tools and layout -------------------------------------------------------
# Every one of these is overridable, e.g.
#     make BUILD_TYPE=Debug JOBS=4
#     make BUILD_DIR=$(pwd)/../tw-cxx-build
CMAKE       ?= cmake
CTEST       ?= ctest
BUILD_DIR   ?= build
DEBUG_DIR   ?= build-debug
SAN_DIR     ?= build-sanitised
BUILD_TYPE  ?= Release
JOBS        ?= $(shell nproc 2>/dev/null || echo 4)
# Where `make install` puts the application. `make install DESTDIR=stage`
# installs under stage/ instead, which is what a distro package wants.
PREFIX      ?= /usr/local

# The IDE. `make run` launches this, `make screenshot` renders it. On macOS the
# build produces a bundle, so the executable is the one inside it. It is quoted
# everywhere it is used, because the bundle's name has spaces in it.
ifeq ($(shell uname -s),Darwin)
APP         ?= $(BUILD_DIR)/Time Warp Studio.app/Contents/MacOS/Time Warp Studio
else
APP         ?= $(BUILD_DIR)/time_warp_ide
endif

SCREENSHOT      ?= $(BUILD_DIR)/layout.png
SCREENSHOT_SIZE ?= 1400x900

# The test and UI suites need no display, but the offscreen platform has to be
# named explicitly or a future widget test will try to open one.
QT_QPA_PLATFORM ?= offscreen

# CMake options. WERROR and TESTS default to ON, matching the CMakeLists
# defaults, so a plain `make` means "what CI means".
WERROR      ?= ON
TESTS       ?= ON
CMAKE_FLAGS ?=
CTEST_ARGS  ?=

# Passed through: `make VERBOSE=1` shows the compiler command lines.
VERBOSE     ?=

# -- Configure --------------------------------------------------------------

CONFIGURE_FLAGS = \
	-DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
	-DTIME_WARP_EXPR_WERROR=$(WERROR) \
	-DTIME_WARP_EXPR_BUILD_TESTS=$(TESTS) \
	$(CMAKE_FLAGS)

# Re-run on every invocation rather than caching on presence of CMakeCache.txt:
# CMake's configure step is a fraction of a second when nothing changed, and
# making it unconditional is what lets `make WERROR=OFF` actually take effect
# instead of silently reusing the previous flags.
#
# Output is captured so a healthy configure stays quiet; anything that says
# "not found" is printed, because a silently skipped Qt6 component is exactly
# what you want to hear about. A real failure prints everything and stops.
configure:
	@printf '  configure  %s  [%s]\n' '$(BUILD_DIR)' '$(BUILD_TYPE)'
	@o=$$($(CMAKE) -S . -B $(BUILD_DIR) $(CONFIGURE_FLAGS) 2>&1) || { \
		printf '%s\n' "$$o" | sed 's/^/    /' >&2; \
		if printf '%s\n' "$$o" | grep -q "IMPORTED_LOCATION or IMPORTED_IMPLIB not set"; then \
			printf '\n    The Qt6 CMake package has no location for the %s configuration.\n' '$(BUILD_TYPE)' >&2; \
			printf '    That is a stale or half-installed Qt package, or a build tree configured\n' >&2; \
			printf '    while it changed. Re-run once the package installation has finished, or\n' >&2; \
			printf '    delete %s and configure again.\n' '$(BUILD_DIR)' >&2; \
		fi; \
		exit 1; }; \
	 printf '%s\n' "$$o" | grep -E "not found" | sed 's/^-- /    /' || true

# -- Build ------------------------------------------------------------------

all: build

build: configure ## Build the project (the default target)
	@printf '  build      %s  [-j%s]\n' '$(BUILD_DIR)' '$(JOBS)'
	@$(CMAKE) --build $(BUILD_DIR) --parallel $(JOBS) --config $(BUILD_TYPE) \
		$(if $(VERBOSE),--verbose)

# Separate tree, not the same one with different flags: switching
# CMAKE_BUILD_TYPE in place recompiles everything anyway, so keeping two
# directories costs nothing and lets a release build and a -g build coexist.
#
# BUILD_TYPE has to travel with BUILD_DIR. `make test BUILD_DIR=build-debug` on
# its own would reconfigure that tree as Release -- the `configure` step takes
# whatever BUILD_TYPE says, not what the tree was built with -- which is why
# `test-debug` below exists rather than leaving it to be remembered.
debug: ## Build unoptimised with assertions, in a separate tree
	@$(MAKE) --no-print-directory BUILD_DIR=$(DEBUG_DIR) BUILD_TYPE=Debug build

rebuild: ## Delete the build tree and build it again from scratch
	@$(MAKE) --no-print-directory clean
	@$(MAKE) --no-print-directory build

strict: ## Build with warnings as errors and every feature on
	@$(MAKE) --no-print-directory WERROR=ON TESTS=ON build

lenient: ## Build without -Werror, for working through a warning backlog
	@$(MAKE) --no-print-directory WERROR=OFF build

# -- Test -------------------------------------------------------------------

# `make test CTEST_ARGS="-R ui_layout -V"` runs one suite verbosely.
test: ctest-run ## Build, then run every test suite
# `ctest -N` rather than parsing JSON: it is present in every CMake this file
# supports, and the guard means an unconfigured tree says so in words instead of
# printing an empty list.
test-list: ## List the registered test suites
	@test -f $(BUILD_DIR)/CTestTestfile.cmake \
		|| { echo "  not configured yet -- run 'make' first"; exit 0; }
	@$(CTEST) --test-dir $(BUILD_DIR) -N \
		| sed -n 's/^ *Test *#[0-9]*: */  /p'

# Worth running as well as `test`, not instead of it. A debug build has no
# -DNDEBUG, so Qt's Q_ASSERT and the standard library's hardening checks are
# live -- they catch out-of-bounds indices and invalid assumptions that the
# release build silences.
test-debug: ## Build and test unoptimised, with assertions active
	@$(MAKE) --no-print-directory BUILD_DIR=$(DEBUG_DIR) BUILD_TYPE=Debug test

# A third tree, sanitised. ASan catches what an assertion cannot: reads and
# writes to memory that is no longer valid, including a queued Qt signal
# delivered after the frame that owned the receiver has returned. Returns
# non-zero on the first finding, so it drops straight into CI.
#
#     make asan
#     make asan ASAN=thread          # ThreadSanitizer, for the racing paths
#     make asan ASAN=undefined       # uninitialised reads and integer overflow
ASAN       ?= address
ASAN_FLAGS ?= -fsanitize=$(ASAN) -fno-omit-frame-pointer -g
asan: ## Build and test under AddressSanitizer (ASAN=thread/undefined to switch)
	@$(MAKE) --no-print-directory BUILD_DIR=$(SAN_DIR) BUILD_TYPE=Debug \
		CMAKE_FLAGS="-DCMAKE_CXX_FLAGS='$(ASAN_FLAGS)' -DCMAKE_EXE_LINKER_FLAGS='-fsanitize=$(ASAN)'" \
		ctest-run

# The shared body: build whatever is configured, then run CTest. Both `test` and
# `asan` go through it, so the command exists in exactly one place.
ctest-run: build
	@echo "  ctest      $(BUILD_DIR)"
	@QT_QPA_PLATFORM=$(QT_QPA_PLATFORM) $(CTEST) --test-dir $(BUILD_DIR) \
		--output-on-failure $(CTEST_ARGS)

# The run report the harnesses and a person both use: one program through the
# native engines, with what it printed and what it drew. Built on demand so
# `make smoke` at the top level has something to spawn.
check: build ## Build the run-report tool the harnesses drive
	@test -x "$(BUILD_DIR)/time_warp_check" \
		|| { echo "error: time_warp_check was not built (Qt Core is required)." >&2; exit 1; }

# The curriculum through the same library the Lessons panel asks: every worked
# answer passes its checkpoint and every starter fails it. Cheap enough to run on
# every change, and the only thing that keeps lesson data honest -- nothing else
# executes a `.json` file.
teach-check: build ## Check that every lesson solution passes and starter fails
	@test -x "$(BUILD_DIR)/time_warp_teach" \
		|| { echo "error: time_warp_teach was not built (Qt Core is required)." >&2; exit 1; }
	@"$(BUILD_DIR)/time_warp_teach" validate

# -- Run --------------------------------------------------------------------

# Guarded rather than unconditional: without Qt6 Widgets the configure step
# succeeds and simply does not create this target, so the failure would otherwise
# be a bare "No such file or directory" with nothing pointing at the cause.
run: build ## Build, then launch the app
	@test -x "$(APP)" || { \
		echo "error: $(APP) was not built." >&2; \
		echo "       Qt6 Widgets was probably not found. Check with: make info" >&2; \
		exit 1; \
	}
	@"$(APP)"

screenshot: build ## Render the window to a PNG offscreen and exit
	@test -x "$(APP)" || { echo "error: $(APP) was not built." >&2; exit 1; }
	@QT_QPA_PLATFORM=offscreen "$(APP)" --screenshot $(SCREENSHOT) \
		--size $(SCREENSHOT_SIZE)

reset-layout: build ## Clear the stored window layout, then run
	@test -x "$(APP)" || { echo "error: $(APP) was not built." >&2; exit 1; }
	@"$(APP)" --reset

# -- Diagnostics ------------------------------------------------------------

info: ## Show tool versions and which Qt6 components CMake found
	@echo "cmake    : $$($(CMAKE) --version 2>/dev/null | head -n 1 || echo missing)"
	@echo "ctest    : $$($(CTEST) --version 2>/dev/null | head -n 1 || echo missing)"
	@echo "compiler : $$($${CXX:-c++} --version 2>/dev/null | head -n 1 || echo missing)"
	@echo "jobs     : $(JOBS)"
	@echo "build    : $(BUILD_DIR) [$(BUILD_TYPE)]  WERROR=$(WERROR) TESTS=$(TESTS)"
	@echo
	@echo "Qt6 components:"
	@$(CMAKE) -S . -B $(BUILD_DIR) $(CONFIGURE_FLAGS) 2>&1 \
		| grep -E "Qt6|not found" | sed 's/^-- //' | sed 's/^/  /' || true

# -- Clean ------------------------------------------------------------------

clean: ## Remove the release build tree
	@echo "  clean      $(BUILD_DIR)"
	@rm -rf $(BUILD_DIR)

distclean: ## Remove every build tree this Makefile creates
	@echo "  clean      $(BUILD_DIR) $(DEBUG_DIR) $(SAN_DIR)"
	@rm -rf $(BUILD_DIR) $(DEBUG_DIR) $(SAN_DIR) $(SCREENSHOT)

# -- Help -------------------------------------------------------------------

help: ## Show this help
# One shell block with a single `|| true`, so `make help | head` cannot report a
# broken pipe. Each recipe line is its own shell, so a trailing `|| true` on one
# line does nothing for the lines after it -- and make prints `*** Broken pipe`
# for those, which reads like a failure when it is only a closed pipe.
	@{ grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'; \
	   echo; \
	   echo "  Variables (all overridable): BUILD_TYPE=$(BUILD_TYPE) BUILD_DIR=$(BUILD_DIR)"; \
	   echo "                              JOBS=$(JOBS) WERROR=$(WERROR) TESTS=$(TESTS)"; \
	 } || true

install: build ## Install to PREFIX (DESTDIR=stage to stage a package)
	@DESTDIR=$(DESTDIR) $(CMAKE) --install $(BUILD_DIR) --prefix $(PREFIX)
	@echo "Installed to $(DESTDIR)$(PREFIX): time_warp_ide is in $(PREFIX)/bin,"
	@echo "or in 'Time Warp Studio.app' at the top of that prefix on macOS."

# CMake has no uninstall target by design, so this uses the manifest the last
# install wrote. It removes the files and leaves directories in place, which is
# the convention: a package manager owns the directories.
uninstall: ## Remove what install put in PREFIX
	@if [ ! -f $(BUILD_DIR)/install_manifest.txt ]; then \
		echo "No $(BUILD_DIR)/install_manifest.txt: run 'make install' first" >&2; \
		exit 1; \
	fi
	@xargs -r rm -f < $(BUILD_DIR)/install_manifest.txt
	@echo "Removed the files listed in $(BUILD_DIR)/install_manifest.txt"

.PHONY: all build configure debug rebuild strict lenient test test-list \
        test-debug asan ctest-run check teach-check run screenshot reset-layout \
        info clean distclean install uninstall help
