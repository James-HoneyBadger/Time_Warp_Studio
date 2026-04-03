# WASM Build Configuration for Time Warp Studio Language Interpreters
# Compiles all language interpreters to WebAssembly for browser execution

.PHONY: wasm-all wasm-clean wasm-test wasm-bench wasm-basic wasm-logo wasm-pilot wasm-pascal wasm-prolog wasm-forth wasm-c wasm-optimize

# Emscripten Compiler Configuration
EMCC ?= emcc
EMSDK_ENV ?= source $(EMSDK)/emsdk_env.sh && 

# Compiler Flags
# -O3: Aggressive optimization
# --no-entry: No main() function required
# -s WASM=1: Enable WebAssembly
# -s ALLOW_MEMORY_GROWTH=1: Allow memory expansion
# -s EXPORTED_FUNCTIONS: Export functions to JavaScript
# -s EXPORTED_RUNTIME_METHODS: Export runtime methods
WASM_FLAGS := -O3 \
	-s WASM=1 \
	--no-entry \
	-s ALLOW_MEMORY_GROWTH=1 \
	-s INITIAL_MEMORY=268435456 \
	-s MAXIMUM_MEMORY=536870912 \
	-s TOTAL_STACK=1048576 \
	-s STACK_SIZE=1048576

# Export all interpreter functions
EXPORT_FUNCS := _execute_code,_init_interpreter,_cleanup,_get_error,_get_output,_malloc,_free

# Output directories
WASM_OUTPUT_DIR := Platforms/backend/wasm
WASM_SRC_DIR := Platforms/wasm

# Interpreter source files
BASIC_SRC := $(WASM_SRC_DIR)/interpreters/basic.c
LOGO_SRC := $(WASM_SRC_DIR)/interpreters/logo.c
PILOT_SRC := $(WASM_SRC_DIR)/interpreters/pilot.c
PASCAL_SRC := $(WASM_SRC_DIR)/interpreters/pascal.c
PROLOG_SRC := $(WASM_SRC_DIR)/interpreters/prolog.c
FORTH_SRC := $(WASM_SRC_DIR)/interpreters/forth.c
C_SRC := $(WASM_SRC_DIR)/interpreters/c_lang.c

# Common source files
COMMON_SRCS := $(WASM_SRC_DIR)/common/memory.c \
               $(WASM_SRC_DIR)/common/error.c \
               $(WASM_SRC_DIR)/common/output.c \
               $(WASM_SRC_DIR)/common/graphics.c

# Include paths
INCLUDES := -I$(WASM_SRC_DIR)/include -I$(WASM_SRC_DIR)/common

# Build all WASM modules
wasm-all: wasm-basic wasm-logo wasm-pilot wasm-pascal wasm-prolog wasm-forth wasm-c wasm-optimize
	@echo "✅ All WASM modules compiled successfully"
	@ls -lh $(WASM_OUTPUT_DIR)/*.wasm

# BASIC Interpreter
wasm-basic: $(WASM_OUTPUT_DIR)/basic.wasm

$(WASM_OUTPUT_DIR)/basic.wasm: $(BASIC_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling BASIC interpreter to WASM..."
	$(EMCC) $(BASIC_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/basic.js
	@echo "✅ BASIC WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/basic.wasm

# Logo Interpreter (with graphics support)
wasm-logo: $(WASM_OUTPUT_DIR)/logo.wasm

$(WASM_OUTPUT_DIR)/logo.wasm: $(LOGO_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling Logo interpreter to WASM..."
	$(EMCC) $(LOGO_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/logo.js
	@echo "✅ Logo WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/logo.wasm

# PILOT Interpreter
wasm-pilot: $(WASM_OUTPUT_DIR)/pilot.wasm

$(WASM_OUTPUT_DIR)/pilot.wasm: $(PILOT_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling PILOT interpreter to WASM..."
	$(EMCC) $(PILOT_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/pilot.js
	@echo "✅ PILOT WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/pilot.wasm

# Pascal Interpreter
wasm-pascal: $(WASM_OUTPUT_DIR)/pascal.wasm

$(WASM_OUTPUT_DIR)/pascal.wasm: $(PASCAL_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling Pascal interpreter to WASM..."
	$(EMCC) $(PASCAL_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/pascal.js
	@echo "✅ Pascal WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/pascal.wasm

# Prolog Interpreter
wasm-prolog: $(WASM_OUTPUT_DIR)/prolog.wasm

$(WASM_OUTPUT_DIR)/prolog.wasm: $(PROLOG_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling Prolog interpreter to WASM..."
	$(EMCC) $(PROLOG_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/prolog.js
	@echo "✅ Prolog WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/prolog.wasm

# Forth Interpreter
wasm-forth: $(WASM_OUTPUT_DIR)/forth.wasm

$(WASM_OUTPUT_DIR)/forth.wasm: $(FORTH_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling Forth interpreter to WASM..."
	$(EMCC) $(FORTH_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/forth.js
	@echo "✅ Forth WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/forth.wasm

# C Interpreter
wasm-c: $(WASM_OUTPUT_DIR)/c_lang.wasm

$(WASM_OUTPUT_DIR)/c_lang.wasm: $(C_SRC) $(COMMON_SRCS)
	@mkdir -p $(WASM_OUTPUT_DIR)
	@echo "📦 Compiling C interpreter to WASM..."
	$(EMCC) $(C_SRC) $(COMMON_SRCS) \
		$(WASM_FLAGS) \
		-s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)] \
		-s EXPORTED_RUNTIME_METHODS=[ccall,cwrap] \
		$(INCLUDES) \
		-o $(WASM_OUTPUT_DIR)/c_lang.js
	@echo "✅ C WASM module created"
	@ls -lh $(WASM_OUTPUT_DIR)/c_lang.wasm

# Optimize WASM modules (reduce size)
wasm-optimize:
	@echo "📦 Optimizing WASM modules..."
	@for wasm in $(WASM_OUTPUT_DIR)/*.wasm; do \
		if [ -f "$$wasm" ]; then \
			echo "  Optimizing $$wasm..."; \
			wasm-opt -O4 -o "$$wasm" "$$wasm" || echo "  (wasm-opt not available)"; \
		fi; \
	done
	@echo "✅ WASM optimization complete"

# Run WASM tests
wasm-test:
	@echo "🧪 Running WASM tests..."
	npm run test -- --testPathPattern=wasm
	@echo "✅ WASM tests complete"

# Run WASM benchmarks
wasm-bench:
	@echo "⏱️  Running WASM benchmarks..."
	node scripts/wasm-benchmark.js
	@echo "✅ WASM benchmarks complete"

# Show WASM module sizes
wasm-sizes:
	@echo "📊 WASM Module Sizes:"
	@ls -lh $(WASM_OUTPUT_DIR)/*.wasm | awk '{print "  " $$9 " (" $$5 ")"}'
	@echo ""
	@echo "📊 Gzipped Sizes:"
	@for wasm in $(WASM_OUTPUT_DIR)/*.wasm; do \
		gzip -c "$$wasm" | wc -c | awk -v f="$$wasm" '{printf "  %s: %d bytes\n", f, $$1}'; \
	done

# Clean WASM build artifacts
wasm-clean:
	@echo "🗑️  Cleaning WASM build artifacts..."
	rm -rf $(WASM_OUTPUT_DIR)/*.wasm
	rm -rf $(WASM_OUTPUT_DIR)/*.js
	rm -rf $(WASM_OUTPUT_DIR)/*.map
	@echo "✅ Clean complete"

# Full rebuild
wasm-rebuild: wasm-clean wasm-all
	@echo "✅ Full rebuild complete"

# Development build (faster, less optimization)
wasm-dev:
	@echo "⚡ Building development WASM modules..."
	$(eval DEV_FLAGS := -O0 -g -s WASM=1 --no-entry -s ALLOW_MEMORY_GROWTH=1 -s EXPORTED_FUNCTIONS=[$(EXPORT_FUNCS)])
	$(EMCC) $(BASIC_SRC) $(COMMON_SRCS) $(DEV_FLAGS) $(INCLUDES) -o $(WASM_OUTPUT_DIR)/basic.js
	$(EMCC) $(LOGO_SRC) $(COMMON_SRCS) $(DEV_FLAGS) $(INCLUDES) -o $(WASM_OUTPUT_DIR)/logo.js
	@echo "✅ Development build complete"

# Production build (maximum optimization)
wasm-prod: wasm-all wasm-optimize
	@echo "✅ Production build complete"
	@echo "Sizes:"
	@make wasm-sizes

# Install Emscripten (if not already installed)
wasm-setup-emsdk:
	@echo "📦 Setting up Emscripten SDK..."
	git clone https://github.com/emscripten-core/emsdk.git emsdk
	cd emsdk && ./emsdk install latest && ./emsdk activate latest
	@echo "✅ Emscripten SDK setup complete"
	@echo "Run: source emsdk/emsdk_env.sh"

# Check Emscripten installation
wasm-check:
	@echo "🔍 Checking Emscripten installation..."
	@which emcc || echo "❌ emcc not found in PATH"
	@emcc --version || echo "❌ emcc not available"
	@echo "✅ Check complete"

# Help
wasm-help:
	@echo "WASM Build System - Time Warp Studio"
	@echo ""
	@echo "Targets:"
	@echo "  make wasm-all          - Build all WASM modules"
	@echo "  make wasm-basic        - Build BASIC interpreter"
	@echo "  make wasm-logo         - Build Logo interpreter"
	@echo "  make wasm-pilot        - Build PILOT interpreter"
	@echo "  make wasm-pascal       - Build Pascal interpreter"
	@echo "  make wasm-prolog       - Build Prolog interpreter"
	@echo "  make wasm-forth        - Build Forth interpreter"
	@echo "  make wasm-c            - Build C interpreter"
	@echo "  make wasm-clean        - Clean WASM artifacts"
	@echo "  make wasm-test         - Run WASM tests"
	@echo "  make wasm-bench        - Run WASM benchmarks"
	@echo "  make wasm-sizes        - Show module sizes"
	@echo "  make wasm-dev          - Development build"
	@echo "  make wasm-prod         - Production build"
	@echo "  make wasm-setup-emsdk  - Install Emscripten SDK"
	@echo "  make wasm-check        - Check Emscripten"
	@echo ""

# Default target
.DEFAULT_GOAL := wasm-help
