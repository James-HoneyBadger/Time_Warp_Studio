.PHONY: help install dev optional test smoke demos lint format typecheck run run-safe build clean version

PYTHON ?= python3
PIP    ?= pip
SRC    := Platforms/Python
TESTS  := $(SRC)/time_warp/tests

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
install: ## Install core dependencies
	$(PIP) install -r $(SRC)/requirements.txt

dev: install ## Install dev + test dependencies
	$(PIP) install -r $(SRC)/requirements-dev.txt

optional: ## Install optional extras (speech, gamepad, OS theme detection)
	$(PIP) install -r $(SRC)/requirements-optional.txt

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
run: ## Launch the IDE
	$(PYTHON) $(SRC)/time_warp_ide.py

run-safe: ## Launch via smart launcher (handles venv + deps)
	$(PYTHON) run.py

# ---------------------------------------------------------------------------
# Testing
# ---------------------------------------------------------------------------
test: ## Run the full pytest suite (offscreen Qt)
	QT_QPA_PLATFORM=offscreen $(PYTHON) -m pytest $(TESTS) -q --tb=short

smoke: ## Run quick smoke tests
	$(PYTHON) $(SRC)/smoke_test.py

demos: ## Verify all example programs
	$(PYTHON) test_all_examples.py

# ---------------------------------------------------------------------------
# Code Quality
# ---------------------------------------------------------------------------
lint: ## Run ruff linter
	$(PYTHON) -m ruff check $(SRC)

format: ## Format code with ruff
	$(PYTHON) -m ruff format $(SRC)

typecheck: ## Run pyright type checking
	$(PYTHON) -m pyright $(SRC)/time_warp

# ---------------------------------------------------------------------------
# Packaging
# ---------------------------------------------------------------------------
build: ## Build native binary via PyInstaller
	bash Scripts/build_native.sh

# ---------------------------------------------------------------------------
# Maintenance
# ---------------------------------------------------------------------------
clean: ## Remove caches, build artifacts, test reports
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name .pytest_cache -exec rm -rf {} + 2>/dev/null || true
	rm -rf build/ dist/ *.egg-info test_reports/ htmlcov/ .coverage
	rm -rf $(SRC)/build $(SRC)/dist $(SRC)/*.egg-info

version: ## Show project version
	@cat VERSION
