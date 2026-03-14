.PHONY: help install dev test test-basic test-comprehensive lint format run clean docker demos

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
test: ## Run quick smoke tests
	$(PYTHON) test_runner.py --basic

test-comprehensive: ## Run full test suite with coverage
	$(PYTHON) test_runner.py --comprehensive

test-unit: ## Run pytest directly
	$(PYTHON) -m pytest $(TESTS) -v --tb=short

demos: ## Verify all demo programs
	$(PYTHON) test_all_demos.py

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
# Docker
# ---------------------------------------------------------------------------
docker: ## Build Docker image
	docker build -t time-warp-studio .

docker-up: ## Start Docker Compose stack
	docker compose up -d

docker-down: ## Stop Docker Compose stack
	docker compose down

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
