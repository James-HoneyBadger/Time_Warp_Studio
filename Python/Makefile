# Simple helpers for TempleCode/Time Warp

.PHONY: run run-example test clean

# Launch GUI IDE
run:
	python -m time_warp.app

# Run a TempleCode script (.tc)
# Usage: make run-example FILE=examples/hello.tc
run-example:
ifndef FILE
	$(error Please provide FILE=path/to/program.tc)
endif
	python scripts/run_templecode.py $(FILE)

# Run Python tests (requires pytest)
# Optionally: pip install -r requirements-dev.txt
test:
	pytest -q

# Clean caches/artifacts
clean:
	rm -rf .mypy_cache */__pycache__ __pycache__ .pytest_cache
