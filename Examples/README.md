# Examples

This folder contains short demo programs and full showcase examples for all languages supported by Time Warp IDE.

How to run headlessly (CI-friendly):

1. Use the Python headless runner in `Platforms/Python/scripts/ci_run_examples.py`.

```bash
# run the default CI example set
Platforms/Python/.venv/bin/python Platforms/Python/scripts/ci_run_examples.py

# run a single example with fixtures
Platforms/Python/.venv/bin/python Platforms/Python/scripts/ci_run_examples.py Examples/basic/showcase.bas
```

Fixtures: if a fixture file exists at `Examples/fixtures/<relative>/name.in` it will be used
to provide input lines during execution. Example fixtures are provided for interactive demos:

- `Examples/fixtures/basic/showcase.in`
- `Examples/fixtures/pilot/showcase.in`
- `Examples/fixtures/pascal/showcase.in`
- `Examples/fixtures/c/showcase.in`

Non-interactive examples (run to completion):
- `Examples/basic/hello_world.bas`
- `Examples/logo/showcase.logo`
- `Examples/prolog/showcase.pl`

Interactive examples with fixtures: run via the CI runner as described above.
