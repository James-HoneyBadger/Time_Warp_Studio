# BASIC Examples

This folder contains short BASIC examples and a large showcase program demonstrating many language features.

Files:
- `hello_world.bas` — simple PRINT demo
- `input.bas` — demonstrates INPUT and variable handling
- `loops.bas` — FOR/STEP examples
- `math.bas` — arithmetic and expressions
- `subroutines.bas` — GOSUB/RETURN example
- `guessing_game.bas` — interactive guessing game (random numbers)
- `showcase.bas` — large, interactive showcase demonstrating menus, games, ascii canvas, and more

Run quick examples with the Python runner:

```bash
# run hello world
Platforms/Python/.venv/bin/python Platforms/Python/scripts/ci_run_examples.py Examples/basic/hello_world.bas

# run full showcase (interactive) with fixture to automate inputs
Platforms/Python/.venv/bin/python Platforms/Python/scripts/ci_run_examples.py Examples/basic/showcase.bas
```
