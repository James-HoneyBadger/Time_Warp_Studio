#!/usr/bin/env python3
"""
Deep-dive verification of ALL demo programs in Examples/.

For each demo file:
  1. Detect language from file extension
  2. Load program via TimeWarpInterpreter.load_program()
  3. Execute via interpreter.execute(turtle)
  4. Collect output and check for errors (❌ lines)
  5. Report results

Usage:
    python test_all_demos.py              # run all
    python test_all_demos.py logo         # run only logo demos
    python test_all_demos.py --verbose    # show full output
"""

import json
import os
import subprocess
import sys
import time

ROOT = os.path.dirname(os.path.abspath(__file__))
EXAMPLES_DIR = os.path.join(ROOT, "Examples")

# Files that require interactive input — skip or note
INTERACTIVE_FILES = {
    "adventure.bas",      # INPUT prompts
    "budget_tracker.bas", # INPUT prompts
    "hangman.pas",        # readln
    "history_quiz.pilot", # ACCEPT
    "task_manager.py",    # input()
    "quiz_game.scm",      # interactive
    "contact_stack.ht",   # HyperTalk interactive
    "rpg_engine.lua",     # interactive game
    "todo_app.js",        # interactive
}


def collect_demo_files():
    """Gather all demo files grouped by language directory."""
    demos = []
    for lang_dir in sorted(os.listdir(EXAMPLES_DIR)):
        lang_path = os.path.join(EXAMPLES_DIR, lang_dir)
        if not os.path.isdir(lang_path):
            continue
        if lang_dir in ("demo", "fixtures"):
            continue
        for fname in sorted(os.listdir(lang_path)):
            fpath = os.path.join(lang_path, fname)
            if os.path.isfile(fpath):
                demos.append((lang_dir, fname, fpath))
    return demos


# Subprocess worker script executed per demo
_WORKER_SCRIPT = r'''
import json, os, sys, traceback
sys.path.insert(0, os.path.join(os.environ["TWS_ROOT"], "Platforms", "Python"))
from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState
fpath, ext = sys.argv[1], sys.argv[2]
try:
    with open(fpath, "r", encoding="utf-8", errors="replace") as f:
        source = f.read()
    language = Language.from_extension(ext)
    interp = Interpreter()
    turtle = TurtleState()
    interp.load_program(source, language=language)
    output = interp.execute(turtle)
    error_lines = [l for l in output if "\u274c" in l]
    info_lines = [l for l in output if l.strip()]
    if error_lines:
        status = "ERRORS"
    elif not info_lines and not output:
        status = "EMPTY"
    else:
        status = "OK"
    print(json.dumps({"status": status, "output": output, "errors": error_lines}))
except Exception:
    print(json.dumps({"status": "EXEC_ERROR", "output": [], "errors": [], "exc": traceback.format_exc()}))
'''


def run_demo(lang_dir, fname, fpath, verbose=False):
    """Run a single demo in a subprocess with hard 15s timeout."""
    _, ext = os.path.splitext(fname)
    env = os.environ.copy()
    env["TWS_ROOT"] = ROOT
    try:
        proc = subprocess.run(
            [sys.executable, "-c", _WORKER_SCRIPT, fpath, ext],
            capture_output=True, text=True, timeout=15, env=env,
        )
        stdout = proc.stdout.strip()
        if not stdout:
            return "EXEC_ERROR", [], [], proc.stderr.strip() or "No output from worker"
        data = json.loads(stdout)
        return data["status"], data.get("output", []), data.get("errors", []), data.get("exc")
    except subprocess.TimeoutExpired:
        return "TIMEOUT", [], [], "Execution timed out (15s)"
    except Exception as e:
        return "EXEC_ERROR", [], [], str(e)


def main():
    verbose = "--verbose" in sys.argv or "-v" in sys.argv
    filter_lang = None
    for arg in sys.argv[1:]:
        if not arg.startswith("-"):
            filter_lang = arg.lower()

    demos = collect_demo_files()
    if filter_lang:
        demos = [(d, f, p) for d, f, p in demos if d.lower() == filter_lang]

    results = {"OK": [], "ERRORS": [], "LOAD_ERROR": [], "EXEC_ERROR": [],
               "EMPTY": [], "TIMEOUT": [], "INTERACTIVE": []}

    print(f"\n{'='*70}")
    print(f"  Time Warp Studio — Demo Program Verification")
    print(f"  {len(demos)} programs to test")
    print(f"{'='*70}\n")

    for lang_dir, fname, fpath in demos:
        label = f"{lang_dir}/{fname}"

        if fname in INTERACTIVE_FILES:
            results["INTERACTIVE"].append(label)
            print(f"  ⏭  {label:45s} INTERACTIVE (skipped)")
            continue

        t0 = time.time()
        status, output, errors, exc = run_demo(lang_dir, fname, fpath, verbose)
        elapsed = time.time() - t0

        results[status].append(label)

        if status == "OK":
            out_count = len([l for l in output if l.strip()])
            print(f"  ✅ {label:45s} OK ({out_count} lines, {elapsed:.2f}s)")
        elif status == "EMPTY":
            print(f"  ⚠️  {label:45s} EMPTY OUTPUT ({elapsed:.2f}s)")
        elif status == "ERRORS":
            n_err = len(errors)
            print(f"  ❌ {label:45s} {n_err} ERRORS ({elapsed:.2f}s)")
            if verbose:
                for e in errors[:10]:
                    print(f"       {e}")
            else:
                for e in errors[:3]:
                    print(f"       {e}")
                if n_err > 3:
                    print(f"       ... and {n_err - 3} more errors")
        elif status == "LOAD_ERROR":
            print(f"  💥 {label:45s} LOAD ERROR")
            if exc:
                for line in exc.strip().split("\n")[-3:]:
                    print(f"       {line}")
        elif status == "EXEC_ERROR":
            print(f"  💥 {label:45s} EXEC ERROR ({elapsed:.2f}s)")
            if exc:
                for line in exc.strip().split("\n")[-3:]:
                    print(f"       {line}")
        elif status == "TIMEOUT":
            print(f"  ⏱  {label:45s} TIMEOUT ({elapsed:.2f}s)")
        elif status == "SKIP":
            print(f"  ⏭  {label:45s} SKIPPED: {exc}")

    # Summary
    print(f"\n{'='*70}")
    print(f"  RESULTS SUMMARY")
    print(f"{'='*70}")
    print(f"  ✅ OK:          {len(results['OK'])}")
    print(f"  ⚠️  Empty:       {len(results['EMPTY'])}")
    print(f"  ❌ Errors:      {len(results['ERRORS'])}")
    print(f"  💥 Load Errors: {len(results['LOAD_ERROR'])}")
    print(f"  💥 Exec Errors: {len(results['EXEC_ERROR'])}")
    print(f"  ⏱  Timeouts:    {len(results['TIMEOUT'])}")
    print(f"  ⏭  Interactive: {len(results['INTERACTIVE'])}")

    total_tested = len(results['OK']) + len(results['EMPTY']) + len(results['ERRORS']) + len(results['LOAD_ERROR']) + len(results['EXEC_ERROR']) + len(results['TIMEOUT'])
    total_pass = len(results['OK']) + len(results['EMPTY'])
    print(f"\n  Total tested: {total_tested}  |  Pass: {total_pass}  |  Fail: {total_tested - total_pass}")

    if results["ERRORS"]:
        print(f"\n  FILES WITH ERRORS:")
        for f in results["ERRORS"]:
            print(f"    - {f}")
    if results["LOAD_ERROR"]:
        print(f"\n  FILES WITH LOAD ERRORS:")
        for f in results["LOAD_ERROR"]:
            print(f"    - {f}")
    if results["EXEC_ERROR"]:
        print(f"\n  FILES WITH EXEC ERRORS:")
        for f in results["EXEC_ERROR"]:
            print(f"    - {f}")
    if results["TIMEOUT"]:
        print(f"\n  FILES WITH TIMEOUTS:")
        for f in results["TIMEOUT"]:
            print(f"    - {f}")

    print()

    # Return exit code
    fail_count = len(results['ERRORS']) + len(results['LOAD_ERROR']) + len(results['EXEC_ERROR']) + len(results['TIMEOUT'])
    return 1 if fail_count > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
