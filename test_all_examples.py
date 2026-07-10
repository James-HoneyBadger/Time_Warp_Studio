#!/usr/bin/env python3
"""Test all example programs in Examples/ directory."""

import sys
from pathlib import Path
import time

sys.path.insert(0, str(Path(__file__).parent / "Platforms" / "Python"))

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState

REPO_ROOT = Path(__file__).parent
EXAMPLES_DIR = REPO_ROOT / "Examples"

# Map directory names to Language enum values
LANG_MAP = {
    "basic": Language.BASIC,
    "pilot": Language.PILOT,
    "logo": Language.LOGO,
    "c": Language.C,
    "pascal": Language.PASCAL,
    "prolog": Language.PROLOG,
    "forth": Language.FORTH,
    "brainfuck": Language.BRAINFUCK,
    "javascript": Language.JAVASCRIPT,
    "lua": Language.LUA,
    "hypertalk": Language.HYPERTALK,
    "erlang": Language.ERLANG,
    "lisp": Language.LISP,
    "cobol": Language.COBOL,
    "tcl": Language.TCL,
    "postscript": Language.POSTSCRIPT,
    "ruby": Language.RUBY,
    "python": Language.PYTHON_LANG,
    "haskell": Language.HASKELL,
    "asm6502": Language.ASM6502,
    "perl": Language.PERL,
    "rexx": Language.REXX,
    "smalltalk": Language.SMALLTALK,
    "apl": Language.APL,
}

TIMEOUT_SECONDS = 15

def run_example(example_path: Path):
    """Run a single example file and return result."""
    lang_dir = example_path.parent.name
    if lang_dir not in LANG_MAP:
        return "SKIP", "Unknown language directory"
    
    lang = LANG_MAP[lang_dir]
    
    try:
        source = example_path.read_text(encoding='utf-8')
        interp = Interpreter()
        turtle = TurtleState()
        
        interp.load_program(source, lang)
        
        # Run with timeout
        start = time.time()
        try:
            output = interp.execute(turtle)
            elapsed = time.time() - start
            
            # Check for errors in output
            output_text = "\n".join(output) if isinstance(output, list) else str(output)
            if "❌" in output_text:
                return "FAIL", f"Error in output: {output_text[:100]}"
            
            return "PASS", f"Executed in {elapsed:.2f}s"
        except Exception as e:
            elapsed = time.time() - start
            if elapsed > TIMEOUT_SECONDS:
                return "TIMEOUT", f"Timeout after {TIMEOUT_SECONDS}s"
            return "FAIL", str(e)[:100]
    except Exception as e:
        return "ERROR", str(e)[:100]

def main():
    results = {"PASS": [], "FAIL": [], "TIMEOUT": [], "SKIP": [], "ERROR": []}
    
    example_files = sorted(EXAMPLES_DIR.rglob("*.bas")) + \
                    sorted(EXAMPLES_DIR.rglob("*.pilot")) + \
                    sorted(EXAMPLES_DIR.rglob("*.logo")) + \
                    sorted(EXAMPLES_DIR.rglob("*.c")) + \
                    sorted(EXAMPLES_DIR.rglob("*.pas")) + \
                    sorted(EXAMPLES_DIR.rglob("*.pl")) + \
                    sorted(EXAMPLES_DIR.rglob("*.fth")) + \
                    sorted(EXAMPLES_DIR.rglob("*.bf")) + \
                    sorted(EXAMPLES_DIR.rglob("*.js")) + \
                    sorted(EXAMPLES_DIR.rglob("*.lua")) + \
                    sorted(EXAMPLES_DIR.rglob("*.hts")) + \
                    sorted(EXAMPLES_DIR.rglob("*.erl")) + \
                    sorted(EXAMPLES_DIR.rglob("*.lisp")) + \
                    sorted(EXAMPLES_DIR.rglob("*.cob")) + \
                    sorted(EXAMPLES_DIR.rglob("*.tcl")) + \
                    sorted(EXAMPLES_DIR.rglob("*.ps")) + \
                    sorted(EXAMPLES_DIR.rglob("*.rb")) + \
                    sorted(EXAMPLES_DIR.rglob("*.py")) + \
                    sorted(EXAMPLES_DIR.rglob("*.hs")) + \
                    sorted(EXAMPLES_DIR.rglob("*.asm")) + \
                    sorted(EXAMPLES_DIR.rglob("*.pm")) + \
                    sorted(EXAMPLES_DIR.rglob("*.rex")) + \
                    sorted(EXAMPLES_DIR.rglob("*.st")) + \
                    sorted(EXAMPLES_DIR.rglob("*.apl"))
    
    print(f"Testing {len(example_files)} examples...")
    print()
    
    for i, example_file in enumerate(example_files, 1):
        status, msg = run_example(example_file)
        results[status].append((example_file, msg))
        
        symbol = "✓" if status == "PASS" else "✗" if status in ["FAIL", "TIMEOUT"] else "⊘"
        rel_path = example_file.relative_to(REPO_ROOT)
        print(f"[{i:3d}] {symbol} {status:8s} {rel_path}: {msg}")
    
    print("\n" + "="*80)
    print(f"SUMMARY:")
    print(f"  PASS:    {len(results['PASS']):3d}")
    print(f"  FAIL:    {len(results['FAIL']):3d}")
    print(f"  TIMEOUT: {len(results['TIMEOUT']):3d}")
    print(f"  SKIP:    {len(results['SKIP']):3d}")
    print(f"  ERROR:   {len(results['ERROR']):3d}")
    print(f"  TOTAL:   {len(example_files):3d}")
    print("="*80)
    
    if results['FAIL']:
        print(f"\nFailing examples ({len(results['FAIL'])}):")
        for path, msg in results['FAIL']:
            print(f"  - {path.relative_to(REPO_ROOT)}: {msg}")
    
    if results['TIMEOUT']:
        print(f"\nTimeout examples ({len(results['TIMEOUT'])}):")
        for path, msg in results['TIMEOUT']:
            print(f"  - {path.relative_to(REPO_ROOT)}: {msg}")

if __name__ == "__main__":
    main()
