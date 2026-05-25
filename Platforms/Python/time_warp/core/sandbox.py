"""Subprocess-based execution sandbox for whole-program language executors.

Each whole-program executor is run inside a fresh Python subprocess so that
runaway programs (infinite loops, memory explosions) cannot destabilise the
main IDE process.

Resource limits applied inside the subprocess (Linux only):
  * RLIMIT_AS  — virtual address space: 512 MB
  * RLIMIT_CPU — CPU seconds: slightly above MAX_EXECUTION_TIME so the OS
                 kills the process *after* the interpreter's own timeout fires

The worker communicates via stdin (JSON payload) / stdout (JSON result).
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Any

# Path to the worker script bundled alongside this file
_WORKER = Path(__file__).with_name("_sandbox_worker.py")

# Hard limits for the subprocess (Linux)
_MEMORY_LIMIT_BYTES = 512 * 1024 * 1024   # 512 MB virtual address space
_CPU_EXTRA_SECONDS  = 2                    # grace on top of timeout arg


def _preexec_limit(cpu_seconds: int) -> None:
    """Called in the child process (Linux fork) to set resource limits."""
    try:
        import resource
        # Virtual address space
        resource.setrlimit(
            resource.RLIMIT_AS,
            (_MEMORY_LIMIT_BYTES, _MEMORY_LIMIT_BYTES),
        )
        # CPU time
        resource.setrlimit(
            resource.RLIMIT_CPU,
            (cpu_seconds, cpu_seconds + 2),
        )
    except (ImportError, ValueError, resource.error):
        pass  # Platform may not support all limits


def run_in_sandbox(
    language_name: str,
    source: str,
    timeout: float = 10.0,
    variables: dict[str, Any] | None = None,
) -> tuple[str, dict[str, Any]]:
    """Execute *source* for *language_name* in an isolated subprocess.

    Returns ``(output_text, updated_variables)``.
    On timeout or crash the output contains an ``❌`` error message.
    """
    payload = json.dumps(
        {
            "language": language_name,
            "source": source,
            "variables": variables or {},
        }
    )

    cpu_secs = max(1, int(timeout) + _CPU_EXTRA_SECONDS)
    preexec = None
    if sys.platform.startswith("linux"):
        import functools
        preexec = functools.partial(_preexec_limit, cpu_secs)

    try:
        proc = subprocess.run(
            [sys.executable, str(_WORKER)],
            input=payload,
            capture_output=True,
            text=True,
            timeout=timeout + 1.0,  # outer wall-clock safety net
            preexec_fn=preexec,
        )
        if proc.returncode == 0:
            try:
                result = json.loads(proc.stdout)
                return result.get("output", ""), result.get("variables", {})
            except json.JSONDecodeError:
                return proc.stdout or "❌ Sandbox: malformed worker output\n", {}
        else:
            stderr = (proc.stderr or "")[:500]
            return f"❌ Sandbox process exited with code {proc.returncode}\n{stderr}", {}
    except subprocess.TimeoutExpired:
        return f"❌ Error: Execution timeout ({timeout}s exceeded)\n", {}
    except FileNotFoundError:
        return "❌ Sandbox: Python interpreter not found\n", {}
    except Exception as exc:  # noqa: BLE001
        return f"❌ Sandbox error: {exc}\n", {}


def is_sandbox_available() -> bool:
    """Return True if subprocess sandboxing is usable on this platform.

    Sandboxing is **opt-in**: it is only enabled when the environment
    variable ``TWS_SANDBOX=1`` is set.  This keeps the test suite fast
    and avoids subprocess overhead for normal IDE usage.  Users who want
    hard resource-limit isolation (e.g. running untrusted student code)
    can set ``TWS_SANDBOX=1`` before launching the IDE.
    """
    if os.environ.get("TWS_SANDBOX", "0") != "1":
        return False
    return _WORKER.exists() and bool(sys.executable)
