"""The worker speaks UTF-8 on every platform, whatever the locale says.

This is a regression test for a failure that only Windows could show. `sys.stdout`
there is the ANSI code page -- cp1252 on a default install -- while the protocol
is UTF-8, and the emitter writes real characters rather than `\\u` escapes. The
first reply carrying a lesson title or an emoji therefore raised
`UnicodeEncodeError`; that is a `ValueError`, which the emitter reads as "the
client has gone" and exits on. So the worker answered `ready`, then died at its
first real message, and every bridge test on Windows reported a timeout with no
output to explain it.

`PYTHONIOENCODING` is how the two non-UTF-8 streams are simulated here: it is the
same mechanism Windows uses, and it works on Linux and macOS.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path
from typing import List

import pytest

WORKER = Path(__file__).resolve().parents[2] / "worker.py"

#: A request whose reply is ASCII, and one whose reply is not: BASIC's output
#: carries a turtle emoji, and the curriculum has punctuation no code page 1252
#: install has to represent.
REQUESTS = [
    {"t": "lessons"},
    {
        "t": "run",
        "language": "basic",
        "source": '10 PRINT "héllo"\n20 END\n',
        "name": "hello.bas",
    },
]


def run_worker(encoding: str) -> List[dict]:
    """Drive the worker with `REQUESTS`, returning the messages it answered."""
    environment = dict(os.environ)
    environment["PYTHONPATH"] = str(WORKER.parent)
    environment["PYTHONIOENCODING"] = encoding
    stdin = "".join(json.dumps(request) + "\n" for request in REQUESTS)
    completed = subprocess.run(  # noqa: S603 - our own script, our own requests
        [sys.executable, str(WORKER)],
        input=stdin,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        env=environment,
        timeout=120,
        check=False,
    )
    if not completed.stdout:
        raise AssertionError(
            f"the worker said nothing with {encoding=}: {completed.stderr}"
        )
    return [json.loads(line) for line in completed.stdout.splitlines() if line]


@pytest.mark.parametrize("encoding", ["utf-8", "cp1252", "ascii"])
def test_the_worker_answers_whatever_the_stream_encoding_is(encoding: str) -> None:
    messages = run_worker(encoding)
    kinds = [message["t"] for message in messages]

    assert kinds[0] == "ready"
    assert "lessons" in kinds, f"no curriculum reply with {encoding=}"
    assert "done" in kinds, f"the run never finished with {encoding=}"


def test_the_run_reports_its_output_undamaged() -> None:
    messages = run_worker("cp1252")
    printed = "".join(
        message.get("text", "") for message in messages if message["t"] == "out"
    )
    assert "héllo" in printed, "the program's output lost its non-ASCII text"
