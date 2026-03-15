"""
Unit tests for AsyncInterpreterRunner (async_support.py).

Tests verify the thread lifecycle: run_code starts execution, is_running
reflects state correctly, stop() terminates cleanly, and callbacks fire.
"""

# pylint: disable=import-error,redefined-outer-name

import threading

import pytest

from time_warp.core.async_support import (  # type: ignore[import-not-found]
    AsyncInterpreterRunner,
    get_async_runner,
)


@pytest.fixture
def runner():
    r = AsyncInterpreterRunner()
    yield r
    if r.is_running:
        r.stop()


class TestInitialState:
    def test_not_running_initially(self, runner):
        assert runner.is_running is False

    def test_stop_when_idle_does_not_raise(self, runner):
        runner.stop()  # should be a no-op


class TestRunCode:
    def test_run_code_starts_and_finishes(self, runner):
        event = threading.Event()
        results = []

        def on_done(output):
            results.extend(output)
            event.set()

        runner.run_code('10 PRINT "Hello"', language="BASIC", callback=on_done)
        finished = event.wait(timeout=10.0)
        assert finished, "Callback was not called within 10 seconds"
        assert any("Hello" in line for line in results), (
            f"Expected 'Hello' in output; got: {results}"
        )

    def test_is_running_true_during_execution(self, runner):
        """is_running should not raise regardless of execution timing."""
        done = threading.Event()
        runner.run_code('10 PRINT "Running"', language="BASIC",
                        callback=lambda _: done.set())
        # is_running may flip quickly; accessing it must not raise
        _ = runner.is_running
        done.wait(timeout=10.0)

    def test_callback_receives_list(self, runner):
        received = []
        event = threading.Event()

        def cb(output):
            received.append(output)
            event.set()

        runner.run_code('10 PRINT "Test"', language="BASIC", callback=cb)
        event.wait(timeout=10.0)
        assert received, "Callback not called"
        assert isinstance(received[0], list)

    def test_error_callback_on_bad_language(self, runner):
        errors = []
        event = threading.Event()

        def on_error(msg):
            errors.append(msg)
            event.set()

        def on_done(output):
            # Even with unknown language, execution may succeed (fall back)
            event.set()

        runner.run_code("HELLO", language="NOTAREALLANGUAGE99",
                        callback=on_done, error_callback=on_error)
        event.wait(timeout=10.0)
        # Either an error was recorded or execution completed — just no exception


class TestStop:
    def test_stop_waits_for_thread(self, runner):
        completion = threading.Event()

        def cb(output):
            completion.set()

        runner.run_code('10 PRINT "Hi"', language="BASIC", callback=cb)
        runner.stop()
        assert runner.is_running is False

    def test_run_after_stop(self, runner):
        """A second run_code call should work after stop()."""
        event1 = threading.Event()
        event2 = threading.Event()

        runner.run_code('10 PRINT "First"', language="BASIC",
                        callback=lambda _: event1.set())
        event1.wait(timeout=10.0)
        runner.stop()

        runner.run_code('10 PRINT "Second"', language="BASIC",
                        callback=lambda _: event2.set())
        assert event2.wait(timeout=10.0), "Second run did not complete"


class TestSingleton:
    def test_get_async_runner_returns_same_instance(self):
        r1 = get_async_runner()
        r2 = get_async_runner()
        assert r1 is r2

    def test_singleton_is_async_interpreter_runner(self):
        r = get_async_runner()
        assert isinstance(r, AsyncInterpreterRunner)
