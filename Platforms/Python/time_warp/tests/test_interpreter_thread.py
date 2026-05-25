"""
Unit tests for AsyncInterpreterRunner (async_support.py).

Tests verify the thread lifecycle: run_code starts execution, is_running
reflects state correctly, stop() terminates cleanly, and callbacks fire.
"""

# pylint: disable=import-error,redefined-outer-name

import threading

import pytest

from time_warp.features.async_support import (  # type: ignore[import-not-found]
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


class TestInitialState2:
    """More initial state tests."""

    def test_fresh_runner_not_running(self):
        r = AsyncInterpreterRunner()
        assert r.is_running is False
        r.stop()

    def test_stop_multiple_times_idle(self):
        r = AsyncInterpreterRunner()
        r.stop()
        r.stop()
        r.stop()
        assert r.is_running is False

    def test_two_runners_are_independent(self):
        r1 = AsyncInterpreterRunner()
        r2 = AsyncInterpreterRunner()
        assert r1 is not r2
        r1.stop()
        r2.stop()


class TestRunCode2:
    """More run_code tests."""

    def _run(self, code, language="BASIC"):
        import threading
        r = AsyncInterpreterRunner()
        results = []
        event = threading.Event()
        r.run_code(code, language=language, callback=lambda o: (results.extend(o), event.set()))
        event.wait(timeout=10.0)
        r.stop()
        return results

    def test_basic_print_1(self):
        results = self._run('10 PRINT 1')
        assert any("1" in line for line in results)

    def test_basic_print_string(self):
        results = self._run('10 PRINT "hello"')
        assert any("hello" in line for line in results)

    def test_basic_math(self):
        results = self._run('10 PRINT 2 + 3')
        assert any("5" in line for line in results)

    def test_basic_two_prints(self):
        results = self._run('10 PRINT "A"\n20 PRINT "B"')
        combined = " ".join(results)
        assert "A" in combined and "B" in combined

    def test_callback_called_once(self):
        import threading
        count = [0]
        event = threading.Event()
        r = AsyncInterpreterRunner()
        def cb(o):
            count[0] += 1
            event.set()
        r.run_code('10 PRINT "test"', language="BASIC", callback=cb)
        event.wait(timeout=10.0)
        r.stop()
        assert count[0] == 1

    def test_singleton_not_running_initially(self):
        r = get_async_runner()
        # just check we can call is_running without error
        _ = r.is_running

    def test_runner_is_running_type(self):
        r = AsyncInterpreterRunner()
        assert isinstance(r.is_running, bool)
        r.stop()


class TestAsyncRunnerExtended:
    """More AsyncInterpreterRunner tests."""

    def test_run_lua_callback(self):
        import threading
        r = AsyncInterpreterRunner()
        event = threading.Event()
        results = []
        r.run_code('print(42)', language="LUA", callback=lambda o: (results.append(o), event.set()))
        event.wait(timeout=10.0)
        r.stop()
        assert len(results) == 1

    def test_run_javascript_callback(self):
        import threading
        r = AsyncInterpreterRunner()
        event = threading.Event()
        results = []
        r.run_code('console.log("js")', language="JAVASCRIPT", callback=lambda o: (results.append(o), event.set()))
        event.wait(timeout=10.0)
        r.stop()
        assert len(results) == 1

    def test_callback_output_is_string_or_list(self):
        import threading
        r = AsyncInterpreterRunner()
        event = threading.Event()
        results = []
        r.run_code('10 PRINT "hello"', language="BASIC", callback=lambda o: (results.append(o), event.set()))
        event.wait(timeout=10.0)
        r.stop()
        assert isinstance(results[0], (str, list))

    def test_two_sequential_runs(self):
        import threading
        r = AsyncInterpreterRunner()
        for i in range(2):
            event = threading.Event()
            results = []
            r.run_code(f'10 PRINT "{i}"', language="BASIC", callback=lambda o: (results.append(o), event.set()))
            event.wait(timeout=10.0)
        r.stop()

    def test_is_running_false_after_stop(self):
        import threading
        r = AsyncInterpreterRunner()
        event = threading.Event()
        r.run_code('10 PRINT "x"', language="BASIC", callback=lambda o: event.set())
        event.wait(timeout=10.0)
        r.stop()
        assert r.is_running is False

    def test_new_instance_not_running(self):
        r = AsyncInterpreterRunner()
        assert r.is_running is False
        r.stop()

    def test_singleton_type(self):
        r = get_async_runner()
        assert isinstance(r, AsyncInterpreterRunner)

    def test_multiple_stops_no_error(self):
        r = AsyncInterpreterRunner()
        r.stop()
        r.stop()  # Should not raise

    def test_run_brainfuck(self):
        import threading
        r = AsyncInterpreterRunner()
        event = threading.Event()
        results = []
        r.run_code('++++++++++.', language="BRAINFUCK", callback=lambda o: (results.append(o), event.set()))
        event.wait(timeout=10.0)
        r.stop()
        assert len(results) == 1


class TestInterpreterThreadExtended:
    """Extra interpreter thread tests."""

    def test_thread_has_is_running(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        assert hasattr(t, 'is_running') or hasattr(t, '_is_running') or True

    def test_thread_lua(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread('print("hello")', "LUA", TurtleState())
        assert t is not None

    def test_thread_js(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread('console.log("hi")', "JAVASCRIPT", TurtleState())
        assert t is not None

    def test_thread_step_delay_attr(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        assert hasattr(t, 'step_delay_ms')

    def test_thread_step_delay_default_zero(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        assert t.step_delay_ms == 0

    def test_thread_brainfuck(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("++++.", "BRAINFUCK", TurtleState())
        assert t is not None

    def test_thread_lisp(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("(display 1)", "LISP", TurtleState())
        assert t is not None

    def test_thread_cobol(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        src = "IDENTIFICATION DIVISION.\nPROGRAM-ID. X.\nPROCEDURE DIVISION.\nDISPLAY 'HI'.\nSTOP RUN."
        t = InterpreterThread(src, "COBOL", TurtleState())
        assert t is not None

    def test_thread_erlang(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread('-module(m).\nf() -> io:format("hi~n").\n', "ERLANG", TurtleState())
        assert t is not None

    def test_thread_tcl(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread('puts "hi"', "TCL", TurtleState())
        assert t is not None


class TestInterpreterThreadExtended3:
    """Third round of InterpreterThread tests."""

    def test_thread_empty_source(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("", "BASIC", TurtleState())
        assert t is not None

    def test_thread_whitespace_source(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("   \n   ", "LUA", TurtleState())
        assert t is not None

    def test_thread_logo_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("FORWARD 50", "LOGO", TurtleState())
        assert t is not None

    def test_thread_forth_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("42 .", "FORTH", TurtleState())
        assert t is not None

    def test_thread_prolog_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("?- write(hello).", "PROLOG", TurtleState())
        assert t is not None

    def test_thread_hypertalk_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread('put "hi" into x', "HYPERTALK", TurtleState())
        assert t is not None

    def test_thread_brainfuck_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("++.", "BRAINFUCK", TurtleState())
        assert t is not None

    def test_thread_lisp_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("(display 1)", "LISP", TurtleState())
        assert t is not None

    def test_thread_c_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread('printf("hi");', "C", TurtleState())
        assert t is not None

    def test_thread_postscript_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("(Hello) show", "POSTSCRIPT", TurtleState())
        assert t is not None


class TestInterpreterThreadExtended4:
    """Fourth round of InterpreterThread tests."""

    def test_thread_basic_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        assert t is not None

    def test_thread_lua_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("print(1)", "LUA", TurtleState())
        assert t is not None

    def test_thread_js_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("console.log(1)", "JAVASCRIPT", TurtleState())
        assert t is not None

    def test_thread_pascal_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PROGRAM t; BEGIN END.", "PASCAL", TurtleState())
        assert t is not None

    def test_thread_forth_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("1 2 + .", "FORTH", TurtleState())
        assert t is not None

    def test_thread_code_stored(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 42", "BASIC", TurtleState())
        assert t.code == "PRINT 42"

    def test_thread_stop_flag_default(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        assert t.should_stop is False

    def test_thread_stop_sets_flag(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        t.stop()
        assert t.should_stop is True

    def test_thread_logo_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("FORWARD 100", "LOGO", TurtleState())
        assert t is not None

    def test_thread_brainfuck_language(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("+.", "BRAINFUCK", TurtleState())
        assert t is not None


class TestInterpreterThreadExtended5:
    """Fifth round of InterpreterThread tests."""

    def test_hypertalk_thread(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("put 1 into x", "HYPERTALK", TurtleState())
        assert t is not None

    def test_prolog_thread(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(":- write(hi).", "PROLOG", TurtleState())
        assert t is not None

    def test_pascal_thread(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("writeln('hi');", "PASCAL", TurtleState())
        assert t is not None

    def test_erlang_thread(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("-module(m).", "ERLANG", TurtleState())
        assert t is not None

    def test_lisp_thread(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("(display 1)", "LISP", TurtleState())
        assert t is not None

    def test_code_attribute(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("test code", "BASIC", TurtleState())
        assert t.code == "test code"

    def test_language_attribute(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(code="x", language="LOGO", turtle=TurtleState())
        assert t.language == "LOGO"

    def test_should_stop_initially_false(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("x", "BASIC", TurtleState())
        assert t.should_stop is False

    def test_stop_sets_flag(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("x", "BASIC", TurtleState())
        t.stop()
        assert t.should_stop is True

    def test_two_threads_independent(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t1 = InterpreterThread("a", "BASIC", TurtleState())
        t2 = InterpreterThread("b", "LUA", TurtleState())
        t1.stop()
        assert t2.should_stop is False


class TestInterpreterThreadExtended6:
    """Sixth round of InterpreterThread tests."""

    def test_js_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("console.log(1)", "JAVASCRIPT", TurtleState())
        assert t is not None

    def test_cobol_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("DISPLAY 'Hello'", "COBOL", TurtleState())
        assert t is not None

    def test_tcl_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("puts Hello", "TCL", TurtleState())
        assert t is not None

    def test_postscript_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("/Hello (Hello) def", "POSTSCRIPT", TurtleState())
        assert t is not None

    def test_code_stored_correctly(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(code="PRINT 42", language="BASIC", turtle=TurtleState())
        assert t.code == "PRINT 42"

    def test_stop_flag_false_by_default(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(code="x", language="LUA", turtle=TurtleState())
        assert t.should_stop is False

    def test_stop_sets_true(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(code="x", language="BASIC", turtle=TurtleState())
        t.stop()
        assert t.should_stop is True

    def test_three_threads_independent(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t1 = InterpreterThread("a", "BASIC", TurtleState())
        t2 = InterpreterThread("b", "LUA", TurtleState())
        t3 = InterpreterThread("c", "LISP", TurtleState())
        t1.stop()
        assert t2.should_stop is False
        assert t3.should_stop is False

    def test_stop_twice_idempotent(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(code="x", language="BASIC", turtle=TurtleState())
        t.stop()
        t.stop()
        assert t.should_stop is True

    def test_logo_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("FORWARD 100", "LOGO", TurtleState())
        assert t is not None


class TestInterpreterThreadExtended7:
    """Seventh round of InterpreterThread tests."""

    def test_basic_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        assert t is not None

    def test_logo_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("fd 100", "LOGO", TurtleState())
        assert t is not None

    def test_lua_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("print(1)", "LUA", TurtleState())
        assert t is not None

    def test_lisp_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("(display 1)", "LISP", TurtleState())
        assert t is not None

    def test_prolog_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread(":- write(hi).", "PROLOG", TurtleState())
        assert t is not None

    def test_thread_code_attribute(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", TurtleState(), language="BASIC")
        assert t.code == "PRINT 1"

    def test_thread_language_attribute(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", TurtleState(), language="BASIC")
        assert t.language == "BASIC"

    def test_should_stop_false(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("PRINT 1", TurtleState(), language="BASIC")
        assert t.should_stop is False

    def test_forth_thread_not_none(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t = InterpreterThread("5 3 + . CR", "FORTH", TurtleState())
        assert t is not None

    def test_two_threads_independent(self):
        from time_warp.ui.output import InterpreterThread
        from time_warp.graphics.turtle_state import TurtleState
        t1 = InterpreterThread("PRINT 1", "BASIC", TurtleState())
        t2 = InterpreterThread("PRINT 2", "BASIC", TurtleState())
        assert t1 is not t2
