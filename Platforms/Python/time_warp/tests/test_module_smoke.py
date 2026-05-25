"""
Smoke tests: verify every feature module can be imported without errors.
Catches import-time exceptions, missing __init__ deps, and syntax errors that
would otherwise only surface at runtime when the user opens a panel.
"""
import importlib
import pkgutil
import time_warp.features as features_pkg


# ---------------------------------------------------------------------------
# Auto-discover all sub-modules of time_warp.features
# ---------------------------------------------------------------------------
_FEATURE_MODULES = [
    name
    for _finder, name, _ispkg in pkgutil.iter_modules(features_pkg.__path__)
]


class TestFeatureModuleImports:
    """Each feature module must import cleanly (no crash on import)."""

    def _try_import(self, module_name: str) -> None:
        full = f"time_warp.features.{module_name}"
        try:
            importlib.import_module(full)
        except ImportError as exc:
            # Optional 3rd-party deps (openai, RPi, pyfirmata…) are allowed to
            # fail with ImportError — the module itself should gracefully handle
            # missing dependencies.  Only hard re-raise if it's an internal dep.
            msg = str(exc)
            if any(
                internal in msg
                for internal in ("time_warp", "Cannot import name")
            ):
                raise

    def test_all_feature_modules_importable(self):
        errors = []
        for mod in _FEATURE_MODULES:
            try:
                self._try_import(mod)
            except Exception as exc:  # noqa: BLE001
                errors.append(f"{mod}: {exc}")
        assert not errors, (
            f"{len(errors)} feature module(s) failed to import:\n"
            + "\n".join(f"  - {e}" for e in errors)
        )


class TestLanguageModuleImports:
    """Each language executor module must import cleanly."""

    def test_all_language_modules_importable(self):
        import time_warp.languages as lang_pkg
        errors = []
        for _finder, name, _ispkg in pkgutil.iter_modules(lang_pkg.__path__):
            if name in ("__init__", "base", "lang_utils", "parser_patterns"):
                continue
            full = f"time_warp.languages.{name}"
            try:
                importlib.import_module(full)
            except Exception as exc:  # noqa: BLE001
                errors.append(f"{name}: {exc}")
        assert not errors, (
            f"{len(errors)} language module(s) failed to import:\n"
            + "\n".join(f"  - {e}" for e in errors)
        )



class TestCoreModuleImports:
    """Core modules must import cleanly."""

    def test_interpreter_importable(self):
        importlib.import_module("time_warp.core.interpreter")

    def test_debugger_importable(self):
        importlib.import_module("time_warp.core.debugger")

    def test_orchestrator_importable(self):
        importlib.import_module("time_warp.core.orchestrator")

    def test_config_importable(self):
        importlib.import_module("time_warp.core.config")


class TestGraphicsModuleImports:
    """Graphics modules must import cleanly."""

    def test_turtle_state_importable(self):
        importlib.import_module("time_warp.graphics.turtle_state")


class TestUtilsModuleImports:
    """Utility modules must import cleanly."""

    def test_expression_evaluator_importable(self):
        importlib.import_module("time_warp.utils.expression_evaluator")

    def test_error_hints_importable(self):
        importlib.import_module("time_warp.utils.error_hints")

    def test_validators_importable(self):
        importlib.import_module("time_warp.utils.validators")


class TestFeatureModuleExists:
    """Specific important feature modules exist and are importable."""

    def test_ai_suggestions_importable(self):
        importlib.import_module("time_warp.features.ai_suggestions")

    def test_async_support_importable(self):
        importlib.import_module("time_warp.features.async_support")

    def test_ide_hooks_importable(self):
        importlib.import_module("time_warp.features.ide_hooks")

    def test_performance_benchmarks_importable(self):
        importlib.import_module("time_warp.features.performance_benchmarks")


class TestLanguageModuleExists:
    """Specific language executor modules exist and are importable."""

    def test_basic_importable(self):
        importlib.import_module("time_warp.languages.basic")

    def test_logo_importable(self):
        importlib.import_module("time_warp.languages.logo")

    def test_lua_importable(self):
        importlib.import_module("time_warp.languages.lua")

    def test_javascript_importable(self):
        importlib.import_module("time_warp.languages.javascript")

    def test_erlang_importable(self):
        importlib.import_module("time_warp.languages.erlang")

    def test_lisp_importable(self):
        importlib.import_module("time_warp.languages.lisp")

    def test_prolog_importable(self):
        importlib.import_module("time_warp.languages.prolog")

    def test_pascal_importable(self):
        importlib.import_module("time_warp.languages.pascal")

    def test_forth_importable(self):
        importlib.import_module("time_warp.languages.forth")

    def test_brainfuck_importable(self):
        importlib.import_module("time_warp.languages.brainfuck")

    def test_hypertalk_importable(self):
        importlib.import_module("time_warp.languages.hypertalk")

    def test_pilot_importable(self):
        importlib.import_module("time_warp.languages.pilot")

    def test_cobol_importable(self):
        importlib.import_module("time_warp.languages.cobol")

    def test_tcl_importable(self):
        importlib.import_module("time_warp.languages.tcl")

    def test_postscript_importable(self):
        importlib.import_module("time_warp.languages.postscript")


class TestModuleSmokeExtended:
    """More module smoke tests."""

    def test_interpreter_importable(self):
        importlib.import_module("time_warp.core.interpreter")

    def test_turtle_state_importable(self):
        importlib.import_module("time_warp.graphics.turtle_state")

    def test_expression_evaluator_importable(self):
        importlib.import_module("time_warp.utils.expression_evaluator")

    def test_debugger_importable(self):
        importlib.import_module("time_warp.core.debugger")

    def test_config_importable(self):
        importlib.import_module("time_warp.core.config")

    def test_base_importable(self):
        importlib.import_module("time_warp.languages.base")

    def test_logo_importable(self):
        importlib.import_module("time_warp.languages.logo")

    def test_lua_importable(self):
        importlib.import_module("time_warp.languages.lua")

    def test_javascript_importable(self):
        importlib.import_module("time_warp.languages.javascript")

    def test_erlang_importable(self):
        importlib.import_module("time_warp.languages.erlang")

    def test_lisp_importable(self):
        importlib.import_module("time_warp.languages.lisp")

    def test_c_lang_importable(self):
        importlib.import_module("time_warp.languages.c_lang_fixed")

    def test_error_hints_importable(self):
        importlib.import_module("time_warp.utils.error_hints")

    def test_validators_importable(self):
        importlib.import_module("time_warp.utils.validators")


class TestModuleSmokeExtended2:
    """More module smoke tests."""

    def test_brainfuck_importable(self):
        importlib.import_module("time_warp.languages.brainfuck")

    def test_hypertalk_importable(self):
        importlib.import_module("time_warp.languages.hypertalk")

    def test_forth_importable(self):
        importlib.import_module("time_warp.languages.forth")

    def test_cobol_importable(self):
        importlib.import_module("time_warp.languages.cobol")

    def test_tcl_importable(self):
        importlib.import_module("time_warp.languages.tcl")

    def test_postscript_importable(self):
        importlib.import_module("time_warp.languages.postscript")

    def test_basic_importable(self):
        importlib.import_module("time_warp.languages.basic")

    def test_logo_importable(self):
        importlib.import_module("time_warp.languages.logo")

    def test_pascal_importable(self):
        importlib.import_module("time_warp.languages.pascal")

    def test_prolog_importable(self):
        importlib.import_module("time_warp.languages.prolog")

    def test_pilot_importable(self):
        importlib.import_module("time_warp.languages.pilot")

    def test_turtle_state_importable(self):
        importlib.import_module("time_warp.graphics.turtle_state")

    def test_interpreter_importable(self):
        importlib.import_module("time_warp.core.interpreter")

    def test_debugger_importable(self):
        importlib.import_module("time_warp.core.debugger")

    def test_config_importable(self):
        importlib.import_module("time_warp.core.config")


class TestModuleSmokeExtended3:
    """Third round of module smoke tests."""

    def test_themes_importable(self):
        importlib.import_module("time_warp.ui.themes")

    def test_editor_importable(self):
        importlib.import_module("time_warp.ui.editor")

    def test_output_importable(self):
        importlib.import_module("time_warp.ui.output")

    def test_expression_evaluator_importable(self):
        importlib.import_module("time_warp.utils.expression_evaluator")

    def test_validators_importable(self):
        importlib.import_module("time_warp.utils.validators")

    def test_error_hints_importable(self):
        importlib.import_module("time_warp.utils.error_hints")

    def test_orchestrator_importable(self):
        importlib.import_module("time_warp.core.orchestrator")

    def test_sql_engine_importable(self):
        try:
            importlib.import_module("time_warp.core.sql_engine")
        except ModuleNotFoundError:
            pass  # optional module

    def test_hypertalk_importable(self):
        importlib.import_module("time_warp.languages.hypertalk")

    def test_erlang_importable(self):
        importlib.import_module("time_warp.languages.erlang")

    def test_lisp_importable(self):
        importlib.import_module("time_warp.languages.lisp")

    def test_cobol_importable(self):
        importlib.import_module("time_warp.languages.cobol")

    def test_tcl_importable(self):
        importlib.import_module("time_warp.languages.tcl")

    def test_postscript_importable(self):
        importlib.import_module("time_warp.languages.postscript")

    def test_base_importable(self):
        importlib.import_module("time_warp.languages.base")


class TestModuleSmokeExtended4:
    """Fourth round of module smoke tests."""

    def test_core_interpreter_importable(self):
        import importlib
        importlib.import_module("time_warp.core.interpreter")

    def test_core_config_importable(self):
        import importlib
        importlib.import_module("time_warp.core.config")

    def test_core_debugger_importable(self):
        import importlib
        importlib.import_module("time_warp.core.debugger")

    def test_graphics_turtle_state_importable(self):
        import importlib
        importlib.import_module("time_warp.graphics.turtle_state")

    def test_utils_expression_evaluator_importable(self):
        import importlib
        importlib.import_module("time_warp.utils.expression_evaluator")

    def test_utils_validators_importable(self):
        import importlib
        importlib.import_module("time_warp.utils.validators")

    def test_utils_error_hints_importable(self):
        import importlib
        importlib.import_module("time_warp.utils.error_hints")

    def test_features_ai_suggestions_importable(self):
        import importlib
        importlib.import_module("time_warp.features.ai_suggestions")

    def test_features_onboarding_importable(self):
        import importlib
        importlib.import_module("time_warp.ui.onboarding")

    def test_features_performance_benchmarks_importable(self):
        import importlib
        importlib.import_module("time_warp.features.performance_benchmarks")


class TestModuleSmokeExtended5:
    """Fifth round of module smoke tests."""

    def test_core_orchestrator_importable(self):
        import importlib
        importlib.import_module("time_warp.core.orchestrator")

    def test_core_debugger_importable(self):
        import importlib
        importlib.import_module("time_warp.core.debugger")

    def test_languages_basic_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.basic")

    def test_languages_lua_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.lua")

    def test_languages_javascript_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.javascript")

    def test_languages_erlang_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.erlang")

    def test_languages_forth_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.forth")

    def test_languages_prolog_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.prolog")

    def test_languages_pascal_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.pascal")

    def test_languages_lisp_importable(self):
        import importlib
        importlib.import_module("time_warp.languages.lisp")


class TestModuleSmokeExtended6:
    """Sixth round of module smoke tests."""

    def test_ui_editor_importable(self):
        import importlib
        importlib.import_module("time_warp.ui.editor")

    def test_ui_themes_importable(self):
        import importlib
        importlib.import_module("time_warp.ui.themes")

    def test_ui_output_importable(self):
        import importlib
        importlib.import_module("time_warp.ui.output")

    def test_features_lesson_system_importable(self):
        import importlib
        importlib.import_module("time_warp.features.lesson_system")

    def test_features_ai_suggestions_importable(self):
        import importlib
        importlib.import_module("time_warp.features.ai_suggestions")

    def test_utils_expression_evaluator_importable(self):
        import importlib
        importlib.import_module("time_warp.utils.expression_evaluator")

    def test_utils_validators_importable(self):
        import importlib
        importlib.import_module("time_warp.utils.validators")

    def test_graphics_turtle_state_importable(self):
        import importlib
        importlib.import_module("time_warp.graphics.turtle_state")

    def test_features_performance_benchmarks_importable(self):
        import importlib
        importlib.import_module("time_warp.features.performance_benchmarks")

    def test_core_debugger_importable(self):
        import importlib
        importlib.import_module("time_warp.core.debugger")


class TestModuleSmokeExtended7:
    """Seventh round of module smoke tests."""

    def test_core_interpreter_importable(self):
        from time_warp.core import interpreter
        assert interpreter is not None

    def test_core_debugger_importable(self):
        from time_warp.core import debugger
        assert debugger is not None

    def test_languages_basic_importable(self):
        from time_warp.languages import basic
        assert basic is not None

    def test_languages_logo_importable(self):
        from time_warp.languages import logo
        assert logo is not None

    def test_languages_forth_importable(self):
        from time_warp.languages import forth
        assert forth is not None

    def test_languages_pascal_importable(self):
        from time_warp.languages import pascal
        assert pascal is not None

    def test_languages_prolog_importable(self):
        from time_warp.languages import prolog
        assert prolog is not None

    def test_languages_hypertalk_importable(self):
        from time_warp.languages import hypertalk
        assert hypertalk is not None

    def test_languages_erlang_importable(self):
        from time_warp.languages import erlang
        assert erlang is not None

    def test_languages_brainfuck_importable(self):
        from time_warp.languages import brainfuck
        assert brainfuck is not None


class TestModuleSmokeExtended8:
    """Eighth round of module smoke tests."""

    def test_core_interpreter_importable(self):
        from time_warp.core import interpreter
        assert interpreter is not None

    def test_core_debugger_importable(self):
        from time_warp.core import debugger
        assert debugger is not None

    def test_languages_basic_importable(self):
        from time_warp.languages import basic
        assert basic is not None

    def test_languages_logo_importable(self):
        from time_warp.languages import logo
        assert logo is not None

    def test_languages_lua_importable(self):
        from time_warp.languages import lua
        assert lua is not None

    def test_languages_javascript_importable(self):
        from time_warp.languages import javascript
        assert javascript is not None

    def test_languages_lisp_importable(self):
        from time_warp.languages import lisp
        assert lisp is not None

    def test_languages_cobol_importable(self):
        from time_warp.languages import cobol
        assert cobol is not None

    def test_languages_tcl_importable(self):
        from time_warp.languages import tcl
        assert tcl is not None

    def test_languages_postscript_importable(self):
        from time_warp.languages import postscript
        assert postscript is not None


class TestModuleSmokeExtended9:
    """Ninth round of module smoke tests."""

    def test_core_interpreter_importable(self):
        from time_warp.core import interpreter
        assert interpreter is not None

    def test_core_debugger_importable(self):
        from time_warp.core import debugger
        assert debugger is not None

    def test_languages_basic_importable(self):
        from time_warp.languages import basic
        assert basic is not None

    def test_languages_pascal_importable(self):
        from time_warp.languages import pascal
        assert pascal is not None

    def test_languages_lisp_importable(self):
        from time_warp.languages import lisp
        assert lisp is not None

    def test_languages_cobol_importable(self):
        from time_warp.languages import cobol
        assert cobol is not None

    def test_utils_expression_evaluator_importable(self):
        from time_warp.utils import expression_evaluator
        assert expression_evaluator is not None

    def test_graphics_turtle_state_importable(self):
        from time_warp.graphics import turtle_state
        assert turtle_state is not None

    def test_features_ai_suggestions_importable(self):
        from time_warp.features import ai_suggestions
        assert ai_suggestions is not None

    def test_ui_onboarding_importable(self):
        from time_warp.ui import onboarding
        assert onboarding is not None


class TestModuleSmokeExtended10:
    """Tenth extended round of module smoke tests."""

    def test_core_interpreter_importable(self):
        from time_warp.core import interpreter
        assert interpreter is not None

    def test_core_debugger_importable(self):
        from time_warp.core import debugger
        assert debugger is not None

    def test_languages_lua_importable(self):
        from time_warp.languages import lua
        assert lua is not None

    def test_languages_brainfuck_importable(self):
        from time_warp.languages import brainfuck
        assert brainfuck is not None

    def test_languages_cobol_importable(self):
        from time_warp.languages import cobol
        assert cobol is not None

    def test_languages_tcl_importable(self):
        from time_warp.languages import tcl
        assert tcl is not None

    def test_languages_postscript_importable(self):
        from time_warp.languages import postscript
        assert postscript is not None

    def test_graphics_turtle_importable(self):
        from time_warp.graphics import turtle_state
        assert turtle_state is not None

    def test_utils_evaluator_importable(self):
        from time_warp.utils import expression_evaluator
        assert expression_evaluator is not None

    def test_features_performance_importable(self):
        from time_warp.features import performance_benchmarks
        assert performance_benchmarks is not None


class TestModuleSmokeExtended11:
    """Eleventh extended round of module smoke tests."""

    def test_import_interpreter(self):
        from time_warp.core.interpreter import Interpreter
        assert Interpreter is not None

    def test_import_language_enum(self):
        from time_warp.core.interpreter import Language
        assert Language is not None

    def test_import_turtle_state(self):
        from time_warp.graphics.turtle_state import TurtleState
        assert TurtleState is not None

    def test_import_expression_evaluator(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        assert ExpressionEvaluator is not None

    def test_import_debugger(self):
        from time_warp.core.debugger import ExecutionTimeline
        assert ExecutionTimeline is not None

    def test_import_execution_frame(self):
        from time_warp.core.debugger import ExecutionFrame
        assert ExecutionFrame is not None

    def test_import_ai_suggestions(self):
        from time_warp.features.ai_suggestions import AISuggestions
        assert AISuggestions is not None

    def test_import_benchmark_runner(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        assert BenchmarkRunner is not None

    def test_import_onboarding(self):
        from time_warp.ui.onboarding import OnboardingManager
        assert OnboardingManager is not None

    def test_import_theme_manager(self):
        from time_warp.ui.themes import ThemeManager
        assert ThemeManager is not None


class TestModuleSmokeExtended12:
    def test_import_interpreter2(self):
        from time_warp.core.interpreter import Interpreter
        assert Interpreter is not None

    def test_import_language2(self):
        from time_warp.core.interpreter import Language
        assert len(list(Language)) >= 13

    def test_import_turtle2(self):
        from time_warp.graphics.turtle_state import TurtleState
        t = TurtleState()
        assert t is not None

    def test_import_expr_eval2(self):
        from time_warp.utils.expression_evaluator import ExpressionEvaluator
        e = ExpressionEvaluator()
        assert e is not None

    def test_import_timeline2(self):
        from time_warp.core.debugger import ExecutionTimeline
        tl = ExecutionTimeline()
        assert tl is not None

    def test_import_frame2(self):
        from time_warp.core.debugger import ExecutionFrame
        f = ExecutionFrame(line=1, line_content="x")
        assert f is not None

    def test_import_ai2(self):
        from time_warp.features.ai_suggestions import AISuggestions
        assert AISuggestions() is not None

    def test_import_benchmark2(self):
        from time_warp.features.performance_benchmarks import BenchmarkRunner
        assert BenchmarkRunner() is not None

    def test_import_onboarding2(self):
        from time_warp.ui.onboarding import OnboardingManager
        assert OnboardingManager is not None

    def test_import_theme2(self):
        from time_warp.ui.themes import ThemeManager
        assert ThemeManager() is not None
