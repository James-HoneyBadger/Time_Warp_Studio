from ..core.interpreter import Language
from .conftest_lang import run, no_errors
from . import fuzzing

# Add fuzz testing for language executors


class FuzzTestExecutors:
    def test_fuzz_basic(self):
        """Fuzz test for BASIC language executor."""
        for _ in range(100):
            source = fuzzing.generate_random_basic_code()
            result = run(source, Language.BASIC)
            assert no_errors(result), f"Fuzz test failed for BASIC: {source}"

    def test_fuzz_logo(self):
        """Fuzz test for Logo language executor."""
        for _ in range(100):
            source = fuzzing.generate_random_logo_code()
            result = run(source, Language.LOGO)
            assert no_errors(result), f"Fuzz test failed for Logo: {source}"

    def test_fuzz_lua(self):
        """Fuzz test for Lua language executor."""
        for _ in range(100):
            source = fuzzing.generate_random_python_code()
            result = run(source, Language.LUA)
            assert no_errors(result), f"Fuzz test failed for Lua: {source}"
