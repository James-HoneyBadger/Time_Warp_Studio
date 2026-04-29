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

