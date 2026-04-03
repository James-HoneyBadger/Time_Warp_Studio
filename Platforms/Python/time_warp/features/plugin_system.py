"""
Plugin System for Time Warp Studio

Discovers and loads plugins from the ``plugins/`` directory.
Each plugin is a sub-directory containing:

- ``__init__.py`` with metadata constants:
    PLUGIN_NAME    (str)
    PLUGIN_VERSION (str)
    PLUGIN_DESCRIPTION (str, optional)

- ``plugin.py`` with a ``SamplePlugin`` class (name can vary) that
  implements:
    ``initialize(ide_instance)`` – called once after IDE startup
    ``shutdown()``               – called when the plugin is unloaded

Usage::

    from time_warp.features.plugin_system import PluginManager
    mgr = PluginManager(ide_instance=window)
    mgr.discover_plugins()          # scans plugins/ directory
    loaded = mgr.get_loaded_plugins()
"""

from __future__ import annotations

import importlib
import importlib.util
import logging
from pathlib import Path
from typing import Any, Dict, List, Optional

from .hardware_integration import HardwareIntegration
from .ai_suggestions import AISuggestions

logger = logging.getLogger(__name__)

# Initialize hardware integration manager
hardware_manager = HardwareIntegration()
# Initialize AI suggestions manager
ai_suggestions_manager = AISuggestions()


class PluginInfo:
    """Metadata and runtime state for a single plugin."""

    def __init__(
        self,
        name: str,
        version: str,
        description: str,
        directory: Path,
    ) -> None:
        self.name = name
        self.version = version
        self.description = description
        self.directory = directory
        self.instance: Optional[Any] = None
        self.loaded: bool = False
        self.error: Optional[str] = None

    def __repr__(self) -> str:
        status = "loaded" if self.loaded else f"error: {self.error}"
        return f"<PluginInfo {self.name} {self.version} [{status}]>"


class PluginManager:
    """Discovers and manages IDE plugins.

    Plugins are expected under a ``plugins/`` directory relative to the
    workspace root (or the path given to ``discover_plugins``).

    Plugin directory layout::

        plugins/
            my_plugin/
                __init__.py   <- PLUGIN_NAME, PLUGIN_VERSION constants
                plugin.py     <- class with initialize() / shutdown()
    """

    def __init__(self, ide_instance: Optional[Any] = None) -> None:
        self._ide = ide_instance
        self._plugins: Dict[str, PluginInfo] = {}

    # ------------------------------------------------------------------
    # Discovery
    # ------------------------------------------------------------------

    def discover_plugins(self, plugin_dir: str = "plugins") -> List[PluginInfo]:
        """Scan *plugin_dir* for plugins, load each one found.

        Returns the list of all discovered ``PluginInfo`` objects (including
        ones that failed to load — check ``PluginInfo.error``).
        """
        plugins_path = Path(plugin_dir)
        if not plugins_path.is_dir():
            logger.info("Plugin directory '%s' does not exist — skipping.", plugin_dir)
            return []

        found: List[PluginInfo] = []
        for entry in sorted(plugins_path.iterdir()):
            if entry.is_dir() and not entry.name.startswith("_"):
                info = self._load_plugin(entry)
                if info is not None:
                    self._plugins[info.name] = info
                    found.append(info)

        logger.info(
            "Plugin discovery complete: %d found, %d loaded.",
            len(found),
            sum(1 for p in found if p.loaded),
        )
        return found

    # ------------------------------------------------------------------
    # Access
    # ------------------------------------------------------------------

    def get_loaded_plugins(self) -> List[PluginInfo]:
        """Return only successfully loaded plugins."""
        return [p for p in self._plugins.values() if p.loaded]

    def get_all_plugins(self) -> List[PluginInfo]:
        """Return every discovered plugin (loaded or not)."""
        return list(self._plugins.values())

    def get_plugin(self, name: str) -> Optional[PluginInfo]:
        """Return a plugin by its PLUGIN_NAME, or ``None``."""
        return self._plugins.get(name)

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def shutdown_all(self) -> None:
        """Call ``shutdown()`` on every loaded plugin."""
        for info in self.get_loaded_plugins():
            try:
                if info.instance is not None and hasattr(info.instance, "shutdown"):
                    info.instance.shutdown()
                info.loaded = False
            except Exception as exc:  # noqa: BLE001
                logger.warning("Error shutting down plugin '%s': %s", info.name, exc)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _load_plugin(self, directory: Path) -> Optional[PluginInfo]:
        """Load a single plugin from *directory*."""
        init_path = directory / "__init__.py"
        plugin_path = directory / "plugin.py"

        if not init_path.exists():
            logger.debug("Skipping '%s': no __init__.py.", directory.name)
            return None
        if not plugin_path.exists():
            logger.debug("Skipping '%s': no plugin.py.", directory.name)
            return None

        # Read metadata from __init__.py
        try:
            spec = importlib.util.spec_from_file_location(
                f"plugins.{directory.name}.__init__", init_path
            )
            if spec is None or spec.loader is None:
                raise ImportError("Cannot create module spec")
            meta_module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(meta_module)  # type: ignore[union-attr]
        except Exception as exc:  # noqa: BLE001
            logger.warning("Cannot import metadata for '%s': %s", directory.name, exc)
            return None

        name: str = getattr(meta_module, "PLUGIN_NAME", directory.name)
        version: str = getattr(meta_module, "PLUGIN_VERSION", "0.0.0")
        description: str = getattr(meta_module, "PLUGIN_DESCRIPTION", "")

        info = PluginInfo(
            name=name,
            version=version,
            description=description,
            directory=directory,
        )

        # Load the plugin class from plugin.py
        try:
            spec = importlib.util.spec_from_file_location(
                f"plugins.{directory.name}.plugin", plugin_path
            )
            if spec is None or spec.loader is None:
                raise ImportError("Cannot create module spec for plugin.py")
            plugin_module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(plugin_module)  # type: ignore[union-attr]
        except Exception as exc:  # noqa: BLE001
            info.error = str(exc)
            logger.warning("Cannot load plugin.py for '%s': %s", name, exc)
            return info

        # Find the plugin class: first class with an ``initialize`` method
        plugin_class = self._find_plugin_class(plugin_module)
        if plugin_class is None:
            info.error = "No class with initialize() found in plugin.py"
            logger.warning("Plugin '%s': %s", name, info.error)
            return info

        # Instantiate and call initialize()
        try:
            instance = plugin_class()
            instance.initialize(self._ide)
            info.instance = instance
            info.loaded = True
            logger.info("Loaded plugin '%s' v%s.", name, version)
        except Exception as exc:  # noqa: BLE001
            info.error = str(exc)
            logger.warning("Error initializing plugin '%s': %s", name, exc)

        return info

    @staticmethod
    def _find_plugin_class(module: Any) -> Optional[type]:
        """Return the first class in *module* that has an ``initialize`` method."""
        for attr_name in dir(module):
            attr = getattr(module, attr_name)
            if (
                isinstance(attr, type)
                and hasattr(attr, "initialize")
                and callable(getattr(attr, "initialize"))
            ):
                return attr
        return None
