"""
Plugin system for Time Warp IDE
Enables community extensions and custom functionality
"""

import importlib.util
import json
import sys
import traceback
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, cast


@dataclass
class PluginManifest:  # pylint: disable=too-many-instance-attributes
    """Plugin manifest information loaded from plugin.json"""

    name: str
    version: str
    description: str
    author: str
    entry_point: str
    dependencies: List[str]
    permissions: List[str]
    languages: List[str]  # Languages this plugin extends
    ui_hooks: List[str]  # UI integration points


class PluginContext:
    """Context provided to plugins during initialization"""

    def __init__(self, ide_instance):
        self.ide = ide_instance
        self.api_version = "1.0.0"
        self.plugin_dir = None

    def register_language_executor(self, language_name: str, executor_class):
        """Register a new language executor"""
        # This will be implemented when language system is extended

    def add_menu_item(self, menu_path: str, action_name: str, callback: Callable):
        """Add item to IDE menu"""
        # Implementation will hook into main window menu system

    def add_toolbar_button(self, icon: str, tooltip: str, callback: Callable):
        """Add button to toolbar"""
        # Implementation will hook into main window toolbar

    def register_file_handler(self, extension: str, handler_class):
        """Register handler for specific file types"""

    def get_setting(self, key: str, default=None):
        """Get plugin-specific setting"""
        # Future: Implement persistent settings storage
        _ = key  # Reserved for future implementation
        return default

    def set_setting(self, key: str, value):
        """Set plugin-specific setting"""


class PluginManager:
    """Manages plugin discovery, loading, and lifecycle"""

    def __init__(self, plugin_dirs: List[Path]):
        self.plugin_dirs = plugin_dirs
        self.loaded_plugins: Dict[str, Any] = {}
        self.plugin_manifests: Dict[str, PluginManifest] = {}
        self.plugin_contexts: Dict[str, PluginContext] = {}

    def discover_plugins(self) -> List[str]:
        """Discover available plugins in plugin directories"""
        discovered = []

        for plugin_dir in self.plugin_dirs:
            if not plugin_dir.exists():
                continue

            for item in plugin_dir.iterdir():
                if item.is_dir() and (item / "plugin.json").exists():
                    plugin_name = item.name
                    discovered.append(plugin_name)

        return discovered

    def load_plugin_manifest(self, plugin_name: str) -> Optional[PluginManifest]:
        """Load plugin manifest from plugin.json"""
        for plugin_dir in self.plugin_dirs:
            manifest_path = plugin_dir / plugin_name / "plugin.json"
            if manifest_path.exists():
                try:
                    with open(manifest_path, "r", encoding="utf-8") as f:
                        data = json.load(f)

                    return PluginManifest(
                        name=data.get("name", plugin_name),
                        version=data.get("version", "1.0.0"),
                        description=data.get("description", ""),
                        author=data.get("author", "Unknown"),
                        entry_point=data.get("entry_point", "__init__.py"),
                        dependencies=data.get("dependencies", []),
                        permissions=data.get("permissions", []),
                        languages=data.get("languages", []),
                        ui_hooks=data.get("ui_hooks", []),
                    )
                except (json.JSONDecodeError, KeyError) as e:
                    print(f"Error loading manifest for plugin {plugin_name}: {e}")
                    return None

        return None

    def validate_plugin(self, manifest: PluginManifest) -> bool:
        """Validate plugin manifest and dependencies"""
        # Check required fields
        if not manifest.name or not manifest.entry_point:
            return False

        # Check dependencies (basic validation)
        for _ in manifest.dependencies:
            # For now, just check if it's a known dependency
            # In future, could check installed packages
            pass

        return True

    # This method contains several early-return paths which are easier to
    # follow than a deeply nested single-return refactor so disable the
    # too-many-return-statements check locally.
    # pylint: disable=too-many-return-statements
    def load_plugin(self, plugin_name: str, ide_context) -> bool:
        """Load and initialize a plugin"""
        if plugin_name in self.loaded_plugins:
            return True  # Already loaded

        manifest = self.load_plugin_manifest(plugin_name)
        if not manifest:
            print(f"Failed to load manifest for plugin {plugin_name}")
            return False

        if not self.validate_plugin(manifest):
            print(f"Plugin {plugin_name} failed validation")
            return False

        # Find plugin directory
        plugin_path = None
        for plugin_dir in self.plugin_dirs:
            candidate = plugin_dir / plugin_name
            if candidate.exists():
                plugin_path = candidate
                break

        if not plugin_path:
            print(f"Plugin directory not found for {plugin_name}")
            return False

        # Load plugin module
        entry_path = plugin_path / manifest.entry_point
        if not entry_path.exists():
            print(f"Plugin entry point not found: {entry_path}")
            return False

        try:
            # Add plugin directory to Python path
            sys.path.insert(0, str(plugin_path))

            # Load module
            spec = importlib.util.spec_from_file_location(
                f"plugin_{plugin_name}", entry_path
            )
            if spec is None:
                print(f"Could not create module spec for {entry_path}")
                return False

            module = importlib.util.module_from_spec(spec)

            # Create plugin context
            context = PluginContext(ide_context)
            context.plugin_dir = plugin_path

            # Inject context into module safely (use setattr)
            setattr(module, "plugin_context", context)

            # Ensure we have a loader before executing the module
            loader = getattr(spec, "loader", None)
            if loader is None:
                print(f"No loader available for plugin {plugin_name}")
                return False

            # Register the module in sys.modules so imports inside the
            # plugin behave as expected and cleanup can find it by name.
            module_name = f"plugin_{plugin_name}"
            sys.modules[module_name] = module

            # Execute module using the loader
            loader.exec_module(module)

            # Store loaded plugin
            self.loaded_plugins[plugin_name] = module
            self.plugin_manifests[plugin_name] = manifest
            self.plugin_contexts[plugin_name] = context

            # Call initialize if available (callable)
            init_fn: Any = getattr(module, "initialize", None)
            if callable(init_fn):
                # mypy/pylint can't always infer the callable nature after
                # getattr; give it a typed reference before calling.
                init_callable = cast(Callable[[Any], Any], init_fn)
                try:
                    # Call the typed function reference with the IDE context
                    # pylint: disable=not-callable
                    init_callable(context)  # type: ignore[call-arg]
                    # pylint: enable=not-callable
                except Exception as err:  # pylint: disable=broad-except
                    # Reraise system-critical signals
                    if isinstance(err, (KeyboardInterrupt, SystemExit)):
                        raise
                    # Non-fatal: plugin level initialization errors should
                    # not take down the IDE; report and continue.
                    print(f"Plugin {plugin_name} initialize failed: {err}")

            print(f"Successfully loaded plugin: {plugin_name}")
            return True

        except (ImportError, AttributeError, RuntimeError) as e:
            print(f"Error loading plugin {plugin_name}: {e}")
            traceback.print_exc()
            return False

    def unload_plugin(self, plugin_name: str) -> bool:
        """Unload a plugin"""
        if plugin_name not in self.loaded_plugins:
            return True

        try:
            module = self.loaded_plugins[plugin_name]

            # Call cleanup if available
            if hasattr(module, "cleanup"):
                context = self.plugin_contexts.get(plugin_name)
                module.cleanup(context)

            # Remove from sys.modules
            module_name = f"plugin_{plugin_name}"
            if module_name in sys.modules:
                del sys.modules[module_name]

            # Clean up references
            del self.loaded_plugins[plugin_name]
            del self.plugin_manifests[plugin_name]
            del self.plugin_contexts[plugin_name]

            print(f"Successfully unloaded plugin: {plugin_name}")
            return True

        except (AttributeError, KeyError) as e:
            print(f"Error unloading plugin {plugin_name}: {e}")
            return False

    def get_loaded_plugins(self) -> List[str]:
        """Get list of currently loaded plugins"""
        return list(self.loaded_plugins.keys())

    def get_plugin_info(self, plugin_name: str) -> Optional[Dict[str, Any]]:
        """Get information about a loaded plugin"""
        if plugin_name not in self.plugin_manifests:
            return None

        manifest = self.plugin_manifests[plugin_name]
        return {
            "name": manifest.name,
            "version": manifest.version,
            "description": manifest.description,
            "author": manifest.author,
            "languages": manifest.languages,
            "ui_hooks": manifest.ui_hooks,
        }

    def call_plugin_hook(self, hook_name: str, *args, **kwargs):
        """Call a hook on all loaded plugins"""
        results = []
        for plugin_name, module in self.loaded_plugins.items():
            if hasattr(module, hook_name):
                try:
                    result = getattr(module, hook_name)(*args, **kwargs)
                    results.append((plugin_name, result))
                except (AttributeError, TypeError) as e:
                    print(
                        f"Error calling hook {hook_name} on plugin "
                        f"{plugin_name}: {e}"
                    )

        return results
