# Plugin Guide for Time Warp Studio

Time Warp Studio supports a simple file-system plugin architecture that lets you add new IDE features, custom language executors, output post-processors, or any Python-based extension without modifying the core codebase.

---

## Quick Start

1. Create a directory inside `plugins/` (in your workspace root):

   ```
   plugins/
       my_plugin/
           __init__.py   ← metadata constants
           plugin.py     ← plugin class
   ```

2. Run Time Warp Studio — plugins are discovered automatically on startup.

   Or discover them programmatically:

   ```python
   from time_warp.features.plugin_system import PluginManager
   mgr = PluginManager(ide_instance=window)
   mgr.discover_plugins("plugins")
   ```

---

## Plugin Structure

### `__init__.py` — Metadata

```python
PLUGIN_NAME        = "My Plugin"          # required
PLUGIN_VERSION     = "1.0.0"              # required
PLUGIN_DESCRIPTION = "Does something useful."  # optional
```

### `plugin.py` — Implementation

Your plugin file must contain **one class** with an `initialize(ide_instance)` method. The `shutdown()` method is optional but recommended for cleanup.

```python
class MyPlugin:
    """A minimal Time Warp Studio plugin."""

    def initialize(self, ide) -> None:
        """Called once after IDE startup.

        Args:
            ide: The main IDE window instance (may be None if loaded
                 outside the IDE, e.g. from tests).
        """
        print("✅ MyPlugin initialized!")
        # Store the IDE reference for later use
        self.ide = ide

    def shutdown(self) -> None:
        """Called when the plugin is unloaded (optional)."""
        print("ℹ️ MyPlugin shutting down.")
```

---

## Plugin Capabilities

Plugins receive the main IDE window as `ide` in `initialize()`. Through this reference you can:

| What to access | How |
|---|---|
| Current source code | `ide.editor.toPlainText()` |
| Output panel | `ide.output_panel.append_text("...")` |
| Canvas / turtle | `ide.canvas.turtle` |
| Current language | `ide.current_language` |
| Run a program | `ide.run_program()` |
| Add a menu action | `ide.menuBar().addMenu(...)` |
| Add a toolbar button | `ide.toolbar.addAction(...)` |

> **Note:** The exact attribute names depend on the IDE version. Always guard attribute access with `hasattr()` for forward compatibility.

---

## Installing a Plugin via CLI

```bash
# Copy plugin directory to plugins/ in the workspace root
python run.py --install-plugin /path/to/my_plugin

# Or just copy manually:
cp -r /path/to/my_plugin plugins/
```

---

## Example Plugins

See `Examples/plugins/` for complete working examples:

| Plugin | Description |
|--------|-------------|
| [`hello_plugin`](../Examples/plugins/hello_plugin/) | Minimal hello-world: prints a greeting on startup |

---

## Plugin Lifecycle

```
IDE starts
    │
    ├── PluginManager.discover_plugins("plugins")
    │       │
    │       ├── For each plugin/subdirectory:
    │       │     1. Import __init__.py → read metadata
    │       │     2. Import plugin.py → find plugin class
    │       │     3. Instantiate class
    │       │     4. Call instance.initialize(ide_instance)
    │       │
    │       └── Return List[PluginInfo]
    │
    ├── IDE runs ...
    │
    └── IDE closes
            │
            └── PluginManager.shutdown_all()
                    └── For each loaded plugin: call instance.shutdown()
```

---

## Error Handling

If a plugin fails to load (import error, missing method, exception in `initialize()`), it is recorded in `PluginInfo.error` and execution continues. No plugin failure can crash the IDE.

```python
for info in mgr.get_all_plugins():
    if info.loaded:
        print(f"✅ {info.name} v{info.version}")
    else:
        print(f"❌ {info.name}: {info.error}")
```

---

## Security Considerations

Plugins run as **trusted code** in the same Python process as the IDE. Only install plugins from sources you trust. Do not install plugins that:

- Call `eval()` or `exec()` on user-provided data
- Open network connections without disclosure
- Write files outside the workspace directory
- Import `os`, `subprocess`, or `sys` without a clear purpose

---

## API Reference

### `PluginManager`

| Method | Description |
|--------|-------------|
| `PluginManager(ide_instance=None)` | Create manager, optionally bound to IDE window |
| `discover_plugins(plugin_dir="plugins")` | Scan directory, load all valid plugins |
| `get_loaded_plugins()` | Return only successfully loaded `PluginInfo` objects |
| `get_all_plugins()` | Return all discovered plugins (loaded or failed) |
| `get_plugin(name)` | Return plugin by `PLUGIN_NAME`, or `None` |
| `shutdown_all()` | Call `shutdown()` on all loaded plugins |

### `PluginInfo`

| Attribute | Type | Description |
|-----------|------|-------------|
| `name` | `str` | Plugin name from `PLUGIN_NAME` |
| `version` | `str` | Plugin version |
| `description` | `str` | Optional description |
| `loaded` | `bool` | `True` if initialized successfully |
| `error` | `str \| None` | Error message if loading failed |
| `instance` | `Any` | The plugin class instance |
