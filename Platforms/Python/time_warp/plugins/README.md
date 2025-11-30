# Time Warp IDE Plugin System

The Time Warp IDE plugin system enables community extensions and custom functionality, transforming the educational IDE into a professional development platform.

## Overview

Plugins are Python modules that extend IDE functionality through:
- Custom language support
- UI enhancements (menus, toolbars, panels)
- File type handlers
- Code analysis and refactoring tools
- Hardware integrations
- Theme and customization options

## Plugin Structure

Each plugin is a directory containing:
```
my_plugin/
├── plugin.json      # Plugin manifest
├── __init__.py      # Main plugin code
└── [other files]    # Additional resources
```

## Plugin Manifest (plugin.json)

```json
{
  "name": "My Plugin",
  "version": "1.0.0",
  "description": "Description of what the plugin does",
  "author": "Your Name",
  "entry_point": "__init__.py",
  "dependencies": ["required_package>=1.0.0"],
  "permissions": ["ui_access", "file_access"],
  "languages": ["basic", "logo"],
  "ui_hooks": ["menu_bar", "toolbar"]
}
```

### Manifest Fields

- **name**: Display name of the plugin
- **version**: Semantic version string
- **description**: Brief description
- **author**: Plugin author/organization
- **entry_point**: Main Python file (usually `__init__.py`)
- **dependencies**: Python packages required
- **permissions**: Access permissions needed
- **languages**: Languages this plugin extends
- **ui_hooks**: UI integration points

## Plugin API

### Initialization

```python
def initialize(context):
    """Called when plugin is loaded"""
    # Access to IDE functionality through context
    context.add_menu_item("Tools/My Tool", "Run", my_function)
    return True

def cleanup(context):
    """Called when plugin is unloaded"""
    # Clean up resources
    pass
```

### Context API

The plugin context provides access to IDE functionality:

```python
# UI Integration
context.add_menu_item(menu_path, action_name, callback)
context.add_toolbar_button(icon, tooltip, callback)

# Language Support
context.register_language_executor(language_name, executor_class)
context.register_file_handler(extension, handler_class)

# Settings
value = context.get_setting(key, default)
context.set_setting(key, value)
```

### Hook Functions

Plugins can define hook functions that are called by the IDE:

```python
def on_file_open(file_path):
    """Called when a file is opened"""
    pass

def on_code_execute(code):
    """Called when code is executed"""
    pass

def on_project_load(project_path):
    """Called when a project is loaded"""
    pass
```

## Sample Plugin

See `sample_plugin/` for a complete working example that demonstrates:
- Plugin initialization and cleanup
- Menu and toolbar integration
- Hook functions
- Proper manifest configuration

## Development Guidelines

### Best Practices

1. **Error Handling**: Always handle exceptions gracefully
2. **Resource Management**: Clean up resources in `cleanup()`
3. **Documentation**: Document all public functions
4. **Compatibility**: Test with different IDE versions
5. **Security**: Validate inputs and avoid unsafe operations

### Permissions

Plugins can request permissions for enhanced functionality:
- `ui_access`: Modify UI elements
- `file_access`: Read/write files
- `network_access`: Make network requests
- `system_access`: Execute system commands

### Testing

Test plugins by:
1. Installing in `time_warp/plugins/`
2. Restarting the IDE
3. Checking plugin manager logs
4. Verifying functionality

## Plugin Manager

The plugin system is managed by `PluginManager` class which:
- Discovers plugins in configured directories
- Validates manifests and dependencies
- Loads/unloads plugins safely
- Provides plugin information and control

## Distribution

Plugins can be distributed as:
- Git repositories for direct cloning
- ZIP archives for manual installation
- Future: Plugin marketplace/registry

## Future Enhancements

Planned features:
- Plugin marketplace
- Automatic updates
- Dependency management
- Sandboxed execution
- Plugin development tools</content>
<parameter name="filePath">/home/james/Time_Warp_Studio-4.0.0/Platforms/Python/time_warp/plugins/README.md