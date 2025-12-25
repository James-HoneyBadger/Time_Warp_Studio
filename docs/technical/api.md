# API Reference

Complete reference for the Time Warp IDE's interpreter API.

## Core Interpreter Class

The main `TimeWarpInterpreter` class handles all language execution and provides a unified interface.

### Initialization

```python
from time_warp.core.interpreter import TimeWarpInterpreter

interpreter = TimeWarpInterpreter(unified_canvas)
```

### Executing Code

```python
result = interpreter.execute(code, language='BASIC')
```

**Parameters:**
- `code` (str): Program source code
- `language` (str): Language name ('BASIC', 'PILOT', 'LOGO', 'PASCAL', 'PROLOG', 'FORTH', 'C')

**Returns:** Execution result object with status and output

### Result Object

```python
result = interpreter.execute(code, language)
result.status        # 'success' or 'error'
result.output        # Text output from program
result.error_msg     # Error message if failed
result.graphics      # List of graphics commands
result.variables     # Current variable state
result.execution_time # Execution duration in ms
```

## Language Executors

Each language has a dedicated executor implementing common interface:

### Executor Interface

```python
class LanguageExecutor:
    def execute_command(self, command: str) -> str:
        """Execute a single command and return output."""
        pass
    
    def execute_program(self, code: str) -> str:
        """Execute complete program and return output."""
        pass
    
    def get_state(self) -> dict:
        """Get current interpreter state (variables, etc)."""
        pass
    
    def reset(self):
        """Reset executor to initial state."""
        pass
```

### Available Executors

**BASIC:**
```python
from time_warp.core.interpreters.basic_executor import BasicExecutor
executor = BasicExecutor(interpreter)
```

**PILOT:**
```python
from time_warp.core.interpreters.pilot_executor import PilotExecutor
executor = PilotExecutor(interpreter)
```

**Logo:**
```python
from time_warp.core.interpreters.logo_executor import LogoExecutor
executor = LogoExecutor(interpreter)
```

**Pascal:**
```python
from time_warp.core.interpreters.pascal_executor import PascalExecutor
executor = PascalExecutor(interpreter)
```

**Prolog:**
```python
from time_warp.core.interpreters.prolog_executor import PrologExecutor
executor = PrologExecutor(interpreter)
```

**Forth:**
```python
from time_warp.core.interpreters.forth_executor import ForthExecutor
executor = ForthExecutor(interpreter)
```

**C:**
```python
from time_warp.core.interpreters.c_executor import CExecutor
executor = CExecutor(interpreter)
```

## Output Format

All executors produce output with standardized emoji prefixes:

```
✅ Execution successful
❌ Error occurred  
ℹ️ Information message
🎨 Graphics/canvas command
🚀 Program execution event
🐢 Turtle graphics command
📝 Input prompt
```

**Example output:**
```
✅ Program executed
🐢 FORWARD 100
🐢 RIGHT 90
📝 Enter your name: 
```

## Graphics Commands

### DrawCommand Format

```python
{
    'type': 'line' | 'circle' | 'rectangle' | 'polygon' | 'text' | 'fill',
    'x': float,
    'y': float,
    'color': (r, g, b),  # RGB tuple 0-255
    'params': {...}       # Type-specific parameters
}
```

### Line Command

```python
{
    'type': 'line',
    'x': 100,
    'y': 200,
    'x2': 300,
    'y2': 400,
    'color': (255, 0, 0),
    'width': 2
}
```

### Circle Command

```python
{
    'type': 'circle',
    'x': 100,
    'y': 100,
    'radius': 50,
    'color': (0, 255, 0),
    'fill': True
}
```

### Rectangle Command

```python
{
    'type': 'rectangle',
    'x': 50,
    'y': 50,
    'width': 200,
    'height': 150,
    'color': (0, 0, 255),
    'fill': False
}
```

### Text Command

```python
{
    'type': 'text',
    'x': 100,
    'y': 100,
    'text': 'Hello World',
    'color': (0, 0, 0),
    'size': 12
}
```

## Safe Expression Evaluation

Safely evaluate mathematical expressions:

```python
from time_warp.core.safe_expression_evaluator import safe_eval

result = safe_eval("2 + 3 * X", {"X": 5})
# Result: 17

variables = {"A": 10, "B": 20}
result = safe_eval("(A + B) * 2", variables)
# Result: 60
```

**Supported operations:**
- Arithmetic: `+`, `-`, `*`, `/`, `//`, `%`, `**`
- Comparison: `>`, `<`, `>=`, `<=`, `==`, `!=`
- Logical: `and`, `or`, `not`
- Functions: `abs()`, `min()`, `max()`, `round()`, `sqrt()`

## Turtle Graphics API

### Turtle State

```python
executor.turtle_x       # X position
executor.turtle_y       # Y position
executor.turtle_angle   # Direction (0-360 degrees)
executor.pen_down       # Boolean: is pen down?
executor.pen_color      # (r, g, b) tuple
executor.pen_width      # Line width in pixels
```

### Turtle Commands

```python
# Movement
executor.forward(distance)
executor.backward(distance)
executor.right(angle)
executor.left(angle)

# Pen control
executor.pen_up()
executor.pen_down()
executor.set_color(r, g, b)
executor.set_width(width)

# Canvas
executor.clear_canvas()
executor.home()  # Return to origin
```

## Plugin System

### Creating a Plugin

```python
# plugins/my_plugin/__init__.py
PLUGIN_NAME = "My Plugin"
PLUGIN_VERSION = "1.0.0"
PLUGIN_DESCRIPTION = "Does something cool"

# plugins/my_plugin/plugin.py
class MyPlugin:
    def initialize(self, ide_instance):
        """Called when IDE initializes plugin."""
        self.ide = ide_instance
        self.register_menu_item("Tools", "My Tool", self.on_menu_click)
    
    def on_menu_click(self):
        """Handle menu item click."""
        code = self.ide.get_current_code()
        result = self.ide.execute(code)
        self.ide.show_result(result)
```

### Loading Plugins

```python
from time_warp.core.plugin_system import PluginManager

manager = PluginManager(interpreter)
manager.discover_plugins('plugins/')
manager.load_plugin('my_plugin')
```

## Async Execution

Run code without blocking UI:

```python
from time_warp.core.async_support import get_async_runner

runner = get_async_runner()

def on_complete(result):
    print(f"Execution complete: {result.status}")

runner.run_code(source_code, language='BASIC', callback=on_complete)
```

## Hardware/IoT APIs

### Arduino Simulation

```python
from time_warp.core.hardware.arduino_controller import ArduinoController

arduino = ArduinoController(simulation_mode=True)

# Digital I/O
arduino.set_pin(13, arduino.HIGH)      # Turn on LED
arduino.read_pin(2)                    # Read digital input

# Analog I/O
arduino.analog_write(3, 255)           # PWM
arduino.analog_read(0)                 # Analog input
```

### Raspberry Pi GPIO

```python
from time_warp.core.hardware.rpi_controller import RPiController

rpi = RPiController(simulation_mode=True)

rpi.setup_pin(17, rpi.OUT)
rpi.set_pin(17, rpi.HIGH)
rpi.cleanup()
```

## Configuration

### Reading Settings

```python
from time_warp.core.config import ConfigManager

config = ConfigManager()
theme = config.get('theme', 'dracula')
font_size = config.get('editor_font_size', 12)
```

### Writing Settings

```python
config.set('theme', 'monokai')
config.set('editor_font_size', 14)
config.save()
```

## Error Handling

### Catching Errors

```python
from time_warp.core.exceptions import (
    SyntaxError,
    RuntimeError,
    DivisionByZeroError
)

try:
    result = interpreter.execute(code, language)
except SyntaxError as e:
    print(f"Syntax error: {e.message}")
    print(f"Line {e.line}: {e.line_content}")
except RuntimeError as e:
    print(f"Runtime error: {e.message}")
```

### Error Object Structure

```python
error = {
    'type': 'SyntaxError',
    'message': 'Unexpected token',
    'line': 5,
    'column': 10,
    'line_content': 'PRINT "Hello',
    'context': ['LINE 4', 'LINE 5 <- ERROR', 'LINE 6']
}
```

## Complete Example: Custom Executor

```python
from time_warp.core.interpreters import LanguageExecutor

class CustomLangExecutor(LanguageExecutor):
    def __init__(self, interpreter):
        self.interpreter = interpreter
        self.variables = {}
        self.output = ""
    
    def execute_command(self, command: str) -> str:
        parts = command.split()
        
        if parts[0].upper() == "PRINT":
            text = ' '.join(parts[1:])
            self.output += text + '\n'
            return "✅ Output printed\n" + self.output
        
        elif parts[0].upper() == "SET":
            var_name = parts[1]
            value = ' '.join(parts[2:])
            self.variables[var_name] = value
            return f"ℹ️ {var_name} = {value}\n"
        
        return "❌ Unknown command\n"
    
    def execute_program(self, code: str) -> str:
        self.output = ""
        self.variables = {}
        
        lines = code.strip().split('\n')
        for line in lines:
            if line.strip():
                self.execute_command(line)
        
        return self.output
    
    def get_state(self) -> dict:
        return {
            'variables': self.variables,
            'output': self.output
        }
    
    def reset(self):
        self.variables = {}
        self.output = ""
```

## Testing Framework

### Unit Test Example

```python
import pytest
from time_warp.core.interpreter import TimeWarpInterpreter

def test_basic_addition():
    code = '''
    X = 5
    Y = 3
    PRINT X + Y
    '''
    
    result = interpreter.execute(code, 'BASIC')
    assert result.status == 'success'
    assert '8' in result.output
```

### Integration Test Example

```python
def test_multi_language_program():
    """Test combining multiple languages."""
    logo_code = "FORWARD 100\nRIGHT 90"
    basic_code = "X = 10\nPRINT X"
    
    logo_result = interpreter.execute(logo_code, 'LOGO')
    basic_result = interpreter.execute(basic_code, 'BASIC')
    
    assert logo_result.status == 'success'
    assert basic_result.status == 'success'
```

## Common Patterns

### Validating Input

```python
def validate_number(value):
    try:
        return float(value)
    except ValueError:
        raise RuntimeError(f"Invalid number: {value}")

def validate_color(r, g, b):
    for value in [r, g, b]:
        if not (0 <= value <= 255):
            raise ValueError(f"Color value out of range: {value}")
```

### Building Output

```python
def format_output(status, message, data=None):
    emoji = "✅" if status == "success" else "❌"
    output = f"{emoji} {message}"
    
    if data:
        for key, value in data.items():
            output += f"\n{key}: {value}"
    
    return output
```

## Performance Tips

1. **Cache parsing results** - Don't reparse unchanged code
2. **Use safe_eval for math** - Faster than eval()
3. **Batch graphics commands** - Send multiple at once
4. **Reset state between programs** - Avoid memory leaks
5. **Profile with cProfile** - Find bottlenecks

---

For more details, see [Technical Reference](README.md) or the source code in `Platforms/Python/time_warp/core/`.
