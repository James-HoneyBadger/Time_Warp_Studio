# Troubleshooting Guide

This guide provides solutions to common issues encountered while using Time Warp Studio. Follow the steps below to resolve problems effectively.

---

## General Issues

### Application Fails to Start
- **Cause**: Missing dependencies or incompatible Python version.
- **Solution**:
  1. Ensure Python 3.10+ is installed.
  2. Run `pip install -r Platforms/Python/requirements.txt` to install dependencies.
  3. Check for error messages in the terminal.

### UI Elements Not Displaying Correctly
- **Cause**: Missing or corrupted PySide6 installation.
- **Solution**:
  1. Reinstall PySide6: `pip install --force-reinstall PySide6`.
  2. Verify your system meets the hardware requirements.

---

## Language-Specific Issues

### BASIC
- **Issue**: Syntax errors in valid programs.
- **Cause**: Unsupported features or incorrect line numbers.
- **Solution**:
  1. Check the [BASIC Language Guide](LANGUAGE_GUIDE.md#basic).
  2. Ensure line numbers are sequential.

### Python
- **Issue**: Import errors in Python scripts.
- **Cause**: Python programs run in a sandbox that only allows a safe set of standard-library modules.
- **Solution**:
  1. Use only the allowed standard-library modules (e.g. `math`, `random`, `collections`, `itertools`, `json`, `re`, `datetime`).
  2. Run larger scripts that need third-party libraries outside the IDE.

---

## Performance Issues

### Slow Turtle Graphics Rendering
- **Cause**: Complex drawings or high-resolution canvas.
- **Solution**:
  1. Simplify the drawing commands.
  2. Reduce loop counts or step sizes.

### High CPU Usage
- **Cause**: Intensive computations or infinite loops.
- **Solution**:
  1. Optimize your code.
  2. Press **Ctrl+F2** (Reset Program) to stop a runaway program.
  3. Use the debugger to identify bottlenecks.

---

## Debugging Tips

- Use the built-in debugger to step through code.
- Check the output panel for error messages.
- Refer to the [User Guide](USER_GUIDE.md) for detailed instructions.

---

For further help, see the [FAQ](../FAQ.md).
