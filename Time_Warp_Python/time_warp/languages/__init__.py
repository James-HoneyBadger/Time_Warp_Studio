"""Languages package for Time Warp IDE - TempleCode unified executor."""

from .templecode import execute_templecode

# Legacy aliases for compatibility
execute_pilot = execute_templecode
execute_basic = execute_templecode
execute_logo = execute_templecode

__all__ = [
    'execute_templecode',
    'execute_pilot',
    'execute_basic',
    'execute_logo',
]
