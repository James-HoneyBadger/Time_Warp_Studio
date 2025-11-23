"""Shim to delegate C execution to the fixed implementation."""

from .c_lang_fixed import execute_c  # re-export for compatibility

__all__ = ["execute_c"]

# Reference to satisfy static checkers about usage
_execute_c_ref = execute_c
