def no_errors(output: str) -> bool:
    """Check if the output contains no error indicators."""
    return "❌" not in output
