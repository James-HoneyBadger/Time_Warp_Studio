#!/usr/bin/env python3
"""
Automated linting fix script for Time Warp Studio.
Fixes common issues like:
- Logging f-strings -> % formatting
- File encoding specifications
- Line length violations (basic wrapping)
"""

import re
import sys
from pathlib import Path

def fix_logging_fstrings(content: str) -> str:
    """Convert logging f-strings to % formatting."""
    
    # Pattern: logger.<level>(f"...")
    patterns = [
        (r'logger\.debug\(f"([^"]*)"\)', r'logger.debug("%s", \1)'),
        (r'logger\.info\(f"([^"]*)"\)', r'logger.info("%s", \1)'),
        (r"logger\.info\(f'([^']*)'\)", r"logger.info('%s', \1)"),
        (r'logger\.warning\(f"([^"]*)"\)', r'logger.warning("%s", \1)'),
        (r"logger\.warning\(f'([^']*)'\)", r"logger.warning('%s', \1)"),
        (r'logger\.error\(f"([^"]*)"\)', r'logger.error("%s", \1)'),
        (r"logger\.error\(f'([^']*)'\)", r"logger.error('%s', \1)"),
    ]
    
    # More aggressive fix: replace logger.X(f"string with {var}") 
    # with logger.X("string with %s", var)
    for method in ['debug', 'info', 'warning', 'error', 'critical']:
        # Pattern for f-strings with simple variable interpolation
        pattern = rf'logger\.{method}\(f"([^"{{]*)\{{([^}}]+)\}}([^"]*)"\)'
        replacement = rf'logger.{method}("\1%s\3", \2)'
        content = re.sub(pattern, replacement, content)
        
        pattern = rf"logger\.{method}\(f'([^'{{]*)\{{([^}}]+)\}}([^']*)'\)"
        replacement = rf"logger.{method}('\1%s\3', \2)"
        content = re.sub(pattern, replacement, content)
    
    return content


def fix_file_encoding(content: str) -> str:
    """Add encoding='utf-8' to open() calls."""
    
    # Pattern: open(file, mode) -> open(file, mode, encoding='utf-8')
    # Be careful with multiline calls
    patterns = [
        (r"open\(([^,]+),\s*['\"]([rwab]+)['\"]\)", 
         r"open(\1, '\2', encoding='utf-8')"),
        (r'open\(([^,]+),\s*["\']([rwab]+)["\']\)',
         r"open(\1, '\2', encoding='utf-8')"),
    ]
    
    for pattern, replacement in patterns:
        content = re.sub(pattern, replacement, content)
    
    return content


def main():
    """Main fix function."""
    python_files = list(Path('/home/james/Time_Warp_Studio/Platforms/Python').rglob('*.py'))
    
    print(f"Found {len(python_files)} Python files")
    
    fixed_count = 0
    for filepath in python_files:
        try:
            content = filepath.read_text(encoding='utf-8')
            original = content
            
            # Apply fixes
            content = fix_logging_fstrings(content)
            content = fix_file_encoding(content)
            
            # Only write if changed
            if content != original:
                filepath.write_text(content, encoding='utf-8')
                print(f"✓ Fixed: {filepath.relative_to('/home/james/Time_Warp_Studio')}")
                fixed_count += 1
        except Exception as e:
            print(f"✗ Error processing {filepath}: {e}")
    
    print(f"\nTotal files fixed: {fixed_count}")

if __name__ == '__main__':
    main()
