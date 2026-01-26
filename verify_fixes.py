#!/usr/bin/env python3
"""Verify linting fixes were applied correctly."""
import re
from pathlib import Path

def verify_fixes():
    fixed_count = 0
    still_broken = 0
    files_with_fixes = 0
    
    for py_file in Path('Platforms/Python').rglob('*.py'):
        try:
            content = py_file.read_text(errors='ignore')
            
            # Check for f-string logging (should be gone)
            fstring_pattern = r'logger\.(debug|info|warning|error|critical)\(f["\']'
            fstring_logs = len(re.findall(fstring_pattern, content))
            if fstring_logs > 0:
                still_broken += fstring_logs
                
            # Check for % formatting logging (should be present)
            percent_pattern = r'logger\.(debug|info|warning|error|critical)\(["\'].*%s'
            percent_logs = len(re.findall(percent_pattern, content))
            if percent_logs > 0:
                fixed_count += percent_logs
                files_with_fixes += 1
        except Exception as e:
            pass
    
    print(f'✓ Fixed logging calls (% format): {fixed_count}')
    print(f'⚠ Remaining f-string logs: {still_broken}')
    print(f'📊 Files with fixed logging: {files_with_fixes}')
    
    return still_broken == 0

if __name__ == '__main__':
    success = verify_fixes()
    exit(0 if success else 1)
