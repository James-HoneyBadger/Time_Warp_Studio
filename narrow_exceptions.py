#!/usr/bin/env python3
"""Narrow broad exception handlers to specific exception types."""

import re
from pathlib import Path


def narrow_exceptions():
    """Replace broad Exception catches with specific types."""
    
    # Pattern: except Exception ... in routes (database/service patterns)
    pattern = r'except Exception(\s+as\s+\w+)?:'
    
    # Safe narrowing based on context
    routes_files = [
        'Platforms/Python/time_warp/routes/rooms.py',
        'Platforms/Python/time_warp/routes/users.py',
        'Platforms/Python/time_warp/routes/sync.py',
        'Platforms/Python/time_warp/routes/collaboration.py',
    ]
    
    # For routes, narrow to (ValueError, KeyError, AttributeError, TypeError)
    routes_replacement = r'except (ValueError, KeyError, AttributeError, TypeError)\1:'
    
    count = 0
    for file_path in routes_files:
        try:
            path = Path(file_path)
            if not path.exists():
                continue
                
            content = path.read_text(encoding='utf-8')
            original = content
            
            # Simple narrowing for route handlers
            content = re.sub(
                r'except Exception(\s+as\s+\w+)?:',
                r'except (ValueError, KeyError, AttributeError, TypeError)\1:',
                content
            )
            
            if content != original:
                path.write_text(content, encoding='utf-8')
                count += len(re.findall(pattern, original))
                print(f'✓ {file_path}')
        
        except Exception as e:
            print(f'⚠️  Error processing {file_path}: {e}')
    
    print(f'\n✅ Narrowed {count} broad exception handlers')


if __name__ == '__main__':
    narrow_exceptions()
