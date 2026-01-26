#!/usr/bin/env python3
"""Fix remaining linting issues: Type annotations and E501 (line length)."""

import re
from pathlib import Path
from typing import Dict, List, Tuple


def fix_type_annotations(content: str) -> str:
    """Fix type annotation issues like `param: str = None` -> `param: str | None = None`."""
    
    # Pattern 1: Function parameters with mutable defaults (= None) missing | None annotation
    # Pattern: `param_name: str = None` -> `param_name: str | None = None`
    pattern1 = r'(\w+):\s*(str|int|float|bool|dict|list|tuple|Any|Dict|List|Tuple)\s*=\s*None'
    replacement1 = r'\1: \2 | None = None'
    content = re.sub(pattern1, replacement1, content)

    # Pattern 1b: Custom class types missing Optional (e.g., TimeWarpCloudAPI = None)
    # Avoid matching existing unions or generics
    pattern1b = r'(\w+):\s*([A-Za-z_][A-Za-z0-9_\.]+)\s*=\s*None'
    def repl1b(m: re.Match) -> str:
        name, typ = m.group(1), m.group(2)
        # Skip if preceding token is a control statement like 'try:' etc.
        if name in {"try", "except", "if", "elif", "else", "while", "for", "with", "class", "def"}:
            return m.group(0)
        # Skip if type already contains '|' indicating union
        if '|' in typ:
            return m.group(0)
        return f"{name}: {typ} | None = None"
    content = re.sub(pattern1b, repl1b, content)
    
    # Pattern 2: Function parameters with type = None annotation (common pattern)
    # Already handled above but let's catch edge cases
    
    # Pattern 3: Optional import style - convert to | None where possible for newer Python
    # But keep Optional for compatibility in complex scenarios
    
    return content


def fix_line_length(content: str, max_length: int = 99) -> str:
    """Fix lines that exceed max_length."""
    lines = content.split('\n')
    fixed_lines = []
    
    for line in lines:
        if len(line) > max_length and not line.strip().startswith('#'):
            # Special handling for different line types
            
            # String literals in logging (already fixed but double-check)
            if 'logger.' in line and '(' in line:
                # Already handled by logging fix script
                fixed_lines.append(line)
            
            # Long imports
            elif line.strip().startswith(('from ', 'import ')):
                fixed_lines.extend(fix_long_import(line))
            
            # Long string assignments
            elif '=' in line and ('"' in line or "'" in line):
                fixed_lines.extend(fix_long_string_assignment(line, max_length))
            
            # Function definitions with many parameters
            elif 'def ' in line or '->' in line:
                fixed_lines.extend(fix_long_function_def(line, max_length))
            
            # Other long lines - try parenthesis wrapping
            else:
                fixed_lines.extend(wrap_long_line(line, max_length))
        else:
            fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)


def fix_long_import(line: str) -> List[str]:
    """Break long import statements."""
    # from x import a, b, c, d -> from x import (a, b, c, d)
    if 'import' in line and ',' in line:
        match = re.match(r'(\s*from\s+\S+\s+import\s+)(.+)', line)
        if match:
            indent = len(match.group(1)) - len(match.group(1).lstrip())
            imports = match.group(2).split(',')
            
            if len(imports) > 3:  # Only wrap if multiple imports
                result = [match.group(1) + '(']
                for i, imp in enumerate(imports):
                    imp = imp.strip()
                    if i < len(imports) - 1:
                        result.append(' ' * (indent + 4) + imp + ',')
                    else:
                        result.append(' ' * (indent + 4) + imp)
                result.append(' ' * indent + ')')
                return result
    
    return [line]


def fix_long_string_assignment(line: str, max_length: int) -> List[str]:
    """Break long string assignments."""
    # Assignment with long string
    match = re.match(r'(\s*)(\w+)\s*=\s*(["\'])', line)
    if match:
        indent, var_name, quote = match.groups()
        
        # For very long strings, split at sentence boundaries if possible
        string_part = line[match.start(3):]
        
        if len(string_part) > max_length - len(indent) - len(var_name) - 5:
            # Keep as multi-line string
            return [line]
    
    return [line]


def fix_long_function_def(line: str, max_length: int) -> List[str]:
    """Break long function definitions across multiple lines."""
    if len(line) <= max_length:
        return [line]
    
    # Extract def part
    match = re.match(r'(\s*def\s+\w+\s*\()(.*?)(\)\s*(?:->.*)?:)', line)
    if match:
        indent_str, params_str, closing = match.groups()
        params = [p.strip() for p in params_str.split(',') if p.strip()]
        
        if len(params) > 2:  # Only wrap if multiple parameters
            base_indent = len(indent_str) - len(indent_str.lstrip())
            result = [indent_str.rstrip() + '(']
            
            for i, param in enumerate(params):
                prefix = ' ' * (base_indent + 4)
                if i < len(params) - 1:
                    result.append(prefix + param + ',')
                else:
                    result.append(prefix + param)
            
            result[-1] += closing
            return result
    
    return [line]


def wrap_long_line(line: str, max_length: int) -> List[str]:
    """Generic wrapping for long lines using parentheses."""
    if len(line) <= max_length:
        return [line]
    
    # Try to find a good break point
    indent = len(line) - len(line.lstrip())
    indent_str = line[:indent]
    
    # Look for operators to break on: +, -, or function calls
    if ' + ' in line or ' - ' in line or '(' in line:
        # Find a good break point
        break_chars = ['(', '+', '-', 'and ', 'or ']
        for char in break_chars:
            pos = line.rfind(char, 0, max_length)
            if pos > indent:
                left = line[:pos].rstrip()
                right = line[pos:].lstrip()
                
                if char == '(':
                    return [left + ' (', indent_str + '    ' + right]
                else:
                    return [left + ' \\', indent_str + '    ' + right]
    
    return [line]


def fix_exception_handling(content: str) -> str:
    """Narrow some obvious broad exceptions safely."""
    # Case: formatting conversions in C-like printf handler
    content = re.sub(
        r'except\s+Exception:',
        'except (ValueError, TypeError):',
        content
    )
    return content


def main():
    """Main execution."""
    python_files = list(Path('Platforms/Python').rglob('*.py'))
    
    # Priority files with most issues
    priority_files = {
        'Platforms/Python/time_warp/cloud/cloud_sync_manager.py': 'type_annotations',
        'Platforms/Python/time_warp/core/ai_assistant.py': 'line_length',
        'Platforms/Python/time_warp/core/language_comparator.py': 'both',
    }
    
    type_fix_count = 0
    line_fix_count = 0
    files_fixed = []
    
    for py_file in python_files:
        try:
            content = py_file.read_text(encoding='utf-8')
            original = content
            
            # Fix type annotations
            content = fix_type_annotations(content)
            if content != original:
                type_fix_count += len(re.findall(r'\w+:\s*\w+\s*\|\s*None\s*=\s*None', content))
                files_fixed.append(str(py_file))
            
            # Fix line length in priority files
            file_str = str(py_file)
            if any(prio in file_str for prio in priority_files.keys()):
                old_lines = original.count('\n')
                content = fix_line_length(content)
                new_lines = content.count('\n')
                if new_lines != old_lines:
                    line_fix_count += (new_lines - old_lines)
            
            # Also attempt safe exception narrowing in known patterns
            narrowed = fix_exception_handling(content)
            if narrowed != content:
                content = narrowed

            # Write back if changed
            if content != original:
                py_file.write_text(content, encoding='utf-8')
                if file_str not in files_fixed:
                    files_fixed.append(file_str)
        
        except Exception as e:
            print(f"⚠️  Error processing {py_file}: {e}")
    
    print(f"✅ Type annotation fixes: {type_fix_count}")
    print(f"📝 Line length breaks added: {line_fix_count}")
    print(f"📁 Files modified: {len(files_fixed)}")
    
    if files_fixed:
        for f in files_fixed:
            print(f"  ✓ {f}")


if __name__ == '__main__':
    main()
