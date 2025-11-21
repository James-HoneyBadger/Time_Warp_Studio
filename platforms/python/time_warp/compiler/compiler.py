"""TempleCode compiler for Time Warp IDE.

Compiles TempleCode programs to C and then to native executables.
"""

import os
import subprocess
import tempfile
from typing import Optional


def compile_to_c(source_code: str) -> str:
    """Compile TempleCode source to C code.
    
    Args:
        source_code: TempleCode program source
        
    Returns:
        Generated C code
        
    Note:
        This is a simplified transpiler. Full implementation would
        require complete parsing and semantic analysis.
    """
    # C program template
    c_template = """
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Variable storage */
double vars[256];  /* Numeric variables A-Z, etc. */
char str_vars[256][256];  /* String variables */

int main() {
    /* Initialize variables */
    memset(vars, 0, sizeof(vars));
    memset(str_vars, 0, sizeof(str_vars));
    
    /* Generated code */
{generated_code}
    
    return 0;
}
"""
    
    # Parse and translate TempleCode to C
    lines = source_code.split('\n')
    c_code_lines = []
    
    for line in lines:
        line = line.strip()
        
        if not line or line.startswith('REM'):
            # Skip empty lines and comments
            continue
        
        # Remove line numbers if present
        parts = line.split(maxsplit=1)
        if parts and parts[0].isdigit():
            line = parts[1] if len(parts) > 1 else ""
        
        line_upper = line.upper()
        
        # Translate PRINT statements
        if line_upper.startswith('PRINT '):
            content = line[6:].strip()
            if content.startswith('"') and content.endswith('"'):
                # String literal
                c_code_lines.append(
                    f'    printf("{content[1:-1]}\\n");'
                )
            else:
                # Variable or expression
                c_code_lines.append(
                    f'    printf("%g\\n", {_translate_expression(content)});'
                )
        
        # Translate LET statements
        elif line_upper.startswith('LET ') or '=' in line:
            if line_upper.startswith('LET '):
                line = line[4:]
            
            if '=' in line:
                parts = line.split('=', 1)
                var_name = parts[0].strip().upper()
                expr = parts[1].strip()
                
                var_index = _var_to_index(var_name)
                c_expr = _translate_expression(expr)
                c_code_lines.append(
                    f'    vars[{var_index}] = {c_expr};'
                )
        
        # Translate INPUT statements
        elif line_upper.startswith('INPUT '):
            var_name = line[6:].strip().upper()
            var_index = _var_to_index(var_name)
            c_code_lines.append(
                f'    printf("? ");'
            )
            c_code_lines.append(
                f'    scanf("%lf", &vars[{var_index}]);'
            )
    
    # Generate final C code
    generated = '\n'.join(c_code_lines)
    return c_template.format(generated_code=generated)


def _var_to_index(var_name: str) -> int:
    """Convert variable name to array index.
    
    Args:
        var_name: Variable name (A, B, X, etc.)
        
    Returns:
        Array index (0-25 for A-Z)
    """
    if len(var_name) == 1 and var_name.isalpha():
        return ord(var_name) - ord('A')
    return 0


def _translate_expression(expr: str) -> str:
    """Translate TempleCode expression to C expression.
    
    Args:
        expr: TempleCode expression
        
    Returns:
        C expression
        
    Note:
        Simplified translation. Full version would use proper parser.
    """
    # Replace variable names with array access
    result = expr
    for letter in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
        if letter in result:
            idx = ord(letter) - ord('A')
            result = result.replace(letter, f'vars[{idx}]')
    
    # Replace ^ with pow()
    if '^' in result:
        # Simple case: A^B becomes pow(A, B)
        parts = result.split('^')
        if len(parts) == 2:
            result = f'pow({parts[0].strip()}, {parts[1].strip()})'
    
    return result


def compile_to_executable(
    source_code: str,
    output_path: str,
    compiler: str = 'gcc'
) -> bool:
    """Compile TempleCode to native executable.
    
    Args:
        source_code: TempleCode program source
        output_path: Path for output executable
        compiler: C compiler to use (gcc, clang, etc.)
        
    Returns:
        True if compilation succeeded, False otherwise
    """
    # Generate C code
    c_code = compile_to_c(source_code)
    
    # Create temporary C file
    with tempfile.NamedTemporaryFile(
        mode='w',
        suffix='.c',
        delete=False
    ) as f:
        c_file = f.name
        f.write(c_code)
    
    try:
        # Compile with system C compiler
        cmd = [compiler, c_file, '-o', output_path, '-lm']
        
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=30
        )
        
        if result.returncode == 0:
            return True
        else:
            print(f"Compilation failed:")
            print(result.stderr)
            return False
            
    except subprocess.TimeoutExpired:
        print("Compilation timed out")
        return False
    except FileNotFoundError:
        print(f"Compiler '{compiler}' not found")
        return False
    finally:
        # Clean up temp file
        try:
            os.unlink(c_file)
        except:
            pass
    
    return False
