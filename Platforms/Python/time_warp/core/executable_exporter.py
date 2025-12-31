"""Export programs to standalone executables."""

from pathlib import Path
from typing import Dict, List, Optional, Tuple
from enum import Enum
import json
import shutil
import tempfile
import subprocess
import sys


class ExportFormat(Enum):
    """Export format options."""
    PYTHON_EXE = "python_exe"  # PyInstaller executable
    HTML5 = "html5"  # HTML5 Canvas for LOGO/graphics
    WINDOWS_EXE = "windows_exe"
    MACOS_APP = "macos_app"
    LINUX_ELF = "linux_elf"
    WEB_APP = "web_app"


class ExportProfile:
    """Profile for code export."""
    
    def __init__(self, name: str, format: ExportFormat, language: str):
        """Initialize export profile."""
        self.name = name
        self.format = format
        self.language = language
        self.include_console = True
        self.include_icon = False
        self.include_splash = False
        self.optimize = True
        self.compress = False
        self.metadata: Dict[str, str] = {
            'author': '',
            'version': '1.0.0',
            'description': ''
        }


class ExecutableExporter:
    """Exports Time Warp programs to standalone executables."""
    
    # Template wrapper scripts
    PYTHON_WRAPPER = '''#!/usr/bin/env python3
"""Auto-generated executable from Time Warp Studio."""

import sys
import os

# Program code will be inserted here
{PROGRAM_CODE}

if __name__ == "__main__":
    main()
'''
    
    HTML5_WRAPPER = '''<!DOCTYPE html>
<html>
<head>
    <title>{TITLE}</title>
    <meta charset="UTF-8">
    <style>
        body {{ font-family: monospace; margin: 20px; }}
        canvas {{ border: 1px solid #ccc; }}
        #output {{ 
            border: 1px solid #ddd; 
            padding: 10px; 
            margin-top: 10px; 
            height: 200px;
            overflow-y: auto;
            background: #f0f0f0;
        }}
        button {{ margin: 10px 5px 10px 0; }}
    </style>
</head>
<body>
    <h1>{TITLE}</h1>
    <button onclick="runProgram()">Run</button>
    <button onclick="resetCanvas()">Reset</button>
    <canvas id="canvas" width="800" height="600"></canvas>
    <div id="output"></div>
    
    <script>
        {PROGRAM_CODE}
    </script>
</body>
</html>
'''
    
    def __init__(self):
        """Initialize exporter."""
        self.temp_dir = Path(tempfile.gettempdir()) / "time_warp_export"
        self.temp_dir.mkdir(exist_ok=True)
    
    def export_python_executable(self,
                                 code: str,
                                 output_path: Path,
                                 include_console: bool = True) -> Tuple[bool, str]:
        """
        Export as Python executable using PyInstaller.
        
        Returns: (success, message)
        """
        try:
            # Check if PyInstaller is installed
            try:
                import PyInstaller
            except ImportError:
                return (False, "PyInstaller not installed. Install with: pip install pyinstaller")
            
            # Create wrapper script
            wrapper_code = self.PYTHON_WRAPPER.format(PROGRAM_CODE=code)
            script_path = self.temp_dir / "program.py"
            
            with open(script_path, 'w') as f:
                f.write(wrapper_code)
            
            # Build with PyInstaller
            cmd = [
                sys.executable, '-m', 'PyInstaller',
                '--onefile',
                f'--distpath={output_path.parent}',
                str(script_path)
            ]
            
            if not include_console:
                cmd.append('--windowed')
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode == 0:
                exe_path = output_path.parent / "program"
                return (True, f"Executable created: {exe_path}")
            else:
                return (False, f"Build failed: {result.stderr}")
        
        except Exception as e:
            return (False, f"Export failed: {str(e)}")
    
    def export_html5(self,
                    code: str,
                    output_path: Path,
                    title: str = "Time Warp Program",
                    language: str = "LOGO") -> Tuple[bool, str]:
        """
        Export as HTML5 Canvas application.
        
        Returns: (success, message)
        """
        try:
            # Convert LOGO/BASIC to JavaScript if needed
            js_code = self._transpile_to_javascript(code, language)
            
            # Generate HTML
            html_content = self.HTML5_WRAPPER.format(
                TITLE=title,
                PROGRAM_CODE=js_code
            )
            
            # Write HTML file
            with open(output_path, 'w') as f:
                f.write(html_content)
            
            return (True, f"HTML5 application created: {output_path}")
        
        except Exception as e:
            return (False, f"Export failed: {str(e)}")
    
    def export_web_app(self,
                      code: str,
                      output_dir: Path,
                      app_name: str = "TimeWarpApp",
                      language: str = "BASIC") -> Tuple[bool, str]:
        """
        Export as a complete web application.
        
        Returns: (success, message)
        """
        try:
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Create directory structure
            (output_dir / "static").mkdir(exist_ok=True)
            (output_dir / "templates").mkdir(exist_ok=True)
            
            # Create Python Flask app
            flask_app = self._create_flask_app(code, language)
            with open(output_dir / "app.py", 'w') as f:
                f.write(flask_app)
            
            # Create requirements.txt
            requirements = "Flask==2.3.0\nWerkzeug==2.3.0\n"
            with open(output_dir / "requirements.txt", 'w') as f:
                f.write(requirements)
            
            # Create README
            readme = self._create_readme(app_name, language)
            with open(output_dir / "README.md", 'w') as f:
                f.write(readme)
            
            return (True, f"Web app created in: {output_dir}\n"
                         f"To run: cd {output_dir} && pip install -r requirements.txt && python app.py")
        
        except Exception as e:
            return (False, f"Export failed: {str(e)}")
    
    def export_shell_script(self,
                           code: str,
                           output_path: Path,
                           language: str = "BASIC") -> Tuple[bool, str]:
        """Export as shell script."""
        try:
            # Create bash wrapper
            script = f'''#!/bin/bash
# Auto-generated from Time Warp Studio ({language})
python3 << 'EOF'
{code}
EOF
'''
            
            with open(output_path, 'w') as f:
                f.write(script)
            
            # Make executable
            output_path.chmod(0o755)
            
            return (True, f"Shell script created: {output_path}")
        
        except Exception as e:
            return (False, f"Export failed: {str(e)}")
    
    def create_installers(self,
                         code: str,
                         output_dir: Path,
                         app_name: str) -> Dict[str, Tuple[bool, str]]:
        """
        Create installers for Windows, macOS, Linux.
        
        Returns: Dict of format -> (success, message)
        """
        results = {}
        
        # Windows installer
        results['windows'] = self.export_python_executable(
            code,
            output_dir / f"{app_name}.exe"
        )
        
        # Shell script (Linux/macOS)
        results['unix'] = self.export_shell_script(
            code,
            output_dir / app_name
        )
        
        return results
    
    def _transpile_to_javascript(self, code: str, language: str) -> str:
        """
        Transpile LOGO/BASIC to JavaScript.
        
        This is a simplified transpiler - a full one would be much more complex.
        """
        if language.upper() == "LOGO":
            return self._transpile_logo_to_js(code)
        elif language.upper() == "BASIC":
            return self._transpile_basic_to_js(code)
        else:
            return f"console.log('Program: {code[:50]}...');"
    
    def _transpile_logo_to_js(self, code: str) -> str:
        """Transpile LOGO to JavaScript."""
        js = """
        const canvas = document.getElementById('canvas');
        const ctx = canvas.getContext('2d');
        let x = canvas.width / 2, y = canvas.height / 2;
        let angle = -90; // Start facing up
        
        function forward(distance) {
            const rad = angle * Math.PI / 180;
            const newX = x + distance * Math.cos(rad);
            const newY = y + distance * Math.sin(rad);
            ctx.beginPath();
            ctx.moveTo(x, y);
            ctx.lineTo(newX, newY);
            ctx.stroke();
            x = newX;
            y = newY;
        }
        
        function right(degrees) {
            angle += degrees;
        }
        
        function left(degrees) {
            angle -= degrees;
        }
        
        function penUp() {
            ctx.globalAlpha = 0.5;
        }
        
        function penDown() {
            ctx.globalAlpha = 1.0;
        }
        
        function setColor(color) {
            ctx.strokeStyle = color;
        }
        
        // Program execution
        try {
            ctx.strokeStyle = 'black';
            ctx.lineWidth = 2;
        """ + code + """
        } catch(e) {
            console.error('Error:', e.message);
        }
        
        function runProgram() {
            location.reload();
        }
        
        function resetCanvas() {
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            x = canvas.width / 2;
            y = canvas.height / 2;
            angle = -90;
        }
        """
        return js
    
    def _transpile_basic_to_js(self, code: str) -> str:
        """Transpile BASIC to JavaScript."""
        js = """
        let output = document.getElementById('output');
        let variables = {};
        
        function print(...args) {
            output.innerHTML += args.join(' ') + '<br>';
        }
        
        function input(prompt) {
            return prompt('> ' + prompt);
        }
        
        try {
            """ + code + """
        } catch(e) {
            print('Error: ' + e.message);
        }
        
        function runProgram() {
            location.reload();
        }
        """
        return js
    
    def _create_flask_app(self, code: str, language: str) -> str:
        """Create Flask web application."""
        return f'''from flask import Flask, render_template, jsonify
import json

app = Flask(__name__)

# Program state
output = []
variables = {{}}

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/run', methods=['POST'])
def run_program():
    global output, variables
    output = []
    variables = {{}}
    
    try:
        # Execute program code
        # {language} code execution here
        return jsonify({{'status': 'success', 'output': output}})
    except Exception as e:
        return jsonify({{'status': 'error', 'message': str(e)}}), 400

@app.route('/api/status')
def status():
    return jsonify({{
        'output': output,
        'variables': variables
    }})

if __name__ == '__main__':
    app.run(debug=True, port=5000)
'''
    
    def _create_readme(self, app_name: str, language: str) -> str:
        """Create README for exported application."""
        return f'''# {app_name}

Auto-generated Time Warp application ({language})

## Installation

```bash
pip install -r requirements.txt
```

## Running

```bash
python app.py
```

Then open http://localhost:5000 in your browser.

## Requirements

- Python 3.7+
- Flask
- Werkzeug

## License

Created with Time Warp Studio
'''


class ExportPresets:
    """Pre-configured export presets."""
    
    @staticmethod
    def get_preset(preset_name: str) -> Optional[ExportProfile]:
        """Get a preset profile."""
        presets = {
            'quick_python': ExportProfile('Quick Python', ExportFormat.PYTHON_EXE, 'BASIC'),
            'logo_html5': ExportProfile('Logo HTML5', ExportFormat.HTML5, 'LOGO'),
            'game_windows': ExportProfile('Game Windows', ExportFormat.WINDOWS_EXE, 'BASIC'),
            'web_app': ExportProfile('Web Application', ExportFormat.WEB_APP, 'BASIC'),
        }
        return presets.get(preset_name)
    
    @staticmethod
    def list_presets() -> List[str]:
        """List available presets."""
        return ['quick_python', 'logo_html5', 'game_windows', 'web_app']
