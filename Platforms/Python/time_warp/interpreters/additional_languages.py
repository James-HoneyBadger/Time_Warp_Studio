"""
Time Warp Studio - Additional Language Support

Adds:
- Ruby language interpreter
- JavaScript/TypeScript interpreter
- Node.js integration
- Interactive REPL support
"""

import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional

# ===== LANGUAGE EXECUTORS =====


class ExecutionMode(Enum):
    """Execution mode for code"""

    INTERPRETER = "interpreter"  # REPL mode
    COMPILED = "compiled"  # Compile then run
    JIT = "jit"  # Just-in-time compilation


@dataclass
class ExecutionResult:
    """Result of code execution"""

    success: bool
    output: str
    errors: List[str]
    warnings: List[str]
    execution_time_ms: float
    memory_used_mb: float
    exit_code: int
    mode: ExecutionMode
    stats: Dict[str, Any]


class LanguageExecutor(ABC):
    """Abstract base for language executors"""

    @property
    @abstractmethod
    def language_name(self) -> str:
        """Language name"""
        pass

    @property
    @abstractmethod
    def version(self) -> str:
        """Language version"""
        pass

    @abstractmethod
    def execute(self, code: str, timeout: float = 30.0) -> ExecutionResult:
        """Execute code"""
        pass

    @abstractmethod
    def validate_syntax(self, code: str) -> tuple[bool, Optional[str]]:
        """Validate code syntax"""
        pass


# ===== RUBY EXECUTOR =====


class RubyExecutor(LanguageExecutor):
    """Ruby language executor"""

    @property
    def language_name(self) -> str:
        return "Ruby"

    @property
    def version(self) -> str:
        try:
            result = subprocess.run(
                ["ruby", "--version"],
                capture_output=True,
                text=True,
                timeout=5,
            )
            return result.stdout.strip()
        except BaseException:
            return "Unknown"

    def execute(self, code: str, timeout: float = 30.0) -> ExecutionResult:
        """Execute Ruby code"""
        import time

        start_time = time.time()

        try:
            # Run Ruby code
            result = subprocess.run(
                ["ruby", "-e", code],
                capture_output=True,
                text=True,
                timeout=timeout,
            )

            duration = (time.time() - start_time) * 1000

            return ExecutionResult(
                success=result.returncode == 0,
                output=result.stdout,
                errors=result.stderr.split("\n") if result.stderr else [],
                warnings=[],
                execution_time_ms=duration,
                memory_used_mb=0,  # Ruby process memory
                exit_code=result.returncode,
                mode=ExecutionMode.INTERPRETER,
                stats={
                    "lines": len(code.split("\n")),
                    "interpreter": "ruby",
                },
            )
        except subprocess.TimeoutExpired:
            duration = (time.time() - start_time) * 1000
            return ExecutionResult(
                success=False,
                output="",
                errors=["Execution timeout"],
                warnings=[],
                execution_time_ms=duration,
                memory_used_mb=0,
                exit_code=-1,
                mode=ExecutionMode.INTERPRETER,
                stats={},
            )
        except Exception as e:
            return ExecutionResult(
                success=False,
                output="",
                errors=[str(e)],
                warnings=[],
                execution_time_ms=0,
                memory_used_mb=0,
                exit_code=-1,
                mode=ExecutionMode.INTERPRETER,
                stats={},
            )

    def validate_syntax(self, code: str) -> tuple[bool, Optional[str]]:
        """Validate Ruby syntax"""
        try:
            result = subprocess.run(
                ["ruby", "-c", "-"],
                input=code,
                capture_output=True,
                text=True,
                timeout=5,
            )

            if result.returncode == 0:
                return True, None
            else:
                return False, result.stderr
        except Exception as e:
            return False, str(e)


# ===== JAVASCRIPT/TYPESCRIPT EXECUTOR =====


class JavaScriptExecutor(LanguageExecutor):
    """JavaScript/TypeScript executor using Node.js"""

    @property
    def language_name(self) -> str:
        return "JavaScript"

    @property
    def version(self) -> str:
        try:
            result = subprocess.run(
                ["node", "--version"],
                capture_output=True,
                text=True,
                timeout=5,
            )
            return result.stdout.strip()
        except BaseException:
            return "Unknown"

    def execute(self, code: str, timeout: float = 30.0) -> ExecutionResult:
        """Execute JavaScript code"""
        import time

        start_time = time.time()

        try:
            # Wrap code to capture output
            wrapped_code = """
const originalLog = console.log;
const output = [];
console.log = function(...args) {{
    output.push(args.map(a => JSON.stringify(a)).join(' '));
}};

try {{
    {code}
}} catch(e) {{
    output.push('Error: ' + e.message);
}}

console.log(JSON.stringify({{output: output, error: null}}));
"""

            result = subprocess.run(
                ["node", "-e", wrapped_code],
                capture_output=True,
                text=True,
                timeout=timeout,
            )

            duration = (time.time() - start_time) * 1000

            # Parse output
            output = result.stdout
            errors = result.stderr.split("\n") if result.stderr else []

            return ExecutionResult(
                success=result.returncode == 0,
                output=output,
                errors=errors,
                warnings=[],
                execution_time_ms=duration,
                memory_used_mb=0,
                exit_code=result.returncode,
                mode=ExecutionMode.JIT,
                stats={
                    "lines": len(code.split("\n")),
                    "interpreter": "node",
                    "engine": "V8",
                },
            )
        except subprocess.TimeoutExpired:
            duration = (time.time() - start_time) * 1000
            return ExecutionResult(
                success=False,
                output="",
                errors=["Execution timeout"],
                warnings=[],
                execution_time_ms=duration,
                memory_used_mb=0,
                exit_code=-1,
                mode=ExecutionMode.JIT,
                stats={},
            )
        except Exception as e:
            return ExecutionResult(
                success=False,
                output="",
                errors=[str(e)],
                warnings=[],
                execution_time_ms=0,
                memory_used_mb=0,
                exit_code=-1,
                mode=ExecutionMode.JIT,
                stats={},
            )

    def validate_syntax(self, code: str) -> tuple[bool, Optional[str]]:
        """Validate JavaScript syntax"""
        try:
            validation_code = """
try {{
    new Function('{code.replace(chr(39), chr(92) + chr(39))}');
    console.log('valid');
}} catch(e) {{
    console.log('invalid: ' + e.message);
}}
"""
            result = subprocess.run(
                ["node", "-e", validation_code],
                capture_output=True,
                text=True,
                timeout=5,
            )

            if "valid" in result.stdout:
                return True, None
            else:
                return False, result.stdout
        except Exception as e:
            return False, str(e)


# ===== REPL SESSION =====


class REPLSession:
    """Interactive REPL session for a language"""

    def __init__(self, executor: LanguageExecutor):
        self.executor = executor
        self.history: List[str] = []
        self.process = None
        self.output_buffer: List[str] = []

    def execute_line(self, code: str) -> str:
        """Execute single line in REPL"""
        result = self.executor.execute(code)
        self.history.append(code)

        if result.success:
            return result.output
        else:
            return f"Error: {', '.join(result.errors)}"

    def get_history(self) -> List[str]:
        """Get execution history"""
        return self.history

    def clear_history(self) -> None:
        """Clear history"""
        self.history.clear()

    def autocomplete(self, partial: str) -> List[str]:
        """Get autocompletion suggestions"""
        # This would implement language-specific autocomplete
        return []


# ===== LANGUAGE REGISTRY =====


class LanguageRegistry:
    """Registry of available language executors"""

    _executors: Dict[str, LanguageExecutor] = {}

    @classmethod
    def register(cls, language: str, executor: LanguageExecutor) -> None:
        """Register a language executor"""
        cls._executors[language.lower()] = executor

    @classmethod
    def get(cls, language: str) -> Optional[LanguageExecutor]:
        """Get executor for language"""
        return cls._executors.get(language.lower())

    @classmethod
    def list_languages(cls) -> List[str]:
        """List all registered languages"""
        return list(cls._executors.keys())

    @classmethod
    def is_available(cls, language: str) -> bool:
        """Check if language is available"""
        return language.lower() in cls._executors


# ===== INITIALIZE REGISTRY =====


def initialize_language_registry():
    """Initialize with built-in language executors"""
    LanguageRegistry.register("ruby", RubyExecutor())
    LanguageRegistry.register("javascript", JavaScriptExecutor())
    LanguageRegistry.register("js", JavaScriptExecutor())
    LanguageRegistry.register("node", JavaScriptExecutor())


# ===== EXAMPLE USAGE =====

if __name__ == "__main__":
    initialize_language_registry()

    # Example: Execute Ruby
    ruby_executor = LanguageRegistry.get("ruby")
    if ruby_executor:
        result = ruby_executor.execute("""
puts "Hello, Ruby!"
5.times { |i| puts i }
""")
        print(f"Ruby Output:\n{result.output}")

    # Example: Execute JavaScript
    js_executor = LanguageRegistry.get("javascript")
    if js_executor:
        result = js_executor.execute("""
console.log("Hello, JavaScript!");
for (let i = 0; i < 5; i++) {
  console.log(i);
}
""")
        print(f"JavaScript Output:\n{result.output}")

    # List available languages
    print(f"\nAvailable languages: {LanguageRegistry.list_languages()}")
