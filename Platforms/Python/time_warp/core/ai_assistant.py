"""AI-powered code assistant for Time Warp Studio."""

from dataclasses import dataclass
from enum import Enum
from typing import Callable, Dict, List, Optional


class AssistantMode(Enum):
    """Assistant operation modes."""

    EXPLAIN_ERROR = "explain_error"
    SUGGEST_CODE = "suggest_code"
    FIX_SYNTAX = "fix_syntax"
    OPTIMIZE_CODE = "optimize_code"
    EXPLAIN_CONCEPT = "explain_concept"
    GENERATE_EXAMPLE = "generate_example"


class CodeLanguage(Enum):
    """Programming languages."""

    BASIC = "BASIC"
    LOGO = "LOGO"
    PILOT = "PILOT"
    PASCAL = "PASCAL"
    C = "C"
    FORTH = "FORTH"
    PROLOG = "PROLOG"


@dataclass
class AssistantSuggestion:
    """Code suggestion from AI."""

    mode: AssistantMode
    code: str
    explanation: str
    confidence: float  # 0.0-1.0
    alternatives: List[str]


@dataclass
class ConversationMessage:
    """Message in assistant conversation."""

    role: str  # 'user', 'assistant'
    content: str
    timestamp: float


class LocalAIAssistant:
    """Offline AI assistant using pattern matching and built-in knowledge."""

    # Built-in knowledge base for common questions
    KNOWLEDGE_BASE = {
        "loops": {
            "BASIC": "FOR i = 1 TO 10\n  PRINT i\nNEXT i",
            "LOGO": "REPEAT 10 [FORWARD 10 RIGHT 36]",
            "PASCAL": "for i := 1 to 10 do WriteLn(i);",
            "C": 'for (int i = 1; i <= 10; i++) printf("%d\\n", i);',
        },
        "conditionals": {
            "BASIC": (
                'IF x > 5 THEN\n  PRINT "Greater"\nELSE\n' '  PRINT "Less"\nEND IF'
            ),
            "LOGO": 'IF :x > 5 [PRINT "Greater"] [PRINT "Less"]',
            "PASCAL": 'if x > 5 then WriteLn("Greater") else WriteLn("Less");',
            "C": 'if (x > 5) printf("Greater\\n"); else printf("Less\\n");',
        },
        "arrays": {
            "BASIC": "DIM arr(10)\narr(0) = 42\nPRINT arr(0)",
            "LOGO": 'MAKE "arr [1 2 3 4 5]',
            "PASCAL": "var arr: array[0..9] of integer; arr[0] := 42;",
            "C": 'int arr[10]; arr[0] = 42; printf("%d\\n", arr[0]);',
        },
        "functions": {
            "BASIC": (
                'SUB greet(name$)\n  PRINT "Hello " name$\nEND SUB\n'
                'CALL greet("World")'
            ),
            "LOGO": ("TO square :size\n  REPEAT 4 " "[FORWARD :size RIGHT 90]\nEND"),
            "PASCAL": (
                "procedure greet(name: string); " 'begin WriteLn("Hello " + name); end;'
            ),
            "C": 'void greet(char *name) { printf("Hello %s\\n", name); }',
        },
    }

    # Common error patterns and fixes
    ERROR_PATTERNS = {
        "undefined variable": {
            "pattern": "variable .* not found",
            "suggestion": (
                "Make sure the variable is declared or assigned before use."
            ),
            "example": "Use: x = 10 before using x",
        },
        "syntax error": {
            "pattern": "syntax error",
            "suggestion": "Check brackets, quotes, and statement endings.",
            "example": "Make sure all IF statements end with ENDIF",
        },
        "type mismatch": {
            "pattern": "type mismatch",
            "suggestion": "Ensure values match expected types.",
            "example": "Don't assign text to a number variable",
        },
        "divide by zero": {
            "pattern": "divide by zero",
            "suggestion": (
                "Add a check before division: IF divisor = 0 THEN PRINT " '"Error"'
            ),
            "example": "IF x <> 0 THEN result = y / x",
        },
    }

    def __init__(self):
        """Initialize local AI assistant."""
        self.conversation_history: List[ConversationMessage] = []
        self.callbacks: Dict[str, List[Callable]] = {}
        self.language = CodeLanguage.BASIC

    def on_event(self, event_name: str, callback: Callable):
        """Register event callback."""
        if event_name not in self.callbacks:
            self.callbacks[event_name] = []
        self.callbacks[event_name].append(callback)

    def load_knowledge_base(self):
        """Load or refresh the local knowledge base.

        The assistant ships with a built-in knowledge base, so this is a
        lightweight no-op kept for UI compatibility.
        """
        self._trigger_callbacks("knowledge_loaded")

    def query(self, prompt: str) -> str:
        """Handle a generic query using built-in heuristics."""
        normalized = prompt.strip().lower()
        for concept in self.KNOWLEDGE_BASE:
            if concept in normalized:
                suggestion = self.suggest_code(concept)
                return suggestion.explanation

        return self.explain_concept(normalized)

    def _trigger_callbacks(self, event_name: str, **kwargs):
        """Trigger callbacks for event."""
        if event_name in self.callbacks:
            for callback in self.callbacks[event_name]:
                callback(**kwargs)

    def explain_error(
        self, error_message: str, _code: Optional[str] = None
    ) -> AssistantSuggestion:
        """Explain an error message."""
        explanation = self._analyze_error(error_message)
        suggestion = (
            f"This error typically means: {explanation['meaning']}\n\n"
            f"How to fix it: {explanation['fix']}\n\n"
            f"Example: {explanation.get('example', 'See documentation')}"
        )

        result = AssistantSuggestion(
            mode=AssistantMode.EXPLAIN_ERROR,
            code="",
            explanation=suggestion,
            confidence=explanation.get("confidence", 0.7),
            alternatives=explanation.get("alternatives", []),
        )

        self._trigger_callbacks("explanation_generated", suggestion=result)
        return result

    def suggest_code(
        self, concept: str, language: Optional[str] = None
    ) -> AssistantSuggestion:
        """Suggest code for a concept."""
        if language:
            self.language = CodeLanguage[language.upper()]

        lang_name = self.language.value
        code = self.KNOWLEDGE_BASE.get(concept.lower(), {}).get(lang_name, "")

        if code:
            explanation = f"Here's how to implement {concept} in {lang_name}:\n\n{code}"
            confidence = 0.95
        else:
            explanation = (
                f"I don't have a built-in example for {concept}. "
                f"Try breaking it down into simpler steps."
            )
            confidence = 0.4

        result = AssistantSuggestion(
            mode=AssistantMode.SUGGEST_CODE,
            code=code,
            explanation=explanation,
            confidence=confidence,
            alternatives=[],
        )

        self._trigger_callbacks("code_suggested", suggestion=result)
        return result

    def fix_syntax(
        self, code: str, _error: Optional[str] = None
    ) -> AssistantSuggestion:
        """Suggest fixes for syntax errors."""
        fixes = self._identify_common_syntax_issues(code)

        if fixes:
            explanation = "Found potential syntax issues:\n\n"
            for i, fix in enumerate(fixes, 1):
                explanation += f"{i}. {fix['issue']}\n   Fix: {fix['suggestion']}\n\n"
            confidence = 0.8
        else:
            explanation = "No obvious syntax errors found. The issue might be semantic."
            confidence = 0.5

        result = AssistantSuggestion(
            mode=AssistantMode.FIX_SYNTAX,
            code="\n".join([f["fixed"] for f in fixes if "fixed" in f]),
            explanation=explanation,
            confidence=confidence,
            alternatives=[],
        )

        self._trigger_callbacks("syntax_fix_suggested", suggestion=result)
        return result

    def optimize_code(self, code: str) -> AssistantSuggestion:
        """Suggest optimizations."""
        suggestions = self._find_optimization_opportunities(code)

        explanation = "Optimization opportunities:\n\n"
        for sug in suggestions:
            explanation += f"• {sug['description']}\n  Impact: {sug['impact']}\n\n"

        result = AssistantSuggestion(
            mode=AssistantMode.OPTIMIZE_CODE,
            code="",
            explanation=explanation,
            confidence=0.7,
            alternatives=[],
        )

        self._trigger_callbacks("optimization_suggested", suggestion=result)
        return result

    def explain_concept(self, concept: str) -> str:
        """Explain a programming concept."""
        explanations = {
            "loop": (
                "A loop repeats a block of code multiple times. USE: FOR "
                "loops for fixed counts, WHILE loops for conditions."
            ),
            "variable": (
                "A variable stores a value that can change. You can assign "
                "different values to it throughout your program."
            ),
            "array": (
                "An array is a list of values. Access items by index "
                "(0, 1, 2, etc.)."
            ),
            "function": (
                "A function is reusable code. Define it once, call it many " "times."
            ),
            "conditional": (
                "IF statements make decisions. "
                "IF condition is true, execute one block, "
                "ELSE execute another."
            ),
            "recursion": (
                "A function calling itself. Useful for trees, fractals, and "
                "divide-and-conquer problems."
            ),
            "variable scope": (
                "Where a variable is accessible. Local = inside function, "
                "Global = everywhere."
            ),
        }

        explanation = explanations.get(
            concept.lower(),
            f"I don't have a built-in explanation for '{concept}'",
        )

        self._trigger_callbacks(
            "concept_explained", concept=concept, explanation=explanation
        )
        return explanation

    def generate_example(self, concept: str, difficulty: str = "beginner") -> str:
        """Generate example code."""
        examples = {
            "loop": {
                "beginner": "FOR i = 1 TO 5\n  PRINT i\nNEXT i",
                "intermediate": ("WHILE x < 100\n  x = x * 2\n  PRINT x\nEND WHILE"),
            },
            "function": {
                "beginner": (
                    'SUB hello()\n  PRINT "Hello World"\nEND SUB\nCALL hello()'
                ),
                "intermediate": (
                    "FUNCTION add(a, b)\n  RETURN a + b\nEND FUNCTION\n"
                    "RESULT = add(3, 4)"
                ),
            },
        }

        example = examples.get(concept.lower(), {}).get(
            difficulty, "No example available"
        )

        self._trigger_callbacks(
            "example_generated",
            concept=concept,
            example=example,
        )
        return example

    def chat(self, user_message: str) -> str:
        """Chat interface for assistance."""
        # Add to conversation history
        self.conversation_history.append(
            ConversationMessage(role="user", content=user_message, timestamp=0)
        )

        # Analyze message and generate response
        response = self._generate_chat_response(user_message)

        self.conversation_history.append(
            ConversationMessage(
                role="assistant",
                content=response,
                timestamp=0,
            )
        )

        self._trigger_callbacks("chat_response", message=response)
        return response

    def get_conversation_history(self) -> List[Dict]:
        """Get conversation history as JSON."""
        return [
            {"role": msg.role, "content": msg.content}
            for msg in self.conversation_history
        ]

    def clear_history(self):
        """Clear conversation history."""
        self.conversation_history = []
        self._trigger_callbacks("history_cleared")

    # Private helper methods

    def _analyze_error(self, error_message: str) -> Dict:
        """Analyze error message and provide guidance."""
        error_lower = error_message.lower()

        for error_type, details in self.ERROR_PATTERNS.items():
            if error_type in error_lower or details["pattern"] in error_lower:
                return {
                    "meaning": details["suggestion"],
                    "fix": details["example"],
                    "confidence": 0.85,
                    "alternatives": [],
                }

        # Generic error handling
        return {
            "meaning": "An error occurred during execution.",
            "fix": (
                "Check the error message for clues. Read the code line by " "line."
            ),
            "confidence": 0.5,
            "alternatives": [],
        }

    def _identify_common_syntax_issues(self, code: str) -> List[Dict]:
        """Identify common syntax problems."""
        issues = []

        # Check for unclosed structures
        open_if = code.count("IF")
        close_if = code.count("ENDIF") + code.count("END IF")
        if open_if > close_if:
            issues.append(
                {
                    "issue": (
                        "Unclosed IF statement (found "
                        f"{open_if} IF, {close_if} ENDIF)"
                    ),
                    "suggestion": "Add ENDIF at the end of IF blocks",
                    "fixed": code,  # Would need to add actual fix
                }
            )

        # Check for unclosed loops
        for_count = code.count("FOR")
        next_count = code.count("NEXT")
        if for_count > next_count:
            issues.append(
                {
                    "issue": (
                        "Unclosed FOR loop (found "
                        f"{for_count} FOR, {next_count} NEXT)"
                    ),
                    "suggestion": "Add NEXT at the end of FOR loops",
                }
            )

        # Check for mismatched quotes
        single_quotes = code.count("'")
        double_quotes = code.count('"')
        if single_quotes % 2 != 0 or double_quotes % 2 != 0:
            issues.append(
                {
                    "issue": "Mismatched quotes",
                    "suggestion": "Check that all quotes are paired",
                }
            )

        return issues

    def _find_optimization_opportunities(self, code: str) -> List[Dict]:
        """Find code optimization opportunities."""
        suggestions = []
        lines = code.split("\n")

        # Check for repeated code
        line_counts: Dict[str, int] = {}
        for line in lines:
            line_counts[line] = line_counts.get(line, 0) + 1

        for line, count in line_counts.items():
            if count > 2 and len(line.strip()) > 10:
                suggestions.append(
                    {
                        "description": f"Repeated code found {count} times",
                        "impact": "HIGH - Extract to function",
                        "suggestion": (
                            "Move this repeated code to a subroutine: "
                            f"{line[:40]}..."
                        ),
                    }
                )

        # Check for nested loops (potential slowdown)
        nested_loops = code.count("FOR") + code.count("WHILE") > 2
        if nested_loops:
            suggestions.append(
                {
                    "description": "Nested loops detected",
                    "impact": "MEDIUM - Consider algorithm changes",
                    "suggestion": (
                        "Nested loops can be slow. Consider if you need both."
                    ),
                }
            )

        return suggestions

    def _generate_chat_response(self, user_message: str) -> str:
        """Generate chat response."""
        msg_lower = user_message.lower()

        # Help queries
        if "help" in msg_lower or "how do i" in msg_lower:
            if "loop" in msg_lower:
                return self.suggest_code("loops").explanation
            elif "function" in msg_lower or "subroutine" in msg_lower:
                return self.suggest_code("functions").explanation
            else:
                return (
                    "I can help with loops, functions, arrays, and "
                    "conditionals. What would you like to learn?"
                )

        # Error queries
        if "error" in msg_lower or "bug" in msg_lower or "fix" in msg_lower:
            return (
                "Share your error message and code snippet, and I'll help " "debug it!"
            )

        # Concept queries
        if "what is" in msg_lower or "explain" in msg_lower:
            # Extract concept
            words = msg_lower.split()
            if "what" in words and "is" in words:
                concept_idx = words.index("is") + 1
                if concept_idx < len(words):
                    concept = words[concept_idx]
                    return self.explain_concept(concept)
            return self.explain_concept("variable")

        # Suggest queries
        if "example" in msg_lower or "show me" in msg_lower:
            return self.generate_example("loop", "beginner")

        # Default response
        return (
            "Ask me about loops, functions, arrays, conditionals, or any "
            "errors you're seeing!"
        )


class RemoteAIAssistant:
    """Optional: Remote AI assistant using OpenAI API."""

    def __init__(self, api_key: Optional[str] = None):
        """Initialize remote assistant."""
        self.api_key = api_key
        self.enabled = api_key is not None
        self.model = "gpt-3.5-turbo"
        self.conversation_history: List[ConversationMessage] = []

    def explain_error(self, error: str, code: Optional[str] = None) -> Optional[str]:
        """Use OpenAI to explain error."""
        if not self.enabled:
            return None

        try:
            import importlib

            openai = importlib.import_module("openai")

            setattr(openai, "api_key", self.api_key)

            prompt = f"Explain this programming error in simple terms: {error}"
            if code:
                prompt += f"\n\nCode:\n{code}"

            response = openai.ChatCompletion.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                temperature=0.7,
            )

            return response.choices[0].message.content

        except ImportError:
            return "OpenAI library not installed. Install with: " "pip install openai"
        except (ValueError, RuntimeError) as exc:
            return f"API error: {str(exc)}"

    def suggest_code(self, description: str, language: str = "BASIC") -> Optional[str]:
        """Use OpenAI to suggest code."""
        if not self.enabled:
            return None

        try:
            import importlib

            openai = importlib.import_module("openai")

            setattr(openai, "api_key", self.api_key)

            prompt = f"Write a {language} program that: {description}"

            response = openai.ChatCompletion.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                temperature=0.7,
            )

            return response.choices[0].message.content

        except (ValueError, TypeError):
            return None
