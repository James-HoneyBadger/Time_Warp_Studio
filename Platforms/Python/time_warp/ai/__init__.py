"""time_warp.ai — DISABLED stub

This module used to contain the AI assistant implementation. For the
current build the AI / collaboration features are intentionally disabled.
This file provides a small, API-compatible "no-op" implementation so other
parts of the application can continue importing the public symbols without
errors.

The public API surface (classes, dataclasses, enums and get_ai_assistant())
keeps the same names so callers don't need to be rewritten in this large
codebase. All operations return a clear disabled response.
"""

from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional


class AIProvider(Enum):
    """Provider types (kept for compatibility)."""

    GITHUB_COPILOT = "github_copilot"
    OPENAI = "openai"
    ANTHROPIC = "anthropic"
    LOCAL_OLLAMA = "ollama"
    LOCAL_LM_STUDIO = "lm_studio"
    NONE = "none"


class AIRequestType(Enum):
    """Request types (compatibility-only)."""

    CODE_COMPLETION = "code_completion"
    ERROR_EXPLANATION = "error_explanation"
    CODE_REVIEW = "code_review"
    LEARNING_SUGGESTION = "learning_suggestion"
    DEBUGGING_HELP = "debugging_help"
    OPTIMIZATION = "optimization"


@dataclass
class AIRequest:
    """Compatibility dataclass for callers that construct requests.

    This stub keeps the same fields so existing callers can construct
    AIRequest(...) without changes. The stub does not perform any
    processing — it is only a typing/shape compatibility helper.
    """

    request_type: AIRequestType
    code_context: str
    language: str
    cursor_position: Optional[int] = None
    error_message: Optional[str] = None
    user_level: str = "beginner"


@dataclass
class AIResponse:
    """Compatibility response object returned by the stubbed assistant."""

    success: bool
    content: str
    suggestions: List[str]
    confidence: float
    metadata: Dict[str, Any]


class AIProviderInterface:
    """Small compatibility interface — providers replaced by no-op stub.

    Existing code that referenced provider objects/type checks can still
    import this class. Instances of providers are not created by the
    disabled assistant.
    """

    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.is_available = False

    def initialize(self) -> bool:  # pragma: no cover - compatibility
        return False

    def generate_completion(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        raise RuntimeError("AI is disabled in this build")

    def explain_error(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        raise RuntimeError("AI is disabled in this build")

    def review_code(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        raise RuntimeError("AI is disabled in this build")

    def get_learning_suggestion(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        raise RuntimeError("AI is disabled in this build")


class GitHubCopilotProvider(AIProviderInterface):
    """Compatibility placeholder for the GitHubCopilot provider."""

    def __init__(self, config: Dict[str, Any]):
        # Keep the base initialization and define provider attributes so
        # static analyzers and type-checkers see expected members.
        super().__init__(config)
        self.model: Optional[str] = None
        self.host: Optional[str] = None

    def initialize(self) -> bool:  # compatibility
        # Always unavailable in the no-op stub
        self.is_available = False
        return False

    def generate_completion(self, request: AIRequest) -> AIResponse:
        raise RuntimeError("AI is disabled in this build")

    def _generate_mock_completion(self, request: AIRequest) -> str:
        """Generate mock completion for demonstration"""
        if request.language.lower() == "basic":
            if "print" in request.code_context.lower():
                return 'PRINT "Hello, World!"'
            if "for" in request.code_context.lower():
                return "FOR I = 1 TO 10\nNEXT I"
        if request.language.lower() == "logo":
            if "forward" in request.code_context.lower():
                return "FORWARD 100 RIGHT 90"
            if "repeat" in request.code_context.lower():
                return "REPEAT 4 [FORWARD 100 RIGHT 90]"

        return "// AI-generated completion"

    def explain_error(self, request: AIRequest) -> AIResponse:
        """Explain programming error using GitHub Copilot"""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Provider not available"},
            )

        # Mock implementation - in reality would call GitHub Copilot API
        explanation = self._generate_mock_explanation(request)

        return AIResponse(
            success=True,
            content=explanation,
            suggestions=[explanation],
            confidence=0.8,
            metadata={"provider": "github_copilot", "model": "GPT-4"},
        )

    def review_code(self, request: AIRequest) -> AIResponse:
        """Review code for improvements using GitHub Copilot"""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Provider not available"},
            )

        # Mock implementation - in reality would call GitHub Copilot API
        review = self._generate_mock_review(request)

        return AIResponse(
            success=True,
            content=review,
            suggestions=[review],
            confidence=0.8,
            metadata={"provider": "github_copilot", "model": "GPT-4"},
        )

    def get_learning_suggestion(self, request: AIRequest) -> AIResponse:
        """Provide learning suggestions using GitHub Copilot"""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Provider not available"},
            )

        # Mock implementation - in reality would call GitHub Copilot API
        suggestion = self._generate_mock_learning_suggestion(request)

        return AIResponse(
            success=True,
            content=suggestion,
            suggestions=[suggestion],
            confidence=0.8,
            metadata={"provider": "github_copilot", "model": "GPT-4"},
        )

    def _generate_mock_explanation(self, request: AIRequest) -> str:
        """Generate mock error explanation"""
        if request.error_message:
            return (
                f"This error '{request.error_message}' typically occurs when "
                f"there's a syntax issue in your {request.language} code. "
                "Check for missing semicolons, incorrect variable names, "
                "or mismatched parentheses."
            )
        return (
            "The error seems to be related to the code structure. "
            "Please check the syntax and ensure all required elements "
            "are present."
        )

    def _generate_mock_review(self, request: AIRequest) -> str:
        """Generate mock code review"""
        return (
            f"Your {request.language} code looks good! Consider adding more "
            "comments for clarity and ensure proper error handling. "
            "The logic appears sound."
        )

    def _generate_mock_learning_suggestion(self, request: AIRequest) -> str:
        """Generate mock learning suggestion"""
        if request.language.lower() == "basic":
            return (
                "Try learning about loops and conditional statements in "
                "BASIC. "
                "Practice with simple programs that use FOR/NEXT loops and "
                "IF/THEN statements."
            )
        if request.language.lower() == "logo":
            return (
                "Experiment with turtle graphics! Try creating patterns using "
                "REPEAT commands and combining FORWARD with RIGHT/LEFT turns."
            )
        return (
            f"Practice writing small programs in {request.language}. "
            "Focus on understanding the basic syntax and common patterns."
        )


class OllamaProvider(AIProviderInterface):
    """Compatibility placeholder for the Ollama provider."""

    def __init__(self, config: Dict[str, Any]):
        # Initialize provider state and attributes required by methods
        super().__init__(config)
        self.model: Optional[str] = None
        self.host: Optional[str] = None

    def initialize(self) -> bool:  # compatibility
        self.is_available = False
        return False

    def generate_completion(self, request: AIRequest) -> AIResponse:
        """Generate completion using local Ollama model"""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Ollama not available"},
            )

        # `requests` is optional; if the dependency is missing return a
        # helpful error so callers can distinguish no-network vs no-deps.
        try:
            try:
                # Optional dependency - import at runtime only
                # pylint: disable=import-outside-toplevel
                import requests  # type: ignore
            except ImportError:  # pragma: no cover - optional dep
                return AIResponse(
                    False,
                    "",
                    [],
                    0.0,
                    {"error": "requests not installed"},
                )
            prompt = (
                f"You are a {request.language} programming assistant. "
                f"Complete this code:\n{request.code_context}"
            )

            payload = {"model": self.model, "prompt": prompt, "stream": False}

            response = requests.post(
                f"{self.host}/api/generate", json=payload, timeout=30
            )

            if response.status_code == 200:
                result = response.json()
                completion = result.get("response", "").strip()

                return AIResponse(
                    success=True,
                    content=completion,
                    suggestions=[completion],
                    confidence=0.75,
                    metadata={
                        "provider": "ollama",
                        "model": self.model,
                    },
                )

            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": f"API error: {response.status_code}"},
            )

        except requests.RequestException as exc:  # pragma: no cover - network
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": str(exc)},
            )

    def explain_error(self, request: AIRequest) -> AIResponse:
        """Explain programming error"""
        return AIResponse(
            False,
            "Not implemented",
            [],
            0.0,
            {"error": "Method not implemented"},
        )

    def review_code(self, request: AIRequest) -> AIResponse:
        """Review code for improvements"""
        return AIResponse(
            False,
            "Not implemented",
            [],
            0.0,
            {"error": "Method not implemented"},
        )

    def get_learning_suggestion(self, request: AIRequest) -> AIResponse:
        """Provide learning suggestions"""
        return AIResponse(
            False,
            "Not implemented",
            [],
            0.0,
            {"error": "Method not implemented"},
        )


class AIAssistant:
    """A tiny, disabled assistant.

    It exposes the same public methods but returns AIResponse payloads that
    clearly indicate that AI is not available. This helps the UI code handle
    the condition gracefully without needing to be rewritten.
    """

    def __init__(self):
        self.providers: Dict[AIProvider, AIProviderInterface] = {}
        self.active_provider = AIProvider.NONE

    def _load_config(self):
        # No configuration when AI is disabled
        self.active_provider = AIProvider.NONE

    def _save_config(self):
        # No-op when AI is disabled
        return

    def _initialize_providers(self):
        # No providers; keep the dict empty so callers get an empty list
        self.providers = {}

    def set_active_provider(self, _provider: AIProvider) -> bool:
        # Cannot set a provider when AI is disabled
        # Parameter intentionally unused in this stub implementation.
        return False

    def get_available_providers(self) -> List[AIProvider]:
        return []

    def is_provider_available(self, _provider: AIProvider) -> bool:
        # Always return False in disabled mode
        return False

    def generate_completion(self, _request: AIRequest) -> AIResponse:
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "AI is disabled"},
        )

    def explain_error(self, _request: AIRequest) -> AIResponse:
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "AI is disabled"},
        )

    def review_code(self, _request: AIRequest) -> AIResponse:
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "AI is disabled"},
        )

    def get_learning_suggestion(self, _request: AIRequest) -> AIResponse:
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "AI is disabled"},
        )


def get_ai_assistant() -> AIAssistant:
    """Get the global AI assistant instance"""
    if "_ai_assistant" not in globals():
        globals()["_ai_assistant"] = AIAssistant()
    return globals()["_ai_assistant"]
