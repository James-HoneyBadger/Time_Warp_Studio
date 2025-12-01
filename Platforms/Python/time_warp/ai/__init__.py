"""time_warp.ai — AI Assistant Implementation

This module provides AI-powered assistance for programming education,
including code completion, error explanation, code review, and learning
suggestions. Supports multiple AI providers including local Ollama models
and GitHub Copilot.
"""

import json
import os
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)
# Pylint: This module is large; acceptable for now (split if needed).
# pylint: disable=too-many-lines

# Optional requests import: gracefully handle environments where requests
# is not installed (e.g., CI runners or stripped-down containers).
try:
    import requests  # type: ignore
except ImportError:  # pragma: no cover - optional dependency
    requests = None  # type: ignore


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
        """Initialize the provider (no-op for compatibility stub)."""
        return False

    def generate_completion(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        """Return a code completion response (stubbed in disabled build)."""
        raise RuntimeError("AI is disabled in this build")

    def explain_error(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        """Return an explanation for a programming error (stubbed)."""
        raise RuntimeError("AI is disabled in this build")

    def review_code(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        """Return a code review (stubbed when AI disabled)."""
        raise RuntimeError("AI is disabled in this build")

    def get_learning_suggestion(
        self, request: AIRequest
    ) -> AIResponse:  # pragma: no cover - compatibility
        """Return learning suggestions (stubbed in this build)."""
        raise RuntimeError("AI is disabled in this build")


class GitHubCopilotProvider(AIProviderInterface):
    """GitHub Copilot provider for AI assistance."""

    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.api_key = config.get("api_key", os.getenv("GITHUB_TOKEN"))
        self.model = config.get("model", "gpt-4")
        self.base_url = config.get("base_url", "https://api.github.com")
        self.timeout = config.get("timeout", 30)

    def initialize(self) -> bool:
        """Initialize the GitHub Copilot provider."""
        if not self.api_key:
            logger.warning("No GitHub token provided for Copilot provider")
            self.is_available = False
            return False

        if requests is None:
            logger.warning("requests not available")
            self.is_available = False
            return False

        try:
            headers = {
                "Authorization": f"Bearer {self.api_key}",
                "Accept": "application/vnd.github.v3+json",
            }
            response = requests.get(
                f"{self.base_url}/user", headers=headers, timeout=10
            )

            if response.status_code == 200:
                self.is_available = True
                logger.info("GitHub Copilot provider initialized successfully")
                return True
            else:
                logger.warning(
                    "GitHub API authentication failed: %s",
                    response.status_code,
                )
        except requests.exceptions.RequestException as e:
            logger.warning(
                "Failed to initialize GitHub Copilot provider: %s",
                e,
            )

        self.is_available = False
        return False

    def generate_completion(self, request: AIRequest) -> AIResponse:
        """Generate code completion using GitHub Copilot."""
        if not self.is_available:
            return AIResponse(
                False, "", [], 0.0, {"error": "GitHub Copilot not available"}
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_completion_prompt(request)
            payload = {
                "model": self.model,
                "messages": [
                    {
                        "role": "system",
                        "content": (
                            f"You are a {request.language} programming "
                            "assistant. Provide only the code completion, "
                            "no explanations."
                        ),
                    },
                    {"role": "user", "content": prompt},
                ],
                "temperature": 0.3,
                "max_tokens": 500,
            }

            headers = {
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json",
            }

            response = requests.post(
                "https://api.githubcopilot.com/chat/completions",
                json=payload,
                headers=headers,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                completion = (
                    result.get("choices", [{}])[0]
                    .get("message", {})
                    .get("content", "")
                    .strip()
                )

                return AIResponse(
                    success=True,
                    content=completion,
                    suggestions=[completion],
                    confidence=0.9,
                    metadata={
                        "provider": "github_copilot",
                        "model": self.model,
                        "usage": result.get("usage", {}),
                    },
                )
            else:
                err_msg = f"GitHub Copilot API error: {response.status_code}"
                return AIResponse(False, "", [], 0.0, {"error": err_msg})

        except (
            requests.exceptions.RequestException,
            ValueError,
        ) as e:
            logger.error(
                "GitHub Copilot completion error: %s",
                e,
            )
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def explain_error(self, request: AIRequest) -> AIResponse:
        """Explain programming errors using GitHub Copilot."""
        if not self.is_available:
            return AIResponse(
                False, "", [], 0.0, {"error": "GitHub Copilot not available"}
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_error_explanation_prompt(request)
            payload = {
                "model": self.model,
                "messages": [
                    {
                        "role": "system",
                        "content": (
                            "You are a helpful programming tutor. "
                            "Explain errors clearly and encouragingly "
                            "for beginners."
                        ),
                    },
                    {"role": "user", "content": prompt},
                ],
                "temperature": 0.1,
                "max_tokens": 1000,
            }

            headers = {
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json",
            }

            response = requests.post(
                "https://api.githubcopilot.com/chat/completions",
                json=payload,
                headers=headers,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                explanation = (
                    result.get("choices", [{}])[0]
                    .get("message", {})
                    .get("content", "")
                    .strip()
                )

                return AIResponse(
                    success=True,
                    content=explanation,
                    suggestions=[explanation],
                    confidence=0.95,
                    metadata={
                        "provider": "github_copilot",
                        "model": self.model,
                        "type": "error_explanation",
                        "usage": result.get("usage", {}),
                    },
                )
            else:
                err_msg = f"GitHub Copilot API error: {response.status_code}"
                return AIResponse(False, "", [], 0.0, {"error": err_msg})

        except (
            requests.exceptions.RequestException,
            ValueError,
        ) as e:
            logger.error(
                "GitHub Copilot error explanation error: %s",
                e,
            )
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def review_code(self, request: AIRequest) -> AIResponse:
        """Review code for improvements using GitHub Copilot."""
        if not self.is_available:
            return AIResponse(
                False, "", [], 0.0, {"error": "GitHub Copilot not available"}
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_code_review_prompt(request)
            payload = {
                "model": self.model,
                "messages": [
                    {
                        "role": "system",
                        "content": (
                            "You are a code reviewer. Provide constructive, "
                            "educational feedback focused on best practices "
                            "and improvements."
                        ),
                    },
                    {"role": "user", "content": prompt},
                ],
                "temperature": 0.2,
                "max_tokens": 1500,
            }

            headers = {
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json",
            }

            response = requests.post(
                "https://api.githubcopilot.com/chat/completions",
                json=payload,
                headers=headers,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                review = (
                    result.get("choices", [{}])[0]
                    .get("message", {})
                    .get("content", "")
                    .strip()
                )

                return AIResponse(
                    success=True,
                    content=review,
                    suggestions=[review],
                    confidence=0.85,
                    metadata={
                        "provider": "github_copilot",
                        "model": self.model,
                        "type": "code_review",
                        "usage": result.get("usage", {}),
                    },
                )
            else:
                err_msg = f"GitHub Copilot API error: {response.status_code}"
                return AIResponse(False, "", [], 0.0, {"error": err_msg})

        except (
            requests.exceptions.RequestException,
            ValueError,
        ) as e:
            logger.error(
                "GitHub Copilot code review error: %s",
                e,
            )
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def get_learning_suggestion(self, request: AIRequest) -> AIResponse:
        """Provide learning suggestions using GitHub Copilot."""
        if not self.is_available:
            return AIResponse(
                False, "", [], 0.0, {"error": "GitHub Copilot not available"}
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_learning_prompt(request)
            payload = {
                "model": self.model,
                "messages": [
                    {
                        "role": "system",
                        "content": (
                            "You are a programming teacher. Suggest "
                            "appropriate next learning steps based on the "
                            "student's current code and skill level."
                        ),
                    },
                    {"role": "user", "content": prompt},
                ],
                "temperature": 0.3,
                "max_tokens": 1000,
            }

            headers = {
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json",
            }

            response = requests.post(
                "https://api.githubcopilot.com/chat/completions",
                json=payload,
                headers=headers,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                suggestion = (
                    result.get("choices", [{}])[0]
                    .get("message", {})
                    .get("content", "")
                    .strip()
                )

                return AIResponse(
                    success=True,
                    content=suggestion,
                    suggestions=[suggestion],
                    confidence=0.9,
                    metadata={
                        "provider": "github_copilot",
                        "model": self.model,
                        "type": "learning_suggestion",
                        "usage": result.get("usage", {}),
                    },
                )
            else:
                err_msg = f"GitHub Copilot API error: {response.status_code}"
                return AIResponse(False, "", [], 0.0, {"error": err_msg})

        except (
            requests.exceptions.RequestException,
            ValueError,
        ) as e:
            logger.error(
                "GitHub Copilot learning suggestion error: %s",
                e,
            )
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def _build_completion_prompt(self, request: AIRequest) -> str:
        """Build a completion prompt for the given request."""
        language = request.language.lower()
        context = request.code_context.strip()

        return (
            f"Complete this {language} code:\n\n"
            f"{context}\n\n"
            "Provide only the completion:"
        )

    def _build_error_explanation_prompt(self, request: AIRequest) -> str:
        """Build an error explanation prompt."""
        language = request.language.lower()
        context = request.code_context.strip()
        error = request.error_message or "Unknown error"

        return (
            "Explain this "
            f"{language} error in simple terms for a beginner:\n\n"
            f"Code:\n{context}\n\nError: {error}\n\n"
            "Please explain what went wrong and how to fix it."
        )

    def _build_code_review_prompt(self, request: AIRequest) -> str:
        """Build a code review prompt."""
        language = request.language.lower()
        context = request.code_context.strip()

        return (
            f"Review this {language} code and provide constructive "
            "feedback:\n\n"
            f"{context}\n\n"
            "Focus on:\n- Correctness\n- Best practices\n- Readability\n"
            "- Potential improvements\n"
            "- Educational value\n\n"
            "Be encouraging and provide specific suggestions."
        )

    def _build_learning_prompt(self, request: AIRequest) -> str:
        """Build a learning suggestion prompt."""
        language = request.language.lower()
        context = request.code_context.strip()
        level = request.user_level

        return (
            f"Based on this {language} code from a {level} "
            "programmer, suggest 2-3 specific next learning objectives "
            "or exercises:\n\n"
            f"Code:\n{context}\n\nWhat should they learn or practice next?"
        )


class OllamaProvider(AIProviderInterface):
    """Ollama provider for local AI models."""

    def __init__(self, config: Dict[str, Any]):
        super().__init__(config)
        self.model = config.get("model", "codellama:7b")
        self.host = config.get("host", "http://localhost:11434")
        self.timeout = config.get("timeout", 30)

    def initialize(self) -> bool:
        """Initialize the Ollama provider by checking availability."""
        if requests is None:
            logger.warning("requests not available for Ollama provider")
            self.is_available = False
            return False

        try:
            # Test connection to Ollama
            response = requests.get(
                f"{self.host}/api/tags",
                timeout=5,
            )
            if response.status_code == 200:
                models = response.json().get("models", [])
                model_names = [m.get("name") for m in models]
                if self.model in model_names:
                    self.is_available = True
                    logger.info(
                        "Ollama provider initialized with model %s",
                        self.model,
                    )
                    return True
                else:
                    logger.warning(
                        "Model %s not found in Ollama. Available: %s",
                        self.model,
                        model_names,
                    )
            else:
                logger.warning(
                    "Ollama API returned status %s",
                    response.status_code,
                )
        except requests.exceptions.RequestException as e:
            logger.warning("Failed to connect to Ollama: %s", e)

        self.is_available = False
        return False

    def generate_completion(self, request: AIRequest) -> AIResponse:
        """Generate code completion using Ollama."""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Ollama not available"},
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_completion_prompt(request)
            payload = {
                "model": self.model,
                "prompt": prompt,
                "stream": False,
                "options": {"temperature": 0.3, "top_p": 0.9},
            }

            response = requests.post(
                f"{self.host}/api/generate",
                json=payload,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                completion = result.get("response", "").strip()

                return AIResponse(
                    success=True,
                    content=completion,
                    suggestions=[completion],
                    confidence=0.8,
                    metadata={
                        "provider": "ollama",
                        "model": self.model,
                        "eval_count": result.get("eval_count", 0),
                        "eval_duration": result.get("eval_duration", 0),
                    },
                )
            else:
                return AIResponse(
                    False,
                    "",
                    [],
                    0.0,
                    {"error": f"Ollama API error: " f"{response.status_code}"},
                )

        except (requests.exceptions.RequestException, ValueError) as e:
            logger.error("Ollama completion error: %s", e)
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def explain_error(self, request: AIRequest) -> AIResponse:
        """Explain programming errors using Ollama."""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Ollama not available"},
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_error_explanation_prompt(request)
            payload = {
                "model": self.model,
                "prompt": prompt,
                "stream": False,
                "options": {"temperature": 0.1, "top_p": 0.9},
            }

            response = requests.post(
                f"{self.host}/api/generate",
                json=payload,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                explanation = result.get("response", "").strip()

                return AIResponse(
                    success=True,
                    content=explanation,
                    suggestions=[explanation],
                    confidence=0.85,
                    metadata={
                        "provider": "ollama",
                        "model": self.model,
                        "type": "error_explanation",
                    },
                )
            else:
                return AIResponse(
                    False,
                    "",
                    [],
                    0.0,
                    {"error": f"Ollama API error: " f"{response.status_code}"},
                )

        except (requests.exceptions.RequestException, ValueError) as e:
            logger.error("Ollama error explanation error: %s", e)
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def review_code(self, request: AIRequest) -> AIResponse:
        """Review code for improvements using Ollama."""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Ollama not available"},
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_code_review_prompt(request)
            payload = {
                "model": self.model,
                "prompt": prompt,
                "stream": False,
                "options": {"temperature": 0.2, "top_p": 0.9},
            }

            response = requests.post(
                f"{self.host}/api/generate",
                json=payload,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                review = result.get("response", "").strip()

                return AIResponse(
                    success=True,
                    content=review,
                    suggestions=[review],
                    confidence=0.75,
                    metadata={
                        "provider": "ollama",
                        "model": self.model,
                        "type": "code_review",
                    },
                )
            else:
                return AIResponse(
                    False,
                    "",
                    [],
                    0.0,
                    {"error": f"Ollama API error: " f"{response.status_code}"},
                )

        except (requests.exceptions.RequestException, ValueError) as e:
            logger.error("Ollama code review error: %s", e)
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def get_learning_suggestion(self, request: AIRequest) -> AIResponse:
        """Provide learning suggestions using Ollama."""
        if not self.is_available:
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "Ollama not available"},
            )

        if requests is None:
            logger.warning("requests not available")
            return AIResponse(
                False,
                "",
                [],
                0.0,
                {"error": "requests not available"},
            )

        try:
            prompt = self._build_learning_prompt(request)
            payload = {
                "model": self.model,
                "prompt": prompt,
                "stream": False,
                "options": {"temperature": 0.3, "top_p": 0.9},
            }

            response = requests.post(
                f"{self.host}/api/generate",
                json=payload,
                timeout=self.timeout,
            )

            if response.status_code == 200:
                result = response.json()
                suggestion = result.get("response", "").strip()

                return AIResponse(
                    success=True,
                    content=suggestion,
                    suggestions=[suggestion],
                    confidence=0.8,
                    metadata={
                        "provider": "ollama",
                        "model": self.model,
                        "type": "learning_suggestion",
                    },
                )
            else:
                return AIResponse(
                    False,
                    "",
                    [],
                    0.0,
                    {"error": f"Ollama API error: " f"{response.status_code}"},
                )

        except (requests.exceptions.RequestException, ValueError) as e:
            logger.error("Ollama learning suggestion error: %s", e)
            return AIResponse(False, "", [], 0.0, {"error": str(e)})

    def _build_completion_prompt(self, request: AIRequest) -> str:
        """Build a completion prompt for the given request."""
        language = request.language.lower()
        context = request.code_context.strip()

        if language == "basic":
            return (
                "You are a BASIC programming assistant. "
                "Complete the following BASIC code naturally and correctly. "
                "Only provide the completion, no explanations.\n\n"
                f"Code to complete:\n{context}\n\nCompletion:"
            )
        elif language == "logo":
            return (
                "You are a Logo programming assistant. Complete the following "
                "Logo code naturally and correctly. Only provide the "
                "completion, no explanations.\n\n"
                f"Code to complete:\n{context}\n\nCompletion:"
            )
        elif language == "pilot":
            return (
                "You are a PILOT programming assistant. "
                "Complete the following PILOT code naturally and correctly. "
                "Only provide the completion, no explanations.\n\n"
                f"Code to complete:\n{context}\n\nCompletion:"
            )
        else:
            return (
                f"You are a {language} programming assistant. Complete the "
                f"following {language} code naturally and correctly. Only "
                "provide the completion, no explanations.\n\n"
                f"Code to complete:\n{context}\n\nCompletion:"
            )

    def _build_error_explanation_prompt(self, request: AIRequest) -> str:
        """Build an error explanation prompt."""
        language = request.language.lower()
        context = request.code_context.strip()
        error = request.error_message or "Unknown error"

        return (
            f"You are a {language} programming tutor. Explain this error in "
            "simple, educational terms suitable for beginners. Be helpful "
            "and encouraging.\n\n"
            f"Language: {language}\nCode: {context}\nError: {error}\n\n"
            "Explanation:"
        )

    def _build_code_review_prompt(self, request: AIRequest) -> str:
        """Build a code review prompt."""
        language = request.language.lower()
        context = request.code_context.strip()

        return (
            f"You are a {language} programming mentor. Review this code and "
            "provide constructive feedback. Focus on:\n- Code correctness\n- "
            "Best practices\n- Readability\n- Potential improvements\n- "
            "Educational value\n\n"
            "Be encouraging and educational. Keep suggestions "
            "actionable.\n\nLanguage: {language}\nCode to review:\n"
            f"{context}\n\nReview:"
        )

    def _build_learning_prompt(self, request: AIRequest) -> str:
        """Build a learning suggestion prompt."""
        language = request.language.lower()
        context = request.code_context.strip()
        level = request.user_level

        return (
            f"You are a {language} programming teacher. Suggest what the "
            f"student should learn next based on their current code. "
            f"Consider their skill level ({level}) and suggest appropriate "
            "next steps.\n\nCurrent code:\n"
            f"{context}\n\n"
            "Suggest 2-3 specific learning objectives or exercises "
            "they should try next:"
        )


class AIAssistant:
    """AI assistant with multiple provider support."""

    def __init__(self):
        self.providers: Dict[AIProvider, AIProviderInterface] = {}
        self.active_provider = AIProvider.NONE
        self.config_file = os.path.expanduser("~/.time_warp/ai_config.json")
        self._load_config()
        self._initialize_providers()

    def _load_config(self):
        """Load AI configuration from file."""
        try:
            if os.path.exists(self.config_file):
                with open(self.config_file, "r", encoding="utf-8") as f:
                    config = json.load(f)
                    provider_name = config.get("active_provider")
                    if provider_name:
                        try:
                            self.active_provider = AIProvider(provider_name)
                        except ValueError:
                            logger.warning(
                                "Unknown provider in config: %s",
                                provider_name,
                            )
                            self.active_provider = AIProvider.NONE
                    else:
                        self.active_provider = AIProvider.NONE
            else:
                self.active_provider = AIProvider.NONE
        except (OSError, ValueError) as e:
            logger.error("Failed to load AI config: %s", e)
            self.active_provider = AIProvider.NONE

    def _save_config(self):
        """Save AI configuration to file."""
        try:
            config_dir = os.path.dirname(self.config_file)
            os.makedirs(config_dir, exist_ok=True)

            active_provider_value = (
                self.active_provider.value if self.active_provider else None
            )
            config = {"active_provider": active_provider_value}

            with open(self.config_file, "w", encoding="utf-8") as f:
                json.dump(config, f, indent=2)
        except OSError as e:
            logger.error("Failed to save AI config: %s", e)

    def _initialize_providers(self):
        """Initialize available AI providers."""
        # Ollama provider (local)
        ollama_config = {
            "model": os.getenv("OLLAMA_MODEL", "codellama:7b"),
            "host": os.getenv("OLLAMA_HOST", "http://localhost:11434"),
            "timeout": 30,
        }
        self.providers[AIProvider.LOCAL_OLLAMA] = OllamaProvider(ollama_config)

        # GitHub Copilot provider
        github_config = {
            "api_key": os.getenv("GITHUB_TOKEN"),
            "model": "gpt-4",
            "timeout": 30,
        }
        gh_provider = GitHubCopilotProvider(github_config)
        self.providers[AIProvider.GITHUB_COPILOT] = gh_provider

        # Initialize all providers
        for provider in self.providers.values():
            provider.initialize()

    def set_active_provider(self, provider: AIProvider) -> bool:
        """Set the active AI provider."""
        provider_obj = self.providers.get(provider)
        if provider_obj is not None and provider_obj.is_available:
            self.active_provider = provider
            self._save_config()
            logger.info("Active AI provider set to: %s", provider.value)
            return True
        else:
            logger.warning("Provider %s not available", provider.value)
            return False

    def get_available_providers(self) -> List[AIProvider]:
        """Get list of available providers."""
        return [p for p, prov in self.providers.items() if prov.is_available]

    def is_provider_available(self, provider: AIProvider) -> bool:
        """Check if a provider is available."""
        prov = self.providers.get(provider)
        return prov is not None and prov.is_available

    def generate_completion(self, request: AIRequest) -> AIResponse:
        """Generate code completion."""
        if self.active_provider and self.active_provider in self.providers:
            prov = self.providers[self.active_provider]
            return prov.generate_completion(request)
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "No active AI provider"},
        )

    def explain_error(self, request: AIRequest) -> AIResponse:
        """Explain programming errors."""
        if self.active_provider and self.active_provider in self.providers:
            prov = self.providers[self.active_provider]
            return prov.explain_error(request)
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "No active AI provider"},
        )

    def review_code(self, request: AIRequest) -> AIResponse:
        """Review code for improvements."""
        if self.active_provider and self.active_provider in self.providers:
            prov = self.providers[self.active_provider]
            return prov.review_code(request)
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "No active AI provider"},
        )

    def get_learning_suggestion(self, request: AIRequest) -> AIResponse:
        """Provide learning suggestions."""
        if self.active_provider and self.active_provider in self.providers:
            prov = self.providers[self.active_provider]
            return prov.get_learning_suggestion(request)
        return AIResponse(
            success=False,
            content="",
            suggestions=[],
            confidence=0.0,
            metadata={"error": "No active AI provider"},
        )


def get_ai_assistant() -> AIAssistant:
    """Get the global AI assistant instance"""
    if "_ai_assistant" not in globals():
        globals()["_ai_assistant"] = AIAssistant()
    return globals()["_ai_assistant"]
