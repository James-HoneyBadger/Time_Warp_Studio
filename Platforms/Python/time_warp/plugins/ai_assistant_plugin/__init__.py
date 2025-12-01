"""
AI Assistant Plugin for Time Warp IDE
Provides AI-powered code assistance features
"""

from time_warp.ai import AIRequest, AIRequestType, get_ai_assistant


def initialize(context):
    """Initialize the AI assistant plugin"""
    print("🤖 AI Assistant Plugin initialized")

    # Store context for later use - use module state to avoid global statement
    _plugin_state["context"] = context

    # Add AI menu items
    context.add_menu_item(
        "AI/Code Completion", "Complete Code", show_code_completion_dialog
    )
    context.add_menu_item(
        "AI/Error Explanation", "Explain Error", show_error_explanation_dialog
    )
    menu_category = "AI/Code Review"
    menu_label = "Review Code"
    context.add_menu_item(menu_category, menu_label, show_code_review_dialog)
    context.add_menu_item(
        "AI/Learning Tips", "Get Learning Tips", show_learning_tips_dialog
    )
    context.add_menu_item(
        "AI/Provider Settings",
        "Configure AI Provider",
        show_provider_settings,
    )

    # Add toolbar button
    context.add_toolbar_button("🤖", "AI Assistant", show_ai_menu)

    # Hook into editor events
    if hasattr(context.ide, "editor"):
        # Connect to editor selection changes for context-aware AI
        pass

    return True


def cleanup(context):
    """Clean up AI assistant plugin resources"""
    print("🤖 AI Assistant Plugin cleaned up")
    # Use the context argument (plugin API compatibility) so linters won't
    # complain about unused parameters.
    _ = context
    _plugin_state["context"] = None


def show_code_completion_dialog():
    """Show dialog for code completion"""
    print("🤖 Opening code completion dialog...")
    # In a real implementation, this would open a dialog
    # For now, just demonstrate the functionality
    demonstrate_ai_feature("code_completion")


def show_error_explanation_dialog():
    """Show dialog for error explanation"""
    print("🤖 Opening error explanation dialog...")
    demonstrate_ai_feature("error_explanation")


def show_code_review_dialog():
    """Show dialog for code review"""
    print("🤖 Opening code review dialog...")
    demonstrate_ai_feature("code_review")


def show_learning_tips_dialog():
    """Show dialog for learning tips"""
    print("🤖 Opening learning tips dialog...")
    demonstrate_ai_feature("learning_suggestion")


def show_provider_settings():
    """Show AI provider configuration dialog"""
    print("🤖 Opening AI provider settings...")

    ai_assistant = get_ai_assistant()
    available_providers = ai_assistant.get_available_providers()

    print("Available AI providers:")
    for provider in available_providers:
        status = (
            "✅ Available"
            if ai_assistant.is_provider_available(provider)
            else "❌ Unavailable"
        )
        print(f"  - {provider.value}: {status}")

    if available_providers:
        print("\nTo configure, set environment variables:")
        print(
            "  For Ollama: OLLAMA_HOST=http://localhost:11434 "
            "OLLAMA_MODEL=codellama:7b"
        )
        print("  For GitHub Copilot: GITHUB_TOKEN=your_token_here")
    else:
        print(
            "\nNo AI providers available. Install Ollama or configure "
            "GitHub Copilot."
        )


def show_ai_menu():
    """Show AI assistant menu"""
    print("🤖 AI Assistant Menu")
    print("===================")
    print("1. Code Completion")
    print("2. Error Explanation")
    print("3. Code Review")
    print("4. Learning Tips")
    print("5. Provider Settings")
    print("\nSelect an option or use the AI menu in the menu bar.")


def demonstrate_ai_feature(feature_type):
    """Demonstrate an AI feature with sample data"""
    ai_assistant = get_ai_assistant()

    # Create sample request based on feature type
    if feature_type == "code_completion":
        request = AIRequest(
            request_type=AIRequestType.CODE_COMPLETION,
            code_context="PRINT",
            language="BASIC",
        )
        response = ai_assistant.generate_completion(request)
        print(f"🤖 Code Completion Result: {response.content}")

    elif feature_type == "error_explanation":
        request = AIRequest(
            request_type=AIRequestType.ERROR_EXPLANATION,
            code_context="PRINT",
            language="BASIC",
            error_message="Syntax error: unexpected end of line",
        )
        response = ai_assistant.explain_error(request)
        print(f"🤖 Error Explanation: {response.content}")

    elif feature_type == "code_review":
        request = AIRequest(
            request_type=AIRequestType.CODE_REVIEW,
            code_context="PRINT 'Hello'\nPRINT 'World'",
            language="BASIC",
        )
        response = ai_assistant.review_code(request)
        print(f"🤖 Code Review: {response.content}")

    elif feature_type == "learning_suggestion":
        request = AIRequest(
            request_type=AIRequestType.LEARNING_SUGGESTION,
            code_context="FOR I = 1 TO 10\nPRINT I\nNEXT I",
            language="BASIC",
            user_level="beginner",
        )
        response = ai_assistant.get_learning_suggestion(request)
        print(f"🤖 Learning Suggestion: {response.content}")


def on_code_selected(selected_text, language):
    """Hook called when code is selected in editor"""
    if len(selected_text.strip()) > 10:  # Only for substantial selections
        print(
            f"🤖 AI Assistant: Code selected ({len(selected_text)} chars) "
            f"in {language}"
        )
        # Could offer context menu items for AI operations on selected code


def on_error_occurred(error_message, code_context, language):
    """Hook called when an error occurs"""
    print(f"🤖 AI Assistant: Error detected in {language}")
    # Reference unused args to satisfy linters; in future can provide
    # a richer handling using these parameters
    _ = (error_message, code_context)
    # Could automatically offer error explanation


# Plugin metadata
PLUGIN_NAME = "AI Assistant Plugin"
PLUGIN_VERSION = "1.0.0"
PLUGIN_DESCRIPTION = "AI-powered code assistance for Time Warp IDE"

# Global context reference
# Plugin state
_plugin_state = {"context": None}
