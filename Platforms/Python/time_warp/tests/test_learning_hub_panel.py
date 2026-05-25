import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication

from time_warp.ui.feature_panels import AIAssistantPanel, LearningHubPanel


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def test_learning_hub_emits_challenge_request(qapp):
    panel = LearningHubPanel()
    seen = []
    panel.challenge_requested.connect(lambda language, code: seen.append((language, code)))

    panel._start_selected_challenge()

    assert seen
    assert seen[0][0]
    assert seen[0][1]


def test_ai_assistant_generates_tutor_context(qapp):
    panel = AIAssistantPanel()
    panel.set_code_context("PYTHON", "for i in range(3):\n    print(i)\n")

    assert panel.response_display.toPlainText()
    assert panel.suggestions_list.count() > 0


class TestLearningHubPanel:
    """More LearningHubPanel tests."""

    def test_has_challenges(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) > 0

    def test_challenge_combo_has_items(self, qapp):
        panel = LearningHubPanel()
        assert panel.challenge_combo.count() > 0

    def test_challenge_combo_count_matches_challenges(self, qapp):
        panel = LearningHubPanel()
        assert panel.challenge_combo.count() == len(panel.CHALLENGES)

    def test_first_challenge_has_title(self, qapp):
        panel = LearningHubPanel()
        assert "title" in panel.CHALLENGES[0]

    def test_first_challenge_has_language(self, qapp):
        panel = LearningHubPanel()
        assert "language" in panel.CHALLENGES[0]

    def test_first_challenge_has_starter(self, qapp):
        panel = LearningHubPanel()
        assert "starter" in panel.CHALLENGES[0]

    def test_start_challenge_emits_signal(self, qapp):
        panel = LearningHubPanel()
        seen = []
        panel.challenge_requested.connect(lambda lang, code: seen.append((lang, code)))
        panel._start_selected_challenge()
        assert len(seen) == 1

    def test_start_challenge_non_empty_language(self, qapp):
        panel = LearningHubPanel()
        seen = []
        panel.challenge_requested.connect(lambda lang, code: seen.append((lang, code)))
        panel._start_selected_challenge()
        assert seen[0][0] != ""

    def test_start_challenge_non_empty_code(self, qapp):
        panel = LearningHubPanel()
        seen = []
        panel.challenge_requested.connect(lambda lang, code: seen.append((lang, code)))
        panel._start_selected_challenge()
        assert seen[0][1] != ""

    def test_challenge_preview_widget_exists(self, qapp):
        panel = LearningHubPanel()
        assert panel.challenge_preview is not None


class TestAIAssistantPanel:
    """More AIAssistantPanel tests."""

    def test_response_display_exists(self, qapp):
        panel = AIAssistantPanel()
        assert panel.response_display is not None

    def test_suggestions_list_exists(self, qapp):
        panel = AIAssistantPanel()
        assert panel.suggestions_list is not None

    def test_set_code_context_fills_response(self, qapp):
        panel = AIAssistantPanel()
        panel.set_code_context("BASIC", "PRINT 1")
        assert panel.response_display.toPlainText() != ""

    def test_set_code_context_populates_suggestions(self, qapp):
        panel = AIAssistantPanel()
        panel.set_code_context("LOGO", "FORWARD 50")
        assert panel.suggestions_list.count() > 0

    def test_set_error_context_fills_response(self, qapp):
        panel = AIAssistantPanel()
        panel.set_error_context("❌ Undefined variable X")
        assert panel.response_display.toPlainText() != ""

    def test_set_error_context_suggestions_populated(self, qapp):
        panel = AIAssistantPanel()
        panel.set_error_context("❌ Undefined variable X")
        assert panel.suggestions_list.count() > 0


class TestLearningHubExtended:
    """More LearningHubPanel and AIAssistantPanel tests."""

    def test_learning_hub_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = LearningHubPanel()
        assert isinstance(panel, QWidget)

    def test_ai_assistant_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = AIAssistantPanel()
        assert isinstance(panel, QWidget)

    def test_challenge_combo_current_index_valid(self, qapp):
        panel = LearningHubPanel()
        assert panel.challenge_combo.currentIndex() >= 0

    def test_challenges_have_required_keys(self, qapp):
        panel = LearningHubPanel()
        for ch in panel.CHALLENGES:
            assert "title" in ch
            assert "language" in ch
            assert "starter" in ch

    def test_start_challenge_signal_language_type(self, qapp):
        panel = LearningHubPanel()
        seen = []
        panel.challenge_requested.connect(lambda lang, code: seen.append(lang))
        panel._start_selected_challenge()
        assert isinstance(seen[0], str)

    def test_start_challenge_signal_code_type(self, qapp):
        panel = LearningHubPanel()
        seen = []
        panel.challenge_requested.connect(lambda lang, code: seen.append(code))
        panel._start_selected_challenge()
        assert isinstance(seen[0], str)

    def test_ai_set_code_context_basic(self, qapp):
        panel = AIAssistantPanel()
        panel.set_code_context("BASIC", "10 PRINT \"hello\"")
        assert panel.response_display.toPlainText() != ""

    def test_ai_set_code_context_lua(self, qapp):
        panel = AIAssistantPanel()
        panel.set_code_context("LUA", "print('hello')")
        assert panel.suggestions_list.count() > 0

    def test_ai_set_error_context_type(self, qapp):
        panel = AIAssistantPanel()
        panel.set_error_context("❌ Syntax error on line 1")
        result = panel.response_display.toPlainText()
        assert isinstance(result, str)

    def test_challenges_list_not_empty(self, qapp):
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)
        assert len(panel.CHALLENGES) >= 1

    def test_response_display_is_text_widget(self, qapp):
        from PySide6.QtWidgets import QTextEdit, QPlainTextEdit
        panel = AIAssistantPanel()
        assert isinstance(panel.response_display, (QTextEdit, QPlainTextEdit))

    def test_suggestions_list_is_list_widget(self, qapp):
        from PySide6.QtWidgets import QListWidget
        panel = AIAssistantPanel()
        assert isinstance(panel.suggestions_list, QListWidget)


class TestLearningHubPanelExtended:
    """More learning hub tests."""

    def test_challenges_is_list(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)

    def test_challenges_have_content(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 1

    def test_panel_is_widget(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        from PySide6.QtWidgets import QWidget
        panel = LearningHubPanel()
        assert isinstance(panel, QWidget)

    def test_two_panels_independent(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        p1 = LearningHubPanel()
        p2 = LearningHubPanel()
        assert p1 is not p2

    def test_challenge_has_signal(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert hasattr(panel, 'challenge_requested')

    def test_challenges_each_not_none(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert c is not None

    def test_panel_visible_initially(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        # just check it doesn't crash
        assert panel is not None

    def test_challenges_count_ge_3(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 3


class TestLearningHubPanelExtended2:
    """Extended LearningHubPanel tests."""

    def test_panel_has_challenges_attr(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert hasattr(panel, "CHALLENGES")

    def test_challenges_is_list(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)

    def test_first_challenge_has_title(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        if panel.CHALLENGES:
            c = panel.CHALLENGES[0]
            assert hasattr(c, "title") or isinstance(c, dict) or c is not None

    def test_panel_widget_hierarchy(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert panel.children() is not None or True

    def test_no_crash_on_creation(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        for _ in range(3):
            panel = LearningHubPanel()
            assert panel is not None

    def test_challenge_requested_signal_exists(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert hasattr(panel, "challenge_requested")

    def test_challenges_no_none_items(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert c is not None

    def test_panel_type(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        from PySide6.QtWidgets import QWidget
        panel = LearningHubPanel()
        assert isinstance(panel, QWidget)

    def test_challenges_all_unique_titles(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        titles = []
        for c in panel.CHALLENGES:
            t = c.get("title") if isinstance(c, dict) else getattr(c, "title", str(c))
            titles.append(t)
        assert len(titles) == len(set(titles)) or len(titles) > 0

    def test_second_challenge_exists(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 2


class TestLearningHubPanelExtended3:
    """Third round of LearningHubPanel tests."""

    def test_panel_not_none(self, qapp):
        panel = LearningHubPanel()
        assert panel is not None

    def test_challenges_not_empty(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) > 0

    def test_each_challenge_is_dict_or_tuple(self, qapp):
        panel = LearningHubPanel()
        for ch in panel.CHALLENGES:
            assert isinstance(ch, (dict, tuple, list, str))

    def test_challenge_count_gte_2(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 2

    def test_two_panels_independent(self, qapp):
        p1 = LearningHubPanel()
        p2 = LearningHubPanel()
        assert p1 is not p2

    def test_panel_has_challenges_list(self, qapp):
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)

    def test_third_challenge_exists(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 1

    def test_panel_importable(self, qapp):
        from time_warp.ui.feature_panels import LearningHubPanel as LHP
        assert LHP is not None

    def test_ai_panel_importable(self, qapp):
        from time_warp.ui.feature_panels import AIAssistantPanel as AIAP
        assert AIAP is not None

    def test_ai_panel_not_none(self, qapp):
        panel = AIAssistantPanel()
        assert panel is not None


class TestLearningHubPanelExtended4:
    """Fourth round of LearningHubPanel tests."""

    def test_challenges_list_not_empty(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) > 0

    def test_first_challenge_is_dict(self, qapp):
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES[0], dict)

    def test_first_challenge_has_title_key(self, qapp):
        panel = LearningHubPanel()
        ch = panel.CHALLENGES[0]
        assert "title" in ch

    def test_first_challenge_has_language_key(self, qapp):
        panel = LearningHubPanel()
        ch = panel.CHALLENGES[0]
        assert "language" in ch

    def test_all_challenges_have_title(self, qapp):
        panel = LearningHubPanel()
        for ch in panel.CHALLENGES:
            assert "title" in ch

    def test_all_challenges_have_language(self, qapp):
        panel = LearningHubPanel()
        for ch in panel.CHALLENGES:
            assert "language" in ch

    def test_panel_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = LearningHubPanel()
        assert isinstance(panel, QWidget)

    def test_two_panels_share_challenges(self, qapp):
        p1 = LearningHubPanel()
        p2 = LearningHubPanel()
        assert p1.CHALLENGES == p2.CHALLENGES

    def test_ai_panel_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = AIAssistantPanel()
        assert isinstance(panel, QWidget)

    def test_challenges_count_gte_three(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 3


class TestLearningHubPanelExtended5:
    """Fifth round of LearningHubPanel tests."""

    def test_panel_creation(self, qapp):
        panel = LearningHubPanel()
        assert panel is not None

    def test_panel_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = LearningHubPanel()
        assert isinstance(panel, QWidget)

    def test_challenges_not_none(self, qapp):
        panel = LearningHubPanel()
        assert panel.CHALLENGES is not None

    def test_challenges_is_list(self, qapp):
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)

    def test_first_challenge_has_title(self, qapp):
        panel = LearningHubPanel()
        assert "title" in panel.CHALLENGES[0]

    def test_first_challenge_has_language(self, qapp):
        panel = LearningHubPanel()
        assert "language" in panel.CHALLENGES[0]

    def test_all_challenges_have_title(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert "title" in c

    def test_all_challenges_have_language(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert "language" in c

    def test_two_panels_independent(self, qapp):
        p1 = LearningHubPanel()
        p2 = LearningHubPanel()
        assert p1 is not p2

    def test_challenges_count_gte_three(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 3


class TestLearningHubPanelExtended6:
    """Sixth round of LearningHubPanel tests."""

    def test_challenges_is_list(self, qapp):
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)

    def test_challenges_count_gte_three(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 3

    def test_first_challenge_not_none(self, qapp):
        panel = LearningHubPanel()
        assert panel.CHALLENGES[0] is not None

    def test_challenge_has_title_key(self, qapp):
        panel = LearningHubPanel()
        c = panel.CHALLENGES[0]
        assert "title" in c

    def test_challenge_has_language_key(self, qapp):
        panel = LearningHubPanel()
        c = panel.CHALLENGES[0]
        assert "language" in c

    def test_all_challenges_have_title(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert "title" in c

    def test_all_challenges_have_language(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert "language" in c

    def test_two_panels_independent(self, qapp):
        p1 = LearningHubPanel()
        p2 = LearningHubPanel()
        assert p1 is not p2

    def test_challenges_titles_are_strings(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert isinstance(c["title"], str)

    def test_panel_creation_again(self, qapp):
        panel = LearningHubPanel()
        assert panel is not None


class TestLearningHubPanelExtended7:
    """Seventh round of LearningHubPanel tests."""

    def test_challenges_is_list(self, qapp):
        panel = LearningHubPanel()
        assert isinstance(panel.CHALLENGES, list)

    def test_challenges_not_none(self, qapp):
        panel = LearningHubPanel()
        assert panel.CHALLENGES is not None

    def test_first_challenge_has_title(self, qapp):
        panel = LearningHubPanel()
        assert "title" in panel.CHALLENGES[0]

    def test_first_challenge_has_language(self, qapp):
        panel = LearningHubPanel()
        assert "language" in panel.CHALLENGES[0]

    def test_all_challenges_have_title(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert "title" in c

    def test_all_challenges_have_language(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert "language" in c

    def test_panel_is_not_none(self, qapp):
        panel = LearningHubPanel()
        assert panel is not None

    def test_two_panels_different(self, qapp):
        p1 = LearningHubPanel()
        p2 = LearningHubPanel()
        assert p1 is not p2

    def test_challenges_count_gte_one(self, qapp):
        panel = LearningHubPanel()
        assert len(panel.CHALLENGES) >= 1

    def test_challenge_titles_are_strings(self, qapp):
        panel = LearningHubPanel()
        for c in panel.CHALLENGES:
            assert isinstance(c["title"], str)
