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
