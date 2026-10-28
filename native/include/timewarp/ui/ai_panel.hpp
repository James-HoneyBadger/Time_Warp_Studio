// SPDX-License-Identifier: MIT
//
// The Assistant page: a conversation beside the editor, with the program in view.
//
// A panel rather than a dialog because the point of the feature is to work *with*
// the program: explain this, fix this, comment this, translate this. A modal window
// that hides the code makes every second question a matter of closing a window.
//
// #### What it owns
//
// The conversation, the request in flight, and the decisions about what to send.
// The window owns the configuration (which is a setting, and persists) and the
// document (which is a document). The panel asks the window for the program through
// a provider callback, so this widget never reaches into a tab it does not own and a
// test can hand it a document of its own.
//
// #### What it will not do
//
// It will not send anything until the user presses a button. There is no suggestion
// that appears after a pause, no automatic explanation of a program the user has
// not asked about, and no request built at start-up: the program on screen is the
// user's, and sending it anywhere is an act they take.

#pragma once

#include <QFrame>
#include <QList>
#include <QString>

#include <functional>

#include "timewarp/ai/config.hpp"
#include "timewarp/ai/prompt.hpp"

class QCheckBox;
class QLabel;
class QPlainTextEdit;
class QPushButton;
class QTextBrowser;

namespace timewarp::ai {
class Client;
}

namespace timewarp::ui {

class AiPanel final : public QFrame {
    Q_OBJECT

public:
    explicit AiPanel(QWidget* parent = nullptr);

    /// Takes the configuration in force. The panel does not save it: a setting is
    /// the window's, and this only reads it.
    void setConfig(const timewarp::ai::Config& config);
    [[nodiscard]] const timewarp::ai::Config& config() const noexcept { return config_; }

    /// How the panel learns what is on screen. Called once per request, on the GUI
    /// thread, so what is sent is the document as it is at the moment the button is
    /// pressed rather than as it was when the panel was built.
    void setContextProvider(std::function<timewarp::ai::Context()> provider);

    /// Asks the assistant. `input` is the user's words: the question, a description
    /// for `Write`, or the target language for `Translate`.
    void ask(timewarp::ai::Task task, const QString& input = QString{});

    /// Puts the cursor in the question box and raises the panel's page.
    void focusInput();
    void clearConversation();

    [[nodiscard]] bool busy() const noexcept { return busy_; }
    /// Whether the configuration can carry a request. The empty state is the
    /// not-configured one, and it says what to do about it.
    [[nodiscard]] bool configured() const;

    // -- for the tests ----------------------------------------------------
    [[nodiscard]] QTextBrowser* transcript() const noexcept { return transcript_; }
    [[nodiscard]] QPlainTextEdit* question() const noexcept { return question_; }
    [[nodiscard]] QLabel* status() const noexcept { return status_; }
    [[nodiscard]] QString lastAnswer() const { return last_answer_; }
    [[nodiscard]] bool emptyStateVisible() const;
    [[nodiscard]] int historySize() const { return history_.size(); }

signals:
    /// The user asked to configure the assistant; the window owns the dialog.
    void configureRequested();
    /// The user asked to put the last answer's code into the editor.
    void insertRequested(const QString& code);
    void statusMessage(const QString& message);

private:
    void buildUi();
    void connectClient();
    /// Everything one request needs: the panel's history plus the new turn.
    [[nodiscard]] timewarp::ai::Conversation compose(timewarp::ai::Task task,
                                                     const QString& input,
                                                     const timewarp::ai::Context& context) const;
    /// Adds one block of rich text at the end of the transcript.
    void appendHtml(const QString& html);
    void renderTurn(const QString& who, const QString& text);
    void renderNotice(const QString& text);
    void setBusy(bool busy);
    void updateEnabledState();
    void scrollToEnd();
    /// The description of what was sent, for the line above the answer.
    [[nodiscard]] QString contextLabel(const timewarp::ai::Context& context) const;

    timewarp::ai::Config config_;
    std::function<timewarp::ai::Context()> context_provider_;
    timewarp::ai::Client* client_ = nullptr;

    QLabel* empty_state_ = nullptr;
    QPushButton* configure_ = nullptr;
    QTextBrowser* transcript_ = nullptr;
    QPlainTextEdit* question_ = nullptr;
    QCheckBox* include_document_ = nullptr;
    QLabel* context_hint_ = nullptr;
    QLabel* privacy_ = nullptr;
    QLabel* status_ = nullptr;
    QPushButton* send_ = nullptr;
    QPushButton* stop_ = nullptr;
    QPushButton* clear_ = nullptr;
    QPushButton* insert_ = nullptr;
    QPushButton* copy_ = nullptr;
    QList<QPushButton*> task_buttons_;

    /// The turns since the last Clear, capped: an assistant that remembers the
    /// whole session costs more with every question and forgets the start of it
    /// anyway.
    QList<timewarp::ai::Message> history_;
    QString last_answer_;
    bool busy_ = false;
};

}  // namespace timewarp::ui
