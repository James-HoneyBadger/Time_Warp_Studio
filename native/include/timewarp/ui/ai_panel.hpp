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
// It will not send anything until the user presses a button. The program on screen
// is the user's, and sending it anywhere is an act they take: nothing here is built
// at start-up and nothing is sent on a timer. The one request that is not a button
// press is the editor's suggested line, and that is a separate switch the user
// turns on deliberately (`Config::inline_suggestions`).

#pragma once

#include <QFrame>
#include <QList>
#include <QString>
#include <QTextCursor>

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
    /// Where a remembered conversation is kept, when the configuration says to keep
    /// one. Defaults to the data directory's `assistant` leaf; a test points it at a
    /// temporary one.
    void setConversationDirectory(const QString& directory);

    /// What this session has spent: questions asked and the tokens the service
    /// reported for them. Counted per session because a total across sessions is a
    /// bill nobody can reconcile with a provider's, and no price table lives here --
    /// the counts are what the service said, which is the part this program knows.
    struct Usage {
        int requests = 0;
        int prompt_tokens = 0;
        int completion_tokens = 0;
    };
    [[nodiscard]] const Usage& sessionUsage() const noexcept { return usage_; }

signals:
    /// The user asked to configure the assistant; the window owns the dialog.
    void configureRequested();
    /// One more answer arrived; the window's Telemetry HUD shows the tally.
    void usageChanged();
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
    /// Grows the answer as a local endpoint streams it, replacing the partial text
    /// rather than appending fragments: what a user reads while waiting is the
    /// answer so far.
    void appendStreamPiece(const QString& piece);
    /// Puts `text` where the streamed answer is being written.
    void replaceStreamed(const QString& text);
    void renderNotice(const QString& text);
    void setBusy(bool busy);
    void updateEnabledState();
    void scrollToEnd();
    /// Puts a remembered conversation back into the transcript, shortened: the turn
    /// that was stored is the whole prompt, program included, and a page of it per
    /// question is not what a reader wants on opening the application.
    void restoreConversation();
    void storeConversation() const;
    /// Where a remembered conversation lives: the directory a test named, else the
    /// data directory's `assistant` leaf.
    [[nodiscard]] QString conversationDirectory() const;
    void updateUsageLabel();
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
    QLabel* usage_label_ = nullptr;
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
    Usage usage_;
    QString conversation_directory_;
    QString last_answer_;
    bool busy_ = false;
    /// A streaming answer: whether one is being written, where it starts, and what
    /// has arrived so far.
    bool streaming_ = false;
    QTextCursor stream_anchor_;
    QString stream_text_;
};

}  // namespace timewarp::ui
