// SPDX-License-Identifier: MIT

#include "timewarp/ui/ai_panel.hpp"

#include "timewarp/ai/client.hpp"
#include "timewarp/ai/history.hpp"
#include "timewarp/ui/app_paths.hpp"

#include <QCheckBox>
#include <QClipboard>
#include <QGuiApplication>
#include <QHBoxLayout>
#include <QLabel>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QShortcut>
#include <QTextBrowser>
#include <QTextCursor>
#include <QVBoxLayout>

#include <utility>

namespace timewarp::ui {

namespace {

using timewarp::ai::Message;
using timewarp::ai::Task;

/// Turns of conversation kept as context. Six exchanges: enough for "and now do
/// the same in Logo", short enough that the request does not grow without bound
/// and the oldest turn does not quietly dominate the answer.
constexpr int kHistoryTurns = 12;

/// The question box's fixed height. Three lines of a question, and the scroll bar
/// beyond it, rather than a box that grows and squeezes the conversation.
constexpr int kQuestionHeight = 64;

}  // namespace

AiPanel::AiPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("aiPanel"));
    setFrameShape(QFrame::StyledPanel);
    client_ = new timewarp::ai::Client{this};
    buildUi();
    connectClient();
    updateEnabledState();
}

void AiPanel::buildUi() {
    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    // The empty state is a banner rather than a replacement for the page: a user
    // who has just been shown the feature should see what it is for, not a blank
    // box with a disabled button.
    empty_state_ = new QLabel{
        tr("The assistant has no endpoint yet. Choose a service, a model and a key, "
           "and this page can explain a program, find what is wrong with it, comment "
           "it or write a new one."),
        this};
    empty_state_->setObjectName(QStringLiteral("aiEmptyState"));
    empty_state_->setWordWrap(true);
    layout->addWidget(empty_state_);
    configure_ = new QPushButton{tr("Configure the assistant…"), this};
    configure_->setObjectName(QStringLiteral("aiConfigureButton"));
    configure_->setToolTip(tr("Choose the service, the model and the key"));
    connect(configure_, &QPushButton::clicked, this, &AiPanel::configureRequested);
    layout->addWidget(configure_);

    auto* context_row = new QHBoxLayout{};
    context_row->setSpacing(6);
    include_document_ = new QCheckBox{tr("Send the program"), this};
    include_document_->setObjectName(QStringLiteral("aiSendDocumentBox"));
    include_document_->setToolTip(
        tr("Include the current document, or the selected lines, with each question. "
           "Unticked, only your words are sent."));
    context_row->addWidget(include_document_);
    context_hint_ = new QLabel{this};
    include_document_->setAccessibleName(tr("Send the program with the question"));
    context_hint_->setObjectName(QStringLiteral("aiContextHint"));
    context_hint_->setWordWrap(true);
    context_row->addWidget(context_hint_, 1);
    layout->addLayout(context_row);
    connect(include_document_, &QCheckBox::toggled, this, [this](bool on) {
        context_hint_->setText(on ? tr("the current program is sent with each question")
                                  : tr("only your words are sent"));
    });

    transcript_ = new QTextBrowser{this};
    transcript_->setObjectName(QStringLiteral("aiTranscript"));
    transcript_->setOpenExternalLinks(false);
    // Framed off: it is a page to read, and a second frame inside a dock's frame is
    // one rectangle too many.
    transcript_->setFrameShape(QFrame::NoFrame);
    transcript_->setAccessibleName(tr("Conversation"));
    layout->addWidget(transcript_, 1);

    question_ = new QPlainTextEdit{this};
    question_->setObjectName(QStringLiteral("aiQuestion"));
    question_->setAccessibleName(tr("Question"));
    question_->setPlaceholderText(tr("Ask about this program…  (Ctrl+Enter sends)"));
    question_->setFixedHeight(kQuestionHeight);
    layout->addWidget(question_);

    auto* tasks = new QHBoxLayout{};
    tasks->setSpacing(4);
    const auto add_task = [this, tasks](const QString& text, const QString& name,
                                        const QString& tip, Task task) {
        auto* button = new QPushButton{text, this};
        button->setObjectName(name);
        button->setToolTip(tip);
        connect(button, &QPushButton::clicked, this, [this, task] {
            ask(task, question_->toPlainText());
        });
        tasks->addWidget(button);
        task_buttons_.append(button);
        return button;
    };
    add_task(tr("Explain"), QStringLiteral("aiExplainButton"),
             tr("Ask what the program does, in the order it runs"),
             Task::Explain);
    add_task(tr("Fix"), QStringLiteral("aiFixButton"),
             tr("Ask for the faults to be found and corrected"), Task::Fix);
    add_task(tr("Comment"), QStringLiteral("aiCommentButton"),
             tr("Ask for the program back with comments added"), Task::Comment);
    add_task(tr("Translate"), QStringLiteral("aiTranslateButton"),
             tr("Type the target language in the box above, then press this"),
             Task::Translate);
    add_task(tr("Write"), QStringLiteral("aiWriteButton"),
             tr("Type what the program should do in the box above, then press this"),
             Task::Write);
    layout->addLayout(tasks);

    auto* actions = new QHBoxLayout{};
    actions->setSpacing(4);
    send_ = new QPushButton{tr("Send"), this};
    send_->setObjectName(QStringLiteral("aiSendButton"));
    send_->setToolTip(tr("Send the question with the program in view (Ctrl+Enter)"));
    stop_ = new QPushButton{tr("Stop"), this};
    stop_->setObjectName(QStringLiteral("aiStopButton"));
    stop_->setToolTip(tr("Abandon the request in flight"));
    clear_ = new QPushButton{tr("Clear"), this};
    clear_->setObjectName(QStringLiteral("aiClearButton"));
    clear_->setToolTip(tr("Forget the conversation so far"));
    insert_ = new QPushButton{tr("Insert"), this};
    insert_->setObjectName(QStringLiteral("aiInsertButton"));
    insert_->setToolTip(tr("Put the answer's code into the editor at the cursor"));
    copy_ = new QPushButton{tr("Copy"), this};
    copy_->setObjectName(QStringLiteral("aiCopyButton"));
    copy_->setToolTip(tr("Copy the answer to the clipboard"));
    auto* settings = new QPushButton{tr("Settings…"), this};
    settings->setObjectName(QStringLiteral("aiSettingsButton"));
    settings->setToolTip(tr("The endpoint, the model and the key"));
    for (QPushButton* button : {send_, stop_, clear_, insert_, copy_, settings}) {
        actions->addWidget(button);
    }
    layout->addLayout(actions);

    connect(send_, &QPushButton::clicked, this,
            [this] { ask(Task::Question, question_->toPlainText()); });
    connect(stop_, &QPushButton::clicked, this, [this] {
        client_->cancel();
        setBusy(false);
        renderNotice(tr("stopped"));
        status_->setText(tr("Request stopped"));
    });
    connect(clear_, &QPushButton::clicked, this, &AiPanel::clearConversation);
    connect(insert_, &QPushButton::clicked, this, [this] {
        if (!last_answer_.isEmpty()) {
            emit insertRequested(timewarp::ai::extract_code(last_answer_));
        }
    });
    connect(copy_, &QPushButton::clicked, this, [this] {
        if (!last_answer_.isEmpty()) {
            QGuiApplication::clipboard()->setText(last_answer_);
            status_->setText(tr("Answer copied"));
        }
    });
    connect(settings, &QPushButton::clicked, this, &AiPanel::configureRequested);

    // Ctrl+Enter sends. Enter alone is a newline, because a question worth asking
    // is often two lines long and a box that sends on Enter cannot hold one.
    for (const auto key : {Qt::Key_Return, Qt::Key_Enter}) {
        auto* shortcut = new QShortcut{QKeySequence{Qt::CTRL | key}, question_};
        shortcut->setContext(Qt::WidgetShortcut);
        connect(shortcut, &QShortcut::activated, this,
                [this] { ask(Task::Question, question_->toPlainText()); });
    }

    privacy_ = new QLabel{this};
    privacy_->setObjectName(QStringLiteral("aiPrivacyNote"));
    privacy_->setWordWrap(true);
    privacy_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    layout->addWidget(privacy_);

    // What this session has spent. Beside the status rather than in it: the status
    // is about the request in front of you, and this is about the ones behind it.
    usage_label_ = new QLabel{this};
    usage_label_->setObjectName(QStringLiteral("aiUsage"));
    usage_label_->setAccessibleName(tr("Assistant usage this session"));
    usage_label_->setWordWrap(true);
    usage_label_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    usage_label_->hide();
    layout->addWidget(usage_label_);

    status_ = new QLabel{this};
    status_->setAccessibleName(tr("Assistant status"));
    status_->setObjectName(QStringLiteral("aiStatus"));
    status_->setWordWrap(true);
    status_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    layout->addWidget(status_);
}

void AiPanel::connectClient() {
    connect(client_, &timewarp::ai::Client::started, this, [this] {
        setBusy(true);
        streaming_ = false;
        stream_text_.clear();
        status_->setText(tr("Asking %1…").arg(config_.model));
    });
    connect(client_, &timewarp::ai::Client::delta, this,
            [this](const QString& text) { appendStreamPiece(text); });
    connect(client_, &timewarp::ai::Client::answered, this,
            [this](const timewarp::ai::Reply& reply) {
                setBusy(false);
                last_answer_ = reply.text;
                if (streaming_) {
                    // The pieces and the finished answer should be the same text;
                    // if they are not, the service's last word is the answer.
                    replaceStreamed(reply.text);
                    streaming_ = false;
                    stream_text_.clear();
                } else {
                    renderTurn(tr("Assistant"), reply.text);
                }
                history_.append(Message{QStringLiteral("assistant"), reply.text});
                while (history_.size() > kHistoryTurns) {
                    history_.removeFirst();
                }
                storeConversation();
                QString detail = reply.model.isEmpty() ? config_.model : reply.model;
                ++usage_.requests;
                usage_.prompt_tokens += reply.prompt_tokens;
                usage_.completion_tokens += reply.completion_tokens;
                updateUsageLabel();
                emit usageChanged();
                if (reply.prompt_tokens > 0 || reply.completion_tokens > 0) {
                    detail += tr(" · %1 in, %2 out")
                                  .arg(reply.prompt_tokens)
                                  .arg(reply.completion_tokens);
                }
                status_->setText(tr("\u2705 %1").arg(detail));
                updateEnabledState();
            });
    connect(client_, &timewarp::ai::Client::failed, this, [this](const QString& message) {
        setBusy(false);
        renderNotice(tr("\u274C %1").arg(message));
        status_->setText(tr("\u274C %1").arg(message));
        emit statusMessage(message);
        updateEnabledState();
    });
}

void AiPanel::setConfig(const timewarp::ai::Config& config) {
    const bool was_busy = busy_;
    const bool was_remembering = config_.remember_conversation;
    config_ = config;
    if (was_remembering && !config_.remember_conversation) {
        // Turning the switch off has to mean *delete what is there*, or the file the
        // user asked not to keep would outlive the setting that asked for it.
        timewarp::ai::ConversationStore{conversationDirectory()}.clear();
    }
    if (config_.remember_conversation && history_.isEmpty()) {
        restoreConversation();
    }
    if (was_busy && !configured()) {
        // A request in flight against an endpoint the user has just replaced would
        // answer into a panel describing something else.
        client_->cancel();
        setBusy(false);
    }
    include_document_->setChecked(config_.send_document);
    context_hint_->setText(config_.send_document
                               ? tr("the current program is sent with each question")
                               : tr("only your words are sent"));
    if (configured() && timewarp::ai::is_local_endpoint(config_)) {
        // A server on this machine is the configuration to recommend to a classroom,
        // so the note says so rather than naming a provider and leaving the reader to
        // work out where it is.
        privacy_->setText(tr("Questions and the text above go to %1 on this machine: "
                             "nothing leaves the computer. Nothing is sent until you "
                             "press a button.")
                              .arg(timewarp::ai::provider_name(config_)));
    } else {
        privacy_->setText(tr("Questions and the text above go to %1. Nothing is sent "
                             "until you press a button.")
                              .arg(configured() ? timewarp::ai::provider_name(config_)
                                                : tr("the configured service")));
    }
    updateEnabledState();
}

void AiPanel::setContextProvider(std::function<timewarp::ai::Context()> provider) {
    context_provider_ = std::move(provider);
}

void AiPanel::setConversationDirectory(const QString& directory) {
    conversation_directory_ = directory;
}

QString AiPanel::conversationDirectory() const {
    if (!conversation_directory_.isEmpty()) {
        return conversation_directory_;
    }
    return timewarp::ui::dataDirFor(QStringLiteral("assistant"));
}

void AiPanel::restoreConversation() {
    const QList<Message> stored = timewarp::ai::ConversationStore{conversationDirectory()}.load();
    if (stored.isEmpty()) {
        return;
    }
    history_ = stored;
    for (const Message& turn : stored) {
        const bool mine = turn.role == QLatin1String("user");
        QString shown = mine ? turn.content.trimmed() : turn.content.trimmed();
        const int line_end = shown.indexOf(QLatin1Char('\n'));
        if (line_end > 0) {
            shown = shown.left(line_end).trimmed();
        }
        if (shown.size() > 200) {
            shown = shown.left(200) + QStringLiteral("\u2026");
        }
        renderTurn(mine ? tr("You") : tr("Assistant"), shown);
    }
    status_->setText(tr("\U0001F4BE %1 earlier turn(s) restored").arg(stored.size()));
}

void AiPanel::storeConversation() const {
    if (!config_.remember_conversation) {
        return;
    }
    // A failure to write is not worth a message of its own: the conversation still
    // works for this session, which is the default behaviour anyway.
    static_cast<void>(
        timewarp::ai::ConversationStore{conversationDirectory()}.save(history_));
}

bool AiPanel::configured() const { return timewarp::ai::ready(config_, nullptr); }

bool AiPanel::emptyStateVisible() const {
    return empty_state_ != nullptr && !empty_state_->isHidden();
}

void AiPanel::focusInput() {
    if (question_ != nullptr) {
        question_->setFocus(Qt::ShortcutFocusReason);
    }
}

void AiPanel::clearConversation() {
    history_.clear();
    last_answer_.clear();
    if (transcript_ != nullptr) {
        transcript_->clear();
    }
    if (status_ != nullptr) {
        status_->clear();
    }
    updateEnabledState();
}

void AiPanel::ask(Task task, const QString& input) {
    if (busy_) {
        return;
    }
    QString why;
    static_cast<void>(timewarp::ai::ready(config_, &why));
    if (!why.isEmpty()) {
        renderNotice(tr("\u274C %1").arg(why));
        status_->setText(tr("\u274C %1").arg(why));
        updateEnabledState();
        return;
    }

    timewarp::ai::Context context;
    if (include_document_->isChecked() && context_provider_) {
        context = context_provider_();
    }

    const timewarp::ai::Conversation conversation = compose(task, input, context);
    const QString words = input.trimmed();
    QString heading = timewarp::ai::task_label(task);
    const QString label = contextLabel(context);
    if (!label.isEmpty()) {
        heading += QStringLiteral(" · ") + label;
    }
    // The user's own words, when there were any. A quick-action button sends the
    // instruction instead, and the heading already says which one: an empty `<pre>`
    // under it would be a blank box with nothing to read.
    renderTurn(heading, words);

    const QString user_text = conversation.messages.isEmpty()
                                  ? QString{}
                                  : conversation.messages.last().content;
    history_.append(Message{QStringLiteral("user"), user_text});
    while (history_.size() > kHistoryTurns) {
        history_.removeFirst();
    }
    storeConversation();

    question_->clear();
    last_answer_.clear();
    updateEnabledState();
    client_->send(config_, conversation);
}

timewarp::ai::Conversation AiPanel::compose(Task task, const QString& input,
                                            const timewarp::ai::Context& context) const {
    timewarp::ai::Conversation conversation =
        timewarp::ai::build_prompt(task, context, config_.persona, input);
    QList<Message> turns = history_;
    for (const Message& message : conversation.messages) {
        turns.append(message);
    }
    while (turns.size() > kHistoryTurns) {
        turns.removeFirst();
    }
    conversation.messages = turns;
    return conversation;
}

QString AiPanel::contextLabel(const timewarp::ai::Context& context) const {
    if (context.file_name.isEmpty()) {
        return context.language;
    }
    return context.language.isEmpty()
               ? context.file_name
               : QStringLiteral("%1 · %2").arg(context.language, context.file_name);
}

void AiPanel::appendHtml(const QString& html) {
    if (transcript_ == nullptr) {
        return;
    }
    // `insertHtml` rather than `append`: append's handling of a string that looks
    // like markup depends on the document's format, and a transcript that quietly
    // showed the tags instead of the answer would still look like a working panel.
    QTextCursor cursor = transcript_->textCursor();
    cursor.movePosition(QTextCursor::End);
    if (!cursor.atBlockStart()) {
        cursor.insertBlock();
    }
    cursor.insertHtml(html);
    transcript_->setTextCursor(cursor);
}

void AiPanel::renderTurn(const QString& who, const QString& text) {
    appendHtml(QStringLiteral("<p style=\"margin:6px 0 2px 0;\"><b>%1</b></p>")
                   .arg(who.toHtmlEscaped()));
    if (!text.isEmpty()) {
        appendHtml(
            QStringLiteral("<pre style=\"white-space:pre-wrap; margin:0;\">%1</pre>")
                .arg(text.toHtmlEscaped()));
    }
    scrollToEnd();
}

void AiPanel::appendStreamPiece(const QString& piece) {
    if (transcript_ == nullptr || piece.isEmpty()) {
        return;
    }
    if (!streaming_) {
        streaming_ = true;
        stream_text_.clear();
        renderTurn(tr("Assistant"), QString{});
        // Everything after this point is the answer being written, and it is
        // replaced on each piece rather than appended to.
        stream_anchor_ = QTextCursor{transcript_->document()};
        stream_anchor_.movePosition(QTextCursor::End);
    }
    stream_text_ += piece;
    replaceStreamed(stream_text_);
}

void AiPanel::replaceStreamed(const QString& text) {
    if (transcript_ == nullptr || stream_anchor_.isNull()) {
        return;
    }
    QTextCursor cursor = stream_anchor_;
    cursor.movePosition(QTextCursor::End, QTextCursor::KeepAnchor);
    cursor.removeSelectedText();
    if (!text.isEmpty()) {
        cursor.insertHtml(
            QStringLiteral("<pre style=\"white-space:pre-wrap; margin:0;\">%1</pre>")
                .arg(text.toHtmlEscaped()));
    }
    scrollToEnd();
}

void AiPanel::renderNotice(const QString& text) {
    appendHtml(
        QStringLiteral("<p style=\"margin:4px 0;\"><i>%1</i></p>").arg(text.toHtmlEscaped()));
    scrollToEnd();
}

void AiPanel::scrollToEnd() {
    if (QScrollBar* bar = transcript_->verticalScrollBar()) {
        bar->setValue(bar->maximum());
    }
}

void AiPanel::setBusy(bool busy) {
    busy_ = busy;
    updateEnabledState();
}

void AiPanel::updateUsageLabel() {
    if (usage_label_ == nullptr) {
        return;
    }
    if (usage_.requests == 0) {
        usage_label_->hide();
        return;
    }
    QString text = tr("This session: %1 question(s), %2 tokens in, %3 out")
                       .arg(usage_.requests)
                       .arg(usage_.prompt_tokens)
                       .arg(usage_.completion_tokens);
    if (usage_.prompt_tokens == 0 && usage_.completion_tokens == 0) {
        // A service that sends no usage events leaves the counts at zero rather than
        // guessing, and saying so is better than three zeros.
        text = tr("This session: %1 question(s), token counts not reported")
                   .arg(usage_.requests);
    }
    usage_label_->setText(text);
    usage_label_->show();
}

void AiPanel::updateEnabledState() {
    const bool ready = configured();
    if (empty_state_ != nullptr) {
        empty_state_->setVisible(!ready);
    }
    if (configure_ != nullptr) {
        configure_->setVisible(!ready);
    }
    if (send_ != nullptr) {
        send_->setEnabled(ready && !busy_);
    }
    if (question_ != nullptr) {
        question_->setEnabled(ready);
    }
    for (QPushButton* button : task_buttons_) {
        button->setEnabled(ready && !busy_);
    }
    if (stop_ != nullptr) {
        stop_->setEnabled(busy_);
    }
    if (clear_ != nullptr) {
        clear_->setEnabled(!busy_ && (!history_.isEmpty() || !last_answer_.isEmpty()));
    }
    if (insert_ != nullptr) {
        insert_->setEnabled(!busy_ && !last_answer_.isEmpty());
    }
    if (copy_ != nullptr) {
        copy_->setEnabled(!last_answer_.isEmpty());
    }
}

}  // namespace timewarp::ui
