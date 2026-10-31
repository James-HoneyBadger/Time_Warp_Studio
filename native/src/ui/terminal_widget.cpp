// SPDX-License-Identifier: MIT

#include "timewarp/ui/terminal_widget.hpp"

#include "timewarp/ui/pty_session.hpp"

#include <QDir>
#include <QFileInfo>
#include <QFont>
#include <QFontMetrics>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QResizeEvent>
#include <QSocketNotifier>
#include <QTextCursor>
#include <QVBoxLayout>

#include <algorithm>

namespace timewarp::ui {

namespace {

/// The shell to run. `$SHELL` is what a person expects "terminal" to mean; the
/// fallback is for a desktop session that does not set it (a service, a container).
[[nodiscard]] QString shellProgram() {
    QString shell = qEnvironmentVariable("SHELL");
    if (shell.isEmpty() || !QFileInfo{shell}.isExecutable()) {
        shell = QStringLiteral("/bin/sh");
    }
    return shell;
}

/// A `cd` the shell will run. Quoted, because a document can live in a folder
/// whose name contains a space or an apostrophe.
[[nodiscard]] QString cdCommand(const QString& directory) {
    QString quoted = directory;
    quoted.replace(QLatin1Char('\''), QStringLiteral("'\\''"));
    return QStringLiteral("cd -- '%1'").arg(quoted);
}

}  // namespace

TerminalWidget::TerminalWidget(QString working_directory, QWidget* parent)
    : QWidget{parent}, working_directory_(std::move(working_directory)) {
    setObjectName(QStringLiteral("terminalPanel"));

    output_view_ = new QPlainTextEdit{this};
    output_view_->setObjectName(QStringLiteral("terminalOutput"));
    output_view_->setReadOnly(true);
    output_view_->setFont(QFont{QStringLiteral("Courier New"), 10});
    output_view_->setLineWrapMode(QPlainTextEdit::NoWrap);
    output_view_->setAccessibleName(tr("Terminal Output"));
    output_view_->setPlaceholderText(
        tr("Commands run in a shell on a terminal of its own. Nothing here is "
           "sandboxed.\u2026"));

    input_ = new QLineEdit{this};
    input_->setObjectName(QStringLiteral("terminalInput"));
    input_->setFont(QFont{QStringLiteral("Courier New"), 10});
    input_->setAccessibleName(tr("Terminal Command"));
    input_->setPlaceholderText(tr("Type a command and press Enter"));
    connect(input_, &QLineEdit::returnPressed, this, &TerminalWidget::submitInput);

    status_ = new QLabel{this};
    status_->setObjectName(QStringLiteral("terminalStatus"));
    status_->setWordWrap(true);

    auto* input_row = new QHBoxLayout;
    auto* prompt = new QLabel{QStringLiteral(">"), this};
    prompt->setFont(QFont{QStringLiteral("Courier New"), 10});
    input_row->addWidget(prompt);
    input_row->addWidget(input_, 1);

    // A terminal with no way to stop the thing in the foreground is a terminal you
    // have to close to interrupt `less`. `Ctrl+C` is the ordinary answer and keeps the
    // session; Restart is for the case where the shell itself is wedged.
    interrupt_ = new QPushButton{tr("Interrupt"), this};
    interrupt_->setObjectName(QStringLiteral("terminalInterrupt"));
    interrupt_->setToolTip(tr("Send Ctrl+C to whatever is running (the session keeps its "
                              "variables and history)."));
    input_row->addWidget(interrupt_);
    restart_ = new QPushButton{tr("Restart"), this};
    restart_->setObjectName(QStringLiteral("terminalRestart"));
    restart_->setToolTip(tr("End this shell and start a new one here."));
    input_row->addWidget(restart_);
    connect(interrupt_, &QPushButton::clicked, this, &TerminalWidget::interrupt);
    connect(restart_, &QPushButton::clicked, this, &TerminalWidget::restart);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(6);
    layout->addWidget(output_view_, 1);
    layout->addLayout(input_row);
    layout->addWidget(status_);

    updateStatus();
}

TerminalWidget::~TerminalWidget() {
    stop();
}

void TerminalWidget::startShell() {
    stop();

    int columns = 80;
    int rows = 24;
    ptySize(&columns, &rows);
    auto session = std::make_unique<PtySession>();
    QString error;
    if (!session->start(shellProgram(), working_directory_, columns, rows, &error)) {
        append(error + QLatin1Char('\n'));
        updateStatus();
        return;
    }
    shell_ = std::move(session);
    notifier_ = new QSocketNotifier{shell_->descriptor(), QSocketNotifier::Read, this};
    connect(notifier_, &QSocketNotifier::activated, this,
            [this] { readShellOutput(); });
    // The queue `PtySession` keeps when the terminal is full is emptied here, so a
    // long paste into a slow foreground job arrives a buffer at a time instead of
    // holding the window still.
    write_notifier_ =
        new QSocketNotifier{shell_->descriptor(), QSocketNotifier::Write, this};
    write_notifier_->setEnabled(false);
    connect(write_notifier_, &QSocketNotifier::activated, this,
            [this] { drainPending(); });
    updateStatus();
}

void TerminalWidget::readShellOutput() {
    if (shell_ == nullptr) {
        return;
    }
    const QByteArray bytes = shell_->read();
    if (!bytes.isEmpty()) {
        apply(filter_.feed(bytes));
    }
    drainPending();
    if (!shell_->isRunning()) {
        const int code = shell_->exitCode();
        if (notifier_ != nullptr) {
            notifier_->setEnabled(false);
            notifier_->deleteLater();
            notifier_ = nullptr;
        }
        // Both notifiers watch the pty's descriptor, so both go before the session
        // that owns it: a notifier left watching a closed descriptor is a warning
        // from Qt and, on a busy machine, a stale file descriptor somebody else now
        // owns.
        if (write_notifier_ != nullptr) {
            write_notifier_->setEnabled(false);
            write_notifier_->deleteLater();
            write_notifier_ = nullptr;
        }
        shell_.reset();
        append(code >= 0 ? tr("\n[shell exited with %1]\n").arg(code)
                         : tr("\n[shell exited]\n"));
        updateStatus();
    }
}

void TerminalWidget::drainPending() {
    if (shell_ == nullptr) {
        return;
    }
    const int still_waiting = shell_->flush();
    if (write_notifier_ != nullptr) {
        write_notifier_->setEnabled(still_waiting > 0);
    }
}

void TerminalWidget::apply(const std::vector<TerminalOp>& ops) {
    for (const TerminalOp& op : ops) {
        if (op.kind == TerminalOp::Kind::EraseBack) {
            eraseBack(op.count);
        } else {
            append(op.text);
        }
    }
}

void TerminalWidget::eraseBack(int count) {
    if (count <= 0 || output_view_ == nullptr) {
        return;
    }
    QTextCursor cursor = output_view_->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.movePosition(QTextCursor::Left, QTextCursor::KeepAnchor, count);
    cursor.removeSelectedText();
}

void TerminalWidget::runCommand(const QString& command) {
    const QString trimmed = command.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }
    if (!isRunning()) {
        startShell();
    }
    if (!isRunning()) {
        return;
    }
    // The terminal echoes what is typed on it, so the transcript shows the command
    // as the shell saw it. Writing our own `> ` copy would say it twice.
    writeLine(trimmed);
}

void TerminalWidget::writeLine(const QString& line) {
    if (shell_ == nullptr) {
        return;
    }
    if (!shell_->write(line.toUtf8() + '\n')) {
        // The one failure a person must not have to guess at: their typing did not
        // reach the shell. Silently dropping it would look like a terminal that
        // ignores the keyboard.
        append(tr("\n[the shell is not reading; that line was not sent]\n"));
        return;
    }
    if (write_notifier_ != nullptr) {
        write_notifier_->setEnabled(shell_->hasPending());
    }
}

void TerminalWidget::interrupt() {
    if (!isRunning()) {
        return;
    }
    // `0x03` is what the line discipline turns into `SIGINT` for the foreground
    // process group; sending the signal directly would miss a shell that is itself
    // waiting on a child.
    const QByteArray bytes{"\x03"};
    if (shell_->write(bytes)) {
        if (write_notifier_ != nullptr) {
            write_notifier_->setEnabled(shell_->hasPending());
        }
    }
    updateStatus();
}

void TerminalWidget::restart() {
    const bool was_running = shell_ != nullptr;
    stop();
    startShell();
    if (was_running) {
        append(tr("\n[a new shell]\n"));
    }
}

void TerminalWidget::setWorkingDirectory(const QString& directory) {
    if (directory.isEmpty() || directory == working_directory_) {
        return;
    }
    working_directory_ = directory;
    if (isRunning()) {
        // `cd` rather than a restart: the shell's variables and history are the
        // reason it is worth keeping alive across a tab change.
        writeLine(cdCommand(directory));
    }
    updateStatus();
}

bool TerminalWidget::isRunning() const {
    return shell_ != nullptr && shell_->isRunning();
}

void TerminalWidget::stop() {
    if (notifier_ != nullptr) {
        notifier_->setEnabled(false);
        notifier_->deleteLater();
        notifier_ = nullptr;
    }
    if (write_notifier_ != nullptr) {
        write_notifier_->setEnabled(false);
        write_notifier_->deleteLater();
        write_notifier_ = nullptr;
    }
    if (shell_ != nullptr) {
        shell_->stop();
        shell_.reset();
    }
    filter_.reset();
    updateStatus();
}

QString TerminalWidget::output() const {
    return output_view_->toPlainText();
}

void TerminalWidget::append(const QString& text) {
    if (text.isEmpty()) {
        return;
    }
    QTextCursor cursor = output_view_->textCursor();
    cursor.movePosition(QTextCursor::End);
    output_view_->setTextCursor(cursor);
    output_view_->insertPlainText(text);
    output_view_->ensureCursorVisible();
}

void TerminalWidget::submitInput() {
    const QString command = input_->text();
    input_->clear();
    runCommand(command);
}

void TerminalWidget::ptySize(int* columns, int* rows) const {
    const QFontMetrics metrics{output_view_->font()};
    const int width = std::max(40, output_view_->viewport()->width());
    const int height = std::max(40, output_view_->viewport()->height());
    // A terminal cell is as wide as a monospaced `M` and as tall as a line.
    *columns = std::max(20, width / std::max(1, metrics.horizontalAdvance(QLatin1Char('M'))));
    *rows = std::max(4, height / std::max(1, metrics.lineSpacing()));
}

void TerminalWidget::resizeEvent(QResizeEvent* event) {
    QWidget::resizeEvent(event);
    if (shell_ != nullptr) {
        int columns = 80;
        int rows = 24;
        ptySize(&columns, &rows);
        shell_->setWindowSize(columns, rows);
    }
}

void TerminalWidget::updateStatus() {
    const QString shell = shellProgram();
    const QString where = working_directory_.isEmpty() ? QDir::currentPath() : working_directory_;
    if (isRunning()) {
        status_->setText(tr("Shell: %1 \u00b7 %2").arg(shell, where));
    } else {
        status_->setText(tr("Shell: %1 \u00b7 %2 (not started)").arg(shell, where));
    }
    if (interrupt_ != nullptr) {
        interrupt_->setEnabled(isRunning());
    }
}

}  // namespace timewarp::ui
