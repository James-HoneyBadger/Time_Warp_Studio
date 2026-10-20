// SPDX-License-Identifier: MIT

#include "timewarp/ui/terminal_widget.hpp"

#include <QDir>
#include <QFileInfo>
#include <QFont>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QProcess>
#include <QTextCursor>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

/// The shell to run. `$SHELL` is what a person expects "terminal" to mean; the
/// fallbacks are for a desktop session that does not set it (a service, a
/// container) and for Windows, which has no `/bin/sh`.
[[nodiscard]] QString shellProgram() {
#ifdef Q_OS_WIN
    const QString shell = qEnvironmentVariable("COMSPEC", QStringLiteral("cmd.exe"));
#else
    QString shell = qEnvironmentVariable("SHELL");
    if (shell.isEmpty() || !QFileInfo{shell}.isExecutable()) {
        shell = QStringLiteral("/bin/sh");
    }
#endif
    return shell;
}

/// A `cd` the shell will run. Quoted, because a document can live in a folder
/// whose name contains a space -- and on Windows, `/d` so a different drive is
/// not silently ignored.
[[nodiscard]] QString cdCommand(const QString& directory) {
#ifdef Q_OS_WIN
    return QStringLiteral("cd /d \"%1\"").arg(QString{directory}.replace('"', QStringLiteral("\"\"")));
#else
    QString quoted = directory;
    quoted.replace(QLatin1Char('\''), QStringLiteral("'\\''"));
    return QStringLiteral("cd -- '%1'").arg(quoted);
#endif
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
        tr("Commands run in your shell. Nothing here is sandboxed.\u2026"));

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
    // A previous shell that has exited is replaced rather than reused: `exit`
    // should give the learner a fresh shell on the next command, not silence.
    stop();

    const QString shell = shellProgram();
    process_ = new QProcess{this};
    process_->setProcessChannelMode(QProcess::MergedChannels);
    if (!working_directory_.isEmpty()) {
        process_->setWorkingDirectory(working_directory_);
    }
    process_->setProgram(shell);
    connect(process_, &QProcess::readyReadStandardOutput, this, [this] {
        if (process_ != nullptr) {
            append(QString::fromLocal8Bit(process_->readAllStandardOutput()));
        }
    });
    connect(process_, &QProcess::errorOccurred, this, [this](QProcess::ProcessError error) {
        if (error == QProcess::FailedToStart) {
            append(tr("Could not start %1. Is it installed?\n").arg(shellProgram()));
            updateStatus();
        }
    });
    connect(process_, qOverload<int, QProcess::ExitStatus>(&QProcess::finished), this,
            [this](int code, QProcess::ExitStatus) {
                append(tr("\n[shell exited with %1]\n").arg(code));
                updateStatus();
            });
    process_->start();
    if (!process_->waitForStarted(2000)) {
        append(tr("Could not start %1: %2\n").arg(shell, process_->errorString()));
        process_->deleteLater();
        process_ = nullptr;
    }
    updateStatus();
}

void TerminalWidget::runCommand(const QString& command) {
    const QString trimmed = command.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }
    if (process_ == nullptr || process_->state() == QProcess::NotRunning) {
        startShell();
    }
    if (process_ == nullptr || process_->state() == QProcess::NotRunning) {
        return;
    }
    // Echoed like a prompt would: the transcript has to show what produced the
    // output beneath it, because the shell prints no prompt of its own.
    append(QStringLiteral("> ") + trimmed + QLatin1Char('\n'));
    // Queued rather than waited on: a write that blocks would stall the window
    // that is supposed to be showing the command it was given.
    process_->write(trimmed.toLocal8Bit() + '\n');
}

void TerminalWidget::setWorkingDirectory(const QString& directory) {
    if (directory.isEmpty() || directory == working_directory_) {
        return;
    }
    working_directory_ = directory;
    if (process_ != nullptr && process_->state() != QProcess::NotRunning) {
        // `cd` rather than a restart: the shell's variables and history are the
        // reason it is worth keeping alive across a tab change.
        process_->write(cdCommand(directory).toLocal8Bit() + '\n');
    }
    updateStatus();
}

bool TerminalWidget::isRunning() const {
    return process_ != nullptr && process_->state() != QProcess::NotRunning;
}

void TerminalWidget::stop() {
    if (process_ == nullptr) {
        return;
    }
    QProcess* process = process_;
    process_ = nullptr;
    // The widget is going away or the shell is being replaced: a `finished`
    // signal during shutdown would append to a view that may not exist.
    disconnect(process, nullptr, this, nullptr);
    if (process->state() != QProcess::NotRunning) {
        process->terminate();
        if (!process->waitForFinished(500)) {
            process->kill();
            process->waitForFinished(500);
        }
    }
    process->deleteLater();
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

void TerminalWidget::updateStatus() {
    const QString shell = shellProgram();
    const QString where = working_directory_.isEmpty() ? QDir::currentPath() : working_directory_;
    if (isRunning()) {
        status_->setText(tr("Shell: %1 \u00b7 %2").arg(shell, where));
    } else {
        status_->setText(tr("Shell: %1 \u00b7 %2 (not started)").arg(shell, where));
    }
}

}  // namespace timewarp::ui
