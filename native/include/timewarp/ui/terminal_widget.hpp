// SPDX-License-Identifier: MIT
//
// An embedded shell: the user's own `$SHELL`, one command line at a time.
//
// Ported from `ui/terminal_widget.py`, with a deliberate restriction. This is
// not a terminal emulator: there is no PTY, so there are no prompts, no job
// control and no full-screen programs. Each line typed here is written to the
// shell's standard input and whatever the shell writes back is appended. That
// makes `echo`, a compiler run or a small script work, and makes `vim`, `top` or
// anything that asks the terminal for its size fail instead of taking the IDE
// over.
//
// It is off unless the user turns it on in Settings, and the window removes it
// in presentation and player modes. Running commands is a support and safety
// cost in a teaching IDE, so the feature is opted into rather than greeted.

#pragma once

#include <QString>
#include <QWidget>

class QLabel;
class QLineEdit;
class QPlainTextEdit;
class QProcess;

namespace timewarp::ui {

class TerminalWidget final : public QWidget {
    Q_OBJECT

public:
    /// `working_directory` is where the shell starts; empty means the process's
    /// own working directory, which is the application's.
    explicit TerminalWidget(QString working_directory = {}, QWidget* parent = nullptr);
    ~TerminalWidget() override;

    /// Runs one line through the shell, as pressing Enter in the input does.
    /// Starts the shell on first use.
    void runCommand(const QString& command);

    /// Moves a running shell to *directory* (with `cd`, so its state survives a
    /// tab change), and remembers it for the next start.
    void setWorkingDirectory(const QString& directory);
    [[nodiscard]] QString workingDirectory() const { return working_directory_; }

    [[nodiscard]] bool isRunning() const;

    /// Ends the shell: terminate, then kill if it does not go. Called when the
    /// window closes or the terminal is switched off, so no child process
    /// outlives the thing that started it.
    void stop();

    /// Everything written to the output so far. For the tests, and for a caller
    /// that wants to save the transcript.
    [[nodiscard]] QString output() const;

private:
    void startShell();
    void append(const QString& text);
    void submitInput();
    void updateStatus();

    QString working_directory_;
    QProcess* process_ = nullptr;
    QPlainTextEdit* output_view_ = nullptr;
    QLineEdit* input_ = nullptr;
    QLabel* status_ = nullptr;
};

}  // namespace timewarp::ui
