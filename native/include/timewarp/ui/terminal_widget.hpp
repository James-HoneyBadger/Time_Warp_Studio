// SPDX-License-Identifier: MIT
//
// An embedded shell, on a pseudo-terminal of its own.
//
// The shell is the user's own `$SHELL`, and it runs on a real pty
// (`pty_session.hpp`), so it prints its prompt, colours its output, keeps job
// control and answers a program that asks how wide the terminal is. What it does
// not have is a screen: the transcript is text, so a program that paints with
// cursor addressing -- `vim`, `top`, a full-screen curses interface -- starts and
// writes its escape sequences into a filter that throws them away. The tab is for
// commands, which is what an IDE's terminal is for.
//
// It is off unless the user turns it on in Settings, and the window removes it in
// presentation and player modes. Running commands is a support and safety cost in a
// teaching IDE, so the feature is opted into rather than greeted.

#pragma once

#include "timewarp/ui/terminal_text.hpp"

#include <QString>
#include <QWidget>

#include <memory>
#include <vector>

class QLabel;
class QLineEdit;
class QPlainTextEdit;
class QPushButton;
class QSocketNotifier;
class QResizeEvent;

namespace timewarp::ui {

class PtySession;

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

    /// Ends the shell and everything it started. Called when the window closes or
    /// the terminal is switched off, so no child process outlives the thing that
    /// started it.
    void stop();

    /// Sends `Ctrl+C` to whatever is in the foreground, which is how a person stops a
    /// command that will not stop on its own. The session and its variables survive,
    /// because a terminal you have to restart to interrupt something is a terminal you
    /// will not use.
    void interrupt();

    /// Ends the shell and starts a fresh one in the current directory. The escape
    /// hatch for a shell that is wedged badly enough that `Ctrl+C` is not enough.
    void restart();

    /// The two controls above, so a test can click what a person clicks.
    [[nodiscard]] QPushButton* interruptButton() const noexcept { return interrupt_; }
    [[nodiscard]] QPushButton* restartButton() const noexcept { return restart_; }

    /// Everything written to the output so far. For the tests, and for a caller
    /// that wants to save the transcript.
    [[nodiscard]] QString output() const;

protected:
    void resizeEvent(QResizeEvent* event) override;

private:
    void startShell();
    void readShellOutput();
    /// Sends what the terminal would not take yet, and stops watching for writable
    /// once the queue is empty.
    void drainPending();
    void apply(const std::vector<TerminalOp>& ops);
    void eraseBack(int count);
    void append(const QString& text);
    void writeLine(const QString& text);
    void ptySize(int* columns, int* rows) const;
    void submitInput();
    void updateStatus();

    QString working_directory_;
    std::unique_ptr<PtySession> shell_;
    QSocketNotifier* notifier_ = nullptr;
    QSocketNotifier* write_notifier_ = nullptr;
    QPushButton* interrupt_ = nullptr;
    QPushButton* restart_ = nullptr;
    /// What the pty's bytes mean: escape sequences stripped, `\r` applied.
    TerminalText filter_;
    QPlainTextEdit* output_view_ = nullptr;
    QLineEdit* input_ = nullptr;
    QLabel* status_ = nullptr;
};

}  // namespace timewarp::ui
