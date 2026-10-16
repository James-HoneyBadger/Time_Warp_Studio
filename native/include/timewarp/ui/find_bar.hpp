// SPDX-License-Identifier: MIT
//
// The find and replace bar.
//
// A frame that sits under the editor tabs and is hidden until it is asked for.
// It is a *view over* `CodeEditor`'s search calls -- the searching, the wrapping
// and the replacing all live with the document, so the same behaviour is what a
// test drives without a window.
//
// It keeps the editor in a `QPointer`, because closing a tab destroys the editor
// while the bar is still on screen pointing at it. A raw pointer there is the
// classic use-after-free with a tidy-looking stack trace somewhere else.
//
// Enter finds the next match, Shift+Enter the previous one, and Escape closes
// the bar -- handled in an event filter on the two entry fields, because a
// `QLineEdit` consumes those keys itself.

#pragma once

#include <QFrame>
#include <QPointer>
#include <QString>

class QCheckBox;
class QEvent;
class QLabel;
class QLineEdit;
class QToolButton;

namespace timewarp::ui {

class CodeEditor;

class FindBar final : public QFrame {
    Q_OBJECT

public:
    explicit FindBar(QWidget* parent = nullptr);

    /// The document to search. Null when there are no tabs, which disables the
    /// controls rather than refusing to draw.
    void setEditor(CodeEditor* editor);

    /// Shows the bar. `withReplacement` reveals the replacement row; the term is
    /// prefilled from the editor's selection when it is on one line, which is how
    /// "find this word I just selected" works without typing it again.
    void open(bool withReplacement);

    /// Re-counts matches. Called after an edit, so the count is a statement about
    /// the document rather than about the last time the term changed.
    void refresh();

    /// Finds the next (or previous) match for the term already in the bar.
    ///
    /// Public because the menu's F3 goes through here rather than through the
    /// editor: the bar owns the term, so a search started from the menu and one
    /// started from the buttons are the same search.
    void findAgain(bool backwards);

    [[nodiscard]] QString term() const;
    void setTerm(const QString& text);
    /// The status text, for a test to assert what the user is told.
    [[nodiscard]] QString statusText() const;
    /// True when the replacement row is showing.
    [[nodiscard]] bool replaceRowVisible() const;

signals:
    /// The bar was closed. The window uses this to return focus to the editor.
    void closed();

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;
    void keyPressEvent(QKeyEvent* event) override;

private:
    void replaceOne();
    void replaceEverything();
    void closeBar();
    void updateStatus();

    QLineEdit* term_ = nullptr;
    QLineEdit* replacement_ = nullptr;
    QCheckBox* case_ = nullptr;
    QLabel* status_ = nullptr;
    QToolButton* previous_ = nullptr;
    QToolButton* next_ = nullptr;
    QToolButton* replace_ = nullptr;
    QToolButton* replace_all_ = nullptr;
    QPointer<CodeEditor> editor_;
};

}  // namespace timewarp::ui
