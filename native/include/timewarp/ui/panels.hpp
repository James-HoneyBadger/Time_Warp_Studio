// SPDX-License-Identifier: MIT
//
// The main window's leaf widgets.
//
// Each of these stands in for a Python widget that is far too large to port as
// part of a layout translation -- `output.py` alone is 1,220 lines. What they do
// share is the part the *layout* depends on: the size hints, the minimums, the
// fixed heights and the signals. So the window's structure is real and testable
// today, and replacing one with the full implementation is a single construction
// site in `MainWindow`.
//
// Where a widget is a genuine simplification rather than a stub, the comment says
// so, so nobody assumes the feature is complete.

#pragma once

#include <QFrame>
#include <QList>
#include <QPair>
#include <QSize>
#include <QString>
#include <QStringList>
#include <QWidget>

class QComboBox;
class QLabel;
class QLineEdit;
class QPushButton;
class QVBoxLayout;

namespace timewarp::ui {

/// Stands in for a panel not yet ported.
///
/// Honours a minimum size, because without one a `QSplitter` collapses the panel
/// to zero and the *layout* stops being verifiable -- which is the only thing
/// this class exists to protect.
class PlaceholderPanel final : public QFrame {
    Q_OBJECT

public:
    /// `standsFor` names the Python class this replaces, and is shown in the
    /// panel so a screenshot of the running window is self-documenting.
    PlaceholderPanel(const QString& title, const QString& standsFor,
                     const QSize& minimum = QSize{120, 80},
                     QWidget* parent = nullptr);

    [[nodiscard]] QSize minimumSizeHint() const override;

private:
    QSize minimum_;
};

/// Stands in for `ImmediateModePanel` (`ui/output.py`).
///
/// A REPL prompt and an entry line. The layout contract is the 44px floor the
/// Python sets so the bar cannot be dragged out of sight; the interpreter
/// plumbing behind it is out of scope here.
class ImmediateModePanel final : public QFrame {
    Q_OBJECT

public:
    /// The Python sets `setMinimumHeight(44)`; kept as a named constant because
    /// the layout test asserts it.
    static constexpr int kMinimumHeight = 44;

    explicit ImmediateModePanel(QWidget* parent = nullptr);

    /// Text the user has typed but not yet submitted, for history recall.
    [[nodiscard]] QString command() const;
    void setCommand(const QString& text);
    void focusInput();

signals:
    /// The user pressed Return on a non-empty command.
    void commandSubmitted(const QString& command);

private:
    QLineEdit* input_ = nullptr;
};

/// Stands in for `FunctionKeyBar` (`ui/function_key_bar.py`).
///
/// The Python widget has three modifier layers and a deck selector. Only the
/// normal layer and the deck selector are reproduced: the layout needs the
/// fixed 30px height and the two signals, and inventing the remaining layers
/// here would mean inventing actions that do not exist yet.
class FunctionKeyBar final : public QFrame {
    Q_OBJECT

public:
    /// Python: `setFixedHeight(30)`.
    static constexpr int kFixedHeight = 30;

    explicit FunctionKeyBar(QWidget* parent = nullptr);

    /// Replaces the deck choices. The Python ships four workspace decks.
    void setDecks(const QStringList& labels, int current = 0);

    [[nodiscard]] int currentDeck() const;

signals:
    /// Emitted with the action name of the pressed key, e.g. `"save"`.
    void actionTriggered(const QString& action);
    /// Emitted with the deck's stable id, e.g. `"debugger_deck"`.
    void deckSelected(const QString& deckId);

private:
    QComboBox* decks_ = nullptr;
    /// Displayed label paired with the stable id the deck is known by.
    QList<QPair<QString, QString>> deckIds_;
};

/// Stands in for `CRTEffectOverlay` (`ui/crt_effect.py`).
///
/// The Python overlay draws scanlines, curvature, vignette, glow and flicker and
/// exposes sliders for each. Only scanlines are implemented: the point of
/// including it at all is the *geometry tracking*, which is where the Python
/// approach is fragile.
///
/// The Python resizes the overlay from `MainWindow.resizeEvent` by reaching into
/// `overlay.parent().rect()`. That only holds while the overlay's parent happens
/// to be the widget being resized; reparenting it -- to the viewport of a
/// scroll area, say -- silently stops it from tracking. This version installs
/// itself as an event filter on the widget it covers, so the coupling is stated
/// instead of assumed.
class CrtOverlay final : public QWidget {
    Q_OBJECT

public:
    explicit CrtOverlay(QWidget* target);

    /// 0..100. Zero disables painting without hiding the widget.
    void setScanlineIntensity(int percent);
    [[nodiscard]] int scanlineIntensity() const noexcept;

    /// The widget this overlay tracks, or null if it has been destroyed.
    [[nodiscard]] QWidget* target() const noexcept;

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;
    void paintEvent(QPaintEvent* event) override;

private:
    void syncToTarget();

    QWidget* target_ = nullptr;
    int intensity_ = 15;
};

}  // namespace timewarp::ui
