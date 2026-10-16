// SPDX-License-Identifier: MIT
//
// A QTabWidget whose size hints reflect only the *current* page, plus the tab-bar
// behaviours the IDE wants (drag-to-reorder, middle-click close, right-click menu).
//
// Translated from the removed PySide6 implementation's main_window.py.
// (`class ResizableTabWidget`).

#pragma once

#include <QSize>
#include <QTabWidget>

class QMenu;
class QMouseEvent;
class QPoint;

namespace timewarp::ui {

/// A tab widget whose size hints follow the visible page instead of the largest
/// one.
///
/// #### Why this subclass exists
///
/// `QTabWidget::minimumSizeHint()` is the **maximum** of every page's hint. Qt
/// hides pages by reparenting them out of the layout, but the hint is computed
/// from all pages regardless -- so one large page (the turtle canvas, say) pins
/// the minimum size of the entire window even while a small page is showing.
/// A `QSplitter` cannot then be dragged smaller, and the window cannot be
/// resized down. Returning a small fixed minimum and deriving the *preferred*
/// size from the current page fixes both.
///
/// #### No back-pointer to the window
///
/// The Python original carries a reference to its `MainWindow` and calls
/// `mw.close_tab()` / reads `mw._tab_states` directly, reaching past two levels
/// of privacy. Every action that is not purely local to a tab bar is a signal
/// here instead, which is what lets this widget be constructed and tested on its
/// own -- and what makes the tab state stay owned by whoever owns the files.
class ResizableTabWidget final : public QTabWidget {
    Q_OBJECT

public:
    /// Extra pixels added to the tab bar's own height when computing `sizeHint`.
    static constexpr int kTabBarMargin = 8;
    /// The fixed minimum returned for both dimensions.
    static constexpr int kMinimumSide = 100;
    /// Used when the current page reports no usable hint.
    static constexpr QSize kFallbackSize{400, 300};

    explicit ResizableTabWidget(QWidget* parent = nullptr);

    /// Small and constant, so a splitter holding this widget can shrink freely.
    [[nodiscard]] QSize minimumSizeHint() const override;

    /// The current page's hint plus the tab bar, so the *window* still asks for a
    /// size that fits what the user is actually looking at.
    [[nodiscard]] QSize sizeHint() const override;

    /// Turns on movable tabs and the context menu. Call once, after
    /// construction: re-enabling would connect the signals twice.
    void enableEnhancedTabBar();

signals:
    /// Middle-click, or "Close Tab" in the context menu.
    void closeRequested(int index);

    /// The user dragged a tab. The owner keeps any parallel per-tab bookkeeping
    /// (file path, modified flag, language) in step; this widget does not know
    /// that such state exists.
    void tabReordered(int from, int to);

    /// "Close Other Tabs". The owner decides, because closing a tab may need to
    /// prompt about unsaved changes -- and any one refusal must abort the batch.
    void closeOthersRequested(int index);

    /// "Close All Tabs", with the same caveat.
    void closeAllRequested();

    /// "Duplicate Tab". Only the owner can read a tab's contents and language.
    void duplicateRequested(int index);

    /// Asked just before the menu is shown, so the owner can append entries it
    /// is better placed to service (copy file path, reveal in file manager).
    /// `menu` is never null and is owned by the caller of this signal.
    void aboutToShowTabMenu(int index, QMenu* menu);

protected:
    void mousePressEvent(QMouseEvent* event) override;

private:
    void showTabContextMenu(const QPoint& pos);

    bool enhanced_ = false;
};

}  // namespace timewarp::ui
