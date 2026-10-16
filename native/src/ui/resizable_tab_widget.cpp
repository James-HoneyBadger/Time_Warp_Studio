// SPDX-License-Identifier: MIT

#include "timewarp/ui/resizable_tab_widget.hpp"

#include <algorithm>

#include <QMenu>
#include <QMouseEvent>
#include <QTabBar>

namespace timewarp::ui {

ResizableTabWidget::ResizableTabWidget(QWidget* parent) : QTabWidget(parent) {}

QSize ResizableTabWidget::minimumSizeHint() const {
    // Constant rather than derived from any page. This is the whole point of the
    // class: the base implementation returns the maximum over all pages, which
    // pins the window's minimum size to the biggest panel in the application.
    return QSize(kMinimumSide, kMinimumSide);
}

QSize ResizableTabWidget::sizeHint() const {
    const QWidget* page = currentWidget();
    if (page == nullptr) {
        return kFallbackSize;
    }

    // Only the tab bar's height is added; its width is bounded by the widest tab
    // text and is not worth modelling here.
    const int barHeight = tabBar()->sizeHint().height() + kTabBarMargin;

    const QSize pageHint = page->sizeHint();
    const int width = pageHint.width() > 0 ? std::max(pageHint.width(), kMinimumSide)
                                           : kFallbackSize.width();
    const int height = pageHint.height() > 0 ? std::max(pageHint.height(), kMinimumSide)
                                             : kFallbackSize.height();
    return QSize(width, height + barHeight);
}

void ResizableTabWidget::enableEnhancedTabBar() {
    if (enhanced_) {
        return;
    }
    enhanced_ = true;

    tabBar()->setMovable(true);

    // The context menu belongs to the tab bar, not the page area, so a
    // right-click on the page itself still reaches the page's own handler.
    tabBar()->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(tabBar(), &QTabBar::customContextMenuRequested, this,
            &ResizableTabWidget::showTabContextMenu);

    connect(tabBar(), &QTabBar::tabMoved, this, &ResizableTabWidget::tabReordered);
}

void ResizableTabWidget::mousePressEvent(QMouseEvent* event) {
    if (event->button() == Qt::MiddleButton) {
        const int index = tabBar()->tabAt(event->position().toPoint());
        if (index >= 0) {
            emit closeRequested(index);
            // Consumed: the base class would otherwise start a drag from the
            // middle button on some platforms.
            event->accept();
            return;
        }
    }
    QTabWidget::mousePressEvent(event);
}

void ResizableTabWidget::showTabContextMenu(const QPoint& pos) {
    const int index = tabBar()->tabAt(pos);
    if (index < 0) {
        return;  // a click on the empty area to the right of the last tab
    }

    QMenu menu{this};
    const QAction* close = menu.addAction(tr("Close Tab"));
    const QAction* closeOthers = menu.addAction(tr("Close Other Tabs"));
    const QAction* closeAll = menu.addAction(tr("Close All Tabs"));
    menu.addSeparator();
    const QAction* duplicate = menu.addAction(tr("Duplicate Tab"));

    // Hand the menu over before it is shown so the owner can add whatever it
    // alone can service -- copying a file path, revealing a file, and so on.
    emit aboutToShowTabMenu(index, &menu);

    // `nullptr` from `exec` means the menu was dismissed without a choice, which
    // is not the same as any of the entries below.
    const QAction* chosen = menu.exec(tabBar()->mapToGlobal(pos));
    if (chosen == nullptr) {
        return;
    }
    if (chosen == close) {
        emit closeRequested(index);
    } else if (chosen == closeOthers) {
        emit closeOthersRequested(index);
    } else if (chosen == closeAll) {
        emit closeAllRequested();
    } else if (chosen == duplicate) {
        emit duplicateRequested(index);
    }
}

}  // namespace timewarp::ui
