// SPDX-License-Identifier: MIT
#pragma once

/// The Qt 5 names for the mouse-event geometry, alongside `qt_compat.hpp`.
///
/// Separate from that header because a Core-only target (the SQL layer) must not
/// be made to find QtGui's headers for a typedef it does not use.

#include <QDropEvent>
#include <QWheelEvent>
#include <QKeySequence>
#include <QMouseEvent>
#include <QPointF>
#include <QtGlobal>

namespace timewarp::core {

/// The cursor position of a mouse event, in the widget's own coordinates.
///
/// Qt 6 renamed `localPos()` to `position()`; the arithmetic in the canvas and
/// the editor is the same either way.
[[nodiscard]] inline QPointF eventPosition(const QMouseEvent* event) {
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    return event->position();
#else
    return event->localPos();
#endif
}

/// The cursor position of a wheel event, in the widget's own coordinates.
/// QWheelEvent::position() arrived in 5.14, so both versions have it; the
/// 5.15 spelling of it is the same call, which is why this one needs no #if.
[[nodiscard]] inline QPointF eventPosition(const QWheelEvent* event) {
    return event->position();
}

/// The position of a drop, in the widget's own coordinates.
[[nodiscard]] inline QPointF eventPosition(const QDropEvent* event) {
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    return event->position();
#else
    return QPointF{event->pos()};
#endif
}

/// A key sequence from its modifiers and key.
///
/// Qt 6 wants a `QKeyCombination`; Qt 5 ORs the two enums into an int, which
/// C++20 calls a deprecated enum-enum conversion and `-Werror` rejects. The
/// integer is what both ultimately take.
[[nodiscard]] inline QKeySequence keySequence(int modifiers, Qt::Key key) {
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    return QKeySequence{
        QKeyCombination{static_cast<Qt::KeyboardModifiers>(modifiers), key}};
#else
    return QKeySequence{modifiers | static_cast<int>(key)};
#endif
}

}  // namespace timewarp::core
