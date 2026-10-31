// SPDX-License-Identifier: MIT
#pragma once

/// Two Qt spellings this tree builds a value with in more than one place.
///
/// Neither is a compatibility shim: there is one Qt here, and these are the calls it
/// makes. What they save is line noise at the call sites, which is worth it when the
/// rest of the line is a menu entry or a table row.

#include <QKeySequence>
#include <QString>
#include <QtGlobal>

#include <string_view>

namespace timewarp::core {

/// A `std::string_view` as a QString, in UTF-8.
///
/// The language table and the canvas' command names are `string_view`s, and
/// `QString::fromUtf8` takes a `QByteArrayView`, so the length is passed explicitly.
[[nodiscard]] inline QString fromUtf8(std::string_view text) {
    return QString::fromUtf8(text.data(), static_cast<int>(text.size()));
}

/// A key sequence from its modifiers and key.
///
/// The modifiers are an `int` because the callers write `Qt::CTRL | Qt::ALT`, which
/// is a `QFlags<Qt::Modifier>`, and some write `Qt::CTRL` alone, which is a bare
/// `Qt::Modifier` -- and `QKeyCombination` wants neither, only a
/// `Qt::KeyboardModifiers`. Forty-odd menu entries do not each need to say so.
[[nodiscard]] inline QKeySequence keySequence(int modifiers, Qt::Key key) {
    return QKeySequence{QKeyCombination{static_cast<Qt::KeyboardModifiers>(modifiers), key}};
}

}  // namespace timewarp::core
