// SPDX-License-Identifier: MIT
#pragma once

/// The handful of names Qt 6 introduced that this tree uses.
///
/// The application is built against Qt 6, and against Qt 5.15 for the Windows 7
/// package, because Qt 6 requires Windows 10 and 5.15 is the last Qt that runs on
/// 7 SP1. The two are otherwise source-compatible here, so what differs is
/// collected in this one header rather than sprinkled through the widgets.

#include <QString>
#include <QtGlobal>

#include <string_view>

namespace timewarp::core {

/// A string's length as an `int`, for APIs that take one (a column width, a
/// byte count). `static_cast<int>` would do it, but on Qt 5 the length is already
/// an `int` and `-Wuseless-cast` -- which these builds run with -- rejects the
/// cast; the implicit conversion is exact on both versions.
[[nodiscard]] inline int intLength(qsizetype length) { return length; }

/// A `std::string_view` as a QString, in UTF-8.
///
/// Qt 6's `QString::fromUtf8` takes a `QByteArrayView`, which a `string_view`
/// converts to; Qt 5's takes a pointer and a length, so the length is passed
/// explicitly. Every language name and extension in the table is a `string_view`.
[[nodiscard]] inline QString fromUtf8(std::string_view text) {
    return QString::fromUtf8(text.data(), intLength(text.size()));
}

}  // namespace timewarp::core
