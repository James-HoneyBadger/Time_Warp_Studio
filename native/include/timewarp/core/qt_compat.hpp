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

/// A run of UTF-16 as a QString, for a platform API that hands one back -- the
/// Windows Credential Manager's blob, for instance.
///
/// Qt 6 spells the element type `char16_t` and Qt 5 spells it `ushort`, and neither
/// converts to the other, so the cast is here once rather than at each caller.
[[nodiscard]] inline QString fromUtf16(const void* data, int length) {
    // The one place the two Qt versions are allowed to differ: this header is the
    // seam, so the `#if` lives here and not at a call site.
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    return QString::fromUtf16(static_cast<const char16_t*>(data), length);
#else
    return QString::fromUtf16(static_cast<const ushort*>(data), length);
#endif
}

}  // namespace timewarp::core
