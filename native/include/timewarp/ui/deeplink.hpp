// SPDX-License-Identifier: AGPL-3.0-only
//
// URL query deep links: the shareable link that opens a program.
//
// A browser page has no command line, so the query string is the only startup
// argument a link can carry. The format is the one `wasm/index.html` documents
// for embeds:
//
//   ?example=logo/mandala.logo        open an example program
//   ?lang=apl&code=<url-safe base64>  open a shared program in that language
//   ?notebook=<url>                   open a `.twnb` notebook fetched from a URL
//   ?run=1                            run whatever the link opened
//
// `code` is base64 with the URL-safe alphabet (`-` and `_` instead of `+` and
// `/`) and no padding, so the link survives a paste into a document and an email
// without escaping. The plain standard alphabet is accepted as well; the decoder
// normalises both before handing the bytes to Qt.
//
// This module is the *parsing* only, and it is kept free of the browser so a test
// can drive it on the desktop: `wasm_deeplink.cpp` is the part that reads the
// page's `location.search` and applies the result to a window.

#pragma once

#include <QString>

namespace timewarp::ui {

/// One deep link, already parsed and decoded.
struct DeepLink {
    /// A path relative to `:/examples`, e.g. `logo/mandala.logo`. Empty when
    /// the link did not name one.
    QString example;
    /// A protocol key (`apl`, `logo`, ...) for a shared program. Empty when the
    /// link is not a shared program.
    QString language;
    /// The decoded program source. Empty unless `language` is set.
    QString source;
    /// The URL of a `.twnb` notebook to fetch. Empty when the link is not one.
    QString notebook;
    /// `?run=1`: run the opened document once the window is up.
    bool run = false;

    /// Whether the link asks for anything at all.
    [[nodiscard]] bool isEmpty() const;
};

/// Parses a raw query string (with or without the leading `?`, as
/// `location.search` returns it) into a `DeepLink`. A malformed value is left
/// empty rather than guessed at: a shared link that opens the wrong program is
/// worse than one that reports the link was not understood.
[[nodiscard]] DeepLink parseDeepLink(const QString& rawQuery);

}  // namespace timewarp::ui
