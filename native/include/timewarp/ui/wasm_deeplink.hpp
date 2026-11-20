// SPDX-License-Identifier: AGPL-3.0-only
//
// The browser half of deep links; see `wasm_deeplink.cpp` and `deeplink.hpp`.
// Kept separate from the parser so the parser can be tested with QtCore alone.

#pragma once

#include <QString>

namespace timewarp::ui {
class MainWindow;
struct DeepLink;
}  // namespace timewarp::ui

namespace timewarp::ui::wasm {

/// The page's query string, as `location.search` returns it (leading `?` and
/// all). Empty on a desktop build, where there is no page to ask.
[[nodiscard]] QString browserQueryString();

/// Fetches `url` and returns its text; empty when the fetch failed or on a
/// desktop build.
[[nodiscard]] QString fetchText(const QString& url);

/// Applies a parsed link to `window`: opens the example, the shared program or
/// the fetched notebook. Returns true when a runnable document (an example or a
/// shared program) was opened; a notebook is opened but does not count, because
/// it has no single current program for `?run=1` to mean. Failures are reported
/// through `error`, not raised: a bad link should leave the application open with
/// the reason on stderr, the way a bad file argument does.
[[nodiscard]] bool applyDeepLink(MainWindow* window, const timewarp::ui::DeepLink& link,
                                 QString* error);

}  // namespace timewarp::ui::wasm
