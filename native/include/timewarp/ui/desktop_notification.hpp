// SPDX-License-Identifier: MIT
//
// One desktop notification, through the freedesktop notification service.
//
// A program that draws for four seconds does not need one, and a program that
// draws for four minutes on a 1982 machine does. The rule the window applies is the
// only one that makes a notification bearable: it is posted when the run finishes
// and the window is *not* the active one, so nobody is told something they are
// watching. It is local, it goes through the session bus, and a session with no
// notification daemon says nothing rather than failing.

#pragma once

#include <QString>

namespace timewarp::ui {

/// The settings key: whether a run that finishes in the background is announced.
/// On by default, because the alternative is a user returning to a window that
/// stopped listening four minutes ago with no way to know.
inline constexpr const char* kSettingNotifyOnFinish = "notifications/onFinish";

/// Whether this build was compiled with Qt DBus.
[[nodiscard]] bool notifications_available();

/// Posts *body* under *title*. False when the build has no DBus, when no
/// notification service is running, or when the service refused the request; the
/// caller has nothing useful to do about any of them, so it is not an error.
bool notifyDesktop(const QString& title, const QString& body, int timeout_ms = 6000);

/// The body for a finished run: the document's name and how it ended. Empty when
/// there is nothing worth saying.
[[nodiscard]] QString runNotificationBody(const QString& document, bool completed,
                                          const QString& error);

}  // namespace timewarp::ui
