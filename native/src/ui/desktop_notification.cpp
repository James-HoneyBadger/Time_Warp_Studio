// SPDX-License-Identifier: MIT

#include "timewarp/ui/desktop_notification.hpp"

#include <QStringList>
#include <QVariantMap>

#if defined(TIME_WARP_HAVE_DBUS)
#include <QDBusConnection>
#include <QDBusConnectionInterface>
#include <QDBusInterface>
#include <QDBusReply>
#endif

namespace timewarp::ui {

bool notifications_available() {
#if defined(TIME_WARP_HAVE_DBUS)
    return true;
#else
    return false;
#endif
}

namespace {

#if defined(TIME_WARP_HAVE_DBUS)
/// The id the service gave the last notification, so the next one replaces it.
///
/// Replacement is keyed on `replaces_id`, not on the application's name: passing
/// zero, which is what this used to do, asks for a *new* notification every time and
/// leaves a stack of boxes for two runs in a row.
uint& lastNotificationId() {
    static uint id = 0;
    return id;
}
#endif

}  // namespace

bool notifications_reachable() {
#if defined(TIME_WARP_HAVE_DBUS)
    const QDBusConnection bus = QDBusConnection::sessionBus();
    if (!bus.isConnected() || bus.interface() == nullptr) {
        return false;
    }
    return bus.interface()->isServiceRegistered(
        QStringLiteral("org.freedesktop.Notifications"));
#else
    return false;
#endif
}

bool notifyDesktop(const QString& title, const QString& body, int timeout_ms) {
#if defined(TIME_WARP_HAVE_DBUS)
    QDBusInterface service{QStringLiteral("org.freedesktop.Notifications"),
                           QStringLiteral("/org/freedesktop/Notifications"),
                           QStringLiteral("org.freedesktop.Notifications"),
                           QDBusConnection::sessionBus()};
    if (!service.isValid()) {
        // No session bus, or nothing owning the name: a machine with no desktop
        // notification service, which is a normal way to run this application.
        return false;
    }
    // The call is given a short timeout: it runs on the GUI thread at the end of a
    // run, and a notification daemon that has stopped answering must not hold the
    // window for the twenty-five seconds the default would.
    constexpr int kCallTimeoutMs = 2000;
    const QDBusReply<uint> reply =
        service.call(QStringLiteral("Notify"), QStringLiteral("Time Warp Studio"),
                     // The id of the box this replaces, so two runs in a row leave
                     // one notification rather than two.
                     lastNotificationId(), QStringLiteral("time-warp-studio"), title,
                     body, QStringList{}, QVariantMap{}, timeout_ms, kCallTimeoutMs);
    if (!reply.isValid()) {
        // Nothing to replace next time: whatever box is out there is not ours.
        lastNotificationId() = 0;
        return false;
    }
    lastNotificationId() = reply.value();
    return true;
#else
    Q_UNUSED(title);
    Q_UNUSED(body);
    Q_UNUSED(timeout_ms);
    return false;
#endif
}

QString runNotificationBody(const QString& document, bool completed, const QString& error) {
    const QString name = document.trimmed().isEmpty() ? QStringLiteral("The program")
                                                      : document.trimmed();
    if (!error.isEmpty()) {
        // The first line of the error: a notification is a headline, and the
        // console already has the whole sentence with its line number.
        return QStringLiteral("%1 stopped with an error: %2")
            .arg(name, error.section(QLatin1Char('\n'), 0, 0));
    }
    return completed ? QStringLiteral("%1 finished").arg(name)
                     : QStringLiteral("%1 stopped").arg(name);
}

}  // namespace timewarp::ui
