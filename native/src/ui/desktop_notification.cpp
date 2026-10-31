// SPDX-License-Identifier: MIT

#include "timewarp/ui/desktop_notification.hpp"

#include <QStringList>
#include <QVariantMap>

#if defined(TIME_WARP_HAVE_DBUS)
#include <QDBusConnection>
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
    const QDBusReply<uint> reply =
        service.call(QStringLiteral("Notify"),
                     // The name that replaces a notification from the same
                     // application, rather than adding one: two runs in a row
                     // should not leave two boxes to dismiss.
                     QStringLiteral("Time Warp Studio"), 0u,
                     QStringLiteral("time-warp-studio"), title, body, QStringList{},
                     QVariantMap{}, timeout_ms);
    return reply.isValid();
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
