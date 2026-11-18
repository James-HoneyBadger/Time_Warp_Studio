// SPDX-License-Identifier: AGPL-3.0-only

#include "timewarp/ui/hb2_window.hpp"

#include <QApplication>
#include <QCoreApplication>
#include <QString>

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("Time Warp Universe"));
    QCoreApplication::setApplicationName(QStringLiteral("Time Warp HB-2"));
    QCoreApplication::setApplicationDisplayName(QStringLiteral("Time Warp HB-2"));

    timewarp::ui::Hb2Window window;
    window.show();
    return application.exec();
}
