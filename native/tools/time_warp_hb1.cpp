// SPDX-License-Identifier: AGPL-3.0-only

#include "timewarp/ui/retro_computer.hpp"

#include <QApplication>
#include <QCoreApplication>
#include <QString>

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("Time Warp Universe"));
    QCoreApplication::setApplicationName(QStringLiteral("Time Warp HB-1"));
    QCoreApplication::setApplicationDisplayName(QStringLiteral("Time Warp HB-1"));

    timewarp::ui::RetroComputerWindow window;
    window.show();
    return application.exec();
}
