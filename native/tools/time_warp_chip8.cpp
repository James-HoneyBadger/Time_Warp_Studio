// SPDX-License-Identifier: AGPL-3.0-only

#include "timewarp/ui/chip8_window.hpp"

#include <QApplication>
#include <QCoreApplication>
#include <QString>

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("Time Warp Universe"));
    QCoreApplication::setApplicationName(QStringLiteral("Time Warp CHIP-8"));
    QCoreApplication::setApplicationDisplayName(QStringLiteral("Time Warp CHIP-8"));

    timewarp::ui::Chip8Window window;
    if (argc > 1) {
        static_cast<void>(window.openFile(QString::fromLocal8Bit(argv[1])));
    }
    window.show();
    return application.exec();
}
