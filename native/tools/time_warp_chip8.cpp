// SPDX-License-Identifier: AGPL-3.0-only
//
// Time Warp CHIP-8's entry point: a machine of its own, not a mode of the Studio.

#include "timewarp/ui/app.hpp"
#include "timewarp/ui/chip8_window.hpp"

#include <QApplication>
#include <QString>

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    timewarp::ui::applyApplicationIdentity(QStringLiteral("TimeWarpChip8"),
                                           QStringLiteral("time-warp-chip8"));

    timewarp::ui::Chip8Window window;
    if (argc > 1) {
        static_cast<void>(window.openFile(QString::fromLocal8Bit(argv[1])));
    }
    window.show();
    return QApplication::exec();
}
