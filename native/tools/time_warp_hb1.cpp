// SPDX-License-Identifier: AGPL-3.0-only
//
// Time Warp HB-1's entry point: a machine of its own, not a mode of the Studio.

#include "timewarp/ui/app.hpp"
#include "timewarp/ui/retro_computer.hpp"

#include <QApplication>

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    timewarp::ui::applyApplicationIdentity(QStringLiteral("TimeWarpHb1"),
                                           QStringLiteral("time-warp-hb1"));

    timewarp::ui::RetroComputerWindow window;
    window.show();
    return QApplication::exec();
}
