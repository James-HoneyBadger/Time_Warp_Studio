// SPDX-License-Identifier: AGPL-3.0-only
//
// Time Warp HB-2's entry point: a machine of its own, not a mode of the Studio.

#include "timewarp/ui/app.hpp"
#include "timewarp/ui/hb2_window.hpp"

#include <QApplication>

int main(int argc, char* argv[]) {
    QApplication application{argc, argv};
    timewarp::ui::applyApplicationIdentity(QStringLiteral("TimeWarpHb2"),
                                           QStringLiteral("time-warp-hb2"));

    timewarp::ui::Hb2Window window;
    window.show();
    return QApplication::exec();
}
