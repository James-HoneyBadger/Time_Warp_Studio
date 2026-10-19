// SPDX-License-Identifier: MIT
//
// Animated GIF writing, in tree.
//
// Qt reads GIF but does not write it, and a dependency for one export is a
// package every user pays for. This is a GIF89a writer with a fixed 6x6x6 global
// palette and the standard LZW compression: the smallest thing that produces a
// file every browser and image viewer opens.
//
// The palette is fixed rather than adaptive on purpose. A turtle drawing is flat
// colours and a handful of them; quantising to six levels per channel is both
// faster than a median cut and smaller, because the table is written once instead
// of once per frame.

#pragma once

#include <QImage>
#include <QString>
#include <QVector>

namespace timewarp::ui::gif {

/// One frame and how long it is shown, in milliseconds.
///
/// Merging a frame that did not change into the one before it is the caller's
/// job: a recording of a canvas that is idle for half its length is then one
/// frame with a longer delay rather than a hundred identical pictures.
struct Frame {
    QImage image;
    int delayMs = 100;
};

/// Adds *image* to *frames*, merging it into the previous frame when the picture
/// did not change: a still canvas is time passing, not a new frame, and a
/// recording of an idle minute should be one frame with a long delay rather than
/// six hundred identical pictures.
void appendFrame(QVector<Frame>& frames, const QImage& image, int delayMs);

/// Writes *frames* as an animated GIF that loops forever.
///
/// Every frame must be the same size and at least 1x1. An empty list, a size
/// mismatch, or a path that cannot be written returns false and fills *error*.
/// The file is written through `QSaveFile`, so a failure part way through leaves
/// no half-written GIF behind.
[[nodiscard]] bool writeGif(const QString& path, const QVector<Frame>& frames,
                            QString* error);

}  // namespace timewarp::ui::gif
