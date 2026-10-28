// SPDX-License-Identifier: MIT
//
// The examples gallery: every shipped program, with its own drawing for a face.
//
// A list of filenames answers "what is there?" and nothing else. The question a
// person actually has, looking at sixty-nine programs they did not write, is what
// each one *does* -- and for a turtle language the answer is a picture. So each
// entry is rendered by running the program headlessly and painting what it drew,
// which is also the honest way to notice that a program stopped drawing.
//
// #### Rendering is bounded, batched and off the critical path
//
// A program is a program: one may loop, ask for input or draw ten thousand lines.
// Each thumbnail runs under the engine's own step budget with input answering "", so
// a runaway is stopped by the same machinery a Run uses, and the dialog opens before
// any of it starts -- a batch of thumbnails is rendered per event-loop turn, so the
// window is usable while the rest arrive. A program that drew nothing gets a card
// naming its language instead of an empty square.

#pragma once

#include <QDialog>
#include <QImage>
#include <QSize>
#include <QString>
#include <QStringList>

class QListWidget;
class QListWidgetItem;
class QPushButton;
class QTimer;

namespace timewarp::ui {

class GalleryDialog final : public QDialog {
    Q_OBJECT

public:
    explicit GalleryDialog(QWidget* parent = nullptr);

    /// The path of the program the user opened, or empty. The window opens it; the
    /// dialog does not know what a document is.
    [[nodiscard]] QString chosen() const noexcept { return chosen_; }

    /// How many programs the gallery listed.
    [[nodiscard]] int count() const;
    /// The paths it listed, in the order they appear.
    [[nodiscard]] QStringList programs() const;

    /// A thumbnail for one program, drawn from the drawing it makes. Public so a
    /// test can render one program without a dialog, and so the window could reuse
    /// it for a preview later. `size` is the image size in device pixels.
    [[nodiscard]] static QImage renderThumbnail(const QString& path, const QSize& size);

signals:
    void programChosen(const QString& path);

private:
    /// Renders up to `kBatch` pending thumbnails, then schedules the next batch.
    void renderNextBatch();
    void choose(QListWidgetItem* item);

    QListWidget* list_ = nullptr;
    QPushButton* open_ = nullptr;
    QTimer* timer_ = nullptr;
    QStringList pending_;
    QString chosen_;

    static constexpr int kBatch = 3;
};

}  // namespace timewarp::ui
