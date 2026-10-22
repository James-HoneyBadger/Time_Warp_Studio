// SPDX-License-Identifier: MIT
//
// The HB-1's configuration: what the machine powers up into, what its
// screen looks like, and how its prompt behaves.
//
// The user-facing reference for every field below is `docs/hb1/config.md`.
//
// This is separate from the editor's Preferences for the same reason the machine
// is a separate window: a 1980s computer had a monitor, a boot ROM and a keyboard,
// and none of those are editor settings. Keeping them in one struct means the
// dialog, the persisted file and the machine itself agree about what "the amber
// one" means.
//
// #### Monitors are palettes, not tints
//
// A phosphor choice replaces every colour the machine draws with: an amber
// machine is amber text on a warm black, not the green machine with a filter over
// it. `retroPalette` is the only place those colours are written down, so the
// stylesheet, the program editor's palette, the canvas surface and the CRT glass
// cannot disagree.

#pragma once

#include <QColor>
#include <QDialog>
#include <QJsonObject>
#include <QList>
#include <QString>

#include "timewarp/ui/panels.hpp"

class QCheckBox;
class QComboBox;
class QLabel;
class QSlider;

namespace timewarp::ui {

/// How much of the start-up message the machine prints after a cold boot.
enum class BootBanner {
    /// The machine name, the ROM, the copyright line and `SYSTEM READY`.
    Full,
    /// The machine name and the ROM only.
    Short,
    /// Nothing: the screen starts empty, as a machine with a quiet ROM would.
    Off,
};

/// The monitor's phosphor. Each one is a whole palette; see the file comment.
enum class PhosphorTint {
    Green,
    Amber,
    White,
    Ice,
};

/// Every colour the machine paints with, for one phosphor.
struct RetroPalette {
    /// Behind the machine, outside the screen frame.
    QColor background;
    /// The header, the prompt bar and other bezel panels.
    QColor panel;
    /// Frame and control outlines.
    QColor border;
    /// Ordinary screen text.
    QColor text;
    /// Headings and the ROM name.
    QColor bright;
    /// Captions and the status line.
    QColor dim;
    /// The prompt and the power lamp: what the eye should find first.
    QColor accent;
    /// Selection and hover fills.
    QColor selection;
    /// The drawing surface behind the turtle.
    QColor canvas;
};

/// The palette for `tint`.
[[nodiscard]] RetroPalette retroPalette(PhosphorTint tint);

/// Everything the machine remembers about how it should be set up. The ROM and
/// screen it was *left* in are separate: those are session state, written by the
/// window; this is configuration, written by the settings dialog.
struct RetroComputerSettings {
    /// "Follow the ROM the machine was last left in", and likewise for the screen.
    static constexpr int kLastUsed = -1;

    /// The ROM to boot into, as a `Language`, or `kLastUsed`.
    int boot_language = kLastUsed;
    /// The screen mode to boot into, as a `RetroComputerWindow::ScreenMode`, or
    /// `kLastUsed`.
    int boot_mode = kLastUsed;
    BootBanner banner = BootBanner::Full;
    /// Keep each ROM's program in the settings file, so it survives a restart.
    bool remember_programs = true;
    /// Switch a text screen to SPLIT when a program draws, so ink is not drawn
    /// where it cannot be seen.
    bool switch_to_split = true;
    /// How many prompt lines Up and Down recall. Zero turns recall off.
    int history_depth = 100;
    PhosphorTint tint = PhosphorTint::Green;
    /// Point size of the OUTPUT screen and the prompt line.
    int screen_text_size = 11;
    /// Point size of the PROGRAM editor. Independent of the screen: a listing is
    /// read at a different distance from a result.
    int editor_text_size = 12;
    /// The glass over the screen, from the same type the editor's CRT uses.
    CrtOverlay::Settings crt{13, 7, 3, 0, 8, QColor{51, 255, 102}};
    /// The machine's own voice: boot chirp, key click, disk rattle, error buzz and
    /// tape warble. A program's `SOUND` is the program's; this is the machine's.
    bool voice_muted = false;
    /// 0..100. Zero is a quiet machine rather than a muted one -- the events are still
    /// counted, and `MachineVoice::sounded()` still says nothing was emitted.
    int voice_volume = 60;

    /// Reads the `retroComputer/config` group. Missing keys keep their defaults,
    /// which is what makes a fresh installation a stock machine.
    [[nodiscard]] static RetroComputerSettings load();
    /// Writes the same group. The caller owns the rest of the machine's state.
    void save() const;

    /// The same fields as a document, and back. A machine image embeds the
    /// configuration, and the field list belongs here with the two functions above
    /// rather than in the image's file: three copies of one list is how a field
    /// ends up in the dialog and in neither of the other two.
    [[nodiscard]] QJsonObject toJson() const;
    /// Missing keys keep their defaults and out-of-range numbers are clamped, so a
    /// hand-edited image is a machine with a setting changed rather than a refusal.
    static RetroComputerSettings fromJson(const QJsonObject& root);
};

/// The machine's configuration screen: three pages of settings with a live
/// preview and a Restore Defaults button.
///
/// Like every settings dialog here, it changes nothing outside itself until the
/// window is told what it holds: **OK** is the window's cue, and cancelling puts
/// the machine back as it was.
class RetroComputerSettingsDialog final : public QDialog {
    Q_OBJECT

public:
    explicit RetroComputerSettingsDialog(const RetroComputerSettings& start,
                                         QWidget* parent = nullptr);

    /// The settings as currently edited.
    [[nodiscard]] RetroComputerSettings settings() const { return settings_; }

    /// Puts every control back to a stock machine. Public because the dialog's
    /// own button is not the only thing that should be able to do it.
    void restoreDefaults();

signals:
    /// Emitted on every change, so the machine behind the dialog can show it
    /// before the user decides.
    void previewRequested(const RetroComputerSettings& settings);

private:
    void buildUi();
    /// Paints the dialog in the machine's current phosphor. It is parented to the
    /// machine, so without a sheet of its own it would inherit the machine's
    /// controls on a plain window.
    void applyStyle();
    /// Writes `settings_` into every control without emitting previews, which is
    /// how Restore Defaults fills the page in one step.
    void applyToControls();
    /// Reads every control into `settings_` and emits nothing.
    void collect();
    /// Collects, then asks the machine to preview.
    void publish();
    /// Writes each slider's value beside it. Separate from the sliders' own
    /// connection because Restore Defaults moves them with their signals blocked.
    void refreshValueLabels();

    /// One slider and the number that describes it.
    struct SliderRow {
        QSlider* slider = nullptr;
        QLabel* label = nullptr;
        QString suffix;
    };

    RetroComputerSettings settings_;
    QList<SliderRow> slider_rows_;

    QComboBox* boot_language_ = nullptr;
    QComboBox* boot_screen_ = nullptr;
    QComboBox* boot_banner_ = nullptr;
    QComboBox* phosphor_ = nullptr;
    QComboBox* history_depth_ = nullptr;
    QSlider* screen_text_size_ = nullptr;
    QSlider* editor_text_size_ = nullptr;
    QSlider* voice_volume_ = nullptr;
    QCheckBox* voice_muted_ = nullptr;
    QCheckBox* remember_programs_ = nullptr;
    QCheckBox* switch_to_split_ = nullptr;
    QSlider* crt_scanlines_ = nullptr;
    QSlider* crt_vignette_ = nullptr;
    QSlider* crt_curvature_ = nullptr;
    QSlider* crt_flicker_ = nullptr;
    QSlider* crt_tint_ = nullptr;
};

}  // namespace timewarp::ui
