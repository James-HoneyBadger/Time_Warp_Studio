// SPDX-License-Identifier: MIT
//
// The settings dialog: every editor customisation in one place, with a live
// preview.
//
// The preview is the point. A preferences window with a font size spinner and no
// sample is a form to fill in and then go and look at; one that shows real code,
// checking itself as you change the tab width, is a window where the answer to
// "what does this do?" is on screen before OK is pressed.
//
// #### Applying
//
// The dialog owns a copy of `EditorSettings` and changes nothing outside itself.
// **Apply** and **OK** both emit `applied` with the current settings; the window
// decides what to do with them (apply to every tab, and remember them on OK).
// Cancelling therefore cannot leave the application in a state the user could not
// name.

#pragma once

#include <QDialog>

#include "timewarp/ui/code_editor.hpp"

class QCheckBox;
class QComboBox;
class QFontComboBox;
class QLabel;
class QSpinBox;

namespace timewarp::ui {

class PreferencesDialog final : public QDialog {
    Q_OBJECT

public:
    explicit PreferencesDialog(const EditorSettings& settings, QWidget* parent = nullptr);

    /// The settings as currently edited.
    [[nodiscard]] EditorSettings settings() const;

    /// The preview editor, so a test can assert that a change reaches it.
    [[nodiscard]] CodeEditor* preview() const noexcept { return preview_; }

signals:
    /// The user pressed Apply or OK. Carries the settings to apply.
    void applied(const EditorSettings& settings);
    /// OK specifically: the caller should also remember them.
    void accepted(const EditorSettings& settings);

private:
    void buildUi(const EditorSettings& settings);
    /// Reads every control into `settings_`, applies it to the preview, and
    /// emits nothing. Called before the dialog decides what to do with it.
    void collect();
    /// Whether the font controls should be enabled: the "system fixed font" box
    /// overrides the family.
    void updateFontControls();

    EditorSettings settings_;
    QCheckBox* system_font_ = nullptr;
    QFontComboBox* font_family_ = nullptr;
    QSpinBox* font_size_ = nullptr;
    QSpinBox* tab_width_ = nullptr;
    QCheckBox* line_numbers_ = nullptr;
    QCheckBox* current_line_ = nullptr;
    QCheckBox* word_wrap_ = nullptr;
    QCheckBox* auto_indent_ = nullptr;
    QCheckBox* completion_ = nullptr;
    QCheckBox* lint_ = nullptr;
    CodeEditor* preview_ = nullptr;
};

}  // namespace timewarp::ui
