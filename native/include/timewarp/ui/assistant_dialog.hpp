// SPDX-License-Identifier: MIT
//
// The assistant, for a window that is not the main one.
//
// The two machines are windows of their own, and neither carries the right-hand
// column the IDE has. This is the same page in a dialog, so a CHIP-8 listing or an
// HB-1 program can be explained without copying it into the editor first -- and so
// there is exactly one assistant implementation rather than a second one that
// drifts.
//
// What it is told is the machine's own program, through the provider: the dialog
// never reads a widget it does not own, and each machine decides what "the program"
// means for it.

#pragma once

#include <QDialog>
#include <QPointer>

#include <functional>

#include "timewarp/ai/prompt.hpp"
#include "timewarp/ui/ai_settings.hpp"

class QPushButton;

namespace timewarp::ui {

class AiPanel;

class AssistantDialog final : public QDialog {
    Q_OBJECT

public:
    /// `context` is asked for the program when a question is sent, on the GUI thread,
    /// so what is sent is what the machine holds at that moment.
    explicit AssistantDialog(std::function<timewarp::ai::Context()> context,
                             QWidget* parent = nullptr);

    [[nodiscard]] AiPanel* panel() const noexcept { return panel_; }
    /// The settings dialog this one opens for **Settings…**, so a test can reach it.
    [[nodiscard]] AiSettingsDialog* settingsDialog() const noexcept {
        return settings_.data();
    }

private:
    void openSettings();

    std::function<timewarp::ai::Context()> context_;
    AiPanel* panel_ = nullptr;
    QPushButton* close_ = nullptr;
    QPointer<AiSettingsDialog> settings_;
};

}  // namespace timewarp::ui
