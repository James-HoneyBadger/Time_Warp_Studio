// SPDX-License-Identifier: MIT
//
// What persists, and the settings dialog.

#include "retro_computer_internal.hpp"

#include <algorithm>
#include <optional>

#include <QCloseEvent>
#include <QComboBox>
#include <QFileInfo>
#include <QFont>
#include <QFrame>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPalette>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTabWidget>
#include <QTextCursor>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/core/qt_gui_compat.hpp"
#ifdef TIME_WARP_HAVE_AI
#include "timewarp/ai/prompt.hpp"
#include "timewarp/ui/assistant_dialog.hpp"
#endif
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace timewarp::ui {

using namespace retro_detail;

void RetroComputerWindow::saveCurrentProgram() {
    if (program_ != nullptr) {
        programs_[static_cast<int>(language_)] = program_->toPlainText();
    }
}

void RetroComputerWindow::saveSettings() {
    QSettings settings;
    settings.beginGroup(QStringLiteral("retroComputer"));
    settings.setValue(QStringLiteral("geometry"), saveGeometry());
    settings.setValue(QStringLiteral("language"), settingsKey(language_));
    settings.setValue(QStringLiteral("mode"), static_cast<int>(mode_));
    if (settings_.remember_programs) {
        for (auto it = programs_.cbegin(); it != programs_.cend(); ++it) {
            settings.setValue(
                QStringLiteral("programs/%1")
                    .arg(settingsKey(static_cast<Language>(it.key()))),
                it.value());
        }
    } else {
        // Switching the memory off has to erase what it already kept, or the next
        // start would show programs the setting says are not remembered.
        settings.remove(QStringLiteral("programs"));
    }
    settings.endGroup();
}

void RetroComputerWindow::restoreSettings() {
    if (settings_restored_) {
        return;
    }
    settings_restored_ = true;

    // Configuration first: the boot ROM and boot screen decide what this session
    // starts in, so they have to be known before the session's own state is read.
    settings_ = RetroComputerSettings::load();

    QSettings settings;
    settings.beginGroup(QStringLiteral("retroComputer"));
    const QByteArray geometry = settings.value(QStringLiteral("geometry")).toByteArray();
    if (!geometry.isEmpty()) {
        restoreGeometry(geometry);
    }
    const Language saved =
        languageFromKey(settings.value(QStringLiteral("language")).toString());
    const int saved_mode =
        settings.value(QStringLiteral("mode"), static_cast<int>(ScreenMode::Split)).toInt();
    if (settings_.remember_programs) {
        for (const Language language : allLanguages()) {
            const QString source =
                settings.value(QStringLiteral("programs/%1").arg(settingsKey(language)))
                    .toString();
            if (!source.isEmpty()) {
                programs_[static_cast<int>(language)] = source;
            }
        }
    }
    settings.endGroup();

    if (settings_.boot_language != RetroComputerSettings::kLastUsed) {
        const auto wanted = static_cast<Language>(settings_.boot_language);
        if (allLanguages().contains(wanted)) {
            language_ = wanted;
        }
    } else if (saved != Language::Unknown) {
        language_ = saved;
    }
    if (settings_.boot_mode != RetroComputerSettings::kLastUsed) {
        mode_ = static_cast<ScreenMode>(std::clamp(settings_.boot_mode, 0, 2));
    } else {
        mode_ = static_cast<ScreenMode>(std::clamp(saved_mode, 0, 2));
    }

    if (language_selector_ != nullptr) {
        const QSignalBlocker blocker{language_selector_};
        const int index = language_selector_->findData(static_cast<int>(language_));
        if (index >= 0) {
            language_selector_->setCurrentIndex(index);
        }
    }
    updateModeControls();
}

void RetroComputerWindow::setSettings(const RetroComputerSettings& settings) {
    applySettings(settings, true);
}

void RetroComputerWindow::applySettings(const RetroComputerSettings& settings, bool remember) {
    RetroComputerSettings next = settings;
    next.history_depth = std::clamp(next.history_depth, 0, 500);
    next.screen_text_size = std::clamp(next.screen_text_size, 8, 24);
    next.editor_text_size = std::clamp(next.editor_text_size, 8, 24);
    next.crt.scanlines = std::clamp(next.crt.scanlines, 0, 100);
    next.crt.vignette = std::clamp(next.crt.vignette, 0, 100);
    next.crt.curvature = std::clamp(next.crt.curvature, 0, 100);
    next.crt.flicker = std::clamp(next.crt.flicker, 0, 100);
    next.crt.phosphor = std::clamp(next.crt.phosphor, 0, 100);
    // The phosphor's colour is the palette's, so a settings file cannot pair a
    // green palette with an amber glow.
    next.crt.phosphor_colour = retroPalette(next.tint).text;
    settings_ = next;

    applyMachineLook();
    applyHistoryDepth();
    if (voice_ != nullptr) {
        voice_->setVolume(next.voice_volume);
        voice_->setMuted(next.voice_muted);
    }
    if (remember) {
        // Session state and configuration are written together: the remember
        // flag decides whether the programs are kept, so the two cannot be saved
        // apart without one of them becoming a lie.
        saveSettings();
        settings_.save();
    }
}

void RetroComputerWindow::applyHistoryDepth() {
    if (settings_.history_depth <= 0) {
        history_.clear();
        history_index_ = 0;
        return;
    }
    while (history_.size() > settings_.history_depth) {
        history_.removeFirst();
    }
    history_index_ = std::min(history_index_, static_cast<int>(history_.size()));
}

#ifdef TIME_WARP_HAVE_AI
void RetroComputerWindow::openAssistant() {
    if (!assistant_.isNull()) {
        assistant_->raise();
        assistant_->activateWindow();
        return;
    }
    // What the assistant is told: the program this ROM is holding, and which ROM
    // that is. The machine's own output is on its tube and is not sent -- a question
    // about the machine's program is not a question about its screen.
    auto* dialog = new AssistantDialog{
        [this] {
            timewarp::ai::Context context;
            context.language = displayName(language_);
            context.file_name = tr("HB-1 %1 program").arg(displayName(language_));
            context.source = program();
            return context;
        },
        this};
    assistant_ = dialog;
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    dialog->show();
}
#endif

RetroComputerSettingsDialog* RetroComputerWindow::openSettings() {
    settings_before_dialog_ = settings_;
    auto* dialog = new RetroComputerSettingsDialog{settings_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &RetroComputerSettingsDialog::previewRequested, this,
            [this](const RetroComputerSettings& preview) {
                // Preview only: the machine repaints, but nothing is written until
                // the dialog is accepted.
                applySettings(preview, false);
            });
    connect(dialog, &QDialog::finished, this, [this, dialog](int result) {
        if (result == QDialog::Accepted) {
            setSettings(dialog->settings());
        } else {
            applySettings(settings_before_dialog_, false);
        }
    });
    dialog->open();
    return dialog;
}

bool RetroComputerWindow::eventFilter(QObject* watched, QEvent* event) {
    if (watched == lamp_label_ && event->type() == QEvent::MouseButtonRelease) {
        togglePower();
        return true;
    }
    if (watched == input_ && event->type() == QEvent::KeyPress) {
        auto* key = static_cast<QKeyEvent*>(event);
        if (key->key() == Qt::Key_Up && !history_.isEmpty()) {
            history_index_ = std::max(0, history_index_ - 1);
            input_->setText(history_.at(history_index_));
            input_->setCursorPosition(input_->text().size());
            return true;
        }
        if (key->key() == Qt::Key_Down) {
            history_index_ = std::min(static_cast<int>(history_.size()), history_index_ + 1);
            input_->setText(history_index_ >= history_.size()
                                ? QString{}
                                : history_.at(history_index_));
            input_->setCursorPosition(input_->text().size());
            return true;
        }
    }
    return QMainWindow::eventFilter(watched, event);
}

void RetroComputerWindow::closeEvent(QCloseEvent* event) {
    saveCurrentProgram();
    saveSettings();
    stopRunner();
    stopRepl();
    QMainWindow::closeEvent(event);
}

}  // namespace timewarp::ui
