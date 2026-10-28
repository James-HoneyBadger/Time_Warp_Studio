// SPDX-License-Identifier: MIT

#include "timewarp/ui/assistant_dialog.hpp"

#include "timewarp/ai/config.hpp"
#include "timewarp/ui/ai_panel.hpp"
#include "timewarp/ui/ai_settings.hpp"

#include <QDialogButtonBox>
#include <QPushButton>
#include <QVBoxLayout>

#include <utility>

namespace timewarp::ui {

AssistantDialog::AssistantDialog(std::function<timewarp::ai::Context()> context,
                                 QWidget* parent)
    : QDialog(parent), context_{std::move(context)} {
    setObjectName(QStringLiteral("assistantDialog"));
    setWindowTitle(tr("\U0001F916 Assistant"));
    setWindowFlag(Qt::WindowContextHelpButtonHint, false);
    resize(720, 560);

    auto* layout = new QVBoxLayout{this};
    panel_ = new AiPanel{this};
    panel_->setObjectName(QStringLiteral("assistantDialogPanel"));
    panel_->setContextProvider(context_);
    panel_->setConfig(timewarp::ai::load());
    layout->addWidget(panel_, 1);

    auto* buttons = new QDialogButtonBox{this};
    close_ = buttons->addButton(tr("Close"), QDialogButtonBox::RejectRole);
    close_->setObjectName(QStringLiteral("assistantDialogClose"));
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::close);
    layout->addWidget(buttons);

    // The panel asks its owner to configure it; here the owner is this dialog, because
    // the machine's window has no settings of its own to offer.
    connect(panel_, &AiPanel::configureRequested, this, &AssistantDialog::openSettings);
}

void AssistantDialog::openSettings() {
    if (!settings_.isNull()) {
        settings_->raise();
        settings_->activateWindow();
        return;
    }
    auto* dialog = new AiSettingsDialog{timewarp::ai::load(), this};
    settings_ = dialog;
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &AiSettingsDialog::saved, this,
            [this](const timewarp::ai::Config& config) {
                timewarp::ai::save(config);
                if (panel_ != nullptr) {
                    panel_->setConfig(config);
                }
            });
    dialog->open();
}

}  // namespace timewarp::ui
