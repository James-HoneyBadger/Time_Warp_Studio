// SPDX-License-Identifier: MIT
//
// The assistant's settings: the endpoint, the model, the key, and a button that
// proves the three of them work.
//
// Separate from the application's Settings dialog on purpose. That one is about how
// text is drawn and is where a learner goes to change a font; this one decides
// whether the program on screen may leave the machine, and it deserves a window
// that says so rather than a group box at the bottom of a long form.
//
// #### Test connection
//
// The one reason this dialog is not four line edits. A wrong base URL, a key with a
// trailing space and a model name that has been retired all fail the same way on
// first use -- with a red sentence in the panel -- and all three are answerable here
// in two seconds, before the user has written the question they were going to ask.

#pragma once

#include <QDialog>
#include <QList>
#include <QString>

#include "timewarp/ai/config.hpp"

class QCheckBox;
class QComboBox;
class QDoubleSpinBox;
class QLabel;
class QLineEdit;
class QPushButton;
class QSpinBox;

namespace timewarp::ai {
class Client;
}

namespace timewarp::ui {

class AiSettingsDialog final : public QDialog {
    Q_OBJECT

public:
    explicit AiSettingsDialog(const timewarp::ai::Config& config, QWidget* parent = nullptr);
    ~AiSettingsDialog() override;

    /// The configuration as currently edited. The key is in it even when the user
    /// asked not to keep it: it is used for this session either way, and `save()`
    /// is what decides whether it reaches the disk.
    [[nodiscard]] timewarp::ai::Config config() const;

    // -- for the tests ----------------------------------------------------
    [[nodiscard]] QLineEdit* baseUrlField() const noexcept { return base_url_; }
    [[nodiscard]] QLineEdit* modelField() const noexcept { return model_; }
    [[nodiscard]] QLineEdit* keyField() const noexcept { return key_; }
    [[nodiscard]] QComboBox* presetChooser() const noexcept { return preset_; }
    [[nodiscard]] QLabel* testStatus() const noexcept { return test_status_; }
    /// What the Test button does, without a click.
    void testConnection();
    /// Asks the endpoint what it can run and fills the chooser. Also called when
    /// the dialog opens on an endpoint on this machine, because the point of a local
    /// server is that you should not have to know the model's name.
    void findModels();
    /// The model chooser, for the tests.
    [[nodiscard]] QComboBox* modelChooser() const noexcept { return models_; }
    /// The voice chooser, so a test can set it without a mouse.
    [[nodiscard]] QComboBox* personaChooser() const noexcept { return persona_; }

signals:
    /// OK was pressed. The window saves it and hands it to the panel.
    void saved(const timewarp::ai::Config& config);

private:
    void buildUi();
    void fillFrom(const timewarp::ai::Config& config);
    void collect();
    /// Reads every control into a configuration. `collect()` is the mutating form,
    /// used before a test or an accept; this is the const one `config()` returns.
    [[nodiscard]] timewarp::ai::Config collected() const;
    /// Choosing a preset writes its protocol, endpoint and model into the fields.
    void applyPreset(int index);
    /// True when the endpoint is on this machine, which is when discovery is worth
    /// doing without being asked.
    [[nodiscard]] bool endpoint_is_local() const;
    /// Points the dropdown at the preset the endpoint matches, without firing the
    /// handler that would overwrite the fields the user typed.
    void syncPresetToEndpoint();
    [[nodiscard]] QString selectedPresetName() const;

    timewarp::ai::Config config_;
    timewarp::ai::Client* test_client_ = nullptr;
    bool adjusting_ = false;

    QCheckBox* enabled_ = nullptr;
    QComboBox* preset_ = nullptr;
    QComboBox* protocol_ = nullptr;
    QLineEdit* base_url_ = nullptr;
    QLineEdit* model_ = nullptr;
    QLineEdit* key_ = nullptr;
    QCheckBox* show_key_ = nullptr;
    QCheckBox* store_key_ = nullptr;
    QCheckBox* send_document_ = nullptr;
    QCheckBox* remember_ = nullptr;
    QCheckBox* inline_ = nullptr;
    QSpinBox* max_tokens_ = nullptr;
    QDoubleSpinBox* temperature_ = nullptr;
    QSpinBox* timeout_ = nullptr;
    QPushButton* test_ = nullptr;
    QPushButton* find_models_ = nullptr;
    QComboBox* models_ = nullptr;
    QComboBox* persona_ = nullptr;
    QLabel* test_status_ = nullptr;
    QLabel* note_ = nullptr;
};

}  // namespace timewarp::ui
