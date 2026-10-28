// SPDX-License-Identifier: MIT

#include "timewarp/ui/ai_settings.hpp"

#include "timewarp/ai/client.hpp"

#include <QCheckBox>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QDoubleSpinBox>
#include <QFormLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QSpinBox>
#include <QUrl>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

using timewarp::ai::Config;
using timewarp::ai::Preset;
using timewarp::ai::Protocol;

/// The last row of the dropdown: the fields as the user typed them, rather than one
/// of the presets.
constexpr auto kCustomPreset = "Custom endpoint";

}  // namespace

AiSettingsDialog::AiSettingsDialog(const Config& config, QWidget* parent)
    : QDialog(parent) {
    setObjectName(QStringLiteral("aiSettingsDialog"));
    setWindowTitle(tr("AI Assistant Settings"));
    setModal(true);
    resize(560, 560);
    test_client_ = new timewarp::ai::Client{this};
    buildUi();
    fillFrom(config);
}

AiSettingsDialog::~AiSettingsDialog() = default;

void AiSettingsDialog::buildUi() {
    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(10);

    enabled_ = new QCheckBox{tr("Use the AI assistant"), this};
    enabled_->setObjectName(QStringLiteral("aiEnabledBox"));
    enabled_->setToolTip(
        tr("Off unless you switch it on. Nothing is sent to a service until you do."));
    layout->addWidget(enabled_);

    auto* service = new QGroupBox{tr("Service"), this};
    auto* form = new QFormLayout{service};

    preset_ = new QComboBox{service};
    preset_->setAccessibleName(tr("Service"));
    preset_->setObjectName(QStringLiteral("aiPresetChooser"));
    preset_->setToolTip(tr("Fills the three fields below. They stay editable."));
    form->addRow(tr("Provider:"), preset_);

    protocol_ = new QComboBox{service};
    protocol_->setAccessibleName(tr("Protocol"));
    protocol_->setObjectName(QStringLiteral("aiProtocolChooser"));
    protocol_->addItem(timewarp::ai::protocol_name(Protocol::OpenAi), QStringLiteral("openai"));
    protocol_->addItem(timewarp::ai::protocol_name(Protocol::Anthropic),
                       QStringLiteral("anthropic"));
    protocol_->setToolTip(tr("The shape of the request. Anthropic's Messages API "
                             "differs from the OpenAI-compatible one every other "
                             "entry here speaks."));
    form->addRow(tr("Protocol:"), protocol_);

    base_url_ = new QLineEdit{service};
    base_url_->setAccessibleName(tr("Endpoint"));
    base_url_->setObjectName(QStringLiteral("aiBaseUrlField"));
    base_url_->setPlaceholderText(QStringLiteral("https://api.deepseek.com/v1"));
    base_url_->setToolTip(tr("The whole prefix, version path included. The protocol's "
                             "own path is appended."));
    form->addRow(tr("Endpoint:"), base_url_);

    model_ = new QLineEdit{service};
    model_->setAccessibleName(tr("Model"));
    model_->setObjectName(QStringLiteral("aiModelField"));
    model_->setPlaceholderText(QStringLiteral("deepseek-chat"));
    auto* model_row = new QHBoxLayout{};
    model_row->setSpacing(6);
    model_row->addWidget(model_, 1);
    find_models_ = new QPushButton{tr("Find models\u2026"), service};
    find_models_->setObjectName(QStringLiteral("aiFindModelsButton"));
    find_models_->setToolTip(tr("Ask the service what it can run, and pick from the "
                                "list rather than typing a name from memory"));
    model_row->addWidget(find_models_);
    form->addRow(tr("Model:"), model_row);

    // The answer to "what can this endpoint run": hidden until there is one, because
    // an empty dropdown is a control that says the service offered nothing when all
    // it means is that nobody has asked yet.
    models_ = new QComboBox{service};
    models_->setAccessibleName(tr("Available models"));
    models_->setObjectName(QStringLiteral("aiModelChooser"));
    models_->setVisible(false);
    form->addRow(QString{}, models_);

    key_ = new QLineEdit{service};
    key_->setAccessibleName(tr("API key"));
    key_->setObjectName(QStringLiteral("aiKeyField"));
    key_->setEchoMode(QLineEdit::Password);
    key_->setPlaceholderText(tr("not needed for a server on this machine"));
    form->addRow(tr("API key:"), key_);

    auto* key_row = new QHBoxLayout{};
    key_row->setSpacing(8);
    show_key_ = new QCheckBox{tr("Show"), service};
    show_key_->setObjectName(QStringLiteral("aiShowKeyBox"));
    store_key_ = new QCheckBox{tr("Keep the key on this machine"), service};
    store_key_->setObjectName(QStringLiteral("aiStoreKeyBox"));
    store_key_->setToolTip(tr("Unticked, the key is used for this session and is "
                              "removed from the settings file."));
    key_row->addWidget(show_key_);
    key_row->addWidget(store_key_);
    key_row->addStretch(1);
    form->addRow(QString{}, key_row);

    note_ = new QLabel{service};
    note_->setObjectName(QStringLiteral("aiPresetNote"));
    note_->setWordWrap(true);
    form->addRow(QString{}, note_);
    layout->addWidget(service);

    auto* limits = new QGroupBox{tr("Limits"), this};
    auto* limits_form = new QFormLayout{limits};
    max_tokens_ = new QSpinBox{limits};
    max_tokens_->setAccessibleName(tr("Reply ceiling"));
    max_tokens_->setObjectName(QStringLiteral("aiMaxTokensSpin"));
    max_tokens_->setRange(16, 32768);
    max_tokens_->setSingleStep(256);
    max_tokens_->setToolTip(tr("The longest reply, in tokens"));
    limits_form->addRow(tr("Reply ceiling:"), max_tokens_);

    temperature_ = new QDoubleSpinBox{limits};
    temperature_->setAccessibleName(tr("Temperature"));
    temperature_->setObjectName(QStringLiteral("aiTemperatureSpin"));
    temperature_->setRange(0.0, 2.0);
    temperature_->setSingleStep(0.1);
    temperature_->setDecimals(1);
    temperature_->setToolTip(tr("0 is repeatable, 2 is inventive. 0.7 is a good start."));
    limits_form->addRow(tr("Temperature:"), temperature_);

    timeout_ = new QSpinBox{limits};
    timeout_->setAccessibleName(tr("Give up after"));
    timeout_->setObjectName(QStringLiteral("aiTimeoutSpin"));
    timeout_->setRange(5, 600);
    timeout_->setSuffix(tr(" s"));
    limits_form->addRow(tr("Give up after:"), timeout_);

    send_document_ = new QCheckBox{tr("Send the current program by default"), limits};
    send_document_->setObjectName(QStringLiteral("aiSendDocumentDefaultBox"));
    limits_form->addRow(QString{}, send_document_);
    layout->addWidget(limits);

    auto* environment = new QLabel{
        tr("The key may also be given as TIME_WARP_AI_API_KEY, which this application "
           "never writes to disk and which wins over the field above. "
           "TIME_WARP_AI_BASE_URL and TIME_WARP_AI_MODEL override the endpoint and "
           "the model in the same way."),
        this};
    environment->setObjectName(QStringLiteral("aiEnvironmentNote"));
    environment->setWordWrap(true);
    layout->addWidget(environment);

    auto* test_row = new QHBoxLayout{};
    test_row->setSpacing(8);
    test_ = new QPushButton{tr("Test connection"), this};
    test_->setObjectName(QStringLiteral("aiTestButton"));
    test_->setToolTip(tr("Send one short question and report what came back"));
    test_status_ = new QLabel{this};
    test_status_->setObjectName(QStringLiteral("aiTestStatus"));
    test_status_->setWordWrap(true);
    test_status_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    test_row->addWidget(test_);
    test_row->addWidget(test_status_, 1);
    layout->addLayout(test_row);
    layout->addStretch(1);

    auto* buttons = new QDialogButtonBox{QDialogButtonBox::Ok | QDialogButtonBox::Cancel,
                                         this};
    buttons->setObjectName(QStringLiteral("aiSettingsButtons"));
    layout->addWidget(buttons);

    // The dropdown's rows: every preset, then the fields as the user left them.
    const QList<Preset> presets = timewarp::ai::presets();
    for (int index = 0; index < presets.size(); ++index) {
        preset_->addItem(presets.at(index).name, presets.at(index).name);
    }
    preset_->addItem(tr("Custom endpoint"), QString::fromLatin1(kCustomPreset));

    connect(preset_, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            &AiSettingsDialog::applyPreset);
    // Editing a field means the user is no longer using the preset they chose:
    // the dropdown says so rather than continuing to claim a provider whose
    // endpoint is no longer in use.
    connect(base_url_, &QLineEdit::textEdited, this, &AiSettingsDialog::syncPresetToEndpoint);
    connect(model_, &QLineEdit::textEdited, this, &AiSettingsDialog::syncPresetToEndpoint);
    connect(protocol_, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            &AiSettingsDialog::syncPresetToEndpoint);
    connect(show_key_, &QCheckBox::toggled, this, [this](bool show) {
        key_->setEchoMode(show ? QLineEdit::Normal : QLineEdit::Password);
    });
    connect(store_key_, &QCheckBox::toggled, this, [this](bool keep) {
        // The field stays usable either way: a key that is not kept still works for
        // this session, which is what a user on a shared machine needs.
        key_->setPlaceholderText(keep ? tr("stored in your settings file")
                                      : tr("used for this session, not saved"));
    });
    connect(test_, &QPushButton::clicked, this, &AiSettingsDialog::testConnection);
    connect(find_models_, &QPushButton::clicked, this, &AiSettingsDialog::findModels);
    connect(models_, QOverload<int>::of(&QComboBox::activated), this, [this](int index) {
        if (index >= 0) {
            model_->setText(models_->itemText(index));
        }
    });
    connect(test_client_, &timewarp::ai::Client::modelsListed, this,
            [this](const QStringList& models) {
                find_models_->setEnabled(true);
                models_->clear();
                models_->addItems(models);
                models_->setVisible(!models.isEmpty());
                if (models.isEmpty()) {
                    test_status_->setText(
                        tr("The service answered but named no models. Type the name "
                           "of one instead."));
                    return;
                }
                test_status_->setText(
                    tr("%1 model(s) available \u2014 pick one above.").arg(models.size()));
                // A model already typed stays: discovery is an offer, not a reset.
                if (model_->text().trimmed().isEmpty()) {
                    model_->setText(models.first());
                }
            });
    connect(test_client_, &timewarp::ai::Client::answered, this,
            [this](const timewarp::ai::Reply& reply) {
                test_->setEnabled(true);
                QString text = reply.text.simplified();
                if (text.size() > 80) {
                    text.truncate(80);
                    text += QStringLiteral("…");
                }
                test_status_->setText(
                    tr("\u2705 %1 answered: %2")
                        .arg(reply.model.isEmpty() ? model_->text().trimmed() : reply.model,
                             text));
            });
    connect(test_client_, &timewarp::ai::Client::failed, this,
            [this](const QString& message) {
                test_->setEnabled(true);
                test_status_->setText(tr("\u274C %1").arg(message));
            });

    connect(buttons, &QDialogButtonBox::accepted, this, [this] {
        collect();
        if (config_.enabled) {
            QString why;
            if (!timewarp::ai::ready(config_, &why)) {
                // Refusing to close is the difference between a setting that is
                // wrong now and one that is discovered wrong at the first question.
                test_status_->setText(tr("\u274C %1").arg(why));
                return;
            }
        }
        emit saved(config_);
        accept();
    });
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);

    // Selecting the row that matches what was loaded must not overwrite it: the
    // fill happens with `adjusting_` set, so the handler knows to stay quiet.
}

void AiSettingsDialog::fillFrom(const Config& config) {
    config_ = config;
    adjusting_ = true;

    enabled_->setChecked(config.enabled);
    const int protocol_index =
        protocol_->findData(timewarp::ai::protocol_key(config.protocol));
    protocol_->setCurrentIndex(protocol_index >= 0 ? protocol_index : 0);
    base_url_->setText(config.base_url);
    model_->setText(config.model);
    key_->setText(config.api_key);
    store_key_->setChecked(config.store_key);
    key_->setPlaceholderText(config.store_key ? tr("stored in your settings file")
                                              : tr("used for this session, not saved"));
    max_tokens_->setValue(config.max_tokens);
    temperature_->setValue(config.temperature);
    timeout_->setValue(config.timeout_seconds);
    send_document_->setChecked(config.send_document);

    const QString name = timewarp::ai::provider_name(config);
    int index = preset_->findData(name);
    if (index < 0) {
        index = preset_->count() - 1;
    }
    preset_->setCurrentIndex(index);
    for (const Preset& preset : timewarp::ai::presets()) {
        if (preset.name == name) {
            note_->setText(preset.note);
            break;
        }
    }
    if (note_->text().isEmpty()) {
        note_->setText(tr("A custom endpoint that speaks one of the two shapes on "
                          "the left."));
    }
    adjusting_ = false;
    test_status_->clear();
    if (endpoint_is_local()) {
        // A server on this machine is the configuration to recommend and the one
        // whose model names nobody remembers: ask it, unasked.
        findModels();
    }
}

Config AiSettingsDialog::collected() const {
    Config config;
    config.enabled = enabled_->isChecked();
    config.protocol =
        timewarp::ai::protocol_from_key(protocol_->currentData().toString())
            .value_or(Protocol::OpenAi);
    config.base_url = base_url_->text().trimmed();
    config.model = model_->text().trimmed();
    config.api_key = key_->text().trimmed();
    config.store_key = store_key_->isChecked();
    config.max_tokens = max_tokens_->value();
    config.temperature = temperature_->value();
    config.timeout_seconds = timeout_->value();
    config.send_document = send_document_->isChecked();
    return config;
}

void AiSettingsDialog::collect() { config_ = collected(); }

void AiSettingsDialog::applyPreset(int index) {
    if (adjusting_ || index < 0) {
        return;
    }
    const QList<Preset> presets = timewarp::ai::presets();
    if (index >= presets.size()) {
        note_->setText(tr("A custom endpoint that speaks one of the two shapes on "
                          "the left."));
        return;
    }
    const Preset& preset = presets.at(index);
    adjusting_ = true;
    const int protocol_index =
        protocol_->findData(timewarp::ai::protocol_key(preset.protocol));
    protocol_->setCurrentIndex(protocol_index >= 0 ? protocol_index : 0);
    base_url_->setText(preset.base_url);
    model_->setText(preset.model);
    adjusting_ = false;
    note_->setText(preset.note);
}

QString AiSettingsDialog::selectedPresetName() const {
    return preset_->currentData().toString();
}

void AiSettingsDialog::syncPresetToEndpoint() {
    if (adjusting_) {
        return;
    }
    Config probe;
    probe.protocol = timewarp::ai::protocol_from_key(protocol_->currentData().toString())
                         .value_or(Protocol::OpenAi);
    probe.base_url = base_url_->text().trimmed();
    const QString name = timewarp::ai::provider_name(probe);
    const int index = preset_->findData(name);
    adjusting_ = true;
    preset_->setCurrentIndex(index >= 0 ? index : preset_->count() - 1);
    adjusting_ = false;
}

void AiSettingsDialog::testConnection() {
    collect();
    // A test is an explicit act, like pressing Send: the checkbox being clear is
    // answered by "you asked me to try it".
    Config probe = config_;
    probe.enabled = true;
    // A connection test should cost a line, not a page.
    probe.max_tokens = 32;

    QString why;
    if (!timewarp::ai::ready(probe, &why)) {
        test_status_->setText(tr("\u274C %1").arg(why));
        return;
    }
    test_->setEnabled(false);
    test_status_->setText(tr("Asking %1…").arg(probe.model));

    timewarp::ai::Conversation conversation;
    conversation.system = QStringLiteral("You are a connectivity test. Be very brief.");
    conversation.messages.append(timewarp::ai::Message{
        QStringLiteral("user"), QStringLiteral("Reply with the single word: ready.")});
    test_client_->send(probe, conversation);
}

bool AiSettingsDialog::endpoint_is_local() const {
    const QUrl url{base_url_->text().trimmed()};
    const QString host = url.host().toLower();
    return host == QStringLiteral("localhost") || host == QStringLiteral("127.0.0.1") ||
           host == QStringLiteral("::1");
}

void AiSettingsDialog::findModels() {
    collect();
    timewarp::ai::Config probe = config_;
    // Asking what is available is an explicit act, like pressing Test.
    probe.enabled = true;
    QString why;
    // Not `ready`: a model name is what this call exists to find.
    if (!timewarp::ai::reachable(probe, &why)) {
        test_status_->setText(tr("\u274C %1").arg(why));
        return;
    }
    find_models_->setEnabled(false);
    test_status_->setText(tr("Asking %1 for its models\u2026").arg(probe.base_url));
    test_client_->listModels(probe);
}

Config AiSettingsDialog::config() const { return collected(); }

}  // namespace timewarp::ui
