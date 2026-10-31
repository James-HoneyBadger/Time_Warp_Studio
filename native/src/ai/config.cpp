// SPDX-License-Identifier: MIT

#include "timewarp/ai/config.hpp"

#include "timewarp/ai/keyring.hpp"

#include <QFile>
#include <QFileDevice>
#include <QSettings>
#include <QUrl>

#include <algorithm>
#include <cmath>

namespace timewarp::ai {

namespace {

/// The values a settings file, an environment variable or a spin box can carry.
/// Clamped on the way in *and* on the way out, so a hand-edited file cannot ask
/// for a ten-token reply or a ten-minute wait and have it honoured.
constexpr int kMinimumMaxTokens = 16;
constexpr int kMaximumMaxTokens = 32768;
constexpr int kMinimumTimeoutSeconds = 5;
constexpr int kMaximumTimeoutSeconds = 600;
constexpr double kMinimumTemperature = 0.0;
constexpr double kMaximumTemperature = 2.0;

[[nodiscard]] int clamped_tokens(int value) {
    return std::clamp(value, kMinimumMaxTokens, kMaximumMaxTokens);
}

[[nodiscard]] int clamped_timeout(int value) {
    return std::clamp(value, kMinimumTimeoutSeconds, kMaximumTimeoutSeconds);
}

[[nodiscard]] double clamped_temperature(double value) {
    if (!std::isfinite(value)) {
        return 0.7;
    }
    return std::clamp(value, kMinimumTemperature, kMaximumTemperature);
}

/// `trimmed`, or empty when the variable is unset or blank. An environment
/// variable set to nothing is how a shell script clears one, so it means "unset"
/// here rather than "override with an empty string".
[[nodiscard]] QString variable(const char* name) {
    const QString value = qEnvironmentVariable(name).trimmed();
    return value;
}

/// An optional boolean environment variable: unset means "no opinion".
[[nodiscard]] std::optional<bool> boolean_variable(const char* name) {
    const QString value = variable(name).toLower();
    if (value.isEmpty()) {
        return std::nullopt;
    }
    if (value == QStringLiteral("1") || value == QStringLiteral("true") ||
        value == QStringLiteral("yes") || value == QStringLiteral("on")) {
        return true;
    }
    if (value == QStringLiteral("0") || value == QStringLiteral("false") ||
        value == QStringLiteral("no") || value == QStringLiteral("off")) {
        return false;
    }
    return std::nullopt;
}

/// The path with its trailing slashes removed, so a person who pasted
/// `https://host/v1/` does not get `https://host/v1//messages`.
[[nodiscard]] QString without_trailing_slash(const QString& url) {
    QString trimmed = url.trimmed();
    while (trimmed.endsWith(QLatin1Char('/'))) {
        trimmed.chop(1);
    }
    return trimmed;
}

[[nodiscard]] QString key_source(const Config& config) {
    if (!environment_key().isEmpty()) {
        return QStringLiteral("key from the environment");
    }
    if (!config.api_key.isEmpty()) {
        return QStringLiteral("key saved");
    }
    return QStringLiteral("no key — a local server needs none");
}

}  // namespace

QString protocol_key(Protocol protocol) {
    switch (protocol) {
        case Protocol::OpenAi:
            return QStringLiteral("openai");
        case Protocol::Anthropic:
            return QStringLiteral("anthropic");
    }
    return QStringLiteral("openai");
}

std::optional<Protocol> protocol_from_key(const QString& key) {
    const QString wanted = key.trimmed().toLower();
    if (wanted == QStringLiteral("openai") || wanted == QStringLiteral("openai-compatible") ||
        wanted == QStringLiteral("chat-completions")) {
        return Protocol::OpenAi;
    }
    if (wanted == QStringLiteral("anthropic") || wanted == QStringLiteral("claude") ||
        wanted == QStringLiteral("messages")) {
        return Protocol::Anthropic;
    }
    return std::nullopt;
}

QString protocol_name(Protocol protocol) {
    switch (protocol) {
        case Protocol::OpenAi:
            return QStringLiteral("OpenAI-compatible");
        case Protocol::Anthropic:
            return QStringLiteral("Anthropic");
    }
    return QStringLiteral("OpenAI-compatible");
}

QString default_base_url(Protocol protocol) {
    // The first preset that speaks this protocol: one table, so a default cannot
    // disagree with what the dialog's dropdown offers.
    for (const Preset& preset : presets()) {
        if (preset.protocol == protocol) {
            return preset.base_url;
        }
    }
    return QStringLiteral("https://api.openai.com/v1");
}

QString default_model(Protocol protocol) {
    for (const Preset& preset : presets()) {
        if (preset.protocol == protocol) {
            return preset.model;
        }
    }
    return QStringLiteral("gpt-4o-mini");
}

bool valid_base_url(const QString& url) {
    const QString trimmed = without_trailing_slash(url);
    if (trimmed.isEmpty()) {
        return false;
    }
    const QUrl parsed{trimmed, QUrl::StrictMode};
    if (!parsed.isValid() || parsed.host().isEmpty()) {
        return false;
    }
    const QString scheme = parsed.scheme().toLower();
    return scheme == QStringLiteral("http") || scheme == QStringLiteral("https");
}

QString endpoint_url(const Config& config) {
    const QString base = without_trailing_slash(config.base_url);
    const QString path = config.protocol == Protocol::Anthropic ? QStringLiteral("/messages")
                                                                : QStringLiteral("/chat/completions");
    // A base URL that already names the path is used as it stands. That is the
    // case for a provider whose documentation hands out the whole endpoint, and
    // appending the path again is the mistake that produces a 404 nobody can
    // explain.
    if (base.endsWith(path)) {
        return base;
    }
    return base + path;
}

QString environment_key() { return variable(kEnvApiKey); }

bool is_local_endpoint(const Config& config) {
    const QUrl url{config.base_url.trimmed()};
    const QString host = url.host().toLower();
    return host == QLatin1String("localhost") || host == QLatin1String("127.0.0.1") ||
           host == QLatin1String("::1") || host.endsWith(QLatin1String(".localhost"));
}

QString effective_key(const Config& config) {
    const QString from_environment = environment_key();
    return from_environment.isEmpty() ? config.api_key.trimmed() : from_environment;
}

bool reachable(const Config& config, QString* why) {
    const auto fail = [why](const QString& reason) {
        if (why != nullptr) {
            *why = reason;
        }
        return false;
    };
    if (!config.enabled) {
        return fail(QStringLiteral("the assistant is switched off"));
    }
    if (config.base_url.trimmed().isEmpty()) {
        return fail(QStringLiteral("no endpoint is configured"));
    }
    if (!valid_base_url(config.base_url)) {
        return fail(QStringLiteral("the endpoint must be an http or https address"));
    }
    if (why != nullptr) {
        why->clear();
    }
    return true;
}

bool ready(const Config& config, QString* why) {
    if (!reachable(config, why)) {
        return false;
    }
    if (config.model.trimmed().isEmpty()) {
        if (why != nullptr) {
            *why = QStringLiteral("no model is named");
        }
        return false;
    }
    return true;
}

Config load() {
    QSettings settings;
    Config config;
    config.enabled = settings.value(QLatin1String{kSettingEnabled}, false).toBool();
    config.protocol =
        protocol_from_key(settings.value(QLatin1String{kSettingProtocol}).toString())
            .value_or(Protocol::OpenAi);
    config.base_url = settings.value(QLatin1String{kSettingBaseUrl}).toString().trimmed();
    config.model = settings.value(QLatin1String{kSettingModel}).toString().trimmed();
    config.api_key = settings.value(QLatin1String{kSettingApiKey}).toString().trimmed();
    if (available_keyring() == KeyRing::System) {
        // The system store is where a key saved on a platform that has one actually
        // lives; the file is the fallback for a machine that has none, or for one
        // whose store refused it.
        const QString kept = keyring_read(QString::fromLatin1(kKeyRingAccount));
        if (!kept.isEmpty()) {
            config.api_key = kept;
        }
    }
    config.store_key = settings.value(QLatin1String{kSettingStoreKey}, true).toBool();
    config.max_tokens = clamped_tokens(settings.value(QLatin1String{kSettingMaxTokens}, 1024).toInt());
    config.timeout_seconds =
        clamped_timeout(settings.value(QLatin1String{kSettingTimeoutSeconds}, 60).toInt());
    config.temperature = clamped_temperature(
        settings.value(QLatin1String{kSettingTemperature}, 0.7).toDouble());
    config.send_document = settings.value(QLatin1String{kSettingSendDocument}, true).toBool();
    config.remember_conversation =
        settings.value(QLatin1String{kSettingRememberConversation}, false).toBool();
    config.inline_suggestions =
        settings.value(QLatin1String{kSettingInlineSuggestions}, false).toBool();
    config.persona = persona_from_key(settings.value(QLatin1String{kSettingPersona}).toString())
                         .value_or(Persona::Plain);

    // The environment's word is final. It is how a session supplies a key it does
    // not want written down, and how a test configures an endpoint without
    // touching the user's settings file.
    if (const std::optional<bool> enabled = boolean_variable(kEnvEnabled)) {
        config.enabled = *enabled;
    }
    if (const QString protocol = variable(kEnvProtocol); !protocol.isEmpty()) {
        if (const std::optional<Protocol> parsed = protocol_from_key(protocol)) {
            config.protocol = *parsed;
        }
    }
    if (const QString url = variable(kEnvBaseUrl); !url.isEmpty()) {
        config.base_url = url;
    }
    if (const QString model = variable(kEnvModel); !model.isEmpty()) {
        config.model = model;
    }
    if (const QString persona = variable(kEnvPersona); !persona.isEmpty()) {
        if (const std::optional<Persona> parsed = persona_from_key(persona)) {
            config.persona = *parsed;
        }
    }
    return config;
}

void save(const Config& config) {
    QSettings settings;
    settings.setValue(QLatin1String{kSettingEnabled}, config.enabled);
    settings.setValue(QLatin1String{kSettingProtocol}, protocol_key(config.protocol));
    settings.setValue(QLatin1String{kSettingBaseUrl}, without_trailing_slash(config.base_url));
    settings.setValue(QLatin1String{kSettingModel}, config.model.trimmed());
    settings.setValue(QLatin1String{kSettingStoreKey}, config.store_key);
    settings.setValue(QLatin1String{kSettingMaxTokens}, clamped_tokens(config.max_tokens));
    settings.setValue(QLatin1String{kSettingTimeoutSeconds},
                      clamped_timeout(config.timeout_seconds));
    settings.setValue(QLatin1String{kSettingTemperature},
                      clamped_temperature(config.temperature));
    settings.setValue(QLatin1String{kSettingSendDocument}, config.send_document);
    settings.setValue(QLatin1String{kSettingRememberConversation},
                      config.remember_conversation);
    settings.setValue(QLatin1String{kSettingInlineSuggestions}, config.inline_suggestions);
    settings.setValue(QLatin1String{kSettingPersona}, persona_key(config.persona));
    const QString key = config.api_key.trimmed();
    const bool keeping = config.store_key && !key.isEmpty();
    bool in_the_system_store = false;
    if (available_keyring() == KeyRing::System) {
        if (keeping) {
            in_the_system_store =
                keyring_write(QString::fromLatin1(kKeyRingAccount), key);
        } else {
            // "Do not keep the key" has to mean *remove the one that is there*, or
            // unticking the box would leave the secret where the dialogue says it
            // is not.
            keyring_erase(QString::fromLatin1(kKeyRingAccount));
        }
    }
    if (keeping && !in_the_system_store) {
        settings.setValue(QLatin1String{kSettingApiKey}, key);
    } else {
        // Either the platform store has it, or it is not being kept: a copy in the
        // settings file would be the secret this dialog promised to take away.
        settings.remove(QLatin1String{kSettingApiKey});
    }
    settings.sync();
    if (keeping && !in_the_system_store) {
        tighten_settings_file();
    }
}

void clear_stored_key() {
    keyring_erase(QString::fromLatin1(kKeyRingAccount));
    QSettings settings;
    settings.remove(QLatin1String{kSettingApiKey});
    settings.sync();
}

QList<Preset> presets() {
    // The endpoints are written as the version path only -- no `/chat/completions`
    // and no `/messages` -- because `endpoint_url()` appends that from the
    // protocol, and a preset that named the whole path would make the two
    // disagree.
    return {
        {QStringLiteral("OpenAI"), Protocol::OpenAi,
         QStringLiteral("https://api.openai.com/v1"), QStringLiteral("gpt-4o-mini"),
         QStringLiteral("OpenAI's own service. The program you ask about is sent to it.")},
        {QStringLiteral("DeepSeek"), Protocol::OpenAi,
         QStringLiteral("https://api.deepseek.com/v1"), QStringLiteral("deepseek-chat"),
         QStringLiteral("DeepSeek speaks the OpenAI-compatible shape, so it needs no "
                        "protocol of its own. The program is sent to api.deepseek.com.")},
        {QStringLiteral("Anthropic"), Protocol::Anthropic,
         QStringLiteral("https://api.anthropic.com/v1"),
         QStringLiteral("claude-3-5-haiku-latest"),
         QStringLiteral("Anthropic's Messages API, which is a different request shape "
                        "from the other entries here.")},
        {QStringLiteral("Ollama (this machine)"), Protocol::OpenAi,
         QStringLiteral("http://localhost:11434/v1"), QStringLiteral("llama3.2"),
         QStringLiteral("A local Ollama server. Nothing leaves this machine.")},
        {QStringLiteral("LM Studio (this machine)"), Protocol::OpenAi,
         QStringLiteral("http://localhost:1234/v1"), QStringLiteral("local-model"),
         QStringLiteral("A local LM Studio server. Nothing leaves this machine.")},
        {QStringLiteral("OpenRouter"), Protocol::OpenAi,
         QStringLiteral("https://openrouter.ai/api/v1"), QStringLiteral("openrouter/auto"),
         QStringLiteral("One key for many vendors' models. The program is sent to "
                        "openrouter.ai, which forwards it to the model you chose.")},
    };
}

QString provider_name(const Config& config) {
    const QString base = without_trailing_slash(config.base_url);
    for (const Preset& preset : presets()) {
        if (preset.protocol == config.protocol &&
            without_trailing_slash(preset.base_url).compare(base, Qt::CaseInsensitive) == 0) {
            return preset.name;
        }
    }
    return QStringLiteral("custom endpoint");
}

QString summary(const Config& config) {
    if (!config.enabled) {
        return QStringLiteral("assistant off");
    }
    const QString model = config.model.trimmed().isEmpty() ? QStringLiteral("no model")
                                                           : config.model.trimmed();
    return QStringLiteral("%1 · %2 · %3")
        .arg(provider_name(config), model, key_source(config));
}

QString redact(QString text, const QString& key) {
    const QString secret = key.trimmed();
    if (secret.isEmpty()) {
        return text;
    }
    text.replace(secret, QStringLiteral("***"));
    return text;
}

void tighten_settings_file() {
    QSettings settings;
    const QString path = settings.fileName();
    if (path.isEmpty() || !QFile::exists(path)) {
        return;
    }
    // Best effort: some filesystems have no permissions to set (a FAT card, a
    // mounted share), and failing to restrict them is not a reason to lose the
    // setting the user just saved.
    QFile::setPermissions(path, QFileDevice::ReadOwner | QFileDevice::WriteOwner);
}

}  // namespace timewarp::ai
