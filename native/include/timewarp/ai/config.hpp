// SPDX-License-Identifier: MIT
//
// The AI assistant's configuration: where the service is, which model to ask, and
// how the key is supplied.
//
// A value type plus free functions, like `EditorSettings` and `Project`: nothing
// here opens a socket, so a test can read, validate and rewrite a configuration
// without a network or a window. `timewarp::ai::Client` is what sends it.
//
// #### Where the key lives
//
// A key is a secret, and a secret in a settings file is a secret on disk. Three
// things follow, and all three are deliberate:
//
//  * `TIME_WARP_AI_API_KEY` wins over anything stored. A user who does not want the
//    key on disk at all can export it and leave the field empty; nothing is then
//    written.
//  * The **save the key** choice is the user's. Unticking it removes the stored key
//    from the settings and leaves the rest of the configuration alone, so the
//    setting can be handed to another machine without the secret in it.
//  * When a key *is* stored, the settings file is restricted to its owner on the
//    platforms where that means something (`tighten_settings_file`).
//
// Nothing here ever logs, echoes or reports the key: `summary()` says whether one
// is present and where it came from, and `redact()` is what every error message
// goes through before it is shown.

#pragma once

#include <QList>
#include <QString>

#include <optional>

namespace timewarp::ai {

/// Which request/response shape the endpoint speaks.
///
/// Two protocols rather than one because they are the two an old-computer user is
/// likely to have: the OpenAI chat-completions shape (OpenAI itself, Ollama, LM
/// Studio, llama.cpp, vLLM, OpenRouter, Mistral, Groq and most others) and
/// Anthropic's Messages API. Anything that speaks the first needs no provider list,
/// which is why the base URL is a field rather than a dropdown of vendors.
enum class Protocol {
    /// `POST {base}/chat/completions`, `Authorization: Bearer`.
    OpenAi,
    /// `POST {base}/messages`, `x-api-key` and `anthropic-version`.
    Anthropic,
};

/// The settings keys, one each rather than a serialised blob: a settings file a
/// person can read is a settings file a person can fix.
inline constexpr const char* kSettingEnabled = "ai/enabled";
inline constexpr const char* kSettingProtocol = "ai/protocol";
inline constexpr const char* kSettingBaseUrl = "ai/baseUrl";
inline constexpr const char* kSettingModel = "ai/model";
inline constexpr const char* kSettingApiKey = "ai/apiKey";
inline constexpr const char* kSettingStoreKey = "ai/storeKey";
inline constexpr const char* kSettingMaxTokens = "ai/maxTokens";
inline constexpr const char* kSettingTimeoutSeconds = "ai/timeoutSeconds";
inline constexpr const char* kSettingTemperature = "ai/temperature";
inline constexpr const char* kSettingSendDocument = "ai/sendDocument";
inline constexpr const char* kSettingRememberConversation = "ai/rememberConversation";
inline constexpr const char* kSettingInlineSuggestions = "ai/inlineSuggestions";

/// The environment variables that override the stored configuration, documented in
/// `docs/AI.md`. The key's is the one that matters: an exported key is never
/// written anywhere.
inline constexpr const char* kEnvApiKey = "TIME_WARP_AI_API_KEY";
inline constexpr const char* kEnvBaseUrl = "TIME_WARP_AI_BASE_URL";
inline constexpr const char* kEnvModel = "TIME_WARP_AI_MODEL";
inline constexpr const char* kEnvProtocol = "TIME_WARP_AI_PROTOCOL";
inline constexpr const char* kEnvEnabled = "TIME_WARP_AI_ENABLED";

/// One assistant configuration.
struct Config {
    /// Off until the user asks for it: an application that starts talking to a
    /// network service because it was installed is not one this project will ship.
    bool enabled = false;
    Protocol protocol = Protocol::OpenAi;
    /// The whole prefix, version path included, without a trailing slash:
    /// `https://api.openai.com/v1`. The protocol's own path is appended by
    /// `endpoint_url()`.
    QString base_url;
    QString model;
    /// The stored key. `effective_key()` is what a request uses, because an
    /// exported key wins over this.
    QString api_key;
    /// Whether `save()` writes `api_key`. Unset by the user in the settings dialog.
    bool store_key = true;
    /// The reply ceiling, in tokens.
    int max_tokens = 1024;
    int timeout_seconds = 60;
    /// 0 to 2. Sent to the service, which may clamp it.
    double temperature = 0.7;
    /// Whether the current document rides along with a question by default.
    /// The panel has its own tick for a single request; this is the default that
    /// tick starts from.
    bool send_document = true;
    /// Whether the conversation survives a restart. Off by default: what the
    /// assistant was asked contains the program, and a file nobody asked for is a
    /// file nobody looked for. Turning it off deletes the stored conversation.
    bool remember_conversation = false;
    /// Whether the editor shows a suggested next line from the configured endpoint.
    /// Off by default, and deliberately not the same switch as `enabled`: a chat
    /// panel is asked things, a ghost line arrives while you type.
    bool inline_suggestions = false;
};

/// `openai` / `anthropic`: the settings value, which is also what
/// `TIME_WARP_AI_PROTOCOL` accepts.
[[nodiscard]] QString protocol_key(Protocol protocol);
[[nodiscard]] std::optional<Protocol> protocol_from_key(const QString& key);
[[nodiscard]] QString protocol_name(Protocol protocol);

/// The endpoint a protocol's default installation uses, and the model it is
/// usually paired with. Only used to pre-fill a dialog.
[[nodiscard]] QString default_base_url(Protocol protocol);
[[nodiscard]] QString default_model(Protocol protocol);

/// A named place to point the assistant at, for the settings dialog's dropdown.
///
/// A preset is a *shorthand*, not a second configuration: choosing one writes its
/// protocol, base URL and model into the fields the user can then edit, and the
/// stored configuration is still only those three. There is no provider field to
/// fall out of step with the endpoint, so `provider_name()` reads the name back by
/// matching the endpoint against this table -- and says "custom endpoint" when
/// nothing matches.
struct Preset {
    QString name;
    Protocol protocol;
    QString base_url;
    QString model;
    /// A sentence for the settings dialog: what the service is and what it costs
    /// the user in privacy.
    QString note;
};

/// The presets, in the order the dialog lists them. DeepSeek speaks the
/// OpenAI-compatible shape, so it is a preset rather than a protocol: the wire
/// format and the vendor are different questions, and only one of them changes how
/// a request is built.
[[nodiscard]] QList<Preset> presets();

/// The preset whose endpoint this configuration uses, or "custom endpoint".
[[nodiscard]] QString provider_name(const Config& config);

/// Whether `url` is something a request can be sent to: absolute, `http` or
/// `https`. Anything else -- a bare word, a `file:` path -- is refused here rather
/// than at the socket.
[[nodiscard]] bool valid_base_url(const QString& url);

/// `base_url` with the protocol's path appended and no doubled slash.
[[nodiscard]] QString endpoint_url(const Config& config);

/// The key in `TIME_WARP_AI_API_KEY`, or empty.
[[nodiscard]] QString environment_key();

/// Whether the endpoint is on this machine. Both shapes stream, so this is no
/// longer the streaming question: it is the privacy one, and the panel says which
/// answer applies beside the question box. A local server -- Ollama, LM Studio,
/// llama.cpp, vLLM on a laptop -- means nothing leaves the computer.
[[nodiscard]] bool is_local_endpoint(const Config& config);

/// The key a request should use: the environment's when it is set, else the stored
/// one. Empty when neither is.
[[nodiscard]] QString effective_key(const Config& config);

/// Whether there is an endpoint to ask anything at all: switched on, and an address
/// a request can go to. A model list needs this and not a model name, which is why
/// it is separate from `ready` -- discovering the model names is what somebody does
/// *because* they do not have one yet.
[[nodiscard]] bool reachable(const Config& config, QString* why = nullptr);

/// Whether a question can be asked, and if not, a sentence saying what is missing.
/// A key is *not* required: a local server needs none, and demanding one would shut
/// out the one configuration that keeps the document on the machine.
[[nodiscard]] bool ready(const Config& config, QString* why = nullptr);

/// Reads the stored configuration and applies the environment overrides. Never
/// fails: an unreadable settings file yields the defaults, which is "off".
[[nodiscard]] Config load();

/// Writes everything but the key, and the key only when `store_key` is set --
/// removing a previously stored one when the user has since said not to keep it.
void save(const Config& config);

/// Removes the stored key and leaves the rest of the settings alone.
void clear_stored_key();

/// The one-line description the panel header and the settings dialog show:
/// the protocol, the model, and where the key comes from. Never the key.
[[nodiscard]] QString summary(const Config& config);

/// `text` with every occurrence of `key` replaced by `***`. Every error path calls
/// this before a message is shown: a service that echoes part of the request must
/// not be able to put the key in a status bar.
[[nodiscard]] QString redact(QString text, const QString& key);

/// Restricts the settings file to its owner, where the platform has a notion of
/// that. Called after a key is written; a no-op elsewhere.
void tighten_settings_file();

}  // namespace timewarp::ai
