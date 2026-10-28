// SPDX-License-Identifier: MIT
//
// The AI assistant: configuration, prompts, and the exchange.
//
// Nothing here touches a network. The configuration and the prompt are pure
// functions, and the client is given a transport of the test's own, so a suite that
// must run on a build machine with no internet still exercises every branch a real
// endpoint would take: a 200 with an OpenAI envelope, a 200 with an Anthropic one, a
// 401 with an error object, a body that is not JSON, a host that cannot be found,
// and a cancellation that arrives after the button was pressed.
//
// What is *not* covered here on purpose: what a real model says. That is not a
// property of this program, and a test that asserted it would be a test of somebody
// else's service.

#include <cstdio>
#include <cstring>

#include <QByteArray>
#include <QCoreApplication>
#include <QEventLoop>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QSettings>
#include <QString>
#include <QStringList>
#include <QTemporaryDir>
#include <QTimer>
#include <QUrl>

#include <algorithm>
#include <utility>

#include "check.hpp"
#include "timewarp/ai/client.hpp"
#include "timewarp/ai/config.hpp"
#include "timewarp/ai/prompt.hpp"
#include "timewarp/lang/registry.hpp"

namespace {

using timewarp::ai::Config;
using timewarp::ai::Context;
using timewarp::ai::Conversation;
using timewarp::ai::Message;
using timewarp::ai::Protocol;
using timewarp::ai::Reply;
using timewarp::ai::Task;

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

/// Runs the event loop until every pending zero-delay timer has fired. The fake
/// transport finishes its reply on one, so one round of the loop is one answer.
void settle() {
    QEventLoop loop;
    QTimer::singleShot(10, &loop, &QEventLoop::quit);
    loop.exec();
}

/// A reply the test decides the outcome of: a status, a body, and optionally a
/// transport error.
class FakeReply final : public QNetworkReply {
public:
    FakeReply(QNetworkAccessManager::Operation operation, const QNetworkRequest& request,
              QObject* parent)
        : QNetworkReply(parent) {
        setOperation(operation);
        setRequest(request);
        setUrl(request.url());
        open(QIODevice::ReadOnly);
    }

    void set_status(int status) {
        setAttribute(QNetworkRequest::HttpStatusCodeAttribute, status);
    }
    void set_body(QByteArray body) { body_ = std::move(body); }
    void set_failure(int error, const QString& text) {
        setError(static_cast<NetworkError>(error), text);
    }

    void finish() {
        setFinished(true);
        emit finished();
    }

    void abort() override {
        if (isFinished()) {
            return;
        }
        setError(OperationCanceledError, QStringLiteral("cancelled"));
        setFinished(true);
        emit finished();
    }

    [[nodiscard]] qint64 bytesAvailable() const override {
        return static_cast<qint64>(body_.size()) - offset_ + QIODevice::bytesAvailable();
    }

protected:
    qint64 readData(char* data, qint64 maximum) override {
        if (offset_ >= body_.size()) {
            return -1;
        }
        const qint64 remaining = static_cast<qint64>(body_.size()) - offset_;
        const qint64 count = std::min(maximum, remaining);
        std::memcpy(data, body_.constData() + offset_, static_cast<std::size_t>(count));
        offset_ += count;
        return count;
    }

private:
    QByteArray body_;
    qint64 offset_ = 0;
};

/// A transport that answers with whatever the test set, and records what was sent.
class FakeNetwork final : public QNetworkAccessManager {
public:
    QNetworkRequest request;
    QNetworkAccessManager::Operation last_operation =
        QNetworkAccessManager::UnknownOperation;
    QByteArray payload;
    int status = 200;
    QByteArray body;
    int network_error = 0;
    QString network_error_text;
    int posts = 0;

    void respond(QByteArray json, int http_status = 200) {
        body = std::move(json);
        status = http_status;
    }

    /// A transport failure: no HTTP status, because the request never got an
    /// answer. `keep_status` models the other case -- Qt flags an HTTP error (a 401
    /// is `AuthenticationRequiredError`) *and* reports the status.
    void fail(int error, bool keep_status = false) {
        network_error = error;
        if (!keep_status) {
            status = 0;
        }
    }

protected:
    QNetworkReply* createRequest(Operation operation, const QNetworkRequest& incoming,
                                 QIODevice* outgoing) override {
        request = incoming;
        last_operation = operation;
        payload = outgoing != nullptr ? outgoing->readAll() : QByteArray{};
        ++posts;
        auto* reply = new FakeReply{operation, incoming, this};
        // Qt's own behaviour: a transport failure has no HTTP status at all, while
        // an HTTP error arrives as both a status *and* a network error (a 401 is
        // `AuthenticationRequiredError`). The client must prefer the status, and
        // this fake is faithful about the difference so the suite can hold it to
        // that.
        reply->set_body(body);
        if (network_error != 0) {
            reply->set_failure(network_error, network_error_text);
        }
        if (status > 0) {
            reply->set_status(status);
        }
        QTimer::singleShot(0, reply, [reply] { reply->finish(); });
        return reply;
    }
};

/// A configuration that is usable without a network or a key.
Config example_config() {
    Config config;
    config.enabled = true;
    config.base_url = QStringLiteral("https://api.deepseek.com/v1");
    config.model = QStringLiteral("deepseek-chat");
    return config;
}

// -- configuration ---------------------------------------------------------

void test_base_url_validation() {
    announce("config: the endpoint is an http address");
    TW_CHECK(timewarp::ai::valid_base_url(QStringLiteral("https://api.openai.com/v1")));
    TW_CHECK(timewarp::ai::valid_base_url(QStringLiteral("http://localhost:11434/v1")));
    TW_CHECK(timewarp::ai::valid_base_url(QStringLiteral("https://api.deepseek.com/v1/")));
    // Anything that is not a place a request can go is refused before the socket,
    // so the settings dialog can say so while the user is typing.
    TW_CHECK(!timewarp::ai::valid_base_url(QString{}));
    TW_CHECK(!timewarp::ai::valid_base_url(QStringLiteral("api.openai.com")));
    TW_CHECK(!timewarp::ai::valid_base_url(QStringLiteral("file:///etc/passwd")));
    TW_CHECK(!timewarp::ai::valid_base_url(QStringLiteral("https://")));
}

void test_endpoint_assembly() {
    announce("config: endpoint");
    Config openai = example_config();
    TW_CHECK_EQ(timewarp::ai::endpoint_url(openai),
                QStringLiteral("https://api.deepseek.com/v1/chat/completions"));

    Config anthropic = example_config();
    anthropic.protocol = Protocol::Anthropic;
    anthropic.base_url = QStringLiteral("https://api.anthropic.com/v1/");
    TW_CHECK_EQ(timewarp::ai::endpoint_url(anthropic),
                QStringLiteral("https://api.anthropic.com/v1/messages"));

    // A base URL that already names the path is used as written: appending it again
    // is the 404 nobody can explain.
    Config whole = example_config();
    whole.base_url = QStringLiteral("https://example.test/v1/chat/completions");
    TW_CHECK_EQ(timewarp::ai::endpoint_url(whole),
                QStringLiteral("https://example.test/v1/chat/completions"));
}

void test_protocol_keys() {
    announce("config: protocol names");
    TW_CHECK_EQ(timewarp::ai::protocol_key(Protocol::OpenAi), QStringLiteral("openai"));
    TW_CHECK_EQ(timewarp::ai::protocol_key(Protocol::Anthropic), QStringLiteral("anthropic"));
    TW_CHECK(timewarp::ai::protocol_from_key(QStringLiteral("OpenAI")) == Protocol::OpenAi);
    TW_CHECK(timewarp::ai::protocol_from_key(QStringLiteral("anthropic")) ==
             Protocol::Anthropic);
    TW_CHECK(timewarp::ai::protocol_from_key(QStringLiteral("claude")) ==
             Protocol::Anthropic);
    TW_CHECK(!timewarp::ai::protocol_from_key(QStringLiteral("smoke-signals")).has_value());
    // Whatever a settings file holds, an unknown protocol reads as the one that
    // most endpoints speak rather than as an error that stops the dialog opening.
    TW_CHECK(timewarp::ai::presets().size() >= 4);
}

void test_presets_include_deepseek() {
    announce("config: DeepSeek is a preset");
    const QList<timewarp::ai::Preset> presets = timewarp::ai::presets();
    const timewarp::ai::Preset* deepseek = nullptr;
    for (const timewarp::ai::Preset& preset : presets) {
        // A preset that names an unreachable address, or the whole protocol path,
        // would fail only when a user clicked Test. Both are assertable here.
        TW_CHECK(timewarp::ai::valid_base_url(preset.base_url));
        TW_CHECK(!preset.model.isEmpty());
        if (preset.name == QStringLiteral("DeepSeek")) {
            deepseek = &preset;
        }
    }
    TW_CHECK(deepseek != nullptr);
    if (deepseek != nullptr) {
        // DeepSeek is a *provider*, not a wire format: it speaks the
        // OpenAI-compatible shape, and that is why the enum did not grow.
        TW_CHECK(deepseek->protocol == Protocol::OpenAi);
        TW_CHECK_EQ(deepseek->model, QStringLiteral("deepseek-chat"));
        TW_CHECK_EQ(deepseek->base_url, QStringLiteral("https://api.deepseek.com/v1"));
    }

    Config config = example_config();
    TW_CHECK_EQ(timewarp::ai::provider_name(config), QStringLiteral("DeepSeek"));
    TW_CHECK(timewarp::ai::summary(config).contains(QStringLiteral("DeepSeek")));
    config.base_url = QStringLiteral("https://example.test/v1");
    TW_CHECK_EQ(timewarp::ai::provider_name(config), QStringLiteral("custom endpoint"));

    // Every protocol the enum has must be reachable from the dropdown, or the
    // dialog would offer a protocol no preset can produce.
    for (const Protocol protocol : {Protocol::OpenAi, Protocol::Anthropic}) {
        TW_CHECK(!timewarp::ai::default_base_url(protocol).isEmpty());
        TW_CHECK(!timewarp::ai::default_model(protocol).isEmpty());
    }
}

void test_readiness() {
    announce("config: what a request needs");
    QString why;
    Config config = example_config();
    TW_CHECK(timewarp::ai::ready(config, &why));
    TW_CHECK(why.isEmpty());

    Config off = example_config();
    off.enabled = false;
    TW_CHECK(!timewarp::ai::ready(off, &why));
    TW_CHECK(why.contains(QStringLiteral("switched off")));

    Config no_model = example_config();
    no_model.model.clear();
    TW_CHECK(!timewarp::ai::ready(no_model, &why));
    TW_CHECK(why.contains(QStringLiteral("model")));

    Config bad_url = example_config();
    bad_url.base_url = QStringLiteral("api.deepseek.com");
    TW_CHECK(!timewarp::ai::ready(bad_url, &why));
    TW_CHECK(why.contains(QStringLiteral("http")));

    // A local server needs no key, and demanding one would shut out the one
    // configuration that keeps the program on the machine.
    Config local = example_config();
    local.base_url = QStringLiteral("http://localhost:11434/v1");
    TW_CHECK(timewarp::ai::ready(local, &why));
}

void test_saved_configuration_round_trip() {
    announce("config: the settings file");
    QSettings{}.clear();

    Config config = example_config();
    config.api_key = QStringLiteral("sk-test-123456");
    config.max_tokens = 2048;
    config.timeout_seconds = 30;
    config.temperature = 0.2;
    config.send_document = false;
    timewarp::ai::save(config);

    const Config loaded = timewarp::ai::load();
    TW_CHECK(loaded.enabled);
    TW_CHECK_EQ(loaded.base_url, QStringLiteral("https://api.deepseek.com/v1"));
    TW_CHECK_EQ(loaded.model, QStringLiteral("deepseek-chat"));
    TW_CHECK_EQ(loaded.api_key, QStringLiteral("sk-test-123456"));
    TW_CHECK_EQ(loaded.max_tokens, 2048);
    TW_CHECK_EQ(loaded.timeout_seconds, 30);
    TW_CHECK(loaded.temperature > 0.19 && loaded.temperature < 0.21);
    TW_CHECK(!loaded.send_document);

    // "Do not keep the key" means remove the one that is there: unticking the box
    // must not leave the secret on disk with the dialogue saying otherwise.
    Config forgetful = loaded;
    forgetful.store_key = false;
    timewarp::ai::save(forgetful);
    TW_CHECK(timewarp::ai::load().api_key.isEmpty());
    // ... and the rest of the configuration survives that.
    TW_CHECK_EQ(timewarp::ai::load().model, QStringLiteral("deepseek-chat"));

    timewarp::ai::save(config);
    timewarp::ai::clear_stored_key();
    TW_CHECK(timewarp::ai::load().api_key.isEmpty());
    TW_CHECK_EQ(timewarp::ai::load().model, QStringLiteral("deepseek-chat"));

    // Nonsense in the file is clamped rather than honoured.
    QSettings settings;
    settings.setValue(QLatin1String{timewarp::ai::kSettingMaxTokens}, 999999);
    settings.setValue(QLatin1String{timewarp::ai::kSettingTimeoutSeconds}, 1);
    settings.setValue(QLatin1String{timewarp::ai::kSettingTemperature}, 9.5);
    settings.sync();
    const Config clamped = timewarp::ai::load();
    TW_CHECK(clamped.max_tokens <= 32768);
    TW_CHECK(clamped.timeout_seconds >= 5);
    TW_CHECK(clamped.temperature <= 2.0);
    QSettings{}.clear();
    QSettings{}.sync();
}

void test_environment_wins() {
    announce("config: the environment wins");
    const QByteArray had_key = qgetenv(timewarp::ai::kEnvApiKey);
    const QByteArray had_url = qgetenv(timewarp::ai::kEnvBaseUrl);
    const QByteArray had_model = qgetenv(timewarp::ai::kEnvModel);

    QSettings{}.clear();
    Config config = example_config();
    config.api_key = QStringLiteral("stored-key");
    timewarp::ai::save(config);
    qputenv(timewarp::ai::kEnvApiKey, "exported-key");
    qputenv(timewarp::ai::kEnvBaseUrl, "http://localhost:1234/v1");
    qputenv(timewarp::ai::kEnvModel, "test-model");
    TW_CHECK_EQ(timewarp::ai::effective_key(config), QStringLiteral("exported-key"));
    const Config from_environment = timewarp::ai::load();
    TW_CHECK_EQ(from_environment.base_url, QStringLiteral("http://localhost:1234/v1"));
    TW_CHECK_EQ(from_environment.model, QStringLiteral("test-model"));
    // The stored key is *not* copied into the loaded configuration: `save()` of the
    // loaded record must not write the exported key to disk.
    TW_CHECK_EQ(from_environment.api_key, QStringLiteral("stored-key"));

    config.api_key.clear();
    TW_CHECK_EQ(timewarp::ai::effective_key(config), QStringLiteral("exported-key"));

    QSettings{}.clear();
    const auto restore = [&](const char* name, const QByteArray& value) {
        if (value.isEmpty()) {
            qunsetenv(name);
        } else {
            qputenv(name, value);
        }
    };
    restore(timewarp::ai::kEnvApiKey, had_key);
    restore(timewarp::ai::kEnvBaseUrl, had_url);
    restore(timewarp::ai::kEnvModel, had_model);
}

void test_redaction() {
    announce("config: a key cannot reach a message");
    const QString key = QStringLiteral("sk-secret-value");
    TW_CHECK_EQ(timewarp::ai::redact(QStringLiteral("rejected sk-secret-value here"), key),
                QStringLiteral("rejected *** here"));
    // No key, nothing to strip, and no accidental corruption of the message.
    TW_CHECK_EQ(timewarp::ai::redact(QStringLiteral("nothing to hide"), QString{}),
                QStringLiteral("nothing to hide"));
    // Every occurrence, not the first.
    TW_CHECK_EQ(timewarp::ai::redact(QStringLiteral("a b a b"), QStringLiteral("a b")),
                QStringLiteral("*** ***"));
    // The summary says a key exists without saying what it is.
    Config config = example_config();
    config.api_key = key;
    TW_CHECK(!timewarp::ai::summary(config).contains(key));
    TW_CHECK(timewarp::ai::summary(config).contains(QStringLiteral("key saved")));
}

// -- prompts ---------------------------------------------------------------

void test_system_prompt_names_the_languages() {
    announce("prompt: the assistant is told what this IDE runs");
    const QString system = timewarp::ai::system_prompt();
    // Read from the language table rather than a second list: a twelfth language
    // must fail here, where somebody can fix the prompt, rather than puzzling a
    // learner whose model answers in the wrong one.
    for (const timewarp::lang::LanguageRow& row : timewarp::lang::kLanguageTable) {
        const QString display =
            QString::fromUtf8(row.display.data(), static_cast<int>(row.display.size()));
        TW_CHECK(system.contains(display));
        TW_CHECK(system.contains(QString::fromLatin1(row.key.data(),
                                                     static_cast<int>(row.key.size()))));
    }
    TW_CHECK(system.contains(QStringLiteral("Turtle")));
    TW_CHECK(system.contains(QStringLiteral("REPEAT")));
    TW_CHECK(system.contains(QStringLiteral("fenced block")));
}

void test_prompt_explain_carries_the_program() {
    announce("prompt: explain sends the program once");
    Context context;
    context.language = QStringLiteral("Logo");
    context.file_name = QStringLiteral("mandala.logo");
    context.source = QStringLiteral("REPEAT 8 [FD 40 RT 45]\n");
    const Conversation conversation =
        timewarp::ai::build_prompt(Task::Explain, context, QString{});
    TW_CHECK_EQ(conversation.system, timewarp::ai::system_prompt());
    TW_CHECK_EQ(conversation.messages.size(), 1);
    const QString text = conversation.messages.at(0).content;
    TW_CHECK_EQ(conversation.messages.at(0).role, QStringLiteral("user"));
    TW_CHECK(text.contains(QStringLiteral("Explain")));
    TW_CHECK(text.contains(QStringLiteral("```logo")));
    TW_CHECK(text.contains(QStringLiteral("mandala.logo")));
    TW_CHECK(text.contains(QStringLiteral("REPEAT 8 [FD 40 RT 45]")));
}

void test_prompt_selection_wins_over_the_document() {
    announce("prompt: a selection is what the question is about");
    Context context;
    context.language = QStringLiteral("BASIC");
    context.source = QStringLiteral("10 PRINT \"A\"\n20 PRINT \"B\"\n");
    context.selection = QStringLiteral("20 PRINT \"B\"");
    const Conversation conversation =
        timewarp::ai::build_prompt(Task::Question, context, QStringLiteral("what does this do?"));
    const QString text = conversation.messages.at(0).content;
    TW_CHECK(text.contains(QStringLiteral("Selected lines")));
    TW_CHECK(text.contains(QStringLiteral("what does this do?")));
    TW_CHECK(!text.contains(QStringLiteral("10 PRINT")));
}

void test_prompt_fix_carries_the_last_run() {
    announce("prompt: fix sends what the run said");
    Context context;
    context.language = QStringLiteral("BASIC");
    context.source = QStringLiteral("10 PRNT \"HI\"\n");
    context.last_output = QStringLiteral("❌ line 10: PRNT is not a command");
    const QString text = timewarp::ai::build_prompt(Task::Fix, context, QString{}).messages
                             .at(0)
                             .content;
    TW_CHECK(text.contains(QStringLiteral("last run reported")));
    TW_CHECK(text.contains(QStringLiteral("PRNT is not a command")));
    // The emoji prefix is the application's own convention and is not something the
    // model should be imitating.
    TW_CHECK(text.contains(QStringLiteral("Find what is wrong")));
}

void test_prompt_write_sends_no_program() {
    announce("prompt: write is not about the open document");
    Context context;
    context.language = QStringLiteral("PILOT");
    context.source = QStringLiteral("T: something else entirely\n");
    const Conversation conversation = timewarp::ai::build_prompt(
        Task::Write, context, QStringLiteral("a quiz about the solar system"));
    const QString text = conversation.messages.at(0).content;
    TW_CHECK(text.contains(QStringLiteral("a quiz about the solar system")));
    TW_CHECK(!text.contains(QStringLiteral("something else entirely")));
}

void test_prompt_truncates_a_huge_program() {
    announce("prompt: a huge program is cut, and says so");
    Context context;
    context.language = QStringLiteral("BASIC");
    context.source = QString(QStringLiteral("10 REM a line\n")).repeated(2000);
    const QString text = timewarp::ai::build_prompt(Task::Explain, context, QString{}).messages
                             .at(0)
                             .content;
    TW_CHECK(text.contains(QStringLiteral("cut here")));
    // The instruction plus the cap plus the fence, comfortably under what a small
    // model accepts.
    TW_CHECK(text.size() < timewarp::ai::max_context_characters() + 4000);
}

void test_translation_target_is_the_users_words() {
    announce("prompt: translate names the target");
    Context context;
    context.language = QStringLiteral("BASIC");
    context.source = QStringLiteral("10 PRINT \"HI\"\n");
    const QString text = timewarp::ai::build_prompt(Task::Translate, context,
                                                    QStringLiteral("Logo"))
                             .messages.at(0)
                             .content;
    TW_CHECK(text.contains(QStringLiteral("in Logo")));
    TW_CHECK(text.contains(QStringLiteral("```basic")));
}

void test_extract_code() {
    announce("prompt: the code inside an answer");
    TW_CHECK_EQ(timewarp::ai::extract_code(
                    QStringLiteral("Here you go:\n\n```logo\nFD 10\n```\nThat draws.")),
                QStringLiteral("FD 10"));
    // The longest block is the program: a two-line illustration before the file is
    // not what Insert means.
    TW_CHECK_EQ(timewarp::ai::extract_code(QStringLiteral(
                    "```basic\n10 PRINT \"X\"\n```\nand the whole file:\n"
                    "```basic\n10 PRINT \"X\"\n20 GOTO 10\n```")),
                QStringLiteral("10 PRINT \"X\"\n20 GOTO 10"));
    // No fence: the answer itself is the program.
    TW_CHECK_EQ(timewarp::ai::extract_code(QStringLiteral("  FD 10\n")),
                QStringLiteral("FD 10"));
    // An unterminated fence is not a program, and must not lose the text.
    TW_CHECK_EQ(timewarp::ai::extract_code(QStringLiteral("```logo\nFD 10")),
                QStringLiteral("```logo\nFD 10"));
}

// -- the exchange ----------------------------------------------------------

void test_openai_body() {
    announce("client: the OpenAI-compatible body");
    Config config = example_config();
    config.max_tokens = 512;
    config.temperature = 0.3;

    Conversation conversation;
    conversation.system = QStringLiteral("be brief");
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hello")});
    conversation.messages.append(
        Message{QStringLiteral("assistant"), QStringLiteral("hi")});
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("again")});

    const QJsonObject root =
        QJsonDocument::fromJson(timewarp::ai::body_for(config, conversation)).object();
    TW_CHECK_EQ(root.value(QStringLiteral("model")).toString(),
                QStringLiteral("deepseek-chat"));
    TW_CHECK_EQ(root.value(QStringLiteral("max_tokens")).toInt(), 512);
    TW_CHECK(root.value(QStringLiteral("temperature")).toDouble() > 0.29);
    TW_CHECK_EQ(root.value(QStringLiteral("stream")).toBool(), false);
    const QJsonArray turns = root.value(QStringLiteral("messages")).toArray();
    TW_CHECK_EQ(turns.size(), 4);
    TW_CHECK_EQ(turns.at(0).toObject().value(QStringLiteral("role")).toString(),
                QStringLiteral("system"));
    TW_CHECK_EQ(turns.at(1).toObject().value(QStringLiteral("content")).toString(),
                QStringLiteral("hello"));
}

void test_anthropic_body() {
    announce("client: the Anthropic body");
    Config config = example_config();
    config.protocol = Protocol::Anthropic;
    config.base_url = QStringLiteral("https://api.anthropic.com/v1");

    Conversation conversation;
    conversation.system = QStringLiteral("be brief");
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("one")});
    // Two questions in a row is legal here and a 400 to Anthropic: adjacent turns
    // with the same role are joined rather than refused.
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("two")});
    conversation.messages.append(
        Message{QStringLiteral("assistant"), QStringLiteral("answer")});
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("three")});

    const QJsonObject root =
        QJsonDocument::fromJson(timewarp::ai::body_for(config, conversation)).object();
    TW_CHECK_EQ(root.value(QStringLiteral("system")).toString(),
                QStringLiteral("be brief"));
    const QJsonArray turns = root.value(QStringLiteral("messages")).toArray();
    TW_CHECK_EQ(turns.size(), 3);
    TW_CHECK_EQ(turns.at(0).toObject().value(QStringLiteral("role")).toString(),
                QStringLiteral("user"));
    TW_CHECK_EQ(turns.at(0).toObject().value(QStringLiteral("content")).toString(),
                QStringLiteral("one\n\ntwo"));
    TW_CHECK_EQ(turns.at(1).toObject().value(QStringLiteral("role")).toString(),
                QStringLiteral("assistant"));
    TW_CHECK(!root.contains(QStringLiteral("stream")));
}

void test_request_headers() {
    announce("client: the request carries the right auth");
    Config config = example_config();
    config.api_key = QStringLiteral("sk-test");
    QNetworkRequest openai = timewarp::ai::request_for(config);
    TW_CHECK_EQ(openai.url().toString(),
                QStringLiteral("https://api.deepseek.com/v1/chat/completions"));
    TW_CHECK_EQ(openai.rawHeader("Authorization").toStdString(), std::string{"Bearer sk-test"});
    TW_CHECK_EQ(openai.header(QNetworkRequest::ContentTypeHeader).toString(),
                QStringLiteral("application/json"));

    Config anthropic = config;
    anthropic.protocol = Protocol::Anthropic;
    anthropic.base_url = QStringLiteral("https://api.anthropic.com/v1");
    const QNetworkRequest request = timewarp::ai::request_for(anthropic);
    TW_CHECK_EQ(request.rawHeader("x-api-key").toStdString(), std::string{"sk-test"});
    TW_CHECK(!request.rawHeader("anthropic-version").isEmpty());
    TW_CHECK(request.rawHeader("Authorization").isEmpty());

    // A local server: no key, no auth header, and no accidental "Bearer " with
    // nothing after it.
    Config local = example_config();
    local.api_key.clear();
    const QNetworkRequest anonymous = timewarp::ai::request_for(local);
    TW_CHECK(anonymous.rawHeader("Authorization").isEmpty());
}

void test_parse_openai_reply() {
    announce("client: reading an OpenAI answer");
    const QByteArray body = R"({"model":"deepseek-chat","choices":[{"message":{"role":"assistant","content":"Draw a square."},"finish_reason":"stop"}],"usage":{"prompt_tokens":12,"completion_tokens":4}})";
    const Reply reply = timewarp::ai::parse_reply(Protocol::OpenAi, 200, body);
    TW_CHECK(reply.ok);
    TW_CHECK_EQ(reply.text, QStringLiteral("Draw a square."));
    TW_CHECK_EQ(reply.model, QStringLiteral("deepseek-chat"));
    TW_CHECK_EQ(reply.prompt_tokens, 12);
    TW_CHECK_EQ(reply.completion_tokens, 4);
    TW_CHECK(reply.error.isEmpty());

    // The array-of-parts form some compatible servers send.
    const Reply parts = timewarp::ai::parse_reply(
        Protocol::OpenAi, 200,
        R"({"choices":[{"message":{"content":[{"type":"text","text":"part one "},{"type":"text","text":"two"}]}}]})");
    TW_CHECK_EQ(parts.text, QStringLiteral("part one two"));
}

void test_parse_anthropic_reply() {
    announce("client: reading an Anthropic answer");
    const QByteArray body = R"({"model":"claude-3-5-haiku-latest","content":[{"type":"text","text":"FD 100"}],"usage":{"input_tokens":9,"output_tokens":3},"stop_reason":"end_turn"})";
    const Reply reply = timewarp::ai::parse_reply(Protocol::Anthropic, 200, body);
    TW_CHECK(reply.ok);
    TW_CHECK_EQ(reply.text, QStringLiteral("FD 100"));
    TW_CHECK_EQ(reply.prompt_tokens, 9);
    TW_CHECK_EQ(reply.completion_tokens, 3);
}

void test_parse_errors() {
    announce("client: reading a refusal");
    const Reply unauthorised = timewarp::ai::parse_reply(
        Protocol::OpenAi, 401,
        R"({"error":{"message":"Incorrect API key provided","type":"invalid_request_error"}})");
    TW_CHECK(!unauthorised.ok);
    TW_CHECK(unauthorised.error.contains(QStringLiteral("key was rejected")));
    TW_CHECK(unauthorised.error.contains(QStringLiteral("Incorrect API key provided")));

    const Reply missing = timewarp::ai::parse_reply(Protocol::Anthropic, 404,
                                                    R"({"type":"error","error":{"type":"not_found_error","message":"model: nope"}})");
    TW_CHECK(!missing.ok);
    TW_CHECK(missing.error.contains(QStringLiteral("base URL")));
    TW_CHECK(missing.error.contains(QStringLiteral("model: nope")));

    const Reply limited = timewarp::ai::parse_reply(Protocol::OpenAi, 429, "Too Many Requests");
    TW_CHECK(!limited.ok);
    TW_CHECK(limited.error.contains(QStringLiteral("rate-limiting")));
    // A body that is not JSON at all is still a sentence, not a crash.
    TW_CHECK(limited.error.contains(QStringLiteral("Too Many Requests")));

    const Reply html = timewarp::ai::parse_reply(Protocol::OpenAi, 502,
                                                 "<html><body>bad gateway</body></html>");
    TW_CHECK(!html.ok);
    TW_CHECK(html.error.contains(QStringLiteral("internal error")));

    // A 200 with nothing in it is reported as an error rather than shown as an
    // empty answer.
    const Reply empty = timewarp::ai::parse_reply(Protocol::OpenAi, 200, R"({"choices":[]})");
    TW_CHECK(!empty.ok);
    TW_CHECK(!empty.error.isEmpty());
}

void test_network_error_sentences() {
    announce("client: transport failures in words");
    TW_CHECK(timewarp::ai::describe_network_error(
                 static_cast<int>(QNetworkReply::HostNotFoundError))
                 .contains(QStringLiteral("host")));
    TW_CHECK(timewarp::ai::describe_network_error(
                 static_cast<int>(QNetworkReply::ConnectionRefusedError))
                 .contains(QStringLiteral("refused")));
    TW_CHECK(timewarp::ai::describe_network_error(
                 static_cast<int>(QNetworkReply::TimeoutError))
                 .contains(QStringLiteral("timed out")));
    TW_CHECK(!timewarp::ai::describe_network_error(9999).isEmpty());
}


// "Which models can this endpoint run?" is the question a person configures the
// assistant by guessing at, and `GET /models` is the answer both protocols share.
// Reading it is a pure function; asking for it is one more kind of exchange.

void test_models_answer_is_read() {
    announce("client: reading a model list");
    const QStringList models = timewarp::ai::parse_models(
        R"({"object":"list","data":[{"id":"llama3.2:latest"},{"id":"deepseek-chat"},{"id":"deepseek-chat"}]})");
    TW_CHECK_EQ(models.size(), 2);
    // Sorted, and never the same name twice: a dropdown of duplicates is a
    // dropdown nobody can use.
    if (models.size() == 2) {
        TW_CHECK_EQ(models.at(0), QStringLiteral("deepseek-chat"));
        TW_CHECK_EQ(models.at(1), QStringLiteral("llama3.2:latest"));
    }
    // A service that does not implement the call, or answers with something else,
    // is an empty list rather than an error: the dialog says "type the name
    // instead".
    TW_CHECK(timewarp::ai::parse_models("not json").isEmpty());
    TW_CHECK(timewarp::ai::parse_models(R"({"models":[]})").isEmpty());

    Config config = example_config();
    config.api_key = QStringLiteral("sk-test");
    const QNetworkRequest request = timewarp::ai::models_request(config);
    TW_CHECK_EQ(request.url().toString(),
                QStringLiteral("https://api.deepseek.com/v1/models"));
    TW_CHECK(!request.rawHeader("Authorization").isEmpty());
}

void test_client_asks_for_the_models() {
    announce("client: a model list, live");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    transport->respond(R"({"data":[{"id":"m-one"},{"id":"m-two"}]})");

    timewarp::ai::Client client{std::move(network)};
    QStringList listed;
    QString failure;
    QObject::connect(&client, &timewarp::ai::Client::modelsListed,
                     [&listed](const QStringList& models) { listed = models; });
    QObject::connect(&client, &timewarp::ai::Client::failed,
                     [&failure](const QString& message) { failure = message; });

    // No model named: asking what the models are must not require knowing one.
    Config config = example_config();
    config.model.clear();
    client.listModels(config);
    settle();
    TW_CHECK(failure.isEmpty());
    TW_CHECK_EQ(listed.size(), 2);
    TW_CHECK_EQ(listed.value(0), QStringLiteral("m-one"));
    // A GET with no body: the request is a question, not a conversation.
    TW_CHECK(transport->last_operation == QNetworkAccessManager::GetOperation);
    TW_CHECK(transport->payload.isEmpty());
}

void test_a_chat_request_is_a_post() {
    announce("client: a conversation is a post");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    transport->respond(R"({"choices":[{"message":{"content":"hello"}}]})");
    timewarp::ai::Client client{std::move(network)};
    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hi")});
    client.send(example_config(), conversation);
    settle();
    TW_CHECK(transport->last_operation == QNetworkAccessManager::PostOperation);
    TW_CHECK(!transport->payload.isEmpty());
}

void test_client_answers() {
    announce("client: a successful exchange");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    transport->respond(
        R"({"model":"deepseek-chat","choices":[{"message":{"content":"try FD 100"}}],"usage":{"prompt_tokens":3,"completion_tokens":2}})");

    timewarp::ai::Client client{std::move(network)};
    QString answer;
    QString failure;
    int starts = 0;
    QObject::connect(&client, &timewarp::ai::Client::started, [&starts] { ++starts; });
    QObject::connect(&client, &timewarp::ai::Client::answered,
                     [&answer](const Reply& reply) { answer = reply.text; });
    QObject::connect(&client, &timewarp::ai::Client::failed,
                     [&failure](const QString& message) { failure = message; });

    Config config = example_config();
    config.api_key = QStringLiteral("sk-test");
    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hello")});
    client.send(config, conversation);
    TW_CHECK(client.busy());
    TW_CHECK_EQ(starts, 1);
    settle();
    TW_CHECK(!client.busy());
    TW_CHECK_EQ(answer, QStringLiteral("try FD 100"));
    TW_CHECK(failure.isEmpty());
    // What the transport received is what the client built: one POST, the model in
    // the body, and the key in the header rather than the body.
    TW_CHECK_EQ(transport->posts, 1);
    TW_CHECK_EQ(transport->request.rawHeader("Authorization").toStdString(),
                std::string{"Bearer sk-test"});
    const QJsonObject sent = QJsonDocument::fromJson(transport->payload).object();
    TW_CHECK_EQ(sent.value(QStringLiteral("model")).toString(),
                QStringLiteral("deepseek-chat"));
    TW_CHECK(!transport->payload.contains("sk-test"));
}

void test_client_reports_and_redacts() {
    announce("client: a failure never carries the key");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    // A service that echoes the key back in its complaint -- which is exactly the
    // shape a proxy in the middle produces.
    transport->respond("{\"error\":{\"message\":\"bad key sk-leaky\"}}", 401);
    // ... and Qt calls a 401 an `AuthenticationRequiredError`, which is why the
    // client reads the status before the error: the service's own sentence is the
    // one that says which of the two things the user can change is wrong.
    transport->fail(static_cast<int>(QNetworkReply::AuthenticationRequiredError), true);

    timewarp::ai::Client client{std::move(network)};
    QString failure;
    QObject::connect(&client, &timewarp::ai::Client::failed,
                     [&failure](const QString& message) { failure = message; });

    Config config = example_config();
    config.api_key = QStringLiteral("sk-leaky");
    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hi")});
    client.send(config, conversation);
    settle();
    TW_CHECK(!failure.isEmpty());
    TW_CHECK(failure.contains(QStringLiteral("***")));
    TW_CHECK(!failure.contains(QStringLiteral("sk-leaky")));
    // The service's message, in the words a person can act on -- not "the network
    // request failed", which is what a status-less reading of the same reply gives.
    TW_CHECK(failure.contains(QStringLiteral("key was rejected")));
    TW_CHECK(failure.contains(QStringLiteral("bad key")));
}

void test_client_transport_error() {
    announce("client: a refused connection is a sentence");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    transport->fail(static_cast<int>(QNetworkReply::ConnectionRefusedError));

    timewarp::ai::Client client{std::move(network)};
    QString failure;
    QObject::connect(&client, &timewarp::ai::Client::failed,
                     [&failure](const QString& message) { failure = message; });
    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hi")});
    client.send(example_config(), conversation);
    settle();
    TW_CHECK(failure.contains(QStringLiteral("refused")));
    TW_CHECK(!client.busy());
}

void test_client_refuses_an_unusable_configuration() {
    announce("client: no model, no request");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    timewarp::ai::Client client{std::move(network)};
    QString failure;
    QObject::connect(&client, &timewarp::ai::Client::failed,
                     [&failure](const QString& message) { failure = message; });

    Config config = example_config();
    config.enabled = false;
    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hi")});
    client.send(config, conversation);
    TW_CHECK(failure.contains(QStringLiteral("switched off")));
    // Nothing was sent: the refusal happens before any bytes leave.
    TW_CHECK_EQ(transport->posts, 0);
}

void test_client_cancel_drops_the_answer() {
    announce("client: stop means stop");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    transport->respond(
        R"({"choices":[{"message":{"content":"too late"}}]})");

    timewarp::ai::Client client{std::move(network)};
    QString answer;
    QString failure;
    QObject::connect(&client, &timewarp::ai::Client::answered,
                     [&answer](const Reply& reply) { answer = reply.text; });
    QObject::connect(&client, &timewarp::ai::Client::failed,
                     [&failure](const QString& message) { failure = message; });

    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hi")});
    client.send(example_config(), conversation);
    client.cancel();
    settle();
    // The reply that arrives after the stop is dropped rather than reported as an
    // error: the user asked for silence, and a red "cancelled" afterwards is the
    // program arguing with them.
    TW_CHECK(answer.isEmpty());
    TW_CHECK(failure.isEmpty());
    TW_CHECK(!client.busy());
}

void test_client_sends_one_at_a_time() {
    announce("client: a second send supersedes the first");
    auto network = std::make_unique<FakeNetwork>();
    FakeNetwork* transport = network.get();
    transport->respond(R"({"choices":[{"message":{"content":"second"}}]})");

    timewarp::ai::Client client{std::move(network)};
    QStringList answers;
    QObject::connect(&client, &timewarp::ai::Client::answered,
                     [&answers](const Reply& reply) { answers << reply.text; });
    Conversation conversation;
    conversation.messages.append(Message{QStringLiteral("user"), QStringLiteral("hi")});
    client.send(example_config(), conversation);
    client.send(example_config(), conversation);
    settle();
    TW_CHECK_EQ(answers.size(), 1);
    TW_CHECK_EQ(answers.value(0), QStringLiteral("second"));
    TW_CHECK_EQ(transport->posts, 2);
}

}  // namespace

int main(int argc, char** argv) {
    // The settings cases write through QSettings, so the identity and the format
    // are pinned before anything reads them, and the file lives in a scratch
    // directory that dies with the run: a suite must never touch the assistant's
    // real configuration, which holds a key.
    QCoreApplication app{argc, argv};
    QCoreApplication::setOrganizationName(QStringLiteral("TimeWarpAiTests"));
    QCoreApplication::setApplicationName(QStringLiteral("AiTests"));
    QSettings::setDefaultFormat(QSettings::IniFormat);
    QTemporaryDir settings_dir;
    TW_CHECK(settings_dir.isValid());
    QSettings::setPath(QSettings::IniFormat, QSettings::UserScope, settings_dir.path());

    test_base_url_validation();
    test_endpoint_assembly();
    test_protocol_keys();
    test_presets_include_deepseek();
    test_readiness();
    test_saved_configuration_round_trip();
    test_environment_wins();
    test_redaction();

    test_system_prompt_names_the_languages();
    test_prompt_explain_carries_the_program();
    test_prompt_selection_wins_over_the_document();
    test_prompt_fix_carries_the_last_run();
    test_prompt_write_sends_no_program();
    test_prompt_truncates_a_huge_program();
    test_translation_target_is_the_users_words();
    test_extract_code();

    test_openai_body();
    test_anthropic_body();
    test_request_headers();
    test_parse_openai_reply();
    test_parse_anthropic_reply();
    test_parse_errors();
    test_network_error_sentences();
    test_models_answer_is_read();
    test_client_asks_for_the_models();
    test_a_chat_request_is_a_post();
    test_client_answers();
    test_client_reports_and_redacts();
    test_client_transport_error();
    test_client_refuses_an_unusable_configuration();
    test_client_cancel_drops_the_answer();
    test_client_sends_one_at_a_time();
    return tw::test::summary("ai_tests");
}
