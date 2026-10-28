// SPDX-License-Identifier: MIT

#include "timewarp/ai/client.hpp"

#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonValue>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QUrl>

#include <utility>

namespace timewarp::ai {

namespace {

/// Long enough to say what is wrong, short enough that a service that answers with
/// an HTML error page does not fill the panel with markup.
constexpr int kErrorDetailCharacters = 300;

/// The request body, both protocols. Shared so the two shapes sit next to each
/// other and a reader can see exactly what differs: the system turn's home, and the
/// name of the role list.
[[nodiscard]] QJsonObject body_object(const Config& config,
                                      const Conversation& conversation) {
    QJsonObject root;
    root.insert(QStringLiteral("model"), config.model.trimmed());
    root.insert(QStringLiteral("max_tokens"), config.max_tokens);
    root.insert(QStringLiteral("temperature"), config.temperature);

    QJsonArray turns;
    // Every request streams. A local server did from the beginning; a hosted one
    // used to be asked for the whole answer so that its token counts and its single
    // finished string arrived together, and both protocols now send those *inside*
    // the stream -- OpenAI in a final `usage` chunk, Anthropic in `message_start`
    // and `message_delta` -- so there is nothing left to trade away for them, and
    // Stop can end an answer that is half written.
    constexpr bool stream = true;
    if (config.protocol == Protocol::Anthropic) {
        root.insert(QStringLiteral("system"), conversation.system);
        // Anthropic defaults to a whole answer, so the field is only sent when it is
        // true.
        root.insert(QStringLiteral("stream"), stream);
        // Anthropic's API requires the turns to alternate and to begin with a
        // user turn. A conversation with two questions in a row (the first one
        // failed) is legal to us and a 400 to it, so adjacent turns with the same
        // role are joined rather than refused.
        QString pending_role;
        QString pending_text;
        const auto flush = [&turns, &pending_role, &pending_text] {
            if (!pending_role.isEmpty()) {
                turns.append(QJsonObject{{QStringLiteral("role"), pending_role},
                                         {QStringLiteral("content"), pending_text}});
                pending_role.clear();
                pending_text.clear();
            }
        };
        for (const Message& message : conversation.messages) {
            const QString role =
                message.role == QLatin1String("assistant") ? QStringLiteral("assistant")
                                                           : QStringLiteral("user");
            if (role == pending_role) {
                pending_text += QStringLiteral("\n\n") + message.content;
                continue;
            }
            flush();
            pending_role = role;
            pending_text = message.content;
        }
        flush();
    } else {
        if (!conversation.system.trimmed().isEmpty()) {
            turns.append(QJsonObject{{QStringLiteral("role"), QStringLiteral("system")},
                                     {QStringLiteral("content"), conversation.system}});
        }
        for (const Message& message : conversation.messages) {
            turns.append(QJsonObject{{QStringLiteral("role"), message.role},
                                     {QStringLiteral("content"), message.content}});
        }
        root.insert(QStringLiteral("stream"), stream);
        if (stream) {
            // Without this the streamed answer carries no token counts at all, and
            // the panel's "· 412 in, 96 out" would silently become zero.
            root.insert(QStringLiteral("stream_options"),
                        QJsonObject{{QStringLiteral("include_usage"), true}});
        }
    }
    root.insert(QStringLiteral("messages"), turns);
    return root;
}

/// The service's own complaint when it sent one, else the start of what it did send.
[[nodiscard]] QString detail_of(const QJsonObject& root, const QByteArray& body) {
    const QJsonValue error = root.value(QStringLiteral("error"));
    if (error.isObject()) {
        const QString message = error.toObject().value(QStringLiteral("message")).toString();
        if (!message.trimmed().isEmpty()) {
            return message.trimmed();
        }
    }
    if (error.isString() && !error.toString().trimmed().isEmpty()) {
        return error.toString().trimmed();
    }
    QString text = QString::fromUtf8(body).simplified();
    if (text.size() > kErrorDetailCharacters) {
        text.truncate(kErrorDetailCharacters);
        text += QStringLiteral("...");
    }
    return text;
}

/// What a status code means, in the terms of *this* application: not "unauthorized"
/// but which of the two things the user can change is wrong.
[[nodiscard]] QString http_reason(int status) {
    if (status == 400) {
        return QStringLiteral("the service rejected the request");
    }
    if (status == 401 || status == 403) {
        return QStringLiteral("the key was rejected — check the key and that it belongs "
                              "to this endpoint");
    }
    if (status == 404) {
        return QStringLiteral("the endpoint was not found — the base URL should end at "
                              "the version path, such as https://api.openai.com/v1");
    }
    if (status == 413) {
        return QStringLiteral("the program is too large for this model");
    }
    if (status == 429) {
        return QStringLiteral("the service is rate-limiting requests — wait a moment "
                              "and try again");
    }
    if (status >= 500) {
        return QStringLiteral("the service reported an internal error");
    }
    return QStringLiteral("the service answered HTTP %1").arg(status);
}

/// The text of an OpenAI-style content value: a string, or the array of parts that
/// some providers send instead.
[[nodiscard]] QString openai_content(const QJsonValue& content) {
    if (content.isString()) {
        return content.toString();
    }
    QString text;
    for (const QJsonValue& part : content.toArray()) {
        const QJsonObject object = part.toObject();
        if (object.value(QStringLiteral("type")).toString() == QLatin1String("text")) {
            text += object.value(QStringLiteral("text")).toString();
        }
    }
    return text;
}

}  // namespace

QByteArray body_for(const Config& config, const Conversation& conversation) {
    return QJsonDocument{body_object(config, conversation)}.toJson(QJsonDocument::Compact);
}

QNetworkRequest models_request(const Config& config) {
    const QString base = config.base_url.trimmed();
    QString trimmed = base;
    while (trimmed.endsWith(QLatin1Char('/'))) {
        trimmed.chop(1);
    }
    QNetworkRequest request{QUrl{trimmed + QStringLiteral("/models")}};
    request.setRawHeader("Accept", "application/json");
    request.setHeader(QNetworkRequest::UserAgentHeader, QStringLiteral("TimeWarpStudio"));
    request.setTransferTimeout(config.timeout_seconds * 1000);
    request.setAttribute(QNetworkRequest::RedirectPolicyAttribute,
                         QNetworkRequest::NoLessSafeRedirectPolicy);
    const QString key = effective_key(config);
    if (!key.isEmpty()) {
        if (config.protocol == Protocol::Anthropic) {
            request.setRawHeader("x-api-key", key.toUtf8());
            request.setRawHeader("anthropic-version", "2023-06-01");
        } else {
            request.setRawHeader("Authorization", "Bearer " + key.toUtf8());
        }
    }
    return request;
}

QStringList parse_models(const QByteArray& body) {
    QStringList models;
    const QJsonDocument document = QJsonDocument::fromJson(body);
    if (!document.isObject()) {
        return models;
    }
    for (const QJsonValue& entry : document.object().value(QStringLiteral("data")).toArray()) {
        const QString identifier = entry.toObject().value(QStringLiteral("id")).toString();
        if (!identifier.isEmpty() && !models.contains(identifier)) {
            models.append(identifier);
        }
    }
    models.sort(Qt::CaseInsensitive);
    return models;
}

QNetworkRequest request_for(const Config& config) {
    QNetworkRequest request{QUrl{endpoint_url(config)}};
    request.setHeader(QNetworkRequest::ContentTypeHeader,
                      QStringLiteral("application/json"));
    request.setRawHeader("Accept", "application/json");
    request.setHeader(QNetworkRequest::UserAgentHeader, QStringLiteral("TimeWarpStudio"));
    request.setTransferTimeout(config.timeout_seconds * 1000);
    // A redirect is followed only when it is no less safe than the URL given, so an
    // https endpoint cannot be redirected down to plain http behind the user's back.
    request.setAttribute(QNetworkRequest::RedirectPolicyAttribute,
                         QNetworkRequest::NoLessSafeRedirectPolicy);

    const QString key = effective_key(config);
    if (!key.isEmpty()) {
        if (config.protocol == Protocol::Anthropic) {
            request.setRawHeader("x-api-key", key.toUtf8());
            // The Messages API's published version. It is a date because that is
            // what Anthropic asks for, not a version of this application.
            request.setRawHeader("anthropic-version", "2023-06-01");
        } else {
            request.setRawHeader("Authorization", "Bearer " + key.toUtf8());
        }
    }
    return request;
}

Reply parse_reply(Protocol protocol, int status, const QByteArray& body) {
    Reply reply;
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(body, &parse_error);
    const QJsonObject root = document.isObject() ? document.object() : QJsonObject{};

    if (status < 200 || status >= 300) {
        reply.error = http_reason(status);
        const QString detail = detail_of(root, body);
        if (!detail.isEmpty()) {
            reply.error += QStringLiteral(": ") + detail;
        }
        return reply;
    }
    if (!document.isObject()) {
        reply.error = parse_error.error == QJsonParseError::NoError
                          ? QStringLiteral("the service did not answer with JSON")
                          : QStringLiteral("the service's answer could not be read: %1")
                                .arg(parse_error.errorString());
        return reply;
    }

    reply.model = root.value(QStringLiteral("model")).toString();
    if (protocol == Protocol::Anthropic) {
        for (const QJsonValue& part : root.value(QStringLiteral("content")).toArray()) {
            const QJsonObject object = part.toObject();
            if (object.value(QStringLiteral("type")).toString() == QLatin1String("text")) {
                reply.text += object.value(QStringLiteral("text")).toString();
            }
        }
        const QJsonObject usage = root.value(QStringLiteral("usage")).toObject();
        reply.prompt_tokens = usage.value(QStringLiteral("input_tokens")).toInt();
        reply.completion_tokens = usage.value(QStringLiteral("output_tokens")).toInt();
    } else {
        const QJsonArray choices = root.value(QStringLiteral("choices")).toArray();
        const QJsonObject choice = choices.isEmpty() ? QJsonObject{} : choices.at(0).toObject();
        reply.text = openai_content(
            choice.value(QStringLiteral("message")).toObject().value(QStringLiteral("content")));
        const QJsonObject usage = root.value(QStringLiteral("usage")).toObject();
        reply.prompt_tokens = usage.value(QStringLiteral("prompt_tokens")).toInt();
        reply.completion_tokens = usage.value(QStringLiteral("completion_tokens")).toInt();
    }

    if (reply.text.trimmed().isEmpty()) {
        // A refusal is a successful HTTP exchange with nothing in it. Reporting it as
        // an error is what stops the panel showing an empty answer as if it were one.
        const QJsonObject error = root.value(QStringLiteral("error")).toObject();
        const QString message = error.value(QStringLiteral("message")).toString();
        reply.error = message.isEmpty()
                          ? QStringLiteral("the service returned no text")
                          : QStringLiteral("the service returned no text: %1").arg(message);
        return reply;
    }
    reply.ok = true;
    return reply;
}

QString describe_network_error(int error) {
    switch (static_cast<QNetworkReply::NetworkError>(error)) {
        case QNetworkReply::ConnectionRefusedError:
            return QStringLiteral("the connection was refused — is the server running?");
        case QNetworkReply::RemoteHostClosedError:
            return QStringLiteral("the server closed the connection before answering");
        case QNetworkReply::HostNotFoundError:
            return QStringLiteral("the host could not be found — check the endpoint address");
        case QNetworkReply::TimeoutError:
            return QStringLiteral("the request timed out — the service did not answer in time");
        case QNetworkReply::OperationCanceledError:
            return QStringLiteral("the request was cancelled");
        case QNetworkReply::SslHandshakeFailedError:
            return QStringLiteral("the TLS handshake failed — the endpoint's certificate "
                                  "could not be verified");
        case QNetworkReply::TemporaryNetworkFailureError:
            return QStringLiteral("the network dropped the connection");
        case QNetworkReply::ProxyConnectionRefusedError:
        case QNetworkReply::ProxyNotFoundError:
        case QNetworkReply::ProxyAuthenticationRequiredError:
            return QStringLiteral("the proxy refused the connection");
        case QNetworkReply::ContentAccessDenied:
            return QStringLiteral("the service denied access to the endpoint");
        case QNetworkReply::ContentNotFoundError:
            return QStringLiteral("the endpoint was not found — check the base URL");
        case QNetworkReply::UnknownNetworkError:
            return QStringLiteral("the network request failed for an unknown reason");
        default:
            break;
    }
    return QStringLiteral("the network request failed");
}

Client::Client(QObject* parent)
    : QObject(parent), owned_network_(std::make_unique<QNetworkAccessManager>()),
      network_(owned_network_.get()) {}

Client::Client(std::unique_ptr<QNetworkAccessManager> network, QObject* parent)
    : QObject(parent), owned_network_(std::move(network)), network_(owned_network_.get()) {}

Client::~Client() { cancel(); }

void Client::send(const Config& config, const Conversation& conversation) {
    QString why;
    if (!ready(config, &why)) {
        cancel();
        emit failed(why);
        return;
    }
    post(config, request_for(config), body_for(config, conversation), Kind::Chat);
}

void Client::listModels(const Config& config) {
    QString why;
    // No model is needed to ask what the models are.
    if (!reachable(config, &why)) {
        cancel();
        emit failed(why);
        return;
    }
    post(config, models_request(config), QByteArray{}, Kind::Models);
}

void Client::post(const Config& config, const QNetworkRequest& request, const QByteArray& body,
                  Kind kind) {
    cancel();
    if (network_ == nullptr) {
        emit failed(QStringLiteral("no network transport is available"));
        return;
    }

    const QString key = effective_key(config);
    const Protocol protocol = config.protocol;
    const int generation = generation_;
    kind_ = kind;
    streaming_ = kind == Kind::Chat;
    stream_raw_.clear();
    stream_buffer_.clear();
    stream_text_.clear();
    stream_model_.clear();
    stream_error_.clear();
    stream_prompt_tokens_ = 0;
    stream_completion_tokens_ = 0;

    // A GET with no body for discovery; a POST for a conversation. The transport
    // decides nothing else about the difference, which is why `kind_` exists: the
    // answer is read by the parser its request belongs to.
    QNetworkReply* reply = kind == Kind::Models ? network_->get(request)
                                                : network_->post(request, body);
    reply_ = reply;
    if (streaming_) {
        // The pieces are read as they arrive; `finished` then has the whole answer
        // already accumulated, so one code path decides whether it is one.
        connect(reply, &QNetworkReply::readyRead, this,
                [this, reply, generation, protocol] {
                    if (generation != generation_) {
                        return;
                    }
                    consumeStream(reply, protocol);
                });
    }
    connect(reply, &QNetworkReply::finished, this, [this, reply, key, protocol, generation,
                                                    kind] {
        reply->deleteLater();
        if (generation != generation_) {
            return;
        }
        reply_.clear();
        const int status =
            reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
        if (status <= 0) {
            const QString message = describe_network_error(static_cast<int>(reply->error()));
            emit failed(redact(message, key));
            return;
        }
        if (kind == Kind::Chat && streaming_) {
            // Whatever was never announced with `readyRead` is read here: a
            // transport that hands over the whole body at once never emits it, and
            // a service that ignored `stream` answered with one JSON document
            // instead of events. `finishStream` falls back to the ordinary reader
            // for exactly that case.
            consumeStream(reply, protocol);
            // Anything still buffered when the connection closes is a line without
            // its newline; it is parsed rather than dropped.
            if (!stream_buffer_.isEmpty()) {
                handleStreamLine(stream_buffer_, protocol);
                stream_buffer_.clear();
            }
            const Reply parsed = finishStream(status, protocol);
            if (parsed.ok) {
                emit answered(parsed);
            } else {
                emit failed(redact(parsed.error, key));
            }
            return;
        }
        const QByteArray body_bytes = reply->readAll();
        if (kind == Kind::Models) {
            if (status < 200 || status >= 300) {
                const Reply parsed = parse_reply(protocol, status, body_bytes);
                emit failed(redact(parsed.error, key));
                return;
            }
            emit modelsListed(parse_models(body_bytes));
            return;
        }
        const Reply parsed = parse_reply(protocol, status, body_bytes);
        if (parsed.ok) {
            emit answered(parsed);
        } else {
            emit failed(redact(parsed.error, key));
        }
    });
    emit started();
}

void Client::consumeStream(QNetworkReply* reply, Protocol protocol) {
    const QByteArray piece = reply->readAll();
    stream_raw_ += piece;
    stream_buffer_ += piece;
    int newline = stream_buffer_.indexOf('\n');
    while (newline >= 0) {
        const QByteArray line = stream_buffer_.left(newline);
        stream_buffer_.remove(0, newline + 1);
        handleStreamLine(line, protocol);
        newline = stream_buffer_.indexOf('\n');
    }
}

void Client::handleStreamLine(const QByteArray& line, Protocol protocol) {
    const QByteArray text = line.trimmed();
    // A blank line ends an event, `:` is a keep-alive comment, and `event:`/`id:`
    // name things this client does not act on -- only `data:` carries the answer.
    if (text.isEmpty() || text.startsWith(':')) {
        return;
    }
    if (!text.startsWith("data:")) {
        return;
    }
    const QByteArray payload = text.mid(5).trimmed();
    if (payload == "[DONE]") {
        return;
    }
    const QJsonDocument document = QJsonDocument::fromJson(payload);
    if (!document.isObject()) {
        return;
    }
    const QJsonObject root = document.object();
    QString piece_text;
    if (protocol == Protocol::Anthropic) {
        const QString type = root.value(QStringLiteral("type")).toString();
        if (type == QLatin1String("message_start")) {
            const QJsonObject message = root.value(QStringLiteral("message")).toObject();
            stream_model_ = message.value(QStringLiteral("model")).toString();
            stream_prompt_tokens_ = message.value(QStringLiteral("usage"))
                                        .toObject()
                                        .value(QStringLiteral("input_tokens"))
                                        .toInt();
        } else if (type == QLatin1String("content_block_delta")) {
            piece_text = root.value(QStringLiteral("delta"))
                             .toObject()
                             .value(QStringLiteral("text"))
                             .toString();
        } else if (type == QLatin1String("message_delta")) {
            stream_completion_tokens_ = root.value(QStringLiteral("usage"))
                                            .toObject()
                                            .value(QStringLiteral("output_tokens"))
                                            .toInt();
        } else if (type == QLatin1String("error")) {
            stream_error_ = root.value(QStringLiteral("error"))
                                .toObject()
                                .value(QStringLiteral("message"))
                                .toString();
        }
    } else {
        const QJsonArray choices = root.value(QStringLiteral("choices")).toArray();
        if (!choices.isEmpty()) {
            piece_text = openai_content(choices.at(0)
                                            .toObject()
                                            .value(QStringLiteral("delta"))
                                            .toObject()
                                            .value(QStringLiteral("content")));
        }
        const QJsonObject usage = root.value(QStringLiteral("usage")).toObject();
        if (!usage.isEmpty()) {
            stream_prompt_tokens_ = usage.value(QStringLiteral("prompt_tokens")).toInt();
            stream_completion_tokens_ =
                usage.value(QStringLiteral("completion_tokens")).toInt();
        }
        const QString model = root.value(QStringLiteral("model")).toString();
        if (!model.isEmpty()) {
            stream_model_ = model;
        }
    }
    if (piece_text.isEmpty()) {
        return;
    }
    stream_text_ += piece_text;
    emit delta(piece_text);
}

Reply Client::finishStream(int status, Protocol protocol) const {
    if (status < 200 || status >= 300) {
        // A refusal is a JSON body rather than events, and `stream_raw_` has it.
        Reply parsed = parse_reply(protocol, status, stream_raw_);
        if (parsed.error.trimmed().isEmpty()) {
            parsed.error = QStringLiteral("the service refused the request");
        }
        return parsed;
    }
    if (!stream_error_.isEmpty()) {
        Reply parsed;
        parsed.error = stream_error_;
        return parsed;
    }
    if (stream_text_.trimmed().isEmpty()) {
        // The endpoint answered 200 and streamed nothing: either a plain answer this
        // reader could not use, or a service that ignores `stream`. The whole body is
        // still here, so the ordinary reader gets a second chance at it.
        Reply parsed = parse_reply(protocol, status, stream_raw_);
        if (!parsed.ok && parsed.error.trimmed().isEmpty()) {
            parsed.error = QStringLiteral("the service returned no text");
        }
        return parsed;
    }
    Reply reply;
    reply.ok = true;
    reply.text = stream_text_;
    reply.model = stream_model_;
    reply.prompt_tokens = stream_prompt_tokens_;
    reply.completion_tokens = stream_completion_tokens_;
    return reply;
}

void Client::cancel() {
    ++generation_;
    if (reply_.isNull()) {
        return;
    }
    QNetworkReply* reply = reply_.data();
    reply_.clear();
    // The check is repeated after the clear rather than trusted from `isNull()`:
    // GCC's -Wnull-dereference does not follow a QPointer through the clear, and a
    // false positive here would stop every release build.
    if (reply != nullptr) {
        reply->abort();
    }
}

}  // namespace timewarp::ai
