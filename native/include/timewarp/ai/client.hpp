// SPDX-License-Identifier: MIT
//
// The one thing in the AI assistant that opens a socket.
//
// `Client` carries a conversation to a configured endpoint and brings the answer
// back; everything else -- what to ask, how to shape the JSON, how to read a reply
// -- is a free function here or in `prompt.hpp`. That split is what lets the
// interesting half be tested without a network, and it is why the class is small:
// a request in flight, a cancellation, and a report.
//
// #### What it does not do
//
// It does not stream, retry, cache, remember a conversation or decide what to send.
// Those are the panel's, because they are decisions about a session rather than
// about a protocol. A request either finishes with an answer or finishes with a
// sentence, and the caller shows that sentence.
//
// #### The key
//
// It is taken from `effective_key()` at the moment the request is built and is
// never stored in the client, never logged, and stripped from any message the
// service sends back (`redact`). A service that echoes the request in an error must
// not be able to put the key in the status bar.

#pragma once

#include <QByteArray>
#include <QObject>
#include <QStringList>
#include <QPointer>
#include <QString>

#include <memory>

#include "timewarp/ai/config.hpp"
#include "timewarp/ai/prompt.hpp"

class QNetworkAccessManager;
class QNetworkReply;
class QNetworkRequest;

namespace timewarp::ai {

/// Everything one exchange produced. `ok` is the only field a caller has to read;
/// the rest is what the panel shows beside the answer.
struct Reply {
    bool ok = false;
    QString text;
    QString model;
    int prompt_tokens = 0;
    int completion_tokens = 0;
    /// A sentence to show the user when `ok` is false, and empty otherwise.
    QString error;
};

/// The JSON body for *conversation*, in the endpoint's own shape.
[[nodiscard]] QByteArray body_for(const Config& config, const Conversation& conversation);

/// The URL and headers for a request that asks the endpoint what it can run.
/// `GET {base}/models` is the one discovery call both protocols share -- OpenAI
/// made it a convention, Anthropic and every compatible server follow it -- so the
/// settings dialog can offer the real model names instead of making somebody guess
/// at `deepseek-chat` from memory.
[[nodiscard]] QNetworkRequest models_request(const Config& config);

/// The model identifiers out of a `/models` answer: `data[].id`, which is what both
/// protocols return. Empty and no error when the body is not JSON -- a service that
/// does not implement the call is not a failure, it is an endpoint that cannot be
/// browsed.
[[nodiscard]] QStringList parse_models(const QByteArray& body);

/// The URL, the headers and the timeout for a request. The key is applied here and
/// nowhere else.
[[nodiscard]] QNetworkRequest request_for(const Config& config);

/// Reads a finished exchange. `status` is the HTTP status, or 0 when the request
/// never reached the service. Both providers' envelopes, both providers' error
/// shapes, and a body that is not JSON at all are all handled here rather than in
/// the client, so the client has one branch.
[[nodiscard]] Reply parse_reply(Protocol protocol, int status, const QByteArray& body);

/// A transport failure as a sentence a person can act on.
[[nodiscard]] QString describe_network_error(int error);

/// Sends one conversation at a time.
class Client final : public QObject {
    Q_OBJECT

public:
    explicit Client(QObject* parent = nullptr);
    /// A client over a transport the caller owns -- a test's, which answers without
    /// a socket. The manager is deleted with the client.
    explicit Client(std::unique_ptr<QNetworkAccessManager> network, QObject* parent = nullptr);
    ~Client() override;

    [[nodiscard]] bool busy() const noexcept { return !reply_.isNull(); }

    /// Sends *conversation*. A send while one is in flight cancels the first: two
    /// answers arriving out of order into one transcript is worse than a lost
    /// question, and the button that starts one is the button that means "this,
    /// now".
    ///
    /// Asks the endpoint what models it offers. Answers through `modelsListed`, or
    /// through `failed` when the service does not implement the call.
    void listModels(const Config& config);

    /// Emits `failed` rather than throwing when the configuration is not usable, so
    /// a caller has one path for "no" whether the reason is a missing model or a
    /// refused connection.
    void send(const Config& config, const Conversation& conversation);

    /// Abandons the request in flight, if any. Emits nothing: the caller asked.
    void cancel();

signals:
    void started();
    void answered(const timewarp::ai::Reply& reply);
    /// The identifiers a `listModels` call found, in the order the service gave
    /// them. Empty when it answered with nothing usable.
    void modelsListed(const QStringList& models);
    void failed(const QString& message);

private:
    /// What the request in flight was, so its answer is read by the right parser.
    enum class Kind { Chat, Models };

    void post(const Config& config, const QNetworkRequest& request, const QByteArray& body,
              Kind kind);

    std::unique_ptr<QNetworkAccessManager> owned_network_;
    QNetworkAccessManager* network_ = nullptr;
    QPointer<QNetworkReply> reply_;
    /// Bumped by every send and every cancel, so a reply that finishes after it was
    /// abandoned is recognised and dropped instead of reporting a cancellation as
    /// an error.
    int generation_ = 0;
    Kind kind_ = Kind::Chat;
};

}  // namespace timewarp::ai

Q_DECLARE_METATYPE(timewarp::ai::Reply)
