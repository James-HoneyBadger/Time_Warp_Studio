// SPDX-License-Identifier: MIT

#include "timewarp/ai/history.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSaveFile>

namespace timewarp::ai {
namespace {

constexpr int kFormatVersion = 1;

}  // namespace

ConversationStore::ConversationStore(QString directory) : directory_{std::move(directory)} {}

QString ConversationStore::path() const {
    if (directory_.isEmpty()) {
        return {};
    }
    return QDir{directory_}.filePath(QStringLiteral("conversation.json"));
}

QList<Message> ConversationStore::load() const {
    QList<Message> turns;
    const QString where = path();
    if (where.isEmpty()) {
        return turns;
    }
    QFile file{where};
    if (!file.open(QIODevice::ReadOnly)) {
        return turns;
    }
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll());
    if (!document.isObject()) {
        return turns;
    }
    const QJsonValue stored = document.object().value(QStringLiteral("turns"));
    if (!stored.isArray()) {
        return turns;
    }
    for (const QJsonValue& entry : stored.toArray()) {
        const QJsonObject turn = entry.toObject();
        const QString role = turn.value(QStringLiteral("role")).toString();
        const QString content = turn.value(QStringLiteral("content")).toString();
        // A turn this program would not have sent is not one it can send again.
        if (role != QLatin1String("user") && role != QLatin1String("assistant")) {
            continue;
        }
        if (content.trimmed().isEmpty()) {
            continue;
        }
        turns.append(Message{role, content});
    }
    while (turns.size() > kStoredTurns) {
        turns.removeFirst();
    }
    return turns;
}

bool ConversationStore::save(const QList<Message>& turns) const {
    const QString where = path();
    if (where.isEmpty()) {
        return false;
    }
    if (!QDir{}.mkpath(directory_)) {
        return false;
    }
    QList<Message> kept = turns;
    while (kept.size() > kStoredTurns) {
        kept.removeFirst();
    }
    QJsonArray stored;
    for (const Message& turn : kept) {
        stored.append(QJsonObject{{QStringLiteral("role"), turn.role},
                                  {QStringLiteral("content"), turn.content}});
    }
    const QJsonObject root{{QStringLiteral("version"), kFormatVersion},
                           {QStringLiteral("turns"), stored}};

    QSaveFile file{where};
    if (!file.open(QIODevice::WriteOnly)) {
        return false;
    }
    file.write(QJsonDocument{root}.toJson(QJsonDocument::Indented));
    if (!file.commit()) {
        return false;
    }
    // The conversation holds the program, which is the user's text: the same rule the
    // settings file follows for a key.
    QFile::setPermissions(where, QFile::ReadOwner | QFile::WriteOwner);
    return true;
}

void ConversationStore::clear() const {
    const QString where = path();
    if (where.isEmpty()) {
        return;
    }
    QFile::remove(where);
}

}  // namespace timewarp::ai
