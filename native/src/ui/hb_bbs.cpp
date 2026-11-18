// SPDX-License-Identifier: MIT

#include "timewarp/ui/hb_bbs.hpp"

#include "timewarp/ui/app_paths.hpp"

#include <QDateTime>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSaveFile>

namespace timewarp::ui {

namespace {

/// The welcome a new board starts with. Written as the sysop would have: the name of
/// a board nobody has heard of, a rule, and something to reply to -- an empty board
/// teaches nothing about what a board was for.
constexpr auto kWelcomeSubject = "WELCOME TO THE MACHINE";
constexpr auto kWelcomeBody =
    "hello, and welcome to WARP BOARD, the only board this side of 1982.\n"
    "post anything: a question, a program, a complaint about the weather.\n"
    "be kind. the sysop reads everything.\n"
    "- the sysop";

}  // namespace

QString HbBbs::defaultBoardPath() {
    const QString folder = dataDirFor(QStringLiteral("bbs"));
    if (folder.isEmpty()) {
        return {};
    }
    return QDir{folder}.filePath(QStringLiteral("board.json"));
}

bool HbBbs::load(const QString& path) {
    path_ = path;
    system_name_ = QStringLiteral("WARP BOARD");
    sysop_ = QStringLiteral("THE SYSOP");
    error_.clear();
    messages_.clear();

    QFile file{path_};
    if (!path_.isEmpty() && file.open(QIODevice::ReadOnly)) {
        const QJsonDocument document = QJsonDocument::fromJson(file.readAll());
        file.close();
        const QJsonObject root = document.object();
        if (!root.isEmpty()) {
            if (root.contains(QStringLiteral("system"))) {
                system_name_ = root.value(QStringLiteral("system")).toString();
            }
            if (root.contains(QStringLiteral("sysop"))) {
                sysop_ = root.value(QStringLiteral("sysop")).toString();
            }
            for (const QJsonValue& entry : root.value(QStringLiteral("messages")).toArray()) {
                const QJsonObject object = entry.toObject();
                BbsMessage message;
                message.number = object.value(QStringLiteral("number")).toInt();
                message.author = object.value(QStringLiteral("author")).toString();
                message.subject = object.value(QStringLiteral("subject")).toString();
                message.body = object.value(QStringLiteral("body")).toString();
                message.posted = object.value(QStringLiteral("posted")).toString();
                if (message.number > 0 && !message.subject.isEmpty()) {
                    messages_.append(message);
                }
            }
            return true;
        }
        error_ = QStringLiteral("the board file is not readable");
        return false;
    }

    // No file: a new board, seeded and written so the welcome is there for the next
    // caller too.
    BbsMessage welcome;
    welcome.number = 1;
    welcome.author = sysop_;
    welcome.subject = QString::fromUtf8(kWelcomeSubject);
    welcome.body = QString::fromUtf8(kWelcomeBody);
    welcome.posted = QDateTime::currentDateTime().toString(QStringLiteral("yyyy-MM-dd HH:mm"));
    messages_.append(welcome);
    if (path_.isEmpty() || !save()) {
        error_ = path_.isEmpty() ? QStringLiteral("no place to keep the board")
                                 : QStringLiteral("the board could not be written");
    }
    return true;
}

bool HbBbs::save() {
    if (path_.isEmpty()) {
        return false;
    }
    const QDir folder = QFileInfo{path_}.dir();
    if (!folder.exists() && !folder.mkpath(QStringLiteral("."))) {
        return false;
    }
    QJsonArray array;
    for (const BbsMessage& message : messages_) {
        array.append(QJsonObject{{QStringLiteral("number"), message.number},
                                 {QStringLiteral("author"), message.author},
                                 {QStringLiteral("subject"), message.subject},
                                 {QStringLiteral("body"), message.body},
                                 {QStringLiteral("posted"), message.posted}});
    }
    const QJsonObject root{{QStringLiteral("system"), system_name_},
                           {QStringLiteral("sysop"), sysop_},
                           {QStringLiteral("messages"), array}};
    QSaveFile file{path_};
    if (!file.open(QIODevice::WriteOnly)) {
        return false;
    }
    file.write(QJsonDocument{root}.toJson(QJsonDocument::Indented));
    return file.commit();
}

BbsMessage HbBbs::message(int number) const {
    for (const BbsMessage& message : messages_) {
        if (message.number == number) {
            return message;
        }
    }
    return {};
}

int HbBbs::post(const QString& author, const QString& subject, const QString& body,
                const QString& posted) {
    int number = 1;
    for (const BbsMessage& message : messages_) {
        number = std::max(number, message.number + 1);
    }
    BbsMessage message;
    message.number = number;
    message.author = author.trimmed().isEmpty() ? QStringLiteral("ANONYMOUS") : author.trimmed();
    message.subject = subject.trimmed().isEmpty() ? QStringLiteral("(NO SUBJECT)")
                                                  : subject.trimmed();
    message.body = body;
    message.posted = posted;
    messages_.append(message);
    if (!save()) {
        // Nothing that was not written is kept in memory either: a message the
        // caller believes is posted and the next caller cannot see is worse than a
        // refusal at the moment of posting.
        messages_.removeLast();
        error_ = QStringLiteral("the board could not be written");
        return 0;
    }
    return number;
}

}  // namespace timewarp::ui
