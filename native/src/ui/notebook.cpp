// SPDX-License-Identifier: MIT

#include "timewarp/ui/notebook.hpp"

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>

#include <optional>
#include <string>

#include "timewarp/graphics/recording_host.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/scheme/machine.hpp"
#include "timewarp/ui/gallery.hpp"

namespace timewarp::ui {

namespace {

constexpr auto kFormat = "time-warp-notebook";
constexpr auto kVersion = 1;

[[nodiscard]] QString cellKindName(NotebookCell::Kind kind) {
    return kind == NotebookCell::Kind::Code ? QStringLiteral("code")
                                            : QStringLiteral("prose");
}

}  // namespace

Notebook readNotebook(QIODevice& device, QString* error) {
    Notebook notebook;
    const QByteArray raw = device.readAll();
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(raw, &parse_error);
    if (parse_error.error != QJsonParseError::NoError || !document.isObject()) {
        if (error != nullptr) {
            *error = QStringLiteral("this is not a notebook: %1").arg(parse_error.errorString());
        }
        return notebook;
    }
    const QJsonObject root = document.object();
    if (root.value(QStringLiteral("format")).toString() != QLatin1String{kFormat}) {
        if (error != nullptr) {
            *error = QStringLiteral("this is not a Time Warp notebook");
        }
        return notebook;
    }
    notebook.title = root.value(QStringLiteral("title")).toString();
    for (const QJsonValue& value : root.value(QStringLiteral("cells")).toArray()) {
        const QJsonObject cell = value.toObject();
        NotebookCell entry;
        // An unknown kind is read as prose: showing a person's words unformatted is
        // better than dropping them because a later build invented a third kind.
        entry.kind = cell.value(QStringLiteral("kind")).toString() == QLatin1String("code")
                         ? NotebookCell::Kind::Code
                         : NotebookCell::Kind::Prose;
        entry.language = cell.value(QStringLiteral("language")).toString();
        entry.text = cell.value(QStringLiteral("text")).toString();
        notebook.cells.append(entry);
    }
    return notebook;
}

Notebook readNotebook(const QString& path, QString* error) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return {};
    }
    return readNotebook(file, error);
}

QString notebookToJson(const Notebook& notebook) {
    QJsonObject root;
    root.insert(QStringLiteral("format"), QLatin1String{kFormat});
    root.insert(QStringLiteral("version"), kVersion);
    root.insert(QStringLiteral("title"), notebook.title);
    QJsonArray cells;
    for (const NotebookCell& cell : notebook.cells) {
        QJsonObject entry;
        entry.insert(QStringLiteral("kind"), cellKindName(cell.kind));
        if (cell.kind == NotebookCell::Kind::Code) {
            entry.insert(QStringLiteral("language"), cell.language);
        }
        entry.insert(QStringLiteral("text"), cell.text);
        cells.append(entry);
    }
    root.insert(QStringLiteral("cells"), cells);
    return QString::fromUtf8(
               QJsonDocument{root}.toJson(QJsonDocument::Indented))
        .trimmed() + QLatin1Char{'\n'};
}

bool writeNotebook(const QString& path, const Notebook& notebook, QString* error) {
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    const QByteArray bytes = notebookToJson(notebook).toUtf8();
    if (file.write(bytes) != bytes.size()) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    return true;
}

CellResult runNotebookCell(const QString& language, const QString& source,
                           const QSize& drawingSize) {
    CellResult result;
    const std::optional<lang::Language> parsed =
        lang::language_from_key(language.toStdString());
    if (!parsed.has_value()) {
        result.ok = false;
        result.output = QStringLiteral("\u274C unknown language '%1'").arg(language);
        return result;
    }

    graphics::RecordingHost host;
    std::string output;
    if (*parsed == lang::Language::Scheme) {
        scheme::Arena arena;
        scheme::Machine machine{arena, host};
        output = machine.execute(source.toStdString());
    } else {
        lang::Interpreter interpreter{*parsed, host};
        interpreter.set_output_callback([](const std::string&) {});
        // A notebook cell that asks a question gets no answer rather than a stall:
        // there is nowhere to type one, and the window says so.
        interpreter.set_input_callback([](const std::string&) { return std::string{}; });
        interpreter.load_program(source.toStdString());
        interpreter.run();
        for (const std::string& line : interpreter.output()) {
            output += line;
            output.push_back('\n');
        }
    }
    result.output = QString::fromStdString(output);
    if (host.drew() && drawingSize.isValid()) {
        result.drawing = renderRecording(host, drawingSize);
    }
    return result;
}

}  // namespace timewarp::ui
