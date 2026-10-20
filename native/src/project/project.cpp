// SPDX-License-Identifier: MIT

#include "timewarp/project/project.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QRegularExpression>
#include <QSaveFile>
#include <QSet>

#include <algorithm>

namespace timewarp::project {

namespace {

QString absolute(const QString& path) {
    return QDir::cleanPath(QFileInfo{path}.absoluteFilePath());
}

/// True when `path` is `base` or lies below it. A string prefix test would call
/// `/tmp/project-two` part of `/tmp/project`, so the separator is part of the
/// comparison.
bool isInside(const QString& path, const QString& base) {
    if (base.isEmpty()) {
        return false;
    }
    if (path == base) {
        return true;
    }
    return path.startsWith(base.endsWith(QLatin1Char('/')) ? base : base + QLatin1Char('/'));
}

QString sanitisedFileStem(QString name) {
    name = name.trimmed();
    name.replace(QRegularExpression{QStringLiteral("[\\\\/:*?\"<>|]")}, QStringLiteral("_"));
    name.replace(QRegularExpression{QStringLiteral("\\s+")}, QStringLiteral(" "));
    if (name.isEmpty()) {
        name = QStringLiteral("project");
    }
    return name;
}

}  // namespace

QString openFileFilter() {
    return QStringLiteral("Time Warp projects (*.%1);;All files (*)").arg(
        QLatin1String{kExtension});
}

Project Project::create(const QString& name, const QString& root,
                        const QString& filePath) {
    Project project;
    project.root_ = absolute(root);
    project.name_ = name.trimmed();
    if (project.name_.isEmpty()) {
        project.name_ = QFileInfo{project.root_}.fileName();
    }
    if (project.name_.isEmpty()) {
        project.name_ = QStringLiteral("Untitled Project");
    }
    project.file_path_ =
        filePath.isEmpty()
            ? QDir{project.root_}.filePath(sanitisedFileStem(project.name_) +
                                           QLatin1Char('.') + QLatin1String{kExtension})
            : absolute(filePath);
    return project;
}

bool Project::load(const QString& filePath, QString* error) {
    const QString path = absolute(filePath);
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = QStringLiteral("Cannot read %1: %2").arg(path, file.errorString());
        }
        return false;
    }
    const QByteArray bytes = file.readAll();
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(bytes, &parse_error);
    if (parse_error.error != QJsonParseError::NoError) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 is not valid JSON (offset %2): %3")
                         .arg(path)
                         .arg(parse_error.offset)
                         .arg(parse_error.errorString());
        }
        return false;
    }
    if (!document.isObject()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 does not contain a project object").arg(path);
        }
        return false;
    }
    const QJsonObject object = document.object();
    const int format = object.value(QStringLiteral("format")).toInt(0);
    if (format != kFormatVersion) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 uses project format %2; this build reads %3")
                         .arg(path)
                         .arg(format)
                         .arg(kFormatVersion);
        }
        return false;
    }

    const QString file_dir = QFileInfo{path}.absolutePath();
    QString root = object.value(QStringLiteral("root")).toString();
    root = resolve(root.isEmpty() ? QStringLiteral(".") : root, file_dir);
    if (root.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 has no project root").arg(path);
        }
        return false;
    }

    QString name = object.value(QStringLiteral("name")).toString().trimmed();
    if (name.isEmpty()) {
        name = QFileInfo{path}.completeBaseName();
    }

    QStringList groups;
    const QJsonValue groups_value = object.value(QStringLiteral("groups"));
    if (groups_value.isArray()) {
        for (const QJsonValue& entry : groups_value.toArray()) {
            const QString group = entry.toString().trimmed();
            if (!group.isEmpty() && !groups.contains(group)) {
                groups.append(group);
            }
        }
    }

    QList<ProjectFile> files;
    const QJsonValue files_value = object.value(QStringLiteral("files"));
    if (!files_value.isUndefined() && !files_value.isArray()) {
        if (error != nullptr) {
            *error = QStringLiteral("%1 has a files entry that is not a list").arg(path);
        }
        return false;
    }
    for (const QJsonValue& entry : files_value.toArray()) {
        if (!entry.isObject()) {
            continue;
        }
        const QJsonObject record = entry.toObject();
        const QString stored = record.value(QStringLiteral("path")).toString();
        if (stored.trimmed().isEmpty()) {
            continue;
        }
        const QString member = resolve(stored, root);
        if (member.isEmpty()) {
            continue;
        }
        QString group = record.value(QStringLiteral("group")).toString().trimmed();
        if (!group.isEmpty() && !groups.contains(group)) {
            // A group that was not declared is still a group someone meant. Adding
            // it keeps a hand-edited file usable rather than silently ungrouping
            // its programs.
            groups.append(group);
        }
        const bool duplicate =
            std::any_of(files.cbegin(), files.cend(),
                        [&member](const ProjectFile& existing) {
                            return samePath(existing.path, member);
                        });
        if (!duplicate) {
            files.append(ProjectFile{member, group});
        }
    }

    // Only now, with every field read and validated, replace the object. A
    // failed load leaves the caller's open project exactly as it was.
    name_ = name;
    root_ = root;
    file_path_ = path;
    groups_ = groups;
    files_ = files;
    return true;
}

bool Project::save(QString* error) const {
    if (!isValid()) {
        if (error != nullptr) {
            *error = QStringLiteral("This project has no project file path");
        }
        return false;
    }
    const QFileInfo info{file_path_};
    const QDir parent{info.absolutePath()};
    if (!parent.exists() && !parent.mkpath(QStringLiteral("."))) {
        if (error != nullptr) {
            *error = QStringLiteral("Cannot create %1").arg(info.absolutePath());
        }
        return false;
    }

    QSaveFile file{file_path_};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        if (error != nullptr) {
            *error = QStringLiteral("Cannot write %1: %2").arg(file_path_, file.errorString());
        }
        return false;
    }
    const QByteArray bytes = toJson();
    if (file.write(bytes) != bytes.size()) {
        if (error != nullptr) {
            *error = QStringLiteral("Cannot write %1: %2").arg(file_path_, file.errorString());
        }
        file.cancelWriting();
        return false;
    }
    if (!file.commit()) {
        if (error != nullptr) {
            *error = QStringLiteral("Cannot write %1: %2").arg(file_path_, file.errorString());
        }
        return false;
    }
    return true;
}

void Project::setName(const QString& name) {
    const QString trimmed = name.trimmed();
    if (!trimmed.isEmpty()) {
        name_ = trimmed;
    }
}

bool Project::addGroup(const QString& name) {
    const QString trimmed = name.trimmed();
    if (trimmed.isEmpty() || groups_.contains(trimmed)) {
        return false;
    }
    groups_.append(trimmed);
    return true;
}

bool Project::renameGroup(const QString& from, const QString& to) {
    const QString trimmed = to.trimmed();
    if (!groups_.contains(from) || trimmed.isEmpty() || groups_.contains(trimmed)) {
        return false;
    }
    groups_[groups_.indexOf(from)] = trimmed;
    for (ProjectFile& file : files_) {
        if (file.group == from) {
            file.group = trimmed;
        }
    }
    return true;
}

bool Project::removeGroup(const QString& name) {
    const int index = groups_.indexOf(name);
    if (index < 0) {
        return false;
    }
    groups_.removeAt(index);
    for (ProjectFile& file : files_) {
        if (file.group == name) {
            file.group.clear();
        }
    }
    return true;
}

bool Project::contains(const QString& path) const { return indexOf(path) >= 0; }

QString Project::groupOf(const QString& path) const {
    const int index = indexOf(path);
    return index < 0 ? QString{} : files_.at(index).group;
}

QStringList Project::filesIn(const QString& group) const {
    QStringList result;
    for (const ProjectFile& file : files_) {
        if (file.group == group) {
            result.append(file.path);
        }
    }
    return result;
}

bool Project::addFile(const QString& path, const QString& group) {
    if (path.trimmed().isEmpty()) {
        return false;
    }
    const QString member = absolute(path);
    if (member.isEmpty() || contains(member)) {
        return false;
    }
    QString target = group.trimmed();
    if (!target.isEmpty() && !groups_.contains(target)) {
        groups_.append(target);
    }
    files_.append(ProjectFile{member, target});
    return true;
}

int Project::addFiles(const QStringList& paths, const QString& group) {
    int added = 0;
    for (const QString& path : paths) {
        if (addFile(path, group)) {
            ++added;
        }
    }
    return added;
}

bool Project::removeFile(const QString& path) {
    const int index = indexOf(path);
    if (index < 0) {
        return false;
    }
    files_.removeAt(index);
    return true;
}

bool Project::setGroup(const QString& path, const QString& group) {
    const int index = indexOf(path);
    if (index < 0) {
        return false;
    }
    QString target = group.trimmed();
    if (!target.isEmpty() && !groups_.contains(target)) {
        groups_.append(target);
    }
    if (files_.at(index).group == target) {
        return false;
    }
    files_[index].group = target;
    return true;
}

QStringList Project::missingFiles() const {
    QStringList missing;
    for (const ProjectFile& file : files_) {
        if (!QFileInfo::exists(file.path)) {
            missing.append(file.path);
        }
    }
    return missing;
}

int Project::pruneMissing() {
    const int before = files_.size();
    files_.erase(std::remove_if(files_.begin(), files_.end(),
                                [](const ProjectFile& file) {
                                    return !QFileInfo::exists(file.path);
                                }),
                 files_.end());
    return before - files_.size();
}

QString Project::displayPath(const QString& path) const {
    const QString member = absolute(path);
    if (isInside(member, root_)) {
        const QString relative = QDir{root_}.relativeFilePath(member);
        return relative.isEmpty() ? QStringLiteral(".") : relative;
    }
    return member;
}

QByteArray Project::toJson() const {
    const QString project_dir =
        isValid() ? QFileInfo{file_path_}.absolutePath() : root_;
    QJsonObject object;
    object.insert(QStringLiteral("format"), kFormatVersion);
    object.insert(QStringLiteral("name"), name_);
    object.insert(QStringLiteral("root"), relativeTo(root_, project_dir));

    QJsonArray groups;
    for (const QString& group : groups_) {
        groups.append(group);
    }
    object.insert(QStringLiteral("groups"), groups);

    QJsonArray files;
    for (const ProjectFile& file : files_) {
        QJsonObject record;
        record.insert(QStringLiteral("path"), relativeTo(file.path, root_));
        if (!file.group.isEmpty()) {
            record.insert(QStringLiteral("group"), file.group);
        }
        files.append(record);
    }
    object.insert(QStringLiteral("files"), files);
    return QJsonDocument{object}.toJson(QJsonDocument::Indented);
}

QString Project::relativeTo(const QString& path, const QString& base) {
    const QString member = absolute(path);
    if (base.isEmpty() || !isInside(member, base)) {
        return member;
    }
    const QString relative = QDir{base}.relativeFilePath(member);
    return relative.isEmpty() ? QStringLiteral(".") : relative;
}

QString Project::resolve(const QString& stored, const QString& base) {
    if (stored.isEmpty()) {
        return {};
    }
    const QFileInfo info{stored};
    if (info.isAbsolute()) {
        return QDir::cleanPath(info.absoluteFilePath());
    }
    return QDir::cleanPath(QDir{base}.absoluteFilePath(stored));
}

int Project::indexOf(const QString& path) const {
    const QString member = absolute(path);
    for (int index = 0; index < files_.size(); ++index) {
        if (samePath(files_.at(index).path, member)) {
            return index;
        }
    }
    return -1;
}

bool Project::samePath(const QString& lhs, const QString& rhs) {
    if (lhs == rhs) {
        return true;
    }
    // Case-insensitive filesystems are the reason this is not just `==`. The
    // canonical form also folds symlinks, so `/tmp/p` and `/private/tmp/p` are one
    // file on macOS; when a path does not exist, its absolute clean form is the
    // best answer available.
    const auto canonical = [](const QString& path) {
        const QString resolved = QFileInfo{path}.canonicalFilePath();
        return resolved.isEmpty() ? absolute(path) : resolved;
    };
    return canonical(lhs) == canonical(rhs);
}

}  // namespace timewarp::project
