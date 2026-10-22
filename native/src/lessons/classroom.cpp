// SPDX-License-Identifier: MIT
//
// The classroom. See `classroom.hpp` for the layout, the bundle format and what
// marking deliberately does not touch.

#include "timewarp/lessons/classroom.hpp"

#include <algorithm>
#include <cstddef>
#include <map>
#include <utility>

#include <QDateTime>
#include <QDir>
#include <QDirIterator>
#include <QFile>
#include <QFileInfo>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QSaveFile>
#include <QStandardPaths>

#include <zlib.h>

#include "timewarp/lessons/progress.hpp"
#include "timewarp/lessons/verify.hpp"

namespace timewarp::lessons {
namespace {

// ---------------------------------------------------------------------------
// Zip
// ---------------------------------------------------------------------------
//
// A bundle is a plain zip: a class handed to a school has to open with the tools
// they already have, and `teach.py` writes the same thing. The reader accepts
// both methods a real archive uses -- stored and deflated -- because a bundle may
// have been made by either side of this port.

void put16(QByteArray& out, quint16 value) {
    out.append(static_cast<char>(value & 0xFF));
    out.append(static_cast<char>((value >> 8) & 0xFF));
}

void put32(QByteArray& out, quint32 value) {
    for (int shift = 0; shift < 32; shift += 8) {
        out.append(static_cast<char>((value >> shift) & 0xFF));
    }
}

[[nodiscard]] quint16 read16(const QByteArray& bytes, int offset) {
    return static_cast<quint16>(static_cast<unsigned char>(bytes.at(offset)) |
                                (static_cast<unsigned char>(bytes.at(offset + 1)) << 8));
}

[[nodiscard]] quint32 read32(const QByteArray& bytes, int offset) {
    quint32 value = 0;
    for (int index = 3; index >= 0; --index) {
        value = (value << 8) | static_cast<unsigned char>(bytes.at(offset + index));
    }
    return value;
}

[[nodiscard]] QByteArray deflateRaw(const QByteArray& data, bool* ok) {
    z_stream stream{};
    if (deflateInit2(&stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, 8,
                     Z_DEFAULT_STRATEGY) != Z_OK) {
        *ok = false;
        return {};
    }
    QByteArray out;
    out.resize(static_cast<int>(deflateBound(&stream, static_cast<uLong>(data.size()))) + 16);
    stream.next_in = reinterpret_cast<Bytef*>(const_cast<char*>(data.constData()));
    stream.avail_in = static_cast<uInt>(data.size());
    stream.next_out = reinterpret_cast<Bytef*>(out.data());
    stream.avail_out = static_cast<uInt>(out.size());
    const int result = deflate(&stream, Z_FINISH);
    *ok = result == Z_STREAM_END;
    out.resize(static_cast<int>(stream.total_out));
    deflateEnd(&stream);
    return *ok ? out : QByteArray{};
}

[[nodiscard]] QByteArray inflateRaw(const QByteArray& data, quint32 expected, bool* ok) {
    z_stream stream{};
    if (inflateInit2(&stream, -MAX_WBITS) != Z_OK) {
        *ok = false;
        return {};
    }
    QByteArray out;
    out.resize(static_cast<int>(expected) + 1);
    stream.next_in = reinterpret_cast<Bytef*>(const_cast<char*>(data.constData()));
    stream.avail_in = static_cast<uInt>(data.size());
    stream.next_out = reinterpret_cast<Bytef*>(out.data());
    stream.avail_out = static_cast<uInt>(out.size());
    const int result = inflate(&stream, Z_FINISH);
    *ok = result == Z_STREAM_END;
    out.resize(static_cast<int>(stream.total_out));
    inflateEnd(&stream);
    return *ok ? out : QByteArray{};
}

struct ZipEntry {
    QByteArray name;
    QByteArray data;
};

[[nodiscard]] bool writeZip(const QString& path, const std::vector<ZipEntry>& entries,
                            QString* error) {
    struct Record {
        QByteArray name;
        quint32 crc = 0;
        quint32 compressed = 0;
        quint32 size = 0;
        quint32 offset = 0;
        quint16 method = 0;
    };
    QByteArray out;
    std::vector<Record> records;
    for (const ZipEntry& entry : entries) {
        Record record;
        record.name = entry.name;
        record.size = static_cast<quint32>(entry.data.size());
        record.crc = static_cast<quint32>(
            crc32(0L, reinterpret_cast<const Bytef*>(entry.data.constData()),
                  static_cast<uInt>(entry.data.size())));
        QByteArray body = entry.data;
        record.method = 0;
        if (!entry.data.isEmpty()) {
            bool ok = false;
            const QByteArray compressed = deflateRaw(entry.data, &ok);
            if (ok && compressed.size() < entry.data.size()) {
                body = compressed;
                record.method = 8;
            }
        }
        record.compressed = static_cast<quint32>(body.size());
        record.offset = static_cast<quint32>(out.size());
        put32(out, 0x04034b50u);
        put16(out, 20);
        put16(out, 0);
        put16(out, record.method);
        put16(out, 0);  // time: a class's files are dated by the filesystem
        put16(out, 0);  // date
        put32(out, record.crc);
        put32(out, record.compressed);
        put32(out, record.size);
        put16(out, static_cast<quint16>(record.name.size()));
        put16(out, 0);
        out.append(record.name);
        out.append(body);
        records.push_back(record);
    }

    const int directory_offset = out.size();
    for (const Record& record : records) {
        put32(out, 0x02014b50u);
        put16(out, 20);
        put16(out, 20);
        put16(out, 0);
        put16(out, record.method);
        put16(out, 0);
        put16(out, 0);
        put32(out, record.crc);
        put32(out, record.compressed);
        put32(out, record.size);
        put16(out, static_cast<quint16>(record.name.size()));
        put16(out, 0);
        put16(out, 0);
        put16(out, 0);
        put16(out, 0);
        put32(out, 0100644u << 16);  // a regular file, readable and writable
        put32(out, record.offset);
        out.append(record.name);
    }
    const int directory_size = out.size() - directory_offset;
    put32(out, 0x06054b50u);
    put16(out, 0);
    put16(out, 0);
    put16(out, static_cast<quint16>(records.size()));
    put16(out, static_cast<quint16>(records.size()));
    put32(out, static_cast<quint32>(directory_size));
    put32(out, static_cast<quint32>(directory_offset));
    put16(out, 0);

    const QFileInfo info{path};
    if (!QDir{}.mkpath(info.absolutePath())) {
        if (error != nullptr) {
            *error = QStringLiteral("cannot create %1").arg(info.absolutePath());
        }
        return false;
    }
    QFile file{path};
    if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    if (file.write(out) != out.size()) {
        if (error != nullptr) {
            *error = QStringLiteral("the bundle could not be written in full");
        }
        return false;
    }
    return true;
}

[[nodiscard]] bool readZip(const QString& path, std::vector<ZipEntry>* entries,
                           QString* error) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    const QByteArray bytes = file.readAll();
    if (bytes.size() < 22) {
        if (error != nullptr) {
            *error = QStringLiteral("this is not a bundle: it is too short");
        }
        return false;
    }
    // The end-of-central-directory record is at the end, after an optional
    // comment, so it is searched for backwards.
    int eocd = -1;
    const int lowest = std::max(0, static_cast<int>(bytes.size()) - 65557);
    for (int index = static_cast<int>(bytes.size()) - 22; index >= lowest; --index) {
        if (read32(bytes, index) == 0x06054b50u) {
            eocd = index;
            break;
        }
    }
    if (eocd < 0) {
        if (error != nullptr) {
            *error = QStringLiteral("this is not a bundle: no central directory");
        }
        return false;
    }
    const quint16 count = read16(bytes, eocd + 10);
    const quint32 directory_offset = read32(bytes, eocd + 16);
    int cursor = static_cast<int>(directory_offset);
    for (quint16 index = 0; index < count; ++index) {
        if (cursor + 46 > bytes.size() || read32(bytes, cursor) != 0x02014b50u) {
            if (error != nullptr) {
                *error = QStringLiteral("the bundle's central directory is damaged");
            }
            return false;
        }
        const quint16 method = read16(bytes, cursor + 10);
        const quint32 compressed = read32(bytes, cursor + 20);
        const quint32 size = read32(bytes, cursor + 24);
        const quint16 name_length = read16(bytes, cursor + 28);
        const quint16 extra_length = read16(bytes, cursor + 30);
        const quint16 comment_length = read16(bytes, cursor + 32);
        const quint32 local_offset = read32(bytes, cursor + 42);
        if (cursor + 46 + name_length > bytes.size()) {
            if (error != nullptr) {
                *error = QStringLiteral("the bundle's central directory is damaged");
            }
            return false;
        }
        ZipEntry entry;
        entry.name = bytes.mid(cursor + 46, name_length);
        cursor += 46 + name_length + extra_length + comment_length;

        const int local = static_cast<int>(local_offset);
        if (local + 30 > bytes.size() || read32(bytes, local) != 0x04034b50u) {
            if (error != nullptr) {
                *error = QStringLiteral("the bundle has a broken member header");
            }
            return false;
        }
        const quint16 local_name = read16(bytes, local + 26);
        const quint16 local_extra = read16(bytes, local + 28);
        const int data_at = local + 30 + local_name + local_extra;
        if (data_at + static_cast<int>(compressed) > bytes.size()) {
            if (error != nullptr) {
                *error = QStringLiteral("the bundle ends inside a member");
            }
            return false;
        }
        const QByteArray body = bytes.mid(data_at, static_cast<int>(compressed));
        if (method == 0) {
            entry.data = body;
        } else if (method == 8) {
            bool ok = false;
            entry.data = inflateRaw(body, size, &ok);
            if (!ok) {
                if (error != nullptr) {
                    *error = QStringLiteral("a compressed member could not be read");
                }
                return false;
            }
        } else {
            if (error != nullptr) {
                *error = QStringLiteral("the bundle uses a compression method "
                                        "this build does not read");
            }
            return false;
        }
        entries->push_back(std::move(entry));
    }
    return true;
}

// ---------------------------------------------------------------------------
// Files
// ---------------------------------------------------------------------------

[[nodiscard]] QByteArray jsonBytes(const QJsonObject& object) {
    return QJsonDocument{object}.toJson(QJsonDocument::Indented);
}

[[nodiscard]] QJsonObject readObject(const QString& path) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        return {};
    }
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll());
    return document.isObject() ? document.object() : QJsonObject{};
}

[[nodiscard]] bool writeObject(const QString& path, const QJsonObject& object,
                               QString* error) {
    const QFileInfo info{path};
    if (!QDir{}.mkpath(info.absolutePath())) {
        if (error != nullptr) {
            *error = QStringLiteral("cannot create %1").arg(info.absolutePath());
        }
        return false;
    }
    QSaveFile file{path};
    if (!file.open(QIODevice::WriteOnly)) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    file.write(jsonBytes(object));
    if (!file.commit()) {
        if (error != nullptr) {
            *error = file.errorString();
        }
        return false;
    }
    return true;
}

}  // namespace

QString Classroom::default_root() {
    const QString override = qEnvironmentVariable("TIME_WARP_DATA_DIR").trimmed();
    const QString base = override.isEmpty()
                             ? QStandardPaths::writableLocation(QStandardPaths::HomeLocation) +
                                   QStringLiteral("/.time_warp")
                             : override;
    return base + QStringLiteral("/classroom");
}

Classroom::Classroom(QString root) : root_(root.isEmpty() ? default_root() : std::move(root)) {}

QString Classroom::safeName(const QString& text) const {
    QString cleaned;
    for (const QChar character : text.trimmed()) {
        cleaned += (character.isLetterOrNumber() || character == u'-' || character == u'_')
                       ? character
                       : u'-';
    }
    // Runs of dashes collapse, and an empty result still names a directory.
    QString collapsed;
    for (const QChar character : cleaned) {
        if (character == u'-' && collapsed.endsWith(u'-')) {
            continue;
        }
        collapsed += character;
    }
    while (collapsed.startsWith(u'-')) {
        collapsed.remove(0, 1);
    }
    while (collapsed.endsWith(u'-')) {
        collapsed.chop(1);
    }
    return collapsed.isEmpty() ? QStringLiteral("student") : collapsed;
}

bool Classroom::load(QString* error) {
    const QString curriculum = default_curriculum_path();
    if (curriculum.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("the curriculum was not found");
        }
        return false;
    }
    if (!load_curriculum(curriculum, lessons_, error)) {
        return false;
    }
    readAll();
    return true;
}

void Classroom::readAll() {
    students_.clear();
    const QJsonObject roster = readObject(root_ + QStringLiteral("/roster.json"));
    for (const QJsonValue& value : roster.value(QStringLiteral("students")).toArray()) {
        const QJsonObject raw = value.toObject();
        Student student;
        student.id = raw.value(QStringLiteral("id")).toString();
        student.name = raw.value(QStringLiteral("name")).toString();
        student.workDir = raw.value(QStringLiteral("work_dir")).toString();
        if (!student.id.isEmpty()) {
            students_.push_back(std::move(student));
        }
    }

    const QJsonObject assignment = readObject(root_ + QStringLiteral("/assignment.json"));
    assignment_.lessonIds.clear();
    for (const QJsonValue& value : assignment.value(QStringLiteral("lessons")).toArray()) {
        assignment_.lessonIds << value.toString();
    }
    assignment_.note = assignment.value(QStringLiteral("note")).toString();
    assignment_.created = assignment.value(QStringLiteral("created")).toString();

    const QJsonObject presentation = readObject(root_ + QStringLiteral("/presentation.json"));
    presentation_.fontSize = std::clamp(
        presentation.value(QStringLiteral("font_size")).toInt(PresentationSettings::kDefaultFont),
        PresentationSettings::kMinFont, PresentationSettings::kMaxFont);
    presentation_.fullscreen =
        presentation.value(QStringLiteral("fullscreen")).toBool(true);
}

const Lesson* Classroom::find(const QString& id) const {
    return find_lesson(lessons_, id.toStdString());
}

bool Classroom::saveRoster(QString* error) const {
    QJsonArray array;
    for (const Student& student : students_) {
        QJsonObject object;
        object.insert(QStringLiteral("id"), student.id);
        object.insert(QStringLiteral("name"), student.name);
        object.insert(QStringLiteral("work_dir"), student.workDir);
        array.append(object);
    }
    QJsonObject payload;
    payload.insert(QStringLiteral("students"), array);
    return writeObject(root_ + QStringLiteral("/roster.json"), payload, error);
}

bool Classroom::saveAssignment(QString* error) const {
    QJsonArray lessons;
    for (const QString& id : assignment_.lessonIds) {
        lessons.append(id);
    }
    QJsonObject payload;
    payload.insert(QStringLiteral("lessons"), lessons);
    payload.insert(QStringLiteral("note"), assignment_.note);
    payload.insert(QStringLiteral("created"), assignment_.created);
    return writeObject(root_ + QStringLiteral("/assignment.json"), payload, error);
}

bool Classroom::savePresentation(QString* error) const {
    QJsonObject payload;
    payload.insert(QStringLiteral("font_size"), presentation_.fontSize);
    payload.insert(QStringLiteral("fullscreen"), presentation_.fullscreen);
    return writeObject(root_ + QStringLiteral("/presentation.json"), payload, error);
}

bool Classroom::addStudent(const QString& name, const QString& id, const QString& workDir,
                           QString* error) {
    const QString clean = name.trimmed();
    if (clean.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("a student needs a name");
        }
        return false;
    }
    QString identifier = id.trimmed().toLower();
    if (identifier.isEmpty()) {
        identifier = clean.toLower();
        identifier.replace(u' ', u'-');
    }
    for (const Student& student : students_) {
        if (student.id == identifier) {
            if (error != nullptr) {
                *error = QStringLiteral("there is already a student with the id %1")
                             .arg(identifier);
            }
            return false;
        }
    }
    students_.push_back(Student{identifier, clean, workDir});
    return saveRoster(error);
}

bool Classroom::removeStudent(const QString& id) {
    const auto before = students_.size();
    students_.erase(std::remove_if(students_.begin(), students_.end(),
                                   [&](const Student& student) { return student.id == id; }),
                    students_.end());
    if (students_.size() == before) {
        return false;
    }
    (void)saveRoster();  // an administrative change; a failed write is retried
    return true;
}

QString Classroom::studentDirectory(const QString& id) const {
    for (const Student& student : students_) {
        if (student.id != id) {
            continue;
        }
        if (!student.workDir.isEmpty()) {
            return QDir::cleanPath(student.workDir);
        }
        break;
    }
    return root_ + QStringLiteral("/students/") + safeName(id);
}

bool Classroom::assign(const QStringList& lessonIds, const QString& note, QString* error) {
    for (const QString& id : lessonIds) {
        if (find(id) == nullptr) {
            if (error != nullptr) {
                *error = QStringLiteral("no lesson with the id %1").arg(id);
            }
            return false;
        }
    }
    assignment_.lessonIds = lessonIds;
    assignment_.note = note;
    assignment_.created = QDateTime::currentDateTimeUtc().toString(Qt::ISODate);
    return saveAssignment(error);
}

QString Classroom::handoutName(const Lesson& lesson, int index) const {
    return QString::fromStdString(lesson.id) + QStringLiteral("-") +
           QString::number(index + 1) +
           QString::fromStdString(std::string{extension_for(lesson.language)});
}

QStringList Classroom::handoutNames(const Lesson& lesson) const {
    QStringList names;
    for (int index = 0; index < static_cast<int>(lesson.checkpoints.size()); ++index) {
        names << handoutName(lesson, index);
    }
    return names;
}

QStringList Classroom::distribute(const QStringList& lessonIds, QString* error) {
    const QStringList ids = lessonIds.isEmpty() ? assignment_.lessonIds : lessonIds;
    if (ids.isEmpty()) {
        if (error != nullptr) {
            *error = QStringLiteral("nothing has been assigned");
        }
        return {};
    }
    if (students_.empty()) {
        if (error != nullptr) {
            *error = QStringLiteral("there are no students to hand work to");
        }
        return {};
    }
    QStringList written;
    for (const Student& student : students_) {
        const QString directory = studentDirectory(student.id);
        for (const QString& id : ids) {
            const Lesson* lesson = find(id);
            if (lesson == nullptr) {
                continue;
            }
            for (int index = 0; index < static_cast<int>(lesson->checkpoints.size()); ++index) {
                const QString path = directory + QStringLiteral("/") + handoutName(*lesson, index);
                if (QFileInfo::exists(path)) {
                    continue;  // a learner's own work is never overwritten
                }
                if (!QDir{}.mkpath(directory)) {
                    continue;
                }
                QFile file{path};
                if (!file.open(QIODevice::WriteOnly)) {
                    continue;
                }
                file.write(QByteArray::fromStdString(
                    lesson->checkpoints[static_cast<std::size_t>(index)].starter_code));
                file.close();
                written << path;
            }
        }
    }
    return written;
}

QList<Submission> Classroom::collect(const QStringList& lessonIds) {
    const QStringList ids = lessonIds.isEmpty() ? assignment_.lessonIds : lessonIds;
    QList<Submission> found;
    for (const Student& student : students_) {
        const QString directory = studentDirectory(student.id);
        for (const QString& id : ids) {
            const Lesson* lesson = find(id);
            if (lesson == nullptr) {
                continue;
            }
            for (int index = 0; index < static_cast<int>(lesson->checkpoints.size()); ++index) {
                const QString path = directory + QStringLiteral("/") + handoutName(*lesson, index);
                if (!QFileInfo{path}.isFile()) {
                    continue;  // not handed in is not the same as wrong
                }
                QFile file{path};
                if (!file.open(QIODevice::ReadOnly)) {
                    continue;
                }
                // Read once: a learner still editing must not be able to change a
                // mark underneath the teacher reading it.
                const std::string source = file.readAll().toStdString();
                file.close();
                const VerifyResult result = verify_checkpoint(
                    lesson->checkpoints[static_cast<std::size_t>(index)], lesson->language,
                    source);
                Submission submission;
                submission.studentId = student.id;
                submission.lessonId = QString::fromStdString(lesson->id);
                submission.checkpoint = index;
                submission.path = path;
                submission.passed = result.passed;
                submission.message = QString::fromStdString(result.message);
                found.append(submission);
            }
        }
    }
    return found;
}

QList<MarkRow> Classroom::lessonMarks(const QList<Submission>& submissions) const {
    std::map<std::pair<QString, QString>, QList<Submission>> grouped;
    for (const Submission& submission : submissions) {
        grouped[{submission.studentId, submission.lessonId}].append(submission);
    }
    QList<MarkRow> rows;
    for (const auto& [key, items] : grouped) {
        const Lesson* lesson = find(key.second);
        if (lesson == nullptr) {
            continue;
        }
        MarkRow row;
        row.student = key.first;
        row.lesson = key.second;
        row.title = QString::fromStdString(lesson->title);
        row.handedIn = static_cast<int>(items.size());
        row.checkpoints = static_cast<int>(lesson->checkpoints.size());
        row.passed = 0;
        for (const Submission& item : items) {
            row.passed += item.passed ? 1 : 0;
        }
        row.complete = row.handedIn == row.checkpoints && row.passed == row.checkpoints;
        rows.append(row);
    }
    return rows;
}

QList<ClassProgressRow> Classroom::classProgress() const {
    QList<ClassProgressRow> rows;
    for (const Student& student : students_) {
        ProgressStore store;
        QString error;
        ClassProgressRow row;
        row.id = student.id;
        row.name = student.name;
        row.lessons = static_cast<int>(lessons_.size());
        if (store.load(default_curriculum_path(),
                       studentDirectory(student.id) + QStringLiteral("/progress.json"),
                       &error)) {
            for (const Lesson& lesson : lessons_) {
                switch (store.status(lesson.id)) {
                    case LessonStatus::Completed:
                        ++row.completed;
                        ++row.started;
                        break;
                    case LessonStatus::InProgress:
                        ++row.started;
                        break;
                    case LessonStatus::NotStarted:
                        break;
                }
                row.hintsUsed += store.hints_used(lesson.id);
            }
        }
        rows.append(row);
    }
    return rows;
}

bool Classroom::createBundle(const QString& outputPath, QString* error) const {
    std::vector<ZipEntry> entries;
    for (const char* name : {"roster.json", "assignment.json", "presentation.json"}) {
        QFile file{root_ + QStringLiteral("/") + QLatin1String{name}};
        if (file.open(QIODevice::ReadOnly)) {
            entries.push_back(ZipEntry{QByteArray{name}, file.readAll()});
        }
    }
    for (const Student& student : students_) {
        const QString directory = studentDirectory(student.id);
        QDirIterator iterator{directory, QDir::Files, QDirIterator::Subdirectories};
        while (iterator.hasNext()) {
            const QString path = iterator.next();
            // The bundle names members relative to the classroom, the way the
            // reference writes it, so an unpacked bundle is the same tree.
            const QString relative = QDir{root_}.relativeFilePath(path);
            if (relative.startsWith(QStringLiteral(".."))) {
                continue;  // a work directory outside the classroom is not bundled
            }
            QFile file{path};
            if (file.open(QIODevice::ReadOnly)) {
                entries.push_back(ZipEntry{relative.toUtf8(), file.readAll()});
            }
        }
    }
    return writeZip(outputPath, entries, error);
}

bool Classroom::importBundle(const QString& bundlePath, bool replace, QString* error) {
    std::vector<ZipEntry> entries;
    if (!readZip(bundlePath, &entries, error)) {
        return false;
    }
    const QString root = QDir{root_}.absolutePath();
    for (const ZipEntry& entry : entries) {
        const QString name = QString::fromUtf8(entry.name);
        // `extract`-style readers will happily write outside the destination, so
        // the target is resolved and checked before anything is written.
        const QString target = QDir::cleanPath(root + QStringLiteral("/") + name);
        if (!target.startsWith(root + QStringLiteral("/"))) {
            if (error != nullptr) {
                *error = QStringLiteral("the bundle contains a path outside itself: %1")
                             .arg(name);
            }
            return false;
        }
        if (QFileInfo::exists(target) && !replace) {
            continue;
        }
        if (!QDir{}.mkpath(QFileInfo{target}.absolutePath())) {
            continue;
        }
        QFile file{target};
        if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
            if (error != nullptr) {
                *error = file.errorString();
            }
            return false;
        }
        file.write(entry.data);
    }
    readAll();
    (void)saveRoster(error);
    return true;
}

void Classroom::setPresentation(int fontSize, bool fullscreen) {
    presentation_.fontSize =
        std::clamp(fontSize, PresentationSettings::kMinFont, PresentationSettings::kMaxFont);
    presentation_.fullscreen = fullscreen;
    (void)savePresentation();  // a failed write is retried on the next change
}

}  // namespace timewarp::lessons
