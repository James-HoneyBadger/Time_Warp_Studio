// SPDX-License-Identifier: MIT

#include "timewarp/ui/command_reference.hpp"

#include "timewarp/core/share_files.hpp"

#include <QComboBox>
#include <QFile>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QTreeWidget>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

/// The table's effect column, in words rather than in the protocol's spelling. The
/// protocol is `prints:TEXT` because a gate compares it; a person wants "prints".
[[nodiscard]] QString humanEffect(const QString& expect) {
    if (expect.startsWith(QLatin1String("prints:"))) {
        return QStringLiteral("prints %1").arg(expect.mid(7));
    }
    if (expect == QLatin1String("draws")) {
        return QStringLiteral("draws on the canvas");
    }
    if (expect == QLatin1String("error")) {
        return QStringLiteral("reports an error");
    }
    if (expect == QLatin1String("optional")) {
        return QStringLiteral("optional: the build may not have it");
    }
    return QStringLiteral("runs");
}

}  // namespace

QString commandRowsPath() {
    return timewarp::core::shareFile("TIME_WARP_COMMANDS", QStringLiteral("command_rows.json"));
}

QList<CommandRow> loadCommandRows(const QString& path, QString* error) {
    QList<CommandRow> rows;
    if (error != nullptr) {
        error->clear();
    }
    QFile file{path};
    if (path.isEmpty() || !file.open(QIODevice::ReadOnly)) {
        if (error != nullptr) {
            *error = QStringLiteral("The command table was not found.");
        }
        return rows;
    }
    QJsonParseError parse_error{};
    const QJsonDocument document = QJsonDocument::fromJson(file.readAll(), &parse_error);
    if (!document.isObject()) {
        if (error != nullptr) {
            *error = QStringLiteral("The command table could not be read: %1")
                         .arg(parse_error.errorString());
        }
        return rows;
    }
    const QJsonArray array = document.object().value(QStringLiteral("rows")).toArray();
    for (const QJsonValue& entry : array) {
        const QJsonObject object = entry.toObject();
        CommandRow row;
        row.language = object.value(QStringLiteral("language")).toString();
        row.command = object.value(QStringLiteral("command")).toString();
        row.source = object.value(QStringLiteral("source")).toString();
        row.expect = object.value(QStringLiteral("expect")).toString();
        // A row with no command or no program is not a row: the table's own rules
        // say a command is shown with the smallest program that exercises it, and a
        // reference entry nobody can try is a line in a table, not a command.
        if (row.command.isEmpty() || row.source.isEmpty()) {
            continue;
        }
        rows.append(row);
    }
    return rows;
}

CommandReferencePanel::CommandReferencePanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("commandReferencePanel"));
    setFrameShape(QFrame::StyledPanel);
    buildUi();
}

void CommandReferencePanel::buildUi() {
    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    auto* top = new QHBoxLayout{};
    top->setSpacing(6);
    language_ = new QComboBox{this};
    language_->setObjectName(QStringLiteral("commandReferenceLanguage"));
    language_->setAccessibleName(tr("Language"));
    for (const Language language : allLanguages()) {
        language_->addItem(displayName(language), settingsKey(language));
    }
    top->addWidget(language_);

    filter_ = new QLineEdit{this};
    filter_->setAccessibleName(tr("Filter commands"));
    filter_->setObjectName(QStringLiteral("commandReferenceFilter"));
    filter_->setPlaceholderText(tr("Filter commands\\u2026"));
    filter_->setClearButtonEnabled(true);
    top->addWidget(filter_, 1);
    layout->addLayout(top);

    list_ = new QTreeWidget{this};
    list_->setAccessibleName(tr("Commands"));
    list_->setObjectName(QStringLiteral("commandReferenceList"));
    list_->setColumnCount(2);
    list_->setHeaderLabels({tr("Command"), tr("What it does")});
    list_->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    list_->header()->setSectionResizeMode(1, QHeaderView::Stretch);
    list_->setRootIsDecorated(false);
    list_->setAlternatingRowColors(true);
    layout->addWidget(list_, 1);

    effect_ = new QLabel{this};
    effect_->setObjectName(QStringLiteral("commandReferenceEffect"));
    effect_->setWordWrap(true);
    layout->addWidget(effect_);

    detail_ = new QPlainTextEdit{this};
    detail_->setObjectName(QStringLiteral("commandReferenceSource"));
    detail_->setReadOnly(true);
    detail_->setLineWrapMode(QPlainTextEdit::NoWrap);
    detail_->setMaximumHeight(96);
    detail_->setAccessibleName(tr("The smallest program that runs it"));
    layout->addWidget(detail_);

    auto* bottom = new QHBoxLayout{};
    status_ = new QLabel{this};
    status_->setObjectName(QStringLiteral("commandReferenceStatus"));
    status_->setWordWrap(true);
    status_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    bottom->addWidget(status_, 1);
    try_ = new QPushButton{tr("Try it"), this};
    try_->setObjectName(QStringLiteral("commandReferenceTry"));
    try_->setToolTip(tr("Put this program in a new document. Nothing runs until you press Run"));
    try_->setEnabled(false);
    bottom->addWidget(try_);
    layout->addLayout(bottom);

    connect(language_, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
            [this] { rebuild(); });
    connect(filter_, &QLineEdit::textChanged, this, [this] { rebuild(); });
    connect(list_, &QTreeWidget::currentItemChanged, this,
            [this](QTreeWidgetItem*, QTreeWidgetItem*) { updateDetail(); });
    connect(list_, &QTreeWidget::itemActivated, this,
            [this](QTreeWidgetItem*, int) { chooseTryIt(); });
    connect(try_, &QPushButton::clicked, this, &CommandReferencePanel::chooseTryIt);
}

void CommandReferencePanel::ensureLoaded() {
    if (loaded_) {
        return;
    }
    loaded_ = true;
    rows_ = loadCommandRows(commandRowsPath(), &load_error_);
    if (!load_error_.isEmpty()) {
        status_->setText(tr("\\u274C %1").arg(load_error_));
        list_->setEnabled(false);
        return;
    }
    rebuild();
}

void CommandReferencePanel::setLanguage(Language language) {
    ensureLoaded();
    const int index = language_->findData(settingsKey(language));
    if (index >= 0 && index != language_->currentIndex()) {
        language_->setCurrentIndex(index);
    } else {
        rebuild();
    }
}

Language CommandReferencePanel::language() const {
    const QString key = language_ != nullptr ? language_->currentData().toString() : QString{};
    const Language language = languageFromKey(key);
    return language == Language::Unknown ? defaultLanguage() : language;
}

void CommandReferencePanel::setFilter(const QString& text) {
    ensureLoaded();
    filter_->setText(text);
}

QString CommandReferencePanel::selectedSource() const {
    const QTreeWidgetItem* item = list_ != nullptr ? list_->currentItem() : nullptr;
    return item != nullptr ? item->data(0, Qt::UserRole).toString() : QString{};
}

QString CommandReferencePanel::statusText() const {
    return status_ != nullptr ? status_->text() : QString{};
}

int CommandReferencePanel::rowCount() const {
    int count = 0;
    for (const CommandRow& row : rows_) {
        if (row.language.compare(language_->currentData().toString(), Qt::CaseInsensitive) ==
            0) {
            ++count;
        }
    }
    return count;
}

int CommandReferencePanel::visibleRowCount() const {
    return list_ != nullptr ? list_->topLevelItemCount() : 0;
}

void CommandReferencePanel::rebuild() {
    if (!loaded_ || list_ == nullptr) {
        return;
    }
    const QString key = language_->currentData().toString();
    const QString needle = filter_->text().trimmed();
    list_->clear();
    int shown = 0;
    for (const CommandRow& row : rows_) {
        if (row.language.compare(key, Qt::CaseInsensitive) != 0) {
            continue;
        }
        if (!needle.isEmpty() && !row.command.contains(needle, Qt::CaseInsensitive) &&
            !row.source.contains(needle, Qt::CaseInsensitive) &&
            !row.expect.contains(needle, Qt::CaseInsensitive)) {
            continue;
        }
        auto* item = new QTreeWidgetItem{list_};
        item->setText(0, row.command);
        item->setText(1, humanEffect(row.expect));
        item->setData(0, Qt::UserRole, row.source);
        item->setData(1, Qt::UserRole, row.expect);
        item->setToolTip(0, row.source);
        ++shown;
    }
    status_->setText(shown == 0
                         ? (needle.isEmpty()
                                ? tr("No commands for %1 in this table.")
                                      .arg(language_->currentText())
                                : tr("No command matches \\u201c%1\\u201d.").arg(needle))
                         : tr("%1 command(s) for %2").arg(shown).arg(language_->currentText()));
    if (shown > 0) {
        list_->setCurrentItem(list_->topLevelItem(0));
    } else {
        updateDetail();
    }
}

void CommandReferencePanel::updateDetail() {
    const QString source = selectedSource();
    detail_->setPlainText(source);
    const QTreeWidgetItem* item = list_->currentItem();
    effect_->setText(item != nullptr ? humanEffect(item->data(1, Qt::UserRole).toString())
                                     : QString{});
    try_->setEnabled(!source.isEmpty());
}

void CommandReferencePanel::chooseTryIt() {
    const QString source = selectedSource();
    if (source.isEmpty()) {
        return;
    }
    const QTreeWidgetItem* item = list_->currentItem();
    const QString name = item != nullptr ? item->text(0) : QString{};
    emit tryRequested(language_->currentData().toString(), source, name);
    emit statusMessage(tr("Put \\u201c%1\\u201d in a new document").arg(name));
}

}  // namespace timewarp::ui
