// SPDX-License-Identifier: MIT

#include "timewarp/ui/panels.hpp"

#include "timewarp/core/qt_compat.hpp"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <numbers>
#include <utility>

#include <QComboBox>
#include <QDialogButtonBox>
#include <QEvent>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QPainter>
#include <QPainterPath>
#include <QProgressBar>
#include <QPushButton>
#include <QRadialGradient>
#include <QResizeEvent>
#include <QSlider>
#include <QSpinBox>
#include <QTableWidget>
#include <QTextBrowser>
#include <QTimer>
#include <QVector>
#include <QVBoxLayout>

namespace timewarp::ui {

// ---------------------------------------------------------------------------
// PlaceholderPanel
// ---------------------------------------------------------------------------

PlaceholderPanel::PlaceholderPanel(const QString& title, const QString& standsFor,
                                   const QSize& minimum, QWidget* parent)
    : QFrame(parent), minimum_(minimum) {
    setObjectName(QStringLiteral("placeholderPanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(4);

    auto* heading = new QLabel{title, this};
    QFont headingFont = heading->font();
    headingFont.setBold(true);
    heading->setFont(headingFont);
    heading->setAlignment(Qt::AlignCenter);
    layout->addWidget(heading);

    auto* detail = new QLabel{standsFor, this};
    QFont detailFont = detail->font();
    detailFont.setItalic(true);
    detailFont.setPointSizeF(detailFont.pointSizeF() * 0.9);
    detail->setFont(detailFont);
    detail->setAlignment(Qt::AlignCenter);
    detail->setWordWrap(true);
    layout->addWidget(detail);

    layout->addStretch(1);
}

QSize PlaceholderPanel::minimumSizeHint() const { return minimum_; }

// ---------------------------------------------------------------------------
// OutlinePanel
// ---------------------------------------------------------------------------

OutlinePanel::OutlinePanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("outlinePanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(2, 2, 2, 2);
    layout->setSpacing(0);

    list_ = new QListWidget{this};
    list_->setObjectName(QStringLiteral("outlineList"));
    list_->setAccessibleName(tr("Document outline"));
    list_->setFrameShape(QFrame::NoFrame);
    // One click jumps: an outline is a navigation control, and asking a user to
    // double-click a list that looks like a list is a wasted gesture.
    connect(list_, &QListWidget::itemClicked, this, [this](QListWidgetItem* item) {
        if (item != nullptr) {
            emit entryActivated(item->data(Qt::UserRole).toInt());
        }
    });
    connect(list_, &QListWidget::itemActivated, this, [this](QListWidgetItem* item) {
        if (item != nullptr) {
            emit entryActivated(item->data(Qt::UserRole).toInt());
        }
    });
    layout->addWidget(list_, 1);

    empty_ = new QLabel{tr("No procedures, functions or labels in this program."), this};
    empty_->setObjectName(QStringLiteral("outlineEmptyState"));
    empty_->setAlignment(Qt::AlignCenter);
    empty_->setWordWrap(true);
    empty_->setEnabled(false);
    layout->addWidget(empty_, 1);

    setEntries({});
}

void OutlinePanel::setEntries(const QList<timewarp::ui::OutlineEntry>& entries) {
    list_->clear();
    for (const timewarp::ui::OutlineEntry& entry : entries) {
        auto* item = new QListWidgetItem{entry.name, list_};
        item->setData(Qt::UserRole, entry.line);
        item->setToolTip(tr("%1 \u2014 line %2")
                             .arg(timewarp::ui::outlineKindName(entry.kind))
                             .arg(entry.line));
    }
    const bool empty = entries.isEmpty();
    list_->setVisible(!empty);
    empty_->setVisible(empty);
}

int OutlinePanel::entryCount() const { return list_->count(); }

int OutlinePanel::lineAt(int index) const {
    const QListWidgetItem* item = list_->item(index);
    return item == nullptr ? 0 : item->data(Qt::UserRole).toInt();
}

bool OutlinePanel::emptyStateVisible() const { return !empty_->isHidden(); }

// ---------------------------------------------------------------------------
// ClassroomPanel
// ---------------------------------------------------------------------------

ClassroomPanel::ClassroomPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("classroomPanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    summary_ = new QLabel{this};
    summary_->setObjectName(QStringLiteral("classroomSummary"));
    summary_->setWordWrap(true);
    layout->addWidget(summary_);

    marks_ = new QTableWidget{this};
    marks_->setObjectName(QStringLiteral("classroomMarks"));
    marks_->setAccessibleName(tr("Class marks"));
    marks_->setColumnCount(4);
    marks_->setHorizontalHeaderLabels(
        {tr("Student"), tr("Lesson"), tr("Handed in"), tr("Passed")});
    marks_->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    marks_->verticalHeader()->setVisible(false);
    marks_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    marks_->setSelectionBehavior(QAbstractItemView::SelectRows);
    marks_->setSelectionMode(QAbstractItemView::SingleSelection);
    layout->addWidget(marks_, 1);

    empty_ = new QLabel{tr("No class list yet. A teacher creates one with "
                           "`teach.py roster add`."),
                        this};
    empty_->setObjectName(QStringLiteral("classroomEmptyState"));
    empty_->setAlignment(Qt::AlignCenter);
    empty_->setWordWrap(true);
    empty_->setEnabled(false);
    layout->addWidget(empty_, 1);

    auto* buttons = new QHBoxLayout;
    open_ = new QPushButton{tr("Open Student Folder"), this};
    open_->setObjectName(QStringLiteral("classroomOpenFolder"));
    connect(open_, &QPushButton::clicked, this, [this] {
        int row = marks_->currentRow();
        if (row < 0 && marks_->rowCount() > 0) {
            row = 0;
        }
        const QString directory = directoryForRow(row);
        if (!directory.isEmpty()) {
            emit openFolderRequested(directory);
        }
    });
    buttons->addWidget(open_);
    refresh_ = new QPushButton{tr("Refresh"), this};
    refresh_->setObjectName(QStringLiteral("classroomRefresh"));
    connect(refresh_, &QPushButton::clicked, this, [this] { emit refreshRequested(); });
    buttons->addWidget(refresh_);
    layout->addLayout(buttons);

    // Carrying a class to another machine is two buttons rather than a menu: it is
    // the teacher's own page, and a class is what moves. The format is the
    // engine's, so what this writes `teach.py unbundle` can read.
    auto* archive = new QHBoxLayout;
    archive->setSpacing(4);
    bundle_ = new QPushButton{tr("Save Bundle..."), this};
    bundle_->setObjectName(QStringLiteral("classroomBundle"));
    bundle_->setToolTip(tr("Write the class list, its work and its settings to one archive"));
    connect(bundle_, &QPushButton::clicked, this, [this] { emit bundleRequested(); });
    archive->addWidget(bundle_);
    unbundle_ = new QPushButton{tr("Load Bundle..."), this};
    unbundle_->setObjectName(QStringLiteral("classroomUnbundle"));
    unbundle_->setToolTip(tr("Unpack a class archive, leaving existing work alone"));
    connect(unbundle_, &QPushButton::clicked, this, [this] { emit unbundleRequested(); });
    archive->addWidget(unbundle_);
    layout->addLayout(archive);

    setRoster({}, {}, {});
    setMarks({});
}

void ClassroomPanel::setRoster(const QList<timewarp::bridge::ClassroomStudent>& students,
                               const QStringList& assignedLessons, const QString& note) {
    student_count_ = static_cast<int>(students.size());
    directories_.clear();
    names_.clear();
    student_order_.clear();
    for (const timewarp::bridge::ClassroomStudent& student : students) {
        directories_.insert(student.id, student.directory);
        names_.insert(student.id, student.name.isEmpty() ? student.id : student.name);
        student_order_ << student.id;
    }
    // A refresh can bring a new name for a learner whose marks are already on
    // screen, so keep the first column in step rather than leaving the old one.
    for (int index = 0; index < marks_->rowCount(); ++index) {
        QTableWidgetItem* item = marks_->item(index, 0);
        if (item == nullptr) {
            continue;
        }
        const QString id = item->data(Qt::UserRole).toString();
        item->setText(names_.value(id, id));
    }

    // The summary says what the numbers below are about: a table of marks with no
    // statement of which lessons they are for is a table a teacher has to guess at.
    QStringList lines;
    if (student_count_ == 0) {
        lines << tr("No students on the class list.");
    } else if (student_count_ == 1) {
        lines << tr("1 student on the class list.");
    } else {
        lines << tr("%1 students on the class list.").arg(student_count_);
    }
    if (!assignedLessons.isEmpty()) {
        lines << tr("Assigned: %1").arg(assignedLessons.join(QStringLiteral(", ")));
    } else {
        lines << tr("Nothing has been assigned yet.");
    }
    if (!note.isEmpty()) {
        lines << note;
    }
    summary_->setText(lines.join(QStringLiteral(" \u00b7 ")));
    updateSummary();
}

void ClassroomPanel::setMarks(const QList<timewarp::bridge::ClassroomMark>& rows) {
    marks_->setRowCount(static_cast<int>(rows.size()));
    for (int index = 0; index < rows.size(); ++index) {
        const timewarp::bridge::ClassroomMark& row = rows.at(index);
        marks_->setItem(index, 0,
                        new QTableWidgetItem{names_.value(row.student, row.student)});
        marks_->setItem(index, 1, new QTableWidgetItem{row.title.isEmpty() ? row.lesson
                                                                          : row.title});
        marks_->setItem(index, 2,
                        new QTableWidgetItem{QStringLiteral("%1/%2").arg(row.handedIn).arg(
                            row.checkpoints)});
        marks_->setItem(index, 3,
                        new QTableWidgetItem{QStringLiteral("%1/%2").arg(row.passed).arg(
                            row.checkpoints)});
        // The student id travels with the row, so the folder button can find the
        // right directory whatever the visible column ends up showing.
        marks_->item(index, 0)->setData(Qt::UserRole, row.student);
    }
    updateSummary();
}

void ClassroomPanel::setFailure(const QString& request, const QString& error) {
    summary_->setText(tr("%1 could not be answered: %2").arg(request, error));
}

int ClassroomPanel::studentCount() const { return student_count_; }

int ClassroomPanel::markRowCount() const { return marks_->rowCount(); }

bool ClassroomPanel::emptyStateVisible() const { return !empty_->isHidden(); }

QString ClassroomPanel::directoryForRow(int row) const {
    if (row < 0 || row >= marks_->rowCount()) {
        // With no marks yet, the first student on the class list is the useful
        // default: a teacher looking at an unmarked class still wants the folder.
        // The list order, not the hash's: see `student_order_`.
        return student_order_.isEmpty() ? QString{} : directories_.value(student_order_.first());
    }
    const QTableWidgetItem* item = marks_->item(row, 0);
    const QString id = item == nullptr ? QString{} : item->data(Qt::UserRole).toString();
    return directories_.value(id);
}

void ClassroomPanel::updateSummary() {
    const bool empty = student_count_ == 0;
    empty_->setVisible(empty);
    marks_->setVisible(!empty);
    open_->setEnabled(!directories_.isEmpty());
    refresh_->setEnabled(true);
    // An empty class is nothing to carry; an archive to unpack is exactly how a
    // class arrives in the first place, so that button never goes away.
    bundle_->setEnabled(!directories_.isEmpty());
    unbundle_->setEnabled(true);
}

// ---------------------------------------------------------------------------
// ImmediateModePanel
// ---------------------------------------------------------------------------

ImmediateModePanel::ImmediateModePanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("immediateMode"));
    setFrameShape(QFrame::StyledPanel);
    // The floor the Python sets, kept in sync with the layout test. Without it
    // the vertical splitter above can drag the REPL bar down to nothing and the
    // user has no way to type a command.
    setMinimumHeight(kMinimumHeight);

    auto* layout = new QHBoxLayout{this};
    layout->setContentsMargins(6, 4, 6, 4);
    layout->setSpacing(6);

    auto* prompt = new QLabel{QStringLiteral("REPL \u203a"), this};
    QFont promptFont = prompt->font();
    promptFont.setBold(true);
    prompt->setFont(promptFont);
    layout->addWidget(prompt);

    input_ = new QLineEdit{this};
    input_->setObjectName(QStringLiteral("immediateInput"));
    input_->setPlaceholderText(
        tr("Type a command and press Return \u2014 e.g. PRINT 2 + 2"));
    input_->setClearButtonEnabled(true);
    layout->addWidget(input_, 1);

    connect(input_, &QLineEdit::returnPressed, this, [this] {
        const QString text = input_->text().trimmed();
        if (text.isEmpty()) {
            return;
        }
        // The panel does not clear its own input: whether a command is accepted
        // is the interpreter's answer, and the owner decides what to do with it.
        emit commandSubmitted(text);
    });
}

QString ImmediateModePanel::command() const { return input_->text(); }

void ImmediateModePanel::setCommand(const QString& text) { input_->setText(text); }

void ImmediateModePanel::focusInput() {
    input_->setFocus(Qt::ShortcutFocusReason);
    input_->selectAll();
}

// ---------------------------------------------------------------------------
// FunctionKeyBar
// ---------------------------------------------------------------------------

namespace {

/// Every layer's table, transcribed from the Python's `LAYERS`.
///
/// Two deliberate differences from it, both because the key already means
/// something else in this application:
///   * F3 is Find Next (the toolbar's Open button and the Edit menu both claim
///     it today, which was an ambiguous shortcut; Open keeps Ctrl+O and F3
///     belongs to Find, which is what every other editor does with it).
///   * Shift+F9 is Stop, not Run Selection: the Run menu has owned Shift+F9
///     since the port, and a second binding for one key is an ambiguous
///     shortcut rather than a convenience.
///
/// `nullptr` in the Python's table meant a slot with no action; those slots are
/// filled here with the commands this application does have (clear output,
/// clear canvas, export) rather than left as unlabelled dead keys.
struct KeyBinding {
    const char* key;
    const char* action;
    const char* label;
};

// Normal layer: the ten keys a learner presses without a modifier.
constexpr KeyBinding kNormalKeys[] = {
    {"F1", "help", "Help"},        {"F2", "save", "Save"},
    {"F3", "find", "Find"},        {"F4", "outline", "Outline"},
    {"F5", "fit", "Fit"},          {"F6", "next_pane", "Next"},
    {"F7", "step_into", "Trace"},  {"F8", "step_over", "Step"},
    {"F9", "run", "Run"},          {"F10", "palette", "Menu"},
};

// Shift layer: whole-workspace actions.
constexpr KeyBinding kShiftKeys[] = {
    {"Shift+F1", "shortcuts", "Keys"},     {"Shift+F2", "save_as", "SaveAs"},
    {"Shift+F3", "find_previous", "Find\u2191"},
    {"Shift+F4", "find_in_files", "Grep"}, {"Shift+F5", "split_editor", "Split"},
    {"Shift+F6", "toggle_hud", "HUD"},     {"Shift+F7", "toggle_crt", "CRT"},
    {"Shift+F8", "step_out", "Out"},       {"Shift+F9", "stop", "Stop"},
    {"Shift+F10", "cycle_theme", "Theme"},
};

// Control layer: tooling and the debugger.
constexpr KeyBinding kControlKeys[] = {
    {"Ctrl+F1", "keywords", "Keywords"}, {"Ctrl+F2", "stop", "Stop"},
    {"Ctrl+F3", "lessons", "Lessons"},   {"Ctrl+F4", "clear_output", "ClearOut"},
    {"Ctrl+F5", "filter_errors", "Errors"},
    {"Ctrl+F6", "clear_canvas", "ClearGfx"},
    {"Ctrl+F7", "pause", "Pause"},        {"Ctrl+F8", "breakpoint", "Break"},
    {"Ctrl+F9", "debug", "Debug"},        {"Ctrl+F10", "export", "Export"},
};

// label -> stable id, from the Python widget's `_DECKS`.
constexpr std::pair<const char*, const char*> kDecks[] = {
    {"1:CODE", "code_focus"},
    {"2:SPLIT", "split_lab"},
    {"3:GFX", "graphics_studio"},
    {"4:DEBUG", "debugger_deck"},
};

QString layerName(FunctionKeyBar::Layer layer) {
    switch (layer) {
        case FunctionKeyBar::Layer::Shift:
            return QStringLiteral("SHIFT");
        case FunctionKeyBar::Layer::Control:
            return QStringLiteral("CTRL");
        case FunctionKeyBar::Layer::Normal:
            break;
    }
    return QStringLiteral("FN");
}

}  // namespace

QList<FunctionKeyBar::Binding> FunctionKeyBar::bindingsFor(Layer layer) {
    const KeyBinding* table = kNormalKeys;
    int count = static_cast<int>(std::size(kNormalKeys));
    switch (layer) {
        case Layer::Shift:
            table = kShiftKeys;
            count = static_cast<int>(std::size(kShiftKeys));
            break;
        case Layer::Control:
            table = kControlKeys;
            count = static_cast<int>(std::size(kControlKeys));
            break;
        case Layer::Normal:
            break;
    }
    QList<Binding> bindings;
    bindings.reserve(count);
    for (int index = 0; index < count; ++index) {
        bindings.append(Binding{QString::fromUtf8(table[index].key),
                                QString::fromUtf8(table[index].action),
                                QString::fromUtf8(table[index].label)});
    }
    return bindings;
}

FunctionKeyBar::FunctionKeyBar(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("functionKeyBar"));
    setFrameShape(QFrame::StyledPanel);
    setFixedHeight(kFixedHeight);

    auto* layout = new QHBoxLayout{this};
    layout->setContentsMargins(4, 0, 4, 0);
    layout->setSpacing(3);

    decks_ = new QComboBox{this};
    decks_->setObjectName(QStringLiteral("deckSelector"));
    QStringList labels;
    for (const auto& [label, id] : kDecks) {
        const QString text = QString::fromUtf8(label);
        labels << text;
        deckIds_.append({text, QString::fromUtf8(id)});
    }
    decks_->addItems(labels);
    connect(decks_, QOverload<int>::of(&QComboBox::currentIndexChanged), this, [this](int index) {
        if (index >= 0 && index < deckIds_.size()) {
            emit deckSelected(deckIds_.at(index).second);
        }
    });
    layout->addWidget(decks_);

    // The badge is how the other two layers are reachable with a mouse, and it
    // is also the only place the bar says which layer is showing.
    layer_badge_ = new QPushButton{layerName(layer_), this};
    layer_badge_->setObjectName(QStringLiteral("layerBadge"));
    layer_badge_->setToolTip(tr("Function-key layer \u2014 click to cycle "
                                "normal, shift, control"));
    layer_badge_->setFixedWidth(54);
    layer_badge_->setFocusPolicy(Qt::NoFocus);
    connect(layer_badge_, &QPushButton::clicked, this,
            [this] { cycleLayer(); });
    layout->addWidget(layer_badge_);

    keys_ = new QWidget{this};
    keys_->setObjectName(QStringLiteral("functionKeys"));
    keys_layout_ = new QHBoxLayout{keys_};
    keys_layout_->setContentsMargins(0, 0, 0, 0);
    keys_layout_->setSpacing(3);
    layout->addWidget(keys_);

    layout->addStretch(1);

    rebuildKeys();
}

void FunctionKeyBar::rebuildKeys() {
    // Rebuilt rather than relabelled: the three layers have different actions
    // per slot, and a button whose caption and action are updated separately is
    // a button that can end up labelled "Run" and doing "Stop".
    while (QLayoutItem* item = keys_layout_->takeAt(0)) {
        if (QWidget* widget = item->widget()) {
            // Hidden as well as deferred: `deleteLater` runs after this event
            // is delivered, and an unhidden child that is no longer in a layout
            // would still paint at its old geometry until then.
            widget->hide();
            widget->deleteLater();
        }
        delete item;
    }

    layer_badge_->setText(layerName(layer_));
    layer_badge_->setAccessibleName(
        tr("Function-key layer: %1").arg(layerName(layer_)));

    for (const Binding& binding : bindingsFor(layer_)) {
        auto* button = new QPushButton{
            QStringLiteral("%1 %2").arg(binding.keys, binding.caption), keys_};
        button->setProperty("action", binding.action);
        button->setToolTip(QStringLiteral("%1 \u2014 %2")
                               .arg(binding.keys, binding.caption));
        // The action travels as its own argument rather than being captured from
        // the button text, so relabelling a key cannot silently change what it
        // does.
        connect(button, &QPushButton::clicked, this,
                [this, action = binding.action] { emit actionTriggered(action); });
        keys_layout_->addWidget(button);
    }
}

void FunctionKeyBar::setLayer(Layer layer) {
    if (layer_ == layer) {
        return;
    }
    layer_ = layer;
    rebuildKeys();
    emit layerChanged(static_cast<int>(layer_));
}

void FunctionKeyBar::cycleLayer() {
    switch (layer_) {
        case Layer::Normal:
            setLayer(Layer::Shift);
            break;
        case Layer::Shift:
            setLayer(Layer::Control);
            break;
        case Layer::Control:
            setLayer(Layer::Normal);
            break;
    }
}

QList<QPushButton*> FunctionKeyBar::keyButtons() const {
    QList<QPushButton*> buttons;
    for (int index = 0; index < keys_layout_->count(); ++index) {
        if (auto* button = qobject_cast<QPushButton*>(
                keys_layout_->itemAt(index)->widget())) {
            buttons << button;
        }
    }
    return buttons;
}

void FunctionKeyBar::setDecks(const QStringList& labels, int current) {
    deckIds_.clear();
    decks_->blockSignals(true);
    decks_->clear();
    for (const QString& label : labels) {
        decks_->addItem(label);
        deckIds_.append({label, label});
    }
    decks_->setCurrentIndex(current);
    decks_->blockSignals(false);
}

int FunctionKeyBar::currentDeck() const { return decks_->currentIndex(); }

// ---------------------------------------------------------------------------
// CrtOverlay
// ---------------------------------------------------------------------------

CrtOverlay::CrtOverlay(QWidget* target) : QWidget(target), target_(target) {
    setObjectName(QStringLiteral("crtOverlay"));
    // An overlay that eats clicks is worse than no overlay: every control
    // underneath would become dead.
    setAttribute(Qt::WA_TransparentForMouseEvents, true);
    setAttribute(Qt::WA_NoSystemBackground, true);
    setFocusPolicy(Qt::NoFocus);

    if (target_ != nullptr) {
        target_->installEventFilter(this);
        syncToTarget();
    }
    hide();
}

void CrtOverlay::setSettings(const Settings& settings) {
    settings_.scanlines = std::clamp(settings.scanlines, 0, 100);
    settings_.vignette = std::clamp(settings.vignette, 0, 100);
    settings_.curvature = std::clamp(settings.curvature, 0, 100);
    settings_.flicker = std::clamp(settings.flicker, 0, 100);
    settings_.phosphor = std::clamp(settings.phosphor, 0, 100);
    settings_.phosphor_colour = settings.phosphor_colour;

    // The flicker clock runs only while there is flicker to paint: a timer ticking
    // over a still surface is battery spent on nothing.
    if (settings_.flicker > 0) {
        if (flicker_timer_ == nullptr) {
            flicker_timer_ = new QTimer{this};
            flicker_timer_->setObjectName(QStringLiteral("crtFlickerTimer"));
            flicker_timer_->setInterval(80);
            connect(flicker_timer_, &QTimer::timeout, this, [this] {
                flicker_phase_ = (flicker_phase_ + 1) % 12;
                update();
            });
        }
        if (!flicker_timer_->isActive()) {
            flicker_timer_->start();
        }
    } else if (flicker_timer_ != nullptr) {
        flicker_timer_->stop();
        flicker_phase_ = 0;
    }
    update();
}

void CrtOverlay::setScanlineIntensity(int percent) {
    Settings updated = settings_;
    updated.scanlines = percent;
    setSettings(updated);
}

int CrtOverlay::scanlineIntensity() const noexcept { return settings_.scanlines; }

QWidget* CrtOverlay::target() const noexcept { return target_; }

bool CrtOverlay::eventFilter(QObject* watched, QEvent* event) {
    if (watched == target_) {
        if (event->type() == QEvent::Resize) {
            syncToTarget();
        } else if (event->type() == QEvent::Destroy) {
            // The overlay is a child of its target, so this is a formality -- but
            // a dangling pointer here would outlive the run and be read by
            // `target()`.
            target_ = nullptr;
        }
    }
    return QWidget::eventFilter(watched, event);
}

void CrtOverlay::syncToTarget() {
    if (target_ == nullptr) {
        return;
    }
    setGeometry(target_->rect());
    // Children are painted in insertion order, and the overlay is created last,
    // so it is already on top -- but a widget created later would cover it.
    raise();
}

void CrtOverlay::paintEvent(QPaintEvent* /*event*/) {
    QPainter painter{this};
    painter.setRenderHint(QPainter::Antialiasing, true);
    paintPhosphor(painter);
    paintScanlines(painter);
    paintVignette(painter);
    paintCurvature(painter);
}

void CrtOverlay::paintScanlines(QPainter& painter) {
    if (settings_.scanlines <= 0) {
        return;
    }
    // Deliberately understated: a full-strength scanline layer over text is
    // unreadable at 100%. The divisor keeps the effect at the level the Python's
    // default 0.15 intensity produced.
    const int alpha = (settings_.scanlines * 255) / 100 / 3;
    if (alpha <= 0) {
        return;
    }
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor{0, 0, 0, alpha});
    for (int y = 0; y < height(); y += 2) {
        painter.drawRect(0, y, width(), 1);
    }
}

void CrtOverlay::paintVignette(QPainter& painter) {
    if (settings_.vignette <= 0 || width() <= 0 || height() <= 0) {
        return;
    }
    const int alpha = (settings_.vignette * 255) / 100 / 2;
    if (alpha <= 0) {
        return;
    }
    // A radial wash from a clear centre: the tube's face falls off at the corners.
    QRadialGradient gradient{QPointF{width() / 2.0, height() / 2.0},
                             std::hypot(width(), height()) / 2.0};
    gradient.setColorAt(0.55, QColor{0, 0, 0, 0});
    gradient.setColorAt(1.0, QColor{0, 0, 0, alpha});
    painter.setPen(Qt::NoPen);
    painter.setBrush(gradient);
    painter.drawRect(rect());
}

void CrtOverlay::paintCurvature(QPainter& painter) {
    if (settings_.curvature <= 0) {
        return;
    }
    const int alpha = (settings_.curvature * 255) / 100 / 2;
    if (alpha <= 0) {
        return;
    }
    // The bezel: a rounded border inset by a few pixels, with the outside of the
    // radius darkened. Anything more would be a claim to distort the window.
    const double inset = std::max(2.0, std::min(width(), height()) * 0.02);
    const QRectF face = QRectF{rect()}.adjusted(inset, inset, -inset, -inset);
    const double radius = std::min(face.width(), face.height()) * 0.12;
    QPainterPath outer;
    outer.addRect(rect());
    QPainterPath inner;
    inner.addRoundedRect(face, radius, radius);
    painter.setPen(Qt::NoPen);
    painter.setBrush(QColor{0, 0, 0, alpha});
    painter.drawPath(outer.subtracted(inner));
}

void CrtOverlay::paintPhosphor(QPainter& painter) {
    if (settings_.phosphor <= 0) {
        return;
    }
    const int alpha = (settings_.phosphor * 255) / 100 / 6;
    if (alpha <= 0) {
        return;
    }
    QColor tint = settings_.phosphor_colour;
    tint.setAlpha(alpha);
    // Flicker rides on the tint: the same wash, a little stronger on some frames.
    // Twelve steps of a shallow sine rather than a random level, so it breathes
    // instead of strobing.
    if (settings_.flicker > 0) {
        const double wave = std::sin(flicker_phase_ * std::numbers::pi / 6.0);
        const int extra = static_cast<int>(wave * (settings_.flicker * 255) / 100 / 12);
        tint.setAlpha(std::clamp(alpha + extra, 0, 255));
    }
    painter.setPen(Qt::NoPen);
    painter.setBrush(tint);
    painter.drawRect(rect());
}

// ---------------------------------------------------------------------------
// CrtSettingsDialog
// ---------------------------------------------------------------------------

CrtSettingsDialog::CrtSettingsDialog(const CrtOverlay::Settings& start, QWidget* parent)
    : QDialog(parent), settings_(start) {
    setObjectName(QStringLiteral("crtSettingsDialog"));
    setWindowTitle(tr("CRT Settings"));
    setModal(true);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(6);

    // One row per effect, each with a live number: a slider without its value is a
    // control a user cannot describe to anybody else.
    const auto addRow = [this, layout](const QString& object, const QString& label,
                                       int value, int CrtOverlay::Settings::*field) {
        auto* row = new QHBoxLayout;
        auto* name = new QLabel{label, this};
        name->setMinimumWidth(110);
        row->addWidget(name);
        auto* slider = new QSlider{Qt::Horizontal, this};
        slider->setObjectName(object);
        slider->setRange(0, 100);
        slider->setValue(value);
        row->addWidget(slider, 1);
        auto* number = new QLabel{QString::number(value), this};
        number->setMinimumWidth(32);
        number->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        row->addWidget(number);
        layout->addLayout(row);
        connect(slider, &QSlider::valueChanged, this, [this, number, field](int next) {
            number->setText(QString::number(next));
            settings_.*(field) = next;
            emit previewRequested(settings_);
        });
    };

    addRow(QStringLiteral("crtScanlines"), tr("Scanlines"), settings_.scanlines,
           &CrtOverlay::Settings::scanlines);
    addRow(QStringLiteral("crtVignette"), tr("Vignette"), settings_.vignette,
           &CrtOverlay::Settings::vignette);
    addRow(QStringLiteral("crtCurvature"), tr("Curvature"), settings_.curvature,
           &CrtOverlay::Settings::curvature);
    addRow(QStringLiteral("crtFlicker"), tr("Flicker"), settings_.flicker,
           &CrtOverlay::Settings::flicker);
    addRow(QStringLiteral("crtPhosphor"), tr("Phosphor tint"), settings_.phosphor,
           &CrtOverlay::Settings::phosphor);

    auto* buttons = new QDialogButtonBox{QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this};
    buttons->setObjectName(QStringLiteral("crtSettingsButtons"));
    connect(buttons, &QDialogButtonBox::accepted, this, &QDialog::accept);
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);
}

// ---------------------------------------------------------------------------
// LessonPanel
// ---------------------------------------------------------------------------

namespace {

/// The tick and dash the checkpoint list uses. Chosen rather than drawn, because a
/// learner reads the list as a checklist and an empty bullet reads as "nothing
/// here" instead of "not done yet".
constexpr auto kDoneMark = "\u2713";
constexpr auto kTodoMark = "\u00b7";

}  // namespace

LessonPanel::LessonPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("lessonPanel"));
    setFrameShape(QFrame::StyledPanel);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    lessons_ = new QComboBox{this};
    lessons_->setObjectName(QStringLiteral("lessonChooser"));
    lessons_->setAccessibleName(tr("Lesson"));
    lessons_->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    layout->addWidget(lessons_);

    description_ = new QLabel{this};
    description_->setObjectName(QStringLiteral("lessonDescription"));
    description_->setWordWrap(true);
    layout->addWidget(description_);

    checkpoints_ = new QListWidget{this};
    checkpoints_->setObjectName(QStringLiteral("lessonCheckpoints"));
    checkpoints_->setAccessibleName(tr("Checkpoints"));
    // A checkpoint is one line: a title, and whether it is done. Four rows is
    // enough to see the shape of a lesson without the list taking the panel.
    checkpoints_->setMaximumHeight(96);
    layout->addWidget(checkpoints_);

    auto* actions = new QHBoxLayout{};
    actions->setSpacing(4);
    open_ = new QPushButton{tr("Open"), this};
    open_->setObjectName(QStringLiteral("lessonOpenButton"));
    open_->setToolTip(tr("Put this checkpoint's starting program in a document"));
    check_ = new QPushButton{tr("Check"), this};
    check_->setObjectName(QStringLiteral("lessonCheckButton"));
    check_->setToolTip(tr("Run your program and see whether it does what was asked"));
    hint_ = new QPushButton{tr("Hint"), this};
    hint_->setObjectName(QStringLiteral("lessonHintButton"));
    solution_ = new QPushButton{tr("Answer"), this};
    solution_->setObjectName(QStringLiteral("lessonSolutionButton"));
    solution_->setToolTip(tr("Show the worked answer. Try a hint first."));
    reset_ = new QPushButton{tr("Restart"), this};
    reset_->setObjectName(QStringLiteral("lessonResetButton"));
    for (QPushButton* button : {open_, check_, hint_, solution_, reset_}) {
        actions->addWidget(button);
    }
    layout->addLayout(actions);

    detail_ = new QTextBrowser{this};
    detail_->setObjectName(QStringLiteral("lessonDetail"));
    detail_->setOpenExternalLinks(false);
    // Read-only and framed off, because it is there to be read rather than to
    // look like somewhere to type: the editor is somewhere to type.
    detail_->setFrameShape(QFrame::NoFrame);
    layout->addWidget(detail_, 1);

    verdict_ = new QLabel{this};
    verdict_->setObjectName(QStringLiteral("lessonVerdict"));
    verdict_->setWordWrap(true);
    verdict_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    layout->addWidget(verdict_);

    progress_ = new QProgressBar{this};
    progress_->setObjectName(QStringLiteral("lessonProgress"));
    progress_->setTextVisible(true);
    progress_->setFixedHeight(14);
    layout->addWidget(progress_);

    connect(lessons_, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &LessonPanel::onLessonChanged);
    connect(checkpoints_, &QListWidget::currentRowChanged, this,
            &LessonPanel::onCheckpointChanged);
    connect(open_, &QPushButton::clicked, this, [this] {
        emit openCodeRequested(currentLanguageKey(), currentStarter(), currentLesson());
    });
    connect(check_, &QPushButton::clicked, this, [this] {
        if (!currentLesson().isEmpty() && currentCheckpoint() >= 0) {
            emit checkRequested(currentLesson(), currentCheckpoint());
        }
    });
    connect(hint_, &QPushButton::clicked, this, &LessonPanel::showNextHint);
    connect(solution_, &QPushButton::clicked, this, [this] {
        if (!currentLesson().isEmpty() && currentCheckpoint() >= 0) {
            emit solutionRequested(currentLesson(), currentCheckpoint());
        }
    });
    connect(reset_, &QPushButton::clicked, this, [this] {
        if (!currentLesson().isEmpty()) {
            // A restart forgets the verdicts for that lesson, because they are
            // about programs the learner has just chosen to begin again.
            verdicts_.remove(currentLesson());
            emit resetRequested(currentLesson());
        }
    });

    // Nothing is requested here on purpose. A signal emitted during construction
    // is a signal nobody is connected to yet, and the decision about *when* to
    // pay for the curriculum -- when the tab is first looked at -- belongs to the
    // window, which is the only thing that knows when that happens.
}

void LessonPanel::setLessons(const QList<timewarp::bridge::LessonSummary>& lessons) {
    const QString previous = currentLesson();
    curriculum_ = lessons;
    // A new curriculum is a new session: a verdict against an id that may no
    // longer exist is not worth carrying into a report.
    verdicts_.clear();

    const QSignalBlocker blocker{lessons_};
    lessons_->clear();
    for (const timewarp::bridge::LessonSummary& lesson : lessons) {
        lessons_->addItem(tr("%1 \u00b7 %2").arg(lesson.language, lesson.title), lesson.id);
    }

    // The selection is restored by id rather than by index: progress can change
    // the order of nothing, but a refresh must not move a learner off the lesson
    // they were reading.
    int index = previous.isEmpty() ? 0 : lessons_->findData(previous);
    if (index < 0) {
        index = 0;
    }
    if (lessons_->count() > 0) {
        lessons_->setCurrentIndex(index);
    }
    onLessonChanged(lessons_->currentIndex());
}

void LessonPanel::setProgress(const QList<timewarp::bridge::LessonProgress>& rows, int completed,
                              int total) {
    rows_ = rows;
    progress_->setRange(0, total > 0 ? total : 1);
    progress_->setValue(completed);
    progress_->setFormat(tr("%1 of %2 lessons").arg(completed).arg(total));
    refreshCheckpoint();
}

void LessonPanel::setHint(const QString& text, int number, int total) {
    detail_->setPlainText(text);
    if (total > 0) {
        hint_->setToolTip(tr("Hint %1 of %2").arg(number).arg(total));
    }
}

void LessonPanel::setSolution(const QString& text) {
    detail_->setPlainText(tr("Worked answer:\n\n%1").arg(text));
}

void LessonPanel::setChecked(const QString& lesson, int checkpoint, bool passed,
                             const QString& message, bool completed, int finished, int total) {
    // Recorded before the staleness check below: the display only shows the
    // checkpoint on screen, but a report lists every one that was tried, and a
    // verdict that arrives after the panel has moved on is still a verdict.
    verdicts_[lesson][checkpoint] = SessionVerdict{passed, message};

    // A verdict for a different checkpoint than the one on screen is stale, and
    // showing it would tell a learner their answer was wrong when they have
    // already moved on.
    if (lessons_->currentData().toString() != lesson || checkpoint != currentCheckpoint()) {
        return;
    }
    verdict_->setText(message);
    verdict_passed_ = passed;
    refreshVerdictColour();

    if (passed && checkpoint < checkpoints_->count()) {
        // The tick replaces the dash without touching the title, so a learner sees
        // the list they were reading with one line marked rather than a list that
        // has been rebuilt.
        checkpoints_->item(checkpoint)->setText(
            QStringLiteral("%1 %2").arg(QString::fromUtf8(kDoneMark),
                                        checkpoints_->item(checkpoint)->text().mid(2)));
    }
    progress_->setValue(finished);
    // Finishing a lesson is worth saying in the one place the learner is looking,
    // and it is the panel's own sentence: the engine reports the fact, and how to
    // put it in front of somebody is not the engine's business.
    progress_->setFormat(completed
                             ? tr("%1 of %2 lessons \u00b7 this one is finished")
                                   .arg(finished)
                                   .arg(total)
                             : tr("%1 of %2 lessons").arg(finished).arg(total));

    if (passed) {
        // Only move on when there is somewhere to move to, so finishing a lesson
        // does not leave the panel pointing at a checkpoint that does not exist.
        if (checkpoint + 1 < checkpoints_->count()) {
            checkpoints_->setCurrentRow(checkpoint + 1);
        }
        emit progressNeeded();
    }
}

void LessonPanel::setFailure(const QString& request, const QString& error) {
    verdict_->setText(tr("%1 could not be answered: %2").arg(request, error));
    verdict_passed_ = false;
    refreshVerdictColour();
}

void LessonPanel::setVerdictColors(const QColor& passed, const QColor& failed) {
    verdict_pass_ = passed;
    verdict_fail_ = failed;
    refreshVerdictColour();
}

void LessonPanel::refreshVerdictColour() {
    verdict_->setStyleSheet(
        QStringLiteral("color: %1;")
            .arg((verdict_passed_ ? verdict_pass_ : verdict_fail_).name()));
}

void LessonPanel::setPresentationFont(int pointSize) {
    presentation_font_ = pointSize;
    QFont font = description_->font();
    if (pointSize > 0) {
        font.setPointSize(pointSize);
    }
    description_->setFont(font);
    QFont detailFont = detail_->font();
    if (pointSize > 0) {
        detailFont.setPointSize(pointSize);
    }
    detail_->setFont(detailFont);
}

QString LessonPanel::currentLesson() const { return lessons_->currentData().toString(); }

int LessonPanel::currentCheckpoint() const { return checkpoints_->currentRow(); }

QString LessonPanel::currentStarter() const {
    const int index = currentCheckpoint();
    const QString id = currentLesson();
    for (const timewarp::bridge::LessonSummary& lesson : curriculum_) {
        if (lesson.id == id && index >= 0 && index < lesson.checkpointCount()) {
            return lesson.checkpoints.at(index).starter;
        }
    }
    return {};
}

QString LessonPanel::currentLanguageKey() const {
    const QString id = currentLesson();
    for (const timewarp::bridge::LessonSummary& lesson : curriculum_) {
        if (lesson.id == id) {
            return lesson.languageKey;
        }
    }
    return {};
}

QString LessonPanel::verdictText() const { return verdict_->text(); }

int LessonPanel::lessonCount() const { return static_cast<int>(curriculum_.size()); }

timewarp::ui::exporter::LessonSession LessonPanel::session() const {
    using timewarp::ui::exporter::LessonSession;
    using timewarp::ui::exporter::LessonSessionCheckpoint;

    LessonSession session;
    session.current = currentCheckpoint();
    const QString id = currentLesson();
    if (id.isEmpty()) {
        return session;
    }

    const QHash<int, SessionVerdict> no_verdicts;
    const auto found = verdicts_.constFind(id);
    const QHash<int, SessionVerdict>& lesson_verdicts =
        found == verdicts_.constEnd() ? no_verdicts : found.value();

    for (const timewarp::bridge::LessonSummary& lesson : curriculum_) {
        if (lesson.id != id) {
            continue;
        }
        session.lessonTitle = lesson.title;
        session.lessonDescription = lesson.description;
        session.language = lesson.language;
        session.objectives = lesson.objectives;
        for (int index = 0; index < lesson.checkpointCount(); ++index) {
            LessonSessionCheckpoint checkpoint;
            checkpoint.title = lesson.checkpoints.at(index).title;
            checkpoint.description = lesson.checkpoints.at(index).description;
            const auto verdict = lesson_verdicts.constFind(index);
            if (verdict != lesson_verdicts.constEnd()) {
                checkpoint.checked = true;
                checkpoint.passed = verdict.value().passed;
                checkpoint.message = verdict.value().message;
            }
            session.checkpoints.append(checkpoint);
        }
        break;
    }
    for (const timewarp::bridge::LessonProgress& row : rows_) {
        if (row.id == id) {
            session.status = row.status;
            break;
        }
    }
    return session;
}

void LessonPanel::onLessonChanged(int index) {
    if (index < 0 || index >= curriculum_.size()) {
        checkpoints_->clear();
        description_->clear();
        detail_->clear();
        return;
    }
    const timewarp::bridge::LessonSummary& lesson = curriculum_.at(index);
    description_->setText(tr("%1 \u2014 %2").arg(lesson.title, lesson.description));

    const QSignalBlocker blocker{checkpoints_};
    checkpoints_->clear();
    for (int position = 0; position < lesson.checkpointCount(); ++position) {
        checkpoints_->addItem(
            QStringLiteral("%1 %2").arg(QString::fromUtf8(kTodoMark),
                                        lesson.checkpoints.at(position).title));
    }
    hint_number_ = 0;
    detail_->clear();
    verdict_->clear();
    refreshCheckpoint();
    if (checkpoints_->count() > 0) {
        checkpoints_->setCurrentRow(0);
    }
}

void LessonPanel::onCheckpointChanged(int row) {
    Q_UNUSED(row);
    hint_number_ = 0;
    verdict_->clear();
    refreshCheckpoint();
}

void LessonPanel::refreshCheckpoint() {
    const QString id = currentLesson();
    if (id.isEmpty()) {
        return;
    }
    // The learner's place decides which checkpoint the panel opens on, so a
    // learner who comes back to a lesson is put where they left off rather than
    // at the beginning of it.
    for (const timewarp::bridge::LessonProgress& row : std::as_const(rows_)) {
        if (row.id != id) {
            continue;
        }
        for (int position = 0; position < checkpoints_->count(); ++position) {
            const bool done = position < row.checkpoint;
            checkpoints_->item(position)->setText(
                QStringLiteral("%1 %2").arg(QString::fromUtf8(done ? kDoneMark : kTodoMark),
                                            checkpoints_->item(position)->text().mid(2)));
        }
        if (row.checkpoint > 0 && row.checkpoint < checkpoints_->count() &&
            checkpoints_->currentRow() == 0) {
            const QSignalBlocker blocker{checkpoints_};
            checkpoints_->setCurrentRow(row.checkpoint);
        }
        break;
    }
    const int index = currentCheckpoint();
    const int position = lessons_->currentIndex();
    if (position < 0 || index < 0 || position >= curriculum_.size()) {
        return;
    }
    const timewarp::bridge::LessonSummary& lesson = curriculum_.at(position);
    if (index < lesson.checkpointCount()) {
        detail_->setPlainText(lesson.checkpoints.at(index).description);
    }
}

void LessonPanel::showNextHint() {
    const QString id = currentLesson();
    const int index = currentCheckpoint();
    if (id.isEmpty() || index < 0) {
        return;
    }
    // The number asked for is the panel's own count of how many it has shown, so
    // pressing Hint twice gives two different hints rather than the same one.
    emit hintRequested(id, index);
    hint_number_ += 1;
}

// ---------------------------------------------------------------------------
// VariablesPanel
// ---------------------------------------------------------------------------

VariablesPanel::VariablesPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("variablesPanel"));
    setMinimumSize(160, 100);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(4);

    filter_ = new QLineEdit;
    filter_->setObjectName(QStringLiteral("variablesFilter"));
    filter_->setPlaceholderText(tr("Filter by name"));
    filter_->setClearButtonEnabled(true);
    layout->addWidget(filter_);

    table_ = new QTableWidget{0, 3};
    table_->setObjectName(QStringLiteral("variablesTable"));
    table_->setHorizontalHeaderLabels({tr("Name"), tr("Type"), tr("Value")});
    table_->verticalHeader()->setVisible(false);
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    // Without a floor, an empty table sizes the type column to its header text
    // and clips the word "Type" -- which reads as a broken table rather than an
    // empty one.
    table_->horizontalHeader()->setMinimumSectionSize(56);
    // Read-only because the engine holds the value: an edit here would have
    // nowhere to go, and a table that looked editable and discarded the edit is
    // worse than one that does not offer.
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    layout->addWidget(table_, 1);

    status_ = new QLabel;
    status_->setObjectName(QStringLiteral("variablesStatus"));
    layout->addWidget(status_);

    connect(filter_, &QLineEdit::textChanged, this, [this](const QString&) { applyFilter(); });
    applyFilter();
}

void VariablesPanel::setRows(const QList<timewarp::bridge::VariableRow>& rows) {
    rows_ = rows;
    applyFilter();
}

void VariablesPanel::setFilter(const QString& text) { filter_->setText(text); }

int VariablesPanel::rowCount() const { return static_cast<int>(rows_.size()); }

int VariablesPanel::visibleRowCount() const { return table_->rowCount(); }

void VariablesPanel::applyFilter() {
    const QString needle = filter_->text().trimmed();
    table_->setRowCount(0);

    int shown = 0;
    for (const timewarp::bridge::VariableRow& row : rows_) {
        if (!needle.isEmpty() && !row.name.contains(needle, Qt::CaseInsensitive)) {
            continue;
        }
        const int index = table_->rowCount();
        table_->insertRow(index);
        table_->setItem(index, 0, new QTableWidgetItem{row.name});
        table_->setItem(index, 1, new QTableWidgetItem{row.type});
        table_->setItem(index, 2, new QTableWidgetItem{row.value});
        ++shown;
    }

    if (rows_.isEmpty()) {
        // "No variables" after a run is a fact about the program, so it is worth
        // saying; "no variables" before one would be a claim nobody has earned.
        status_->setText(tr("nothing to show yet \u2014 run a program"));
    } else if (shown == rows_.size()) {
        status_->setText(tr("%n variable(s)", "", shown));
    } else {
        status_->setText(tr("%1 of %2 variables").arg(shown).arg(rows_.size()));
    }
}

// ---------------------------------------------------------------------------
// TelemetryPanel
// ---------------------------------------------------------------------------

TelemetryPanel::TelemetryPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("telemetryPanel"));
    setMinimumSize(160, 100);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);

    table_ = new QTableWidget{0, 2};
    table_->setObjectName(QStringLiteral("telemetryTable"));
    table_->setHorizontalHeaderLabels({tr("Measure"), tr("Value")});
    table_->verticalHeader()->setVisible(false);
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    layout->addWidget(table_, 1);

    setRows({});
}

void TelemetryPanel::setRows(const QList<timewarp::bridge::TelemetryRow>& rows) {
    table_->setRowCount(0);
    if (rows.isEmpty()) {
        // One row stating the state, rather than an empty grid that looks broken.
        table_->insertRow(0);
        table_->setItem(0, 0, new QTableWidgetItem{tr("Nothing yet")});
        table_->setItem(0, 1, new QTableWidgetItem{tr("run a program")});
        return;
    }
    for (const timewarp::bridge::TelemetryRow& row : rows) {
        const int index = table_->rowCount();
        table_->insertRow(index);
        table_->setItem(index, 0, new QTableWidgetItem{row.label});
        table_->setItem(index, 1, new QTableWidgetItem{row.value});
    }
}

int TelemetryPanel::rowCount() const {
    // The placeholder row is not a measurement.
    if (table_->rowCount() == 1 && table_->item(0, 0) != nullptr &&
        table_->item(0, 0)->text() == tr("Nothing yet")) {
        return 0;
    }
    return table_->rowCount();
}

QString TelemetryPanel::valueOf(const QString& label) const {
    for (int row = 0; row < table_->rowCount(); ++row) {
        const QTableWidgetItem* name = table_->item(row, 0);
        const QTableWidgetItem* value = table_->item(row, 1);
        if (name != nullptr && value != nullptr && name->text() == label) {
            return value->text();
        }
    }
    return {};
}

// ---------------------------------------------------------------------------
// DebugPanel
// ---------------------------------------------------------------------------

DebugPanel::DebugPanel(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("debugPanel"));
    setMinimumSize(160, 100);

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(6, 6, 6, 6);
    layout->setSpacing(4);

    status_ = new QLabel;
    status_->setObjectName(QStringLiteral("debugStatus"));
    status_->setWordWrap(true);
    layout->addWidget(status_);

    reason_ = new QLabel;
    reason_->setObjectName(QStringLiteral("debugReason"));
    reason_->setWordWrap(true);
    layout->addWidget(reason_);

    auto* transport = new QHBoxLayout;
    transport->setSpacing(4);
    const auto button = [transport](const QString& text, const QString& name) {
        auto* item = new QPushButton{text};
        item->setObjectName(name);
        transport->addWidget(item);
        return item;
    };
    continue_ = button(tr("Continue"), QStringLiteral("debugContinue"));
    pause_ = button(tr("Pause"), QStringLiteral("debugPause"));
    step_into_ = button(tr("Step In"), QStringLiteral("debugStepInto"));
    step_over_ = button(tr("Step Over"), QStringLiteral("debugStepOver"));
    step_out_ = button(tr("Step Out"), QStringLiteral("debugStepOut"));
    stop_ = button(tr("Stop"), QStringLiteral("debugStop"));
    layout->addLayout(transport);

    connect(continue_, &QPushButton::clicked, this, &DebugPanel::continueRequested);
    connect(pause_, &QPushButton::clicked, this, &DebugPanel::pauseRequested);
    connect(step_into_, &QPushButton::clicked, this, &DebugPanel::stepIntoRequested);
    connect(step_over_, &QPushButton::clicked, this, &DebugPanel::stepOverRequested);
    connect(step_out_, &QPushButton::clicked, this, &DebugPanel::stepOutRequested);
    connect(stop_, &QPushButton::clicked, this, &DebugPanel::stopRequested);

    auto* breakpoint_row = new QHBoxLayout;
    breakpoint_row->setSpacing(4);
    breakpoint_row->addWidget(new QLabel{tr("Line")});
    line_ = new QSpinBox;
    line_->setObjectName(QStringLiteral("debugLine"));
    line_->setRange(1, 999999);
    breakpoint_row->addWidget(line_);
    add_ = new QPushButton{tr("Add")};
    add_->setObjectName(QStringLiteral("debugAddBreakpoint"));
    breakpoint_row->addWidget(add_);
    remove_ = new QPushButton{tr("Remove")};
    remove_->setObjectName(QStringLiteral("debugRemoveBreakpoint"));
    breakpoint_row->addWidget(remove_);
    layout->addLayout(breakpoint_row);

    connect(add_, &QPushButton::clicked, this, [this] { addBreakpoint(); });
    connect(remove_, &QPushButton::clicked, this, [this] { removeBreakpoint(); });

    breakpoints_ = new QListWidget;
    breakpoints_->setObjectName(QStringLiteral("debugBreakpoints"));
    breakpoints_->setMaximumHeight(90);
    layout->addWidget(breakpoints_);

    auto* stack_label = new QLabel{tr("Call stack")};
    layout->addWidget(stack_label);
    stack_ = new QListWidget;
    stack_->setObjectName(QStringLiteral("debugStack"));
    stack_->setMaximumHeight(110);
    layout->addWidget(stack_);

    auto* variables_label = new QLabel{tr("Variables here")};
    layout->addWidget(variables_label);
    variables_ = new QTableWidget{0, 2};
    variables_->setObjectName(QStringLiteral("debugVariables"));
    variables_->setHorizontalHeaderLabels({tr("Name"), tr("Value")});
    variables_->verticalHeader()->setVisible(false);
    variables_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    variables_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    variables_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    layout->addWidget(variables_, 1);
    // Double-clicking a binding is how a user who does not know a variable's
    // spelling asks to watch it change.
    connect(variables_, &QTableWidget::cellDoubleClicked, this,
            [this](int row, int) {
                if (QTableWidgetItem* item = variables_->item(row, 0); item != nullptr) {
                    addDataBreakpoint(item->text());
                }
            });

    pause_reason_ = new QLabel;
    pause_reason_->setObjectName(QStringLiteral("debugPauseReason"));
    pause_reason_->setWordWrap(true);
    layout->addWidget(pause_reason_);

    auto* watch_label = new QLabel{tr("Watch expressions")};
    layout->addWidget(watch_label);
    auto* watch_row = new QHBoxLayout;
    watch_row->setSpacing(4);
    watch_input_ = new QLineEdit;
    watch_input_->setObjectName(QStringLiteral("debugWatchInput"));
    watch_input_->setPlaceholderText(tr("X * 2"));
    watch_row->addWidget(watch_input_, 1);
    watch_add_ = new QPushButton{tr("Add")};
    watch_add_->setObjectName(QStringLiteral("debugAddWatch"));
    watch_row->addWidget(watch_add_);
    watch_remove_ = new QPushButton{tr("Remove")};
    watch_remove_->setObjectName(QStringLiteral("debugRemoveWatch"));
    watch_row->addWidget(watch_remove_);
    layout->addLayout(watch_row);
    watches_ = new QTableWidget{0, 2};
    watches_->setObjectName(QStringLiteral("debugWatches"));
    watches_->setHorizontalHeaderLabels({tr("Expression"), tr("Value")});
    watches_->verticalHeader()->setVisible(false);
    watches_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    watches_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    watches_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    watches_->setSelectionBehavior(QAbstractItemView::SelectRows);
    watches_->setMaximumHeight(100);
    layout->addWidget(watches_);

    connect(watch_add_, &QPushButton::clicked, this, [this] { addWatch(); });
    connect(watch_remove_, &QPushButton::clicked, this, [this] { removeWatch(); });
    connect(watch_input_, &QLineEdit::returnPressed, this, [this] { addWatch(); });

    auto* data_label = new QLabel{tr("Break when a variable changes")};
    layout->addWidget(data_label);
    auto* data_row = new QHBoxLayout;
    data_row->setSpacing(4);
    data_input_ = new QLineEdit;
    data_input_->setObjectName(QStringLiteral("debugDataBreakpointInput"));
    data_input_->setPlaceholderText(tr("name"));
    data_row->addWidget(data_input_, 1);
    data_add_ = new QPushButton{tr("Add")};
    data_add_->setObjectName(QStringLiteral("debugAddDataBreakpoint"));
    data_row->addWidget(data_add_);
    data_remove_ = new QPushButton{tr("Remove")};
    data_remove_->setObjectName(QStringLiteral("debugRemoveDataBreakpoint"));
    data_row->addWidget(data_remove_);
    layout->addLayout(data_row);
    data_breakpoints_ = new QListWidget;
    data_breakpoints_->setObjectName(QStringLiteral("debugDataBreakpoints"));
    data_breakpoints_->setMaximumHeight(70);
    layout->addWidget(data_breakpoints_);
    connect(data_add_, &QPushButton::clicked, this,
            [this] { addDataBreakpoint(data_input_->text()); });
    connect(data_remove_, &QPushButton::clicked, this,
            [this] { removeDataBreakpoint(); });
    connect(data_input_, &QLineEdit::returnPressed, this,
            [this] { addDataBreakpoint(data_input_->text()); });

    setAvailable(false, tr("Run a Scheme program to step through it."));
}

void DebugPanel::setAvailable(bool available, const QString& reason) {
    const bool was_available = available_;
    available_ = available;
    reason_->setText(reason);
    reason_->setVisible(!available);
    for (QPushButton* button : {continue_, pause_, step_into_, step_over_, step_out_, stop_}) {
        button->setEnabled(available);
    }
    add_->setEnabled(available);
    remove_->setEnabled(available);
    line_->setEnabled(available);
    watch_input_->setEnabled(available);
    watch_add_->setEnabled(available);
    watch_remove_->setEnabled(available);
    data_input_->setEnabled(available);
    data_add_->setEnabled(available);
    data_remove_->setEnabled(available);
    if (!available) {
        // The lists stay readable: what the last run left is still true even when
        // the current document cannot be stepped.
        status_->setText(tr("not steppable"));
    } else if (!was_available) {
        // Only when capability returns, not on every document refresh: a tab
        // switch while a run is paused must not erase "paused at line 20" or the
        // reason a data breakpoint stopped it.
        setPaused(false, 0, QString{});
    }
}

bool DebugPanel::isAvailable() const { return available_; }

void DebugPanel::setPaused(bool paused, int line, const QString& procedure) {
    if (!available_) {
        return;
    }
    // "running" would be a lie after the run has finished, and the panel cannot
    // tell the two apart: all it knows is that nothing is paused. The status bar
    // is where "running" and "ready" are said.
    if (!paused) {
        status_->setText(tr("not paused"));
        setPauseReason(QString{});
        return;
    }
    // Line 0 means the program has no source line for the next unit (a binary
    // CHIP-8 ROM), so there is nothing to number.
    const QString where =
        procedure.isEmpty() ? tr("paused at line %1").arg(line)
                            : tr("paused at line %1 in %2").arg(line).arg(procedure);
    status_->setText(line > 0 ? where : tr("paused before the next instruction"));
}

void DebugPanel::setPauseReason(const QString& reason) {
    pause_reason_->setText(reason);
    pause_reason_->setVisible(!reason.isEmpty());
}

void DebugPanel::setWatches(const QStringList& expressions) {
    watches_->setRowCount(0);
    for (const QString& expression : expressions) {
        const QString trimmed = expression.trimmed();
        if (trimmed.isEmpty()) {
            continue;
        }
        const int row = watches_->rowCount();
        watches_->insertRow(row);
        watches_->setItem(row, 0, new QTableWidgetItem{trimmed});
        watches_->setItem(row, 1, new QTableWidgetItem{QStringLiteral("\u2014")});
    }
}

QStringList DebugPanel::watches() const {
    QStringList expressions;
    for (int row = 0; row < watches_->rowCount(); ++row) {
        if (QTableWidgetItem* item = watches_->item(row, 0); item != nullptr) {
            expressions.append(item->text());
        }
    }
    return expressions;
}

void DebugPanel::setWatchValues(const QList<WatchRow>& values) {
    // The expressions are the contract; values are matched by row and a value for
    // an expression that is no longer listed is ignored rather than appended.
    watches_->setRowCount(values.size());
    for (int row = 0; row < values.size(); ++row) {
        const WatchRow& entry = values.at(row);
        watches_->setItem(row, 0, new QTableWidgetItem{entry.expression});
        auto* value = new QTableWidgetItem{entry.value};
        if (!entry.ok) {
            value->setToolTip(entry.error);
            value->setForeground(palette().brush(QPalette::Disabled, QPalette::Text));
        }
        watches_->setItem(row, 1, value);
    }
}

void DebugPanel::setDataBreakpoints(const QStringList& names) {
    data_breakpoints_->clear();
    for (const QString& name : names) {
        if (!name.trimmed().isEmpty()) {
            data_breakpoints_->addItem(name.trimmed());
        }
    }
}

QStringList DebugPanel::dataBreakpoints() const {
    QStringList names;
    for (int row = 0; row < data_breakpoints_->count(); ++row) {
        names.append(data_breakpoints_->item(row)->text());
    }
    return names;
}

void DebugPanel::addWatch() {
    const QString expression = watch_input_->text().trimmed();
    if (expression.isEmpty()) {
        return;
    }
    for (int row = 0; row < watches_->rowCount(); ++row) {
        if (watches_->item(row, 0) != nullptr &&
            watches_->item(row, 0)->text() == expression) {
            watch_input_->clear();
            return;
        }
    }
    const int row = watches_->rowCount();
    watches_->insertRow(row);
    watches_->setItem(row, 0, new QTableWidgetItem{expression});
    watches_->setItem(row, 1, new QTableWidgetItem{QStringLiteral("\u2014")});
    watch_input_->clear();
    emitWatches();
}

void DebugPanel::removeWatch() {
    const int row = watches_->currentRow();
    if (row < 0) {
        return;
    }
    watches_->removeRow(row);
    emitWatches();
}

void DebugPanel::addDataBreakpoint(const QString& name) {
    const QString trimmed = name.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }
    const QList<QListWidgetItem*> existing =
        data_breakpoints_->findItems(trimmed, Qt::MatchFixedString | Qt::MatchCaseSensitive);
    if (!existing.isEmpty()) {
        data_input_->clear();
        return;
    }
    data_breakpoints_->addItem(trimmed);
    data_input_->clear();
    emitDataBreakpoints();
}

void DebugPanel::removeDataBreakpoint() {
    if (QListWidgetItem* item = data_breakpoints_->currentItem(); item != nullptr) {
        delete data_breakpoints_->takeItem(data_breakpoints_->row(item));
        emitDataBreakpoints();
    }
}

void DebugPanel::emitWatches() { emit watchesChanged(watches()); }

void DebugPanel::emitDataBreakpoints() {
    emit dataBreakpointsChanged(dataBreakpoints());
}

void DebugPanel::setStack(const QList<Frame>& frames) {
    stack_->clear();
    for (const Frame& frame : frames) {
        stack_->addItem(tr("%1 (line %2)").arg(frame.procedure).arg(frame.line));
    }
    if (frames.isEmpty()) {
        stack_->addItem(tr("(empty)"));
    }
}

void DebugPanel::setVariables(const QList<timewarp::bridge::VariableRow>& rows) {
    variables_->setRowCount(0);
    for (const timewarp::bridge::VariableRow& row : rows) {
        const int index = variables_->rowCount();
        variables_->insertRow(index);
        variables_->setItem(index, 0, new QTableWidgetItem{row.name});
        variables_->setItem(index, 1, new QTableWidgetItem{row.value});
    }
}

void DebugPanel::setBreakpoints(const QList<int>& lines) {
    breakpoints_->clear();
    for (const int line : lines) {
        breakpoints_->addItem(tr("line %1").arg(line));
    }
}

QList<int> DebugPanel::breakpoints() const {
    QList<int> lines;
    for (int row = 0; row < breakpoints_->count(); ++row) {
        // The items read "line 12"; the number after the space is the breakpoint.
        lines.append(breakpoints_->item(row)->text().section(QLatin1Char(' '), 1).toInt());
    }
    return lines;
}

void DebugPanel::setLineNumber(int line) { line_->setValue(line); }

int DebugPanel::lineNumber() const { return line_->value(); }

void DebugPanel::addBreakpoint() {
    const int line = line_->value();
    if (line <= 0) {
        return;
    }
    for (int row = 0; row < breakpoints_->count(); ++row) {
        if (breakpoints_->item(row)->text() == tr("line %1").arg(line)) {
            return;  // Already there: a set, not a list.
        }
    }
    breakpoints_->addItem(tr("line %1").arg(line));
    emitBreakpoints();
}

void DebugPanel::removeBreakpoint() {
    const QString wanted = tr("line %1").arg(line_->value());
    for (int row = 0; row < breakpoints_->count(); ++row) {
        if (breakpoints_->item(row)->text() == wanted) {
            delete breakpoints_->takeItem(row);
            emitBreakpoints();
            return;
        }
    }
}

void DebugPanel::requestContinue() { emit continueRequested(); }
void DebugPanel::requestPause() { emit pauseRequested(); }
void DebugPanel::requestStepInto() { emit stepIntoRequested(); }
void DebugPanel::requestStepOver() { emit stepOverRequested(); }
void DebugPanel::requestStepOut() { emit stepOutRequested(); }
void DebugPanel::requestStop() { emit stopRequested(); }

void DebugPanel::emitBreakpoints() { emit breakpointsChanged(breakpoints()); }

void DebugPanel::toggleBreakpointAt(int line) {
    if (line <= 0) {
        return;
    }
    setLineNumber(line);
    const QString wanted = tr("line %1").arg(line);
    for (int row = 0; row < breakpoints_->count(); ++row) {
        if (breakpoints_->item(row)->text() == wanted) {
            delete breakpoints_->takeItem(row);
            emitBreakpoints();
            return;
        }
    }
    breakpoints_->addItem(wanted);
    emitBreakpoints();
}

// ---------------------------------------------------------------------------
// TurtleInspector and TurtleCommandLog
// ---------------------------------------------------------------------------

namespace {

/// Up to three decimals without the trailing zeros: a log line for `RIGHT 90`
/// should read 90, not 90.000.
QString plain_number(double value) {
    QString text = QString::number(value, 'f', 3);
    if (text.contains(QLatin1Char{'.'})) {
        while (text.endsWith(QLatin1Char{'0'})) {
            text.chop(1);
        }
        if (text.endsWith(QLatin1Char{'.'})) {
            text.chop(1);
        }
    }
    if (text == QLatin1String{"-0"}) {
        text = QStringLiteral("0");
    }
    return text;
}

QString colour_text(std::uint32_t colour) {
    return QStringLiteral("#%1").arg(colour & 0xFFFFFFu, 6, 16, QLatin1Char{'0'}).toUpper();
}

/// A command's text payload, quoted and clipped. A sprite name or a `DRAWTEXT`
/// string can be arbitrarily long, and one of them must not push every argument
/// off the visible line.
QString quoted_text(std::string_view text) {
    QString value = timewarp::core::fromUtf8(text);
    if (value.size() > 24) {
        value = value.left(23) + QChar{0x2026};
    }
    return QLatin1Char{'"'} + value + QLatin1Char{'"'};
}

QString point_count_text(std::size_t count) {
    return count == 1 ? QStringLiteral("1 point") : QStringLiteral("%1 points").arg(count);
}

}  // namespace

TurtleInspector::TurtleInspector(QWidget* parent) : QFrame(parent) {
    setObjectName(QStringLiteral("turtleInspector"));
    setFrameShape(QFrame::NoFrame);
    // The strip takes its height from its font, so the text-size ladder grows it
    // rather than clipping it, and the canvas keeps the rest.
    setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

    auto* row = new QHBoxLayout{this};
    row->setContentsMargins(6, 1, 6, 1);
    row->setSpacing(14);

    pose_ = new QLabel{this};
    pose_->setObjectName(QStringLiteral("turtlePose"));
    pose_->setAccessibleName(tr("Turtle position and heading"));
    pen_ = new QLabel{this};
    pen_->setObjectName(QStringLiteral("turtlePen"));
    pen_->setAccessibleName(tr("Pen state, colour and width"));
    commands_label_ = new QLabel{this};
    commands_label_->setObjectName(QStringLiteral("turtleCommands"));
    commands_label_->setAccessibleName(tr("Drawing commands received"));

    row->addWidget(pose_);
    row->addWidget(pen_);
    row->addStretch(1);
    row->addWidget(commands_label_);

    clear();
}

void TurtleInspector::showPose(const QPointF& position, double heading, bool pen_down,
                               const QColor& pen_colour, double pen_width) {
    // The canvas keeps whatever heading it was turned to, which may be past a
    // full circle or negative; the strip shows the heading a person would name.
    double normalised = std::fmod(heading, 360.0);
    if (normalised < 0.0) {
        normalised += 360.0;
    }
    pose_->setText(tr("X %1   Y %2   Heading %3\u00B0")
                       .arg(plain_number(position.x()), plain_number(position.y()),
                            plain_number(normalised)));
    pen_->setText(tr("Pen %1   %2   %3 px")
                      .arg(pen_down ? tr("DOWN") : tr("UP"), pen_colour.name().toUpper(),
                           plain_number(pen_width)));
}

void TurtleInspector::setCommandCount(int count) {
    commands_ = count;
    commands_label_->setText(count == 1 ? tr("1 command") : tr("%1 commands").arg(count));
}

void TurtleInspector::clear() {
    showPose(QPointF{0.0, 0.0}, 0.0, true, QColor{Qt::white}, 1.0);
    setCommandCount(0);
}

QString TurtleInspector::poseText() const { return pose_->text(); }

QString TurtleInspector::penText() const { return pen_->text(); }

TurtleCommandLog::TurtleCommandLog(QWidget* parent) : QPlainTextEdit(parent) {
    setObjectName(QStringLiteral("turtleLog"));
    setAccessibleName(tr("Turtle Command Log"));
    setReadOnly(true);
    setFont(QFont{QStringLiteral("Courier New"), 10});
    setPlaceholderText(tr("Turtle \U0001F422 commands appear here\u2026"));
    // A command is one line and its arguments are its columns, so wrapping would
    // turn one command into two and break the alignment that makes the log
    // scannable.
    setLineWrapMode(QPlainTextEdit::NoWrap);
    setMaximumBlockCount(kMaxLines);
}

void TurtleCommandLog::appendCommand(const timewarp::canvas::CanvasCommandView& view) {
    ++count_;
    // Numbered, because "the third FORWARD" is how somebody refers to a command
    // when comparing the log with the program that emitted it.
    appendPlainText(QStringLiteral("%1  %2")
                        .arg(count_, 5, 10, QLatin1Char{' '})
                        .arg(describe(view)));
}

void TurtleCommandLog::clearLog() {
    count_ = 0;
    clear();
}

QString TurtleCommandLog::describe(const timewarp::canvas::CanvasCommandView& view) {
    using timewarp::canvas::CanvasCommandKind;

    const timewarp::canvas::CanvasCommand& command = view.command;
    // `to_string` answers a `string_view`, and Qt 5's `fromLatin1` has no such
    // overload: the compatibility seam is where that difference lives.
    const QString name =
        timewarp::core::fromUtf8(timewarp::canvas::to_string(command.kind)).toUpper();

    switch (command.kind) {
        case CanvasCommandKind::Forward:
        case CanvasCommandKind::Back:
        case CanvasCommandKind::Left:
        case CanvasCommandKind::Right:
        case CanvasCommandKind::Pitch:
        case CanvasCommandKind::Roll:
        case CanvasCommandKind::SetX:
        case CanvasCommandKind::SetY:
        case CanvasCommandKind::SetHeading:
        case CanvasCommandKind::SetPenWidth:
            return name + QLatin1Char{' '} + plain_number(command.x);

        case CanvasCommandKind::Goto:
            return QStringLiteral("%1 %2 %3")
                .arg(name, plain_number(command.x), plain_number(command.y));

        case CanvasCommandKind::SetPenColor:
        case CanvasCommandKind::SetBackgroundColor:
        case CanvasCommandKind::FillLastShape:
            return name + QLatin1Char{' '} + colour_text(command.color);

        case CanvasCommandKind::SetPenStyle:
            return QStringLiteral("%1 %2 %3")
                .arg(name, QString::number(command.index), plain_number(command.x));

        case CanvasCommandKind::SetSvgaMode:
            return QStringLiteral("%1 %2\u00D7%3")
                .arg(name, plain_number(command.x), plain_number(command.y));

        case CanvasCommandKind::SetPaletteColor:
            return QStringLiteral("%1 %2 %3")
                .arg(name, QString::number(command.index), colour_text(command.color));

        case CanvasCommandKind::CyclePalette:
            return name + QLatin1Char{' '} + QString::number(command.index);

        case CanvasCommandKind::Circle:
            // The extent is a full circle unless the program asked for an arc.
            return command.y == 360.0
                       ? name + QLatin1Char{' '} + plain_number(command.x)
                       : QStringLiteral("%1 %2 %3")
                             .arg(name, plain_number(command.x), plain_number(command.y));

        case CanvasCommandKind::DrawPoint:
            return QStringLiteral("%1 %2 %3 %4")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     colour_text(command.color));

        case CanvasCommandKind::DrawPolyline:
        case CanvasCommandKind::BezierCurve:
            return QStringLiteral("%1 %2 %3")
                .arg(name, point_count_text(view.point_count()), colour_text(command.color));

        case CanvasCommandKind::DrawRect:
        case CanvasCommandKind::DrawEllipse:
        case CanvasCommandKind::DrawRectGradient:
        case CanvasCommandKind::DrawEllipseGradient:
            return QStringLiteral("%1 %2 %3 %4 %5 %6")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     plain_number(command.w), plain_number(command.h),
                     colour_text(command.color));

        case CanvasCommandKind::DrawArc:
            return QStringLiteral("%1 %2 %3 %4 %5 %6 %7")
                .arg(name, plain_number(command.x), plain_number(command.y),
                     plain_number(command.w), plain_number(command.h),
                     plain_number(command.a), plain_number(command.b));

        case CanvasCommandKind::DrawText:
            return QStringLiteral("%1 %2 at %3 %4 (size %5)")
                .arg(name, quoted_text(view.text), plain_number(command.x),
                     plain_number(command.y), plain_number(command.w));

        case CanvasCommandKind::DefineSprite:
            return QStringLiteral("%1 %2 %3 %4 %5 %6")
                .arg(name, quoted_text(view.text), plain_number(command.x),
                     plain_number(command.y), plain_number(command.w),
                     plain_number(command.h));

        case CanvasCommandKind::PlaceSprite:
        case CanvasCommandKind::MoveSprite:
            return QStringLiteral("%1 %2 %3 %4")
                .arg(name, quoted_text(view.text), plain_number(command.x),
                     plain_number(command.y));

        case CanvasCommandKind::ShowSprite:
        case CanvasCommandKind::HideSprite:
            return name + QLatin1Char{' '} + quoted_text(view.text);

        case CanvasCommandKind::None:
        case CanvasCommandKind::Home:
        case CanvasCommandKind::PenUp:
        case CanvasCommandKind::PenDown:
        case CanvasCommandKind::BeginFill:
        case CanvasCommandKind::EndFill:
        case CanvasCommandKind::ShowTurtle:
        case CanvasCommandKind::HideTurtle:
        case CanvasCommandKind::Clear:
        case CanvasCommandKind::Reset:
        case CanvasCommandKind::ClearSvgaMode:
            break;
    }
    return name;
}

}  // namespace timewarp::ui
