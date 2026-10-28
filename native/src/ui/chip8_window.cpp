// SPDX-License-Identifier: MIT

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/chip8_window.hpp"

#include "timewarp/ui/chip8_library.hpp"
#include "timewarp/ui/chip8_screen.hpp"
#include "timewarp/ui/app_paths.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/sound_player.hpp"

#include <QAction>
#include <QCheckBox>
#include <QCloseEvent>
#include <QDragEnterEvent>
#include <QDropEvent>
#include <QFileDialog>
#include <QFileInfo>
#include <QFont>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QKeySequence>
#include <QLabel>
#include <QSlider>
#include <QMessageBox>
#include <QMimeData>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QSignalBlocker>
#include <QSpinBox>
#include <QStatusBar>
#include <QStringList>
#include <QTabWidget>
#include <QTableWidget>
#include <QTextCursor>
#include <QToolBar>
#include <QUrl>
#include <QVBoxLayout>

#include <algorithm>
#include <array>
#include <cstdint>

namespace timewarp::ui {

namespace {

constexpr std::array<const char*, 16> kRegisterNames{
    "V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7",
    "V8", "V9", "VA", "VB", "VC", "VD", "VE", "VF",
};

[[nodiscard]] QString hex(int value, int digits) {
    return QStringLiteral("%1").arg(value, digits, 16, QLatin1Char('0')).toUpper();
}

}  // namespace

Chip8Window::Chip8Window(QWidget* parent) : QMainWindow(parent) {
    setWindowTitle(tr("CHIP-8 Emulator"));
    setAcceptDrops(true);
    emulator_ = new Chip8Emulator{this};
    sound_ = new SoundPlayer{this};

    buildUi();
    buildToolbar();

    connect(emulator_, &Chip8Emulator::frameReady, this, &Chip8Window::refresh);
    connect(emulator_, &Chip8Emulator::stateChanged, this, &Chip8Window::updateControls);
    connect(emulator_, &Chip8Emulator::errorOccurred, this, [this](const QString& message) {
        statusBar()->showMessage(message, 8000);
    });

    updateControls();
    resize(1040, 620);
}

Chip8Window::~Chip8Window() {
    if (emulator_ != nullptr) {
        emulator_->pause();
        emulator_->releaseAllKeys();
    }
    if (sound_ != nullptr) {
        sound_->stop();
    }
}

Chip8Emulator* Chip8Window::emulator() const { return emulator_; }

Chip8Screen* Chip8Window::screen() const { return screen_; }

void Chip8Window::buildUi() {
    auto* central = new QWidget{this};
    auto* row = new QHBoxLayout{central};
    row->setContentsMargins(8, 8, 8, 8);

    screen_ = new Chip8Screen{central};
    screen_->setColors(QColor{224, 248, 208}, QColor{10, 10, 20});

    // The screen and the rewind strip under it, in one column: a slider beside the
    // keypad would be a control for the machine and not for the picture, and the
    // picture is what it changes.
    auto* screen_column = new QVBoxLayout;
    screen_column->setContentsMargins(0, 0, 0, 0);
    auto* rewind_row = new QHBoxLayout;
    rewind_row->setSpacing(6);
    auto* rewind_caption = new QLabel{tr("\u23EA Rewind"), central};
    rewind_row->addWidget(rewind_caption);
    rewind_ = new QSlider{Qt::Horizontal, central};
    rewind_->setObjectName(QStringLiteral("chip8Rewind"));
    rewind_->setRange(0, 0);
    rewind_->setEnabled(false);
    rewind_->setToolTip(
        tr("Drag back to the machine as it was a moment ago. Run resumes from there."));
    rewind_row->addWidget(rewind_, 1);
    rewind_label_ = new QLabel{central};
    rewind_label_->setObjectName(QStringLiteral("chip8RewindLabel"));
    rewind_label_->setMinimumWidth(84);
    rewind_label_->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    rewind_row->addWidget(rewind_label_);
    screen_column->addLayout(rewind_row);
    screen_column->addWidget(screen_, 1);
    row->addLayout(screen_column, 1);

    auto* column = new QVBoxLayout;
    keypad_ = new Chip8Keypad{central};
    column->addWidget(keypad_, 0, Qt::AlignHCenter);
    auto* hint = new QLabel{tr("Keyboard: 1-4 / Q-R / A-F / Z-V"), central};
    hint->setAlignment(Qt::AlignHCenter);
    column->addWidget(hint);
    buildInspector(column);
    row->addLayout(column, 0);

    setCentralWidget(central);

    connect(screen_, &Chip8Screen::keyChanged, this, [this](int key, bool down) {
        emulator_->setKey(key, down);
        keypad_->setKeyDown(key, down);
    });
    connect(keypad_, &Chip8Keypad::keyChanged, this,
            [this](int key, bool down) { emulator_->setKey(key, down); });

    connect(rewind_, &QSlider::valueChanged, this, [this](int value) {
        if (rewind_ == nullptr) {
            return;
        }
        if (emulator_->rewindPosition() < 0 && value < rewind_->maximum()) {
            // The first touch captures the live state, so the right edge is exactly
            // the frame the user was looking at when they reached for the slider and
            // not the last scheduled snapshot, up to a quarter second older.
            const int moments = emulator_->beginRewind();
            const QSignalBlocker blocker{rewind_};
            rewind_->setRange(0, std::max(0, moments - 1));
            rewind_->setValue(std::max(0, moments - 1));
        }
        const int newest = rewind_->maximum();
        emulator_->rewindTo(value >= newest ? newest : value);
        updateRewind();
        updateControls();
    });

    state_label_ = new QLabel{tr("Open a .ch8 listing or ROM"), this};
    state_label_->setObjectName(QStringLiteral("chip8State"));
    detail_label_ = new QLabel{this};
    detail_label_->setObjectName(QStringLiteral("chip8Detail"));
    statusBar()->addWidget(state_label_);
    statusBar()->addPermanentWidget(detail_label_);
}

#ifdef TIME_WARP_HAVE_AI
void Chip8Window::openAssistant() {
    if (!assistant_.isNull()) {
        assistant_->raise();
        assistant_->activateWindow();
        return;
    }
    // What it is told: the listing, which is the program a reader can ask about. A
    // binary ROM has no text to send, so it sends none rather than a page of hex.
    auto* dialog = new AssistantDialog{
        [this] {
            timewarp::ai::Context context;
            context.language = QStringLiteral("CHIP-8");
            context.file_name = tr("CHIP-8 listing");
            context.source = listing_ != nullptr ? listing_->toPlainText() : QString{};
            return context;
        },
        this};
    assistant_ = dialog;
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    dialog->show();
}
#endif

void Chip8Window::buildToolbar() {
    auto* toolbar = addToolBar(tr("CHIP-8"));
    toolbar->setObjectName(QStringLiteral("chip8Toolbar"));

    run_action_ = toolbar->addAction(tr("&Run"), this, [this] {
        if (emulator_->running()) {
            emulator_->pause();
        } else {
            emulator_->start();
        }
    });
    run_action_->setObjectName(QStringLiteral("chip8RunAction"));
    run_action_->setShortcut(QKeySequence{Qt::Key_F5});

    step_action_ = toolbar->addAction(tr("&Step"), this, [this] { emulator_->step(); });
    step_action_->setObjectName(QStringLiteral("chip8StepAction"));
    step_action_->setShortcut(QKeySequence{Qt::Key_F6});

    reset_action_ = toolbar->addAction(tr("&Reset"), this, [this] { emulator_->reset(); });
    reset_action_->setObjectName(QStringLiteral("chip8ResetAction"));
    reset_action_->setShortcut(QKeySequence{Qt::Key_F7});

    // The listing editor is the "enter code" path; Ctrl+Return assembles and
    // runs without leaving the keyboard.
    assemble_action_ = toolbar->addAction(tr("Assemble && Load"), this,
                                          &Chip8Window::assembleListing);
    assemble_action_->setObjectName(QStringLiteral("chip8AssembleAction"));
    assemble_action_->setShortcut(timewarp::core::keySequence(Qt::CTRL, Qt::Key_Return));

    toolbar->addSeparator();
#ifdef TIME_WARP_HAVE_AI
    // Beside the assembler's own actions: it explains the listing that is about to be
    // assembled, which is the question a learner has at the moment it fails.
    QAction* assistant = toolbar->addAction(tr("\U0001F916 Assistant..."), this,
                                            &Chip8Window::openAssistant);
    assistant->setObjectName(QStringLiteral("chip8AssistantAction"));
    toolbar->addSeparator();
#endif
    toolbar->addAction(tr("&Open ROM..."), this, &Chip8Window::openRomDialog);
    QAction* library = toolbar->addAction(tr("ROM &Library..."), this,
                                          &Chip8Window::openRomLibraryDialog);
    library->setObjectName(QStringLiteral("chip8LibraryAction"));
    toolbar->addSeparator();
    toolbar->addWidget(new QLabel{tr("Speed "), toolbar});

    speed_ = new QSpinBox{toolbar};
    speed_->setObjectName(QStringLiteral("chip8Speed"));
    speed_->setRange(1, kChip8MaximumSpeed);
    speed_->setValue(emulator_->speed());
    speed_->setSuffix(tr(" opcodes/frame"));
    speed_->setToolTip(tr("Opcodes executed per 60 Hz frame. Ten is the original pace."));
    connect(speed_, QOverload<int>::of(&QSpinBox::valueChanged), this,
            [this](int value) { emulator_->setSpeed(value); });
    toolbar->addWidget(speed_);

    mute_ = new QCheckBox{tr("&Mute"), toolbar};
    mute_->setObjectName(QStringLiteral("chip8Mute"));
    connect(mute_, &QCheckBox::toggled, this, [this](bool muted) {
        if (muted && buzzer_on_) {
            sound_->stop();
            buzzer_on_ = false;
        }
    });
    toolbar->addWidget(mute_);
}

void Chip8Window::buildInspector(QVBoxLayout* column) {
    // No parent here: the layout takes ownership and reparents it. Passing the
    // layout itself would be a type error, not a parent.
    auto* tabs = new QTabWidget{};
    inspector_tabs_ = tabs;

    registers_ = new QTableWidget{22, 3, tabs};
    registers_->setObjectName(QStringLiteral("chip8Registers"));
    registers_->setHorizontalHeaderLabels({tr("Register"), tr("Hex"), tr("Decimal")});
    registers_->verticalHeader()->setVisible(false);
    registers_->horizontalHeader()->setStretchLastSection(true);
    registers_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    registers_->setSelectionMode(QAbstractItemView::NoSelection);
    for (int row = 0; row < 22; ++row) {
        QString name;
        if (row < 16) {
            name = QString::fromLatin1(kRegisterNames[static_cast<std::size_t>(row)]);
        } else {
            switch (row) {
                case 16: name = tr("I"); break;
                case 17: name = tr("PC"); break;
                case 18: name = tr("Delay"); break;
                case 19: name = tr("Sound"); break;
                case 20: name = tr("Cycles"); break;
                default: name = tr("Stack"); break;
            }
        }
        registers_->setItem(row, 0, new QTableWidgetItem{name});
        registers_->setItem(row, 1, new QTableWidgetItem{QStringLiteral("\u2014")});
        registers_->setItem(row, 2, new QTableWidgetItem{QStringLiteral("\u2014")});
    }
    registers_->resizeColumnsToContents();
    tabs->addTab(registers_, tr("Registers"));

    memory_ = new QPlainTextEdit{tabs};
    memory_->setObjectName(QStringLiteral("chip8Memory"));
    memory_->setReadOnly(true);
    memory_->setLineWrapMode(QPlainTextEdit::NoWrap);
    QFont mono{QStringLiteral("Monospace")};
    mono.setStyleHint(QFont::TypeWriter);
    mono.setPointSize(9);
    memory_->setFont(mono);
    tabs->addTab(memory_, tr("Memory"));

    // A listing can be typed here and assembled without touching the main
    // editor: the emulator is useful as a scratchpad as well as a viewer.
    auto* listing_page = new QWidget{tabs};
    listing_page_ = listing_page;
    auto* listing_layout = new QVBoxLayout{listing_page};
    listing_layout->setContentsMargins(0, 0, 0, 0);
    listing_ = new QPlainTextEdit{listing_page};
    listing_->setObjectName(QStringLiteral("chip8Listing"));
    listing_->setPlaceholderText(
        tr("Type a CHIP-8 listing here, then Assemble & Load (Ctrl+Return)."));
    listing_->setLineWrapMode(QPlainTextEdit::NoWrap);
    listing_->setFont(mono);
    listing_layout->addWidget(listing_, 1);
    auto* listing_row = new QHBoxLayout;
    listing_status_ = new QLabel{listing_page};
    listing_status_->setObjectName(QStringLiteral("chip8ListingStatus"));
    listing_status_->setWordWrap(true);
    auto* assemble = new QPushButton{tr("Assemble && Load"), listing_page};
    assemble->setObjectName(QStringLiteral("chip8AssembleButton"));
    connect(assemble, &QPushButton::clicked, this, &Chip8Window::assembleListing);
    listing_row->addWidget(listing_status_, 1);
    listing_row->addWidget(assemble);
    listing_layout->addLayout(listing_row);
    tabs->addTab(listing_page, tr("Listing"));

    column->addWidget(tabs, 1);
}

bool Chip8Window::isRomFile(const QString& path) {
    return ui::languageForFile(path) == Language::Chip8;
}

bool Chip8Window::openFile(const QString& path) {
    QString error;
    if (!emulator_->loadFile(path, &error)) {
        QMessageBox::warning(this, tr("Could not load the program"), error);
        return false;
    }
    const QString file_name = QFileInfo{path}.fileName();
    setWindowTitle(tr("CHIP-8 Emulator \u2014 %1").arg(file_name));
    if (emulator_->binary()) {
        setListing(QString{}, tr("Binary ROM loaded. Type a listing to replace it."));
    } else {
        setListing(QString::fromUtf8(emulator_->source()), tr("Loaded %1.").arg(file_name));
    }
    screen_->setFocus(Qt::OtherFocusReason);
    return true;
}

bool Chip8Window::loadSource(const QString& source) {
    // A ROM in the editor is stored with the codec's two escaped bytes; a
    // listing is ordinary UTF-8.
    const QByteArray bytes = source.contains(QChar{0}) ? romTextToBytes(source)
                                                       : source.toUtf8();
    QString error;
    if (!emulator_->loadBytes(bytes, tr("current document"), &error)) {
        QMessageBox::warning(this, tr("Could not load the program"), error);
        return false;
    }
    setWindowTitle(tr("CHIP-8 Emulator \u2014 current document"));
    if (emulator_->binary()) {
        setListing(QString{}, tr("Binary document loaded. Type a listing to replace it."));
    } else {
        setListing(source, tr("Loaded the current document."));
    }
    screen_->setFocus(Qt::OtherFocusReason);
    return true;
}

bool Chip8Window::loadBundledProgram() {
    const QString root = examplesDir();
    if (root.isEmpty()) {
        return false;
    }
    // The paint program is the introduced example because it uses the keypad;
    // the older font demo is the fallback for an installation that lacks it.
    for (const QString& name : {QStringLiteral("chip8/paint.ch8"),
                                QStringLiteral("chip8/font_and_frames.ch8")}) {
        const QString path = root + QLatin1Char('/') + name;
        if (QFileInfo::exists(path)) {
            if (!openFile(path)) {
                return false;
            }
            // The point of the bundled program is that the window is alive when
            // it opens: the paint cursor waits for a key immediately.
            emulator_->start();
            return true;
        }
    }
    return false;
}

void Chip8Window::assembleListing() {
    if (listing_ == nullptr || emulator_ == nullptr) {
        return;
    }
    const QString source = listing_->toPlainText();
    if (source.trimmed().isEmpty()) {
        listing_status_->setText(tr("Type a listing first."));
        return;
    }
    QString error;
    if (!emulator_->loadSource(source, &error)) {
        // The old program is still loaded and the running game was not stopped;
        // the error belongs beside the text that caused it.
        listing_status_->setText(
            tr("\u274C %1").arg(error.section(QLatin1Char('\n'), 0, 0)));
        return;
    }
    setListing(emulator_->binary() ? QString{} : source, tr("Assembled and running."));
    setWindowTitle(tr("CHIP-8 Emulator \u2014 assembled listing"));
    emulator_->start();
    screen_->setFocus(Qt::OtherFocusReason);
}

void Chip8Window::setListing(const QString& text, const QString& status) {
    if (listing_ != nullptr) {
        listing_->setPlainText(text);
        listing_->moveCursor(QTextCursor::Start);
    }
    if (listing_status_ != nullptr) {
        listing_status_->setText(status);
    }
    // A listing is the thing a learner wants to read; a binary ROM has nothing to
    // show, so it stays on the register view.
    if (inspector_tabs_ != nullptr && listing_page_ != nullptr && !text.isEmpty()) {
        inspector_tabs_->setCurrentWidget(listing_page_);
    }
}

void Chip8Window::openRomDialog() {
    const QString path = QFileDialog::getOpenFileName(
        this, tr("Open CHIP-8 program"), QString{},
        tr("CHIP-8 programs (*.ch8 *.c8);;All files (*)"));
    if (!path.isEmpty()) {
        openFile(path);
    }
}

void Chip8Window::openRomLibraryDialog() {
    Chip8LibraryDialog dialog{storedChip8RomFolder(), this};
    if (dialog.exec() != QDialog::Accepted) {
        return;
    }
    const QString path = dialog.selectedPath();
    if (path.isEmpty()) {
        return;
    }
    // Remembered only after a program was actually opened: a folder the user
    // looked at and dismissed is not one the next window should start in.
    storeChip8RomFolder(dialog.folder());
    openFile(path);
}

void Chip8Window::dragEnterEvent(QDragEnterEvent* event) {
    if (!event->mimeData()->hasUrls()) {
        return;
    }
    for (const QUrl& url : event->mimeData()->urls()) {
        if (url.isLocalFile() && isRomFile(url.toLocalFile())) {
            event->acceptProposedAction();
            return;
        }
    }
}

void Chip8Window::dropEvent(QDropEvent* event) {
    for (const QUrl& url : event->mimeData()->urls()) {
        if (url.isLocalFile() && isRomFile(url.toLocalFile())) {
            openFile(url.toLocalFile());
            event->acceptProposedAction();
            return;
        }
    }
}

void Chip8Window::closeEvent(QCloseEvent* event) {
    if (emulator_ != nullptr) {
        emulator_->pause();
        emulator_->releaseAllKeys();
    }
    if (sound_ != nullptr) {
        sound_->stop();
        buzzer_on_ = false;
    }
    QMainWindow::closeEvent(event);
}

void Chip8Window::refresh() {
    const Chip8Snapshot snapshot = emulator_->snapshot();
    screen_->setPixels(snapshot.framebuffer, snapshot.screen_width, snapshot.screen_height);

    const unsigned keys = emulator_->keys();
    for (int key = 0; key < 16; ++key) {
        keypad_->setKeyDown(key, (keys & (1U << static_cast<unsigned>(key))) != 0U);
    }

    updateSound(snapshot);
    updateRewind();

    // A running program changes memory constantly; four refreshes a second is
    // enough to watch it and cheap enough not to compete with the emulation.
    if (!snapshot.running || inspector_countdown_ <= 0) {
        updateInspector(snapshot);
        inspector_countdown_ = 15;
    } else {
        --inspector_countdown_;
    }

    const QString error = emulator_->error();
    if (snapshot.loaded && snapshot.waiting_for_key) {
        state_label_->setText(tr("\u23F3 waiting for a key"));
    } else if (snapshot.running) {
        state_label_->setText(tr("\u25B6 running"));
    } else if (!error.isEmpty()) {
        state_label_->setText(tr("\u274C %1").arg(error));
    } else if (!snapshot.loaded) {
        state_label_->setText(tr("Open a .ch8 listing or ROM"));
    } else if (snapshot.halted) {
        state_label_->setText(tr("\u25A0 halted"));
    } else {
        state_label_->setText(tr("\u23F8 paused"));
    }

    if (snapshot.loaded) {
        detail_label_->setText(
            tr("PC %1  \u00B7  %2 cycles  \u00B7  %3 opcodes/frame (%4 Hz)")
                .arg(hex(snapshot.pc, 3),
                     QString::number(static_cast<qulonglong>(snapshot.cycles)),
                     QString::number(snapshot.speed),
                     QString::number(snapshot.speed * kChip8FramesPerSecond)));
    } else {
        detail_label_->clear();
    }
}

void Chip8Window::updateControls() {
    const Chip8Snapshot snapshot = emulator_->snapshot();
    // A paused machine's timers stop, so a tone that is still playing would
    // outlive the state that asked for it. Resuming restarts it on the next
    // frame, because `buzzer_on_` is false then.
    if (!snapshot.running && buzzer_on_) {
        sound_->stop();
        buzzer_on_ = false;
    }
    if (run_action_ != nullptr) {
        run_action_->setText(snapshot.running ? tr("&Pause") : tr("&Run"));
        // A failed assemble does not stop the running program, so it must not
        // take Pause away; the error is shown beside the listing instead.
        run_action_->setEnabled(
            snapshot.running ||
            (snapshot.loaded && !snapshot.halted && emulator_->error().isEmpty()));
    }
    if (step_action_ != nullptr) {
        step_action_->setEnabled(snapshot.loaded && !snapshot.running && !snapshot.halted);
    }
    if (reset_action_ != nullptr) {
        reset_action_->setEnabled(snapshot.loaded);
    }
    if (speed_ != nullptr) {
        speed_->setEnabled(snapshot.loaded);
        if (speed_->value() != emulator_->speed()) {
            const QSignalBlocker blocker{speed_};
            speed_->setValue(emulator_->speed());
        }
    }
}

void Chip8Window::updateRewind() {
    if (rewind_ == nullptr || rewind_label_ == nullptr) {
        return;
    }
    const int moments = emulator_->rewindCount();
    const QSignalBlocker blocker{rewind_};
    // One moment is no history at all: a slider with a single position is a
    // control that cannot do anything. The enabled state lives here rather than in
    // `updateControls` because this runs on every frame, and a machine that has
    // been stepping is a machine with history whether or not it ever announced a
    // state change.
    rewind_->setRange(0, std::max(0, moments - 1));
    rewind_->setEnabled(emulator_->loaded() && moments > 1);
    const int position = emulator_->rewindPosition();
    if (position >= 0 && position < moments - 1) {
        rewind_->setValue(position);
        const int frames_back = (moments - 1 - position) * kChip8RewindStride;
        const double seconds = static_cast<double>(frames_back) / kChip8FramesPerSecond;
        rewind_label_->setText(tr("\u2212%1 s").arg(seconds, 0, 'f', 2));
    } else {
        rewind_->setValue(rewind_->maximum());
        rewind_label_->setText(moments > 1 ? tr("now") : QString{});
    }
}

void Chip8Window::updateInspector(const Chip8Snapshot& snapshot) {
    if (registers_ == nullptr || snapshot.memory.empty()) {
        return;
    }
    for (int index = 0; index < 16; ++index) {
        const auto value = snapshot.v[static_cast<std::size_t>(index)];
        registers_->item(index, 1)->setText(hex(value, 2));
        registers_->item(index, 2)->setText(QString::number(value));
    }
    registers_->item(16, 1)->setText(hex(snapshot.i, 3));
    registers_->item(16, 2)->setText(QString::number(snapshot.i));
    registers_->item(17, 1)->setText(hex(snapshot.pc, 3));
    registers_->item(17, 2)->setText(QString::number(snapshot.pc));
    registers_->item(18, 1)->setText(QStringLiteral("\u2014"));
    registers_->item(18, 2)->setText(QString::number(snapshot.delay_timer));
    registers_->item(19, 1)->setText(QStringLiteral("\u2014"));
    registers_->item(19, 2)->setText(QString::number(snapshot.sound_timer));
    registers_->item(20, 1)->setText(QStringLiteral("\u2014"));
    registers_->item(20, 2)->setText(
        QString::number(static_cast<qulonglong>(snapshot.cycles)));

    QStringList frames;
    for (auto it = snapshot.stack.rbegin(); it != snapshot.stack.rend(); ++it) {
        frames << hex(*it, 3);
        if (frames.size() >= 24) {
            frames << QStringLiteral("\u2026");
            break;
        }
    }
    registers_->item(21, 1)->setText(frames.isEmpty() ? QStringLiteral("\u2014")
                                                      : frames.join(QLatin1Char(' ')));
    registers_->item(21, 2)->setText(QString::number(snapshot.stack.size()));

    // The memory view keeps its scroll position: resetting to the top four times
    // a second would make the tab unusable.
    const int scroll = memory_->verticalScrollBar()->value();
    QString text;
    text.reserve(4096 * 4);
    for (int base = 0; base < static_cast<int>(snapshot.memory.size()); base += 16) {
        text += hex(base, 4);
        text += QStringLiteral("  ");
        QString ascii;
        for (int offset = 0; offset < 16; ++offset) {
            const std::size_t address = static_cast<std::size_t>(base + offset);
            const std::uint8_t value = address < snapshot.memory.size()
                                           ? snapshot.memory[address]
                                           : std::uint8_t{0};
            text += hex(value, 2);
            text += QLatin1Char(' ');
            ascii += value >= 32 && value < 127 ? QChar{value} : QLatin1Char('.');
        }
        text += QLatin1Char(' ');
        text += ascii;
        text += QLatin1Char('\n');
    }
    memory_->setPlainText(text);
    memory_->verticalScrollBar()->setValue(scroll);
}

void Chip8Window::updateSound(const Chip8Snapshot& snapshot) {
    if (mute_ != nullptr && mute_->isChecked()) {
        if (buzzer_on_) {
            sound_->stop();
            buzzer_on_ = false;
        }
        last_sound_timer_ = snapshot.sound_timer;
        return;
    }

    // A tone is started when ST rises (including from zero) and left alone while
    // it counts down, because restarting the player every frame would stutter.
    if (snapshot.sound_timer > 0 &&
        (!buzzer_on_ || snapshot.sound_timer > last_sound_timer_ + 1)) {
        const int milliseconds =
            std::max(17, (snapshot.sound_timer * 1000) / kChip8FramesPerSecond);
        sound_->note(440.0, milliseconds, timewarp::audio::Waveform::Square);
        buzzer_on_ = true;
    } else if (snapshot.sound_timer == 0 && buzzer_on_) {
        sound_->stop();
        buzzer_on_ = false;
    }
    last_sound_timer_ = snapshot.sound_timer;
}

}  // namespace timewarp::ui
