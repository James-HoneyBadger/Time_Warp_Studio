// SPDX-License-Identifier: MIT

#include "timewarp/ui/hb2_window.hpp"

#include "timewarp/ui/app_paths.hpp"

#include "timewarp/hb2/rom.hpp"
#include "timewarp/io/gamepad.hpp"

#include <QAction>
#include <QApplication>
#include <QCloseEvent>
#include <QFile>
#include <QComboBox>
#include <QFont>
#include <QFontDatabase>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QMenuBar>
#include <QMouseEvent>
#include <QPainter>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTemporaryDir>
#include <QTimer>
#include <QToolBar>
#include <QVBoxLayout>

#include "timewarp/hb2/font.hpp"
#include "timewarp/ui/sound_player.hpp"

namespace timewarp::ui {

using timewarp::hb2::Task;
using timewarp::hb2::Video;

/// What the Program page holds before anything is typed: a listing that assembles,
/// draws and prints, so the first F9 on a fresh machine shows a picture rather than
/// a blank screen.
constexpr char kStarterListing[] =
    "; HB-2 starter. F9 assembles and runs it.\n"
    ";\n"
    "; The screen is MODE 1: 320 x 200, one byte a pixel. INT 3 fills a rectangle and\n"
    "; INT 8 prints a string with the machine's own font.\n"
    "\n"
    "        ORG $1000\n"
    "        MOV R0, #1              ; MODE 1\n"
    "        INT 5\n"
    "        MOV R0, #0\n"
    "        INT 7                   ; clear to black\n"
    "        MOV R0, #10             ; x\n"
    "        MOV R1, #10             ; y\n"
    "        MOV R2, #300            ; width\n"
    "        MOV R3, #2              ; height\n"
    "        MOV R4, #11             ; cyan\n"
    "        INT 3                   ; fill\n"
    "        MOV R0, #12\n"
    "        MOV R1, #30\n"
    "        MOV R2, #296\n"
    "        MOV R3, #30\n"
    "        MOV R4, #1\n"
    "        INT 3\n"
    "        LEA R0, message\n"
    "        INT 8                   ; print\n"
    "        MOV R0, #0\n"
    "        INT 0                   ; exit 0\n"
    "\n"
    "message: DB \"HELLO FROM THE HB-2\", 0\n";

namespace {

/// The DAC's three bits, scaled to a byte. A 1992 palette had eight levels a channel
/// and a monitor that showed them; this is the same ramp, not a taste.
QRgb colourOf(const timewarp::hb2::Rgb& entry) {
    const auto channel = [](std::uint8_t value) {
        return static_cast<int>(value) * 255 / 7;
    };
    return qRgb(channel(entry.r), channel(entry.g), channel(entry.b));
}

int keyToMachine(int key) {
    // The machine's keyboard is the 1982 one, which is ASCII: a letter is itself, and
    // Return is a newline like it always was.
    if (key == Qt::Key_Return || key == Qt::Key_Enter) {
        return '\n';
    }
    if (key == Qt::Key_Backspace) {
        return 8;
    }
    if (key == Qt::Key_Tab) {
        return 9;
    }
    if (key == Qt::Key_Escape) {
        return 27;
    }
    if (key >= Qt::Key_Space && key < 0x7F) {
        return key;
    }
    return 0;
}

}  // namespace

// ---------------------------------------------------------------------------
// The glass
// ---------------------------------------------------------------------------

Hb2Screen::Hb2Screen(timewarp::hb2::Machine& machine, QWidget* parent)
    : QWidget(parent), machine_(machine) {
    setObjectName(QStringLiteral("hb2Screen"));
    setFocusPolicy(Qt::StrongFocus);
    setMinimumSize(320, 200);
    setAutoFillBackground(true);
}

void Hb2Screen::setKeyboard(bool on) {
    keyboard_ = on;
    if (on) {
        setFocus(Qt::OtherFocusReason);
    }
    update();
}

void Hb2Screen::refresh() {
    rebuildImage();
    update();
}

void Hb2Screen::rebuildImage() {
    Video& video = machine_.video();
    if (video.mode() == Video::Mode::Text) {
        // The 1982 tube: 80 x 25 cells, drawn with the machine's own ROM font so the
        // window shows what the machine's memory says rather than a Qt font's idea
        // of it.
        const int width = Video::kTextColumns * timewarp::hb2::kGlyphWidth;
        const int height = Video::kTextRows * timewarp::hb2::kGlyphHeight;
        if (image_.size() != QSize(width, height)) {
            image_ = QImage(width, height, QImage::Format_RGB32);
        }
        image_.fill(colourOf(video.dac(0)));
        for (int row = 0; row < Video::kTextRows; ++row) {
            for (int column = 0; column < Video::kTextColumns; ++column) {
                const std::uint8_t code = video.cell(column, row);
                if (code == ' ' || code == 0) {
                    continue;
                }
                const std::uint8_t attribute =
                    video.read(Video::kAttributesOffset + static_cast<std::uint32_t>(row * 80 + column));
                const QRgb ink = colourOf(video.dac(attribute & 0x0F));
                const auto& glyph = timewarp::hb2::glyphFor(code);
                for (int y = 0; y < timewarp::hb2::kGlyphHeight; ++y) {
                    const std::uint8_t bits = glyph[static_cast<std::size_t>(y)];
                    for (int x = 0; x < timewarp::hb2::kGlyphWidth; ++x) {
                        if ((bits & (0x80U >> x)) != 0) {
                            image_.setPixel(column * 8 + x, row * 8 + y, ink);
                        }
                    }
                }
            }
        }
        return;
    }

    const int width = video.width();
    const int height = video.height();
    if (width == 0 || height == 0) {
        return;
    }
    if (image_.size() != QSize(width, height)) {
        image_ = QImage(width, height, QImage::Format_RGB32);
    }
    // The composed picture is the chip's own: bitmap plus sprites, palette resolved
    // through the DAC. Nothing here invents a colour.
    std::array<QRgb, 256> lookup{};
    for (int index = 0; index < 256; ++index) {
        lookup[static_cast<std::size_t>(index)] = colourOf(video.dac(index));
    }
    for (int y = 0; y < height; ++y) {
        auto* line = reinterpret_cast<QRgb*>(image_.scanLine(y));
        for (int x = 0; x < width; ++x) {
            line[x] = lookup[video.pixel(x, y)];
        }
    }
}

void Hb2Screen::paintEvent(QPaintEvent* /*event*/) {
    QPainter painter{this};
    painter.fillRect(rect(), QColor(8, 8, 10));
    if (image_.isNull()) {
        return;
    }
    const QSize target = image_.size().scaled(size(), Qt::KeepAspectRatio);
    const QRect box{(width() - target.width()) / 2, (height() - target.height()) / 2, target.width(),
                    target.height()};
    // Nearest-neighbour: a 320 x 200 picture blown up should look like pixels, not like
    // a photograph.
    painter.setRenderHint(QPainter::SmoothPixmapTransform, false);
    painter.drawImage(box, image_);
    if (keyboard_) {
        painter.setPen(QColor(90, 220, 120));
        painter.drawRect(box.adjusted(-1, -1, 0, 0));
    }
}

void Hb2Screen::keyPressEvent(QKeyEvent* event) {
    const int code = keyToMachine(event->key());
    if (code == 0) {
        emit keyForWindow(event->key());
        QWidget::keyPressEvent(event);
        return;
    }
    machine_.input().pressKey(static_cast<std::uint8_t>(code));
    event->accept();
}

void Hb2Screen::keyReleaseEvent(QKeyEvent* event) {
    const int code = keyToMachine(event->key());
    if (code != 0) {
        machine_.input().releaseKey(static_cast<std::uint8_t>(code));
    }
    QWidget::keyReleaseEvent(event);
}

void Hb2Screen::mousePressEvent(QMouseEvent* event) {
    setKeyboard(true);
    const QSize target = image_.size().scaled(size(), Qt::KeepAspectRatio);
    const QPoint origin{(width() - target.width()) / 2, (height() - target.height()) / 2};
    const QPointF at = event->position() - QPointF(origin);
    if (target.width() > 0 && target.height() > 0 && at.x() >= 0 && at.y() >= 0) {
        const int x = static_cast<int>(at.x() * image_.width() / target.width());
        const int y = static_cast<int>(at.y() * image_.height() / target.height());
        machine_.desktop().movePointer(x, y);
        machine_.input().movePointer(x, y);
        last_x_ = x;
        last_y_ = y;
    }
    event->accept();
}

void Hb2Screen::mouseMoveEvent(QMouseEvent* event) {
    if (last_x_ < 0) {
        QWidget::mouseMoveEvent(event);
        return;
    }
    (void)event;
    QWidget::mouseMoveEvent(event);
}

void Hb2Screen::focusInEvent(QFocusEvent* event) {
    keyboard_ = true;
    update();
    QWidget::focusInEvent(event);
}

void Hb2Screen::focusOutEvent(QFocusEvent* event) {
    keyboard_ = false;
    update();
    QWidget::focusOutEvent(event);
}

// ---------------------------------------------------------------------------
// The window
// ---------------------------------------------------------------------------

Hb2Window::Hb2Window(QWidget* parent)
    : QMainWindow(parent), monitor_(machine_), shell_(machine_) {
    setObjectName(QStringLiteral("hb2Window"));
    setWindowTitle(tr("\U0001F5A5 HB-2 Model 92"));
    resize(880, 640);

    // The machine keeps its disk, its cassette and its bay beside the Studio's other
    // user data, so what was typed at it is there next time.
    const QString home = timewarp::ui::dataDirFor(QStringLiteral("hb2"));
    if (!home.isEmpty()) {
        modules_.setDirectory((home + QStringLiteral("/modules")).toStdString());
    }

    shell_.setDos(&dos_);
    shell_.setMonitor(&monitor_);
    shell_.setModules(&modules_);
    loadState();

    buildUi();

    sound_ = new SoundPlayer{this};
    timer_ = new QTimer{this};
    timer_->setInterval(16);
    connect(timer_, &QTimer::timeout, this, [this] { tickOnce(); });
    timer_->start();

    // A program that saves to `A:` and is then left running is still a program that
    // saved; the disk is written out periodically rather than only on the way out.
    autosave_ = new QTimer{this};
    autosave_->setInterval(30000);
    connect(autosave_, &QTimer::timeout, this, [this] { saveState(); });
    autosave_->start();
}

Hb2Window::~Hb2Window() = default;

void Hb2Window::closeEvent(QCloseEvent* event) {
    saveState();
    QMainWindow::closeEvent(event);
}

void Hb2Window::loadState() {
    const QString home = timewarp::ui::dataDirFor(QStringLiteral("hb2"));
    if (home.isEmpty()) {
        dos_.format(0);
        return;
    }
    bool restored = false;
    // Both drives, because `B:` is a hard disk somebody will fill and a machine that
    // kept only `A:` would lose it without saying so.
    for (int drive = 0; drive < 2; ++drive) {
        QFile file{home + QStringLiteral("/disk%1.img").arg(drive)};
        if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            const bool ok = dos_.deserialise(drive, file.readAll().toStdString());
            restored = restored || (drive == 0 && ok);
        }
    }
    const QString tape = home + QStringLiteral("/tape.img");
    QFile tape_file{tape};
    if (tape_file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        machine_.tape().deserialise(tape_file.readAll().toStdString());
    }
    if (!restored && dos_.format(0)) {
        // A machine arrives with a formatted system disk and nothing on it, which is
        // what a new computer was.
        dos_.writeFile(0, "README.TXT",
                       std::vector<std::uint8_t>{'H', 'B', '-', '2', ' ', 'M', 'O', 'D', 'E', 'L',
                                                 ' ', '9', '2', '\n'});
    }
}

void Hb2Window::saveState() {
    const QString home = timewarp::ui::dataDirFor(QStringLiteral("hb2"));
    if (home.isEmpty()) {
        return;
    }
    const auto write = [&home](const QString& leaf, const std::string& image) {
        QFile file{home + QLatin1Char('/') + leaf};
        if (file.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate)) {
            file.write(image.data(), static_cast<qint64>(image.size()));
        }
    };
    for (int drive = 0; drive < 2; ++drive) {
        write(QStringLiteral("disk%1.img").arg(drive), dos_.serialise(drive));
    }
    write(QStringLiteral("tape.img"), machine_.tape().serialise());
}

void Hb2Window::buildUi() {
    auto* tabs = new QTabWidget{this};
    tabs->setObjectName(QStringLiteral("hb2Tabs"));

    // -- the screen ---------------------------------------------------------
    auto* screen_page = new QWidget{tabs};
    auto* screen_layout = new QVBoxLayout{screen_page};
    screen_layout->setContentsMargins(4, 4, 4, 4);
    screen_ = new Hb2Screen{machine_, screen_page};
    screen_layout->addWidget(screen_, 1);
    auto* hint = new QLabel{tr("Click the screen to give it the keyboard. Escape returns to the prompt."),
                            screen_page};
    hint->setObjectName(QStringLiteral("hb2Hint"));
    screen_layout->addWidget(hint);
    tabs->addTab(screen_page, tr("\U0001F5A5 Screen"));

    // -- the prompt ---------------------------------------------------------
    auto* prompt_page = new QWidget{tabs};
    auto* prompt_layout = new QVBoxLayout{prompt_page};
    prompt_layout->setContentsMargins(4, 4, 4, 4);
    transcript_ = new QPlainTextEdit{prompt_page};
    transcript_->setObjectName(QStringLiteral("hb2Transcript"));
    transcript_->setReadOnly(true);
    transcript_->setFont(QFontDatabase::systemFont(QFontDatabase::FixedFont));
    prompt_layout->addWidget(transcript_, 1);
    entry_ = new QLineEdit{prompt_page};
    entry_->setObjectName(QStringLiteral("hb2Entry"));
    entry_->setFont(QFontDatabase::systemFont(QFontDatabase::FixedFont));
    entry_->setPlaceholderText(tr("DIR, HELP, RUN, REGS, TASKS, MODULE LIST..."));
    prompt_layout->addWidget(entry_);
    connect(entry_, &QLineEdit::returnPressed, this, [this] {
        const QString line = entry_->text();
        entry_->clear();
        runCommand(line);
    });
    tabs->addTab(prompt_page, tr("\U0001F4AC Prompt"));

    // -- the monitor --------------------------------------------------------
    auto* monitor_page = new QWidget{tabs};
    auto* monitor_layout = new QVBoxLayout{monitor_page};
    monitor_view_ = new QPlainTextEdit{monitor_page};
    monitor_view_->setObjectName(QStringLiteral("hb2Monitor"));
    monitor_view_->setReadOnly(true);
    monitor_view_->setFont(QFontDatabase::systemFont(QFontDatabase::FixedFont));
    monitor_layout->addWidget(monitor_view_, 1);
    auto* monitor_buttons = new QHBoxLayout;
    const auto addButton = [&](const QString& text, auto slot) {
        auto* button = new QPushButton{text, monitor_page};
        connect(button, &QPushButton::clicked, this, slot);
        monitor_buttons->addWidget(button);
        return button;
    };
    addButton(tr("Registers"), [this] { refreshMonitor(); });
    addButton(tr("Step"), [this] {
        runCommand(QStringLiteral("STEP"));
        refreshMonitor();
    });
    addButton(tr("Break"), [this] {
        runCommand(QStringLiteral("BREAK $%1").arg(machine_.cpu().registers().pc, 6, 16, QLatin1Char('0')));
        refreshMonitor();
    });
    addButton(tr("Run to break"), [this] {
        runCommand(QStringLiteral("RUNTO"));
        refreshMonitor();
    });
    monitor_buttons->addStretch(1);
    monitor_layout->addLayout(monitor_buttons);
    tabs->addTab(monitor_page, tr("\U0001F50E Monitor"));

    // -- the listing --------------------------------------------------------
    auto* program_page = new QWidget{tabs};
    auto* program_layout = new QVBoxLayout{program_page};
    auto* rom_row = new QHBoxLayout;
    rom_row->addWidget(new QLabel{tr("ROM socket:"), program_page});
    rom_combo_ = new QComboBox{program_page};
    rom_combo_->setObjectName(QStringLiteral("hb2Rom"));
    rom_combo_->addItem(tr("HB-16 ASSEMBLER"));
    for (const timewarp::lang::Language language : timewarp::hb2::NativeRom::languages()) {
        rom_combo_->addItem(QString::fromUtf8(timewarp::lang::display_name(language).data()));
    }
    connect(rom_combo_, &QComboBox::currentIndexChanged, this, [this](int index) {
        if (index <= 0) {
            machine_.setRomAssembler();
        } else {
            const std::vector<timewarp::lang::Language> socket =
                timewarp::hb2::NativeRom::languages();
            const auto which = static_cast<std::size_t>(index - 1);
            if (which < socket.size()) {
                machine_.setRomLanguage(socket[which]);
                // A language prints, so the screen has to be the one it prints on.
                machine_.video().setMode(timewarp::hb2::Video::Mode::Text);
                machine_.bus().setCompat(true);
                screen_->refresh();
            }
        }
        refreshStatus();
    });
    rom_row->addWidget(rom_combo_);
    rom_row->addStretch(1);
    program_layout->addLayout(rom_row);
    listing_ = new QPlainTextEdit{program_page};
    listing_->setObjectName(QStringLiteral("hb2Listing"));
    listing_->setFont(QFontDatabase::systemFont(QFontDatabase::FixedFont));
    listing_->setPlainText(QString::fromUtf8(kStarterListing));
    program_layout->addWidget(listing_, 1);
    auto* program_buttons = new QHBoxLayout;
    auto* run_button = new QPushButton{tr("Run"), program_page};
    connect(run_button, &QPushButton::clicked, this, [this] {
        shell_.setSource(listing_->toPlainText().toStdString());
        runCommand(QStringLiteral("RUN"));
    });
    program_buttons->addWidget(run_button);
    auto* task_button = new QPushButton{tr("Run as a process"), program_page};
    connect(task_button, &QPushButton::clicked, this, [this] {
        shell_.setSource(listing_->toPlainText().toStdString());
        runCommand(QStringLiteral("SPAWN"));
    });
    program_buttons->addWidget(task_button);
    auto* save_button = new QPushButton{tr("Save to A:"), program_page};
    connect(save_button, &QPushButton::clicked, this, [this] {
        shell_.setSource(listing_->toPlainText().toStdString());
        runCommand(QStringLiteral("SAVE PROGRAM.S"));
    });
    program_buttons->addWidget(save_button);
    program_buttons->addStretch(1);
    program_layout->addLayout(program_buttons);
    tabs->addTab(program_page, tr("\U0001F4C4 Program"));

    setCentralWidget(tabs);

    // -- the chrome ---------------------------------------------------------
    auto* tools = addToolBar(tr("HB-2"));
    tools->setObjectName(QStringLiteral("hb2Tools"));
    QAction* run_action = tools->addAction(tr("Run"), this, [this] { start(); });
    run_action->setShortcut(QKeySequence{Qt::Key_F9});
    QAction* stop_action = tools->addAction(tr("Stop"), this, [this] { stop(); });
    stop_action->setShortcut(QKeySequence{Qt::Key_F10});
    tools->addSeparator();
    tools->addAction(tr("Reset"), this, [this] {
        shell_.setSource(listing_->toPlainText().toStdString());
        runCommand(QStringLiteral("RUN"));
    });
    tools->addAction(tr("Step"), this, [this] {
        runCommand(QStringLiteral("STEP"));
        refreshMonitor();
    });
    tools->addSeparator();
    mode_label_ = new QLabel{tr("MODE 0"), tools};
    mode_label_->setObjectName(QStringLiteral("hb2Mode"));
    tools->addWidget(mode_label_);

    status_ = new QLabel{tr("ready"), this};
    status_->setObjectName(QStringLiteral("hb2Status"));
    statusBar()->addPermanentWidget(status_);

    connect(screen_, &Hb2Screen::keyForWindow, this, [this](int key) {
        if (key == Qt::Key_Escape) {
            entry_->setFocus(Qt::OtherFocusReason);
        }
    });

    runCommand(QStringLiteral("HELP"));
}

void Hb2Window::start() {
    shell_.setSource(listing_->toPlainText().toStdString());
    machine_.reset(0x001000);
    const timewarp::hb2::Assembly assembly = machine_.load(shell_.source());
    if (!assembly.ok) {
        for (const std::string& error : assembly.errors) {
            transcript_->appendPlainText(QString::fromStdString(error));
        }
        status_->setText(tr("assembly failed"));
        return;
    }
    timer_->start();
    status_->setText(tr("running"));
}

void Hb2Window::stop() { status_->setText(tr("stopped")); }

bool Hb2Window::running() const noexcept { return timer_ != nullptr && timer_->isActive(); }

void Hb2Window::setListing(const QString& text) { listing_->setPlainText(text); }

void Hb2Window::runCommand(const QString& line) {
    // The machine's listing is whatever the Program page holds: `RUN` and `SAVE` mean
    // the thing on screen, so the prompt does not have an older copy of it.
    if (listing_ != nullptr) {
        shell_.setSource(listing_->toPlainText().toStdString());
    }
    transcript_->appendPlainText(QStringLiteral("%1 %2").arg(QString::fromStdString(shell_.prompt()), line));
    const std::vector<std::string> answer = shell_.execute(line.toStdString());
    for (const std::string& row : answer) {
        transcript_->appendPlainText(QString::fromStdString(row));
    }
    transcript_->verticalScrollBar()->setValue(transcript_->verticalScrollBar()->maximum());
    refreshMonitor();
}

void Hb2Window::tickOnce() {
    // A pad the window can see is the pad the machine reads, so `STICK` and the
    // input registers answer with what is in the hand rather than an emulated zero.
    if (timewarp::io::Gamepad::instance().available()) {
        const auto axis = [](int index) {
            return static_cast<std::int8_t>(timewarp::io::Gamepad::instance().axis(index) - 128);
        };
        std::uint8_t buttons = 0;
        for (int i = 0; i < 8; ++i) {
            if (timewarp::io::Gamepad::instance().button(i)) {
                buttons |= static_cast<std::uint8_t>(1U << i);
            }
        }
        machine_.input().setJoystick(0, axis(0), axis(1), buttons);
    }
    machine_.runFrames(1, 20000);
    screen_->refresh();
    mixAudio();
    refreshStatus();
    if (machine_.status() != timewarp::hb2::CpuStatus::Running && machine_.scheduler().empty()) {
        // A program that has stopped stops the ticker, so the machine is not spending
        // a frame a sixtieth of a second on a screen that cannot change.
        timer_->stop();
    }
}

void Hb2Window::mixAudio() {
    timewarp::hb2::Audio& audio = machine_.audio();
    bool sounding = false;
    for (int channel = 0; channel < timewarp::hb2::Audio::kChannels; ++channel) {
        if ((audio.read(static_cast<std::uint32_t>(channel) * timewarp::hb2::Audio::kChannelStride) & 1U) != 0) {
            sounding = true;
        }
    }
    if (!sounding) {
        pcm_.clear();
        return;
    }
    const std::vector<std::uint8_t> frame = audio.mix(368);
    pcm_.insert(pcm_.end(), frame.begin(), frame.end());
    // Half a second at a time: the Studio's speaker is a player process, so handing it
    // sixty buffers a second would be worse than the sound.
    if (pcm_.size() >= 11025) {
        sound_->playPcm(pcm_, timewarp::hb2::Audio::kSampleRate);
        pcm_.clear();
    }
}

void Hb2Window::refreshStatus() {
    const timewarp::hb2::Video::Mode mode = machine_.video().mode();
    mode_label_->setText(mode == timewarp::hb2::Video::Mode::Text ? tr("MODE 0  80x25")
                         : mode == timewarp::hb2::Video::Mode::Low  ? tr("MODE 1  320x200")
                                                                    : tr("MODE 2  640x480"));
    QString text = QString::fromStdString(machine_.statusText());
    text += tr("  frames %1").arg(static_cast<qulonglong>(machine_.video().frames()));
    text += tr("  cycles %1").arg(static_cast<qulonglong>(machine_.cpu().registers().cycles));
    if (!machine_.scheduler().empty()) {
        text += tr("  tasks %1").arg(machine_.scheduler().size());
    }
    text += tr("  ROM %1").arg(QString::fromStdString(shell_.romName()));
    status_->setText(text);
}

void Hb2Window::refreshMonitor() {
    if (monitor_view_ == nullptr) {
        return;
    }
    QStringList lines;
    lines << QString::fromStdString(monitor_.registers());
    lines << QString();
    const std::uint32_t at = machine_.cpu().registers().pc;
    for (const std::string& row : monitor_.disassemble(at, 12)) {
        lines << QString::fromStdString(row);
    }
    if (machine_.status() == timewarp::hb2::CpuStatus::Faulted) {
        lines << QString::fromStdString(machine_.cpu().fault());
    }
    monitor_view_->setPlainText(lines.join(QLatin1Char('\n')));
}

}  // namespace timewarp::ui
