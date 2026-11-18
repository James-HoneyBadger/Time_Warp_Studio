// SPDX-License-Identifier: MIT
//
// The window itself: the tube, the prompt bar, the status line and the look.

#include "retro_computer_internal.hpp"

#include <algorithm>
#include <optional>

#include <QCloseEvent>
#include <QComboBox>
#include <QFileInfo>
#include <QFont>
#include <QFrame>
#include <QHBoxLayout>
#include <QInputDialog>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPalette>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollBar>
#include <QSettings>
#include <QShortcut>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTabWidget>
#include <QTextCursor>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/canvas/qt_bridge.hpp"
#include "timewarp/core/qt_names.hpp"
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace timewarp::ui {

using namespace retro_detail;

namespace {

constexpr int kScreenTextStretch = 1;
constexpr int kScreenGraphicsStretch = 1;
constexpr auto kBootCopyright = "(C) 1982 HB SYSTEMS";

}  // namespace


RetroComputerWindow::RetroComputerWindow(QWidget* parent) : QMainWindow(parent) {
    setObjectName(QStringLiteral("retroComputerWindow"));
    setWindowTitle(tr("HB-1"));
    setMinimumSize(720, 520);
    resize(1040, 760);
    buildUi();
    restoreSettings();
    prepared_ = true;
    // The settings are only known after restoreSettings, so the look is applied
    // twice: once by buildUi so there is never an unstyled frame, and again here
    // with the colours and sizes this machine was left in.
    applyMachineLook();
    voice_ = new MachineVoice{this};
    if (sound_ == nullptr) {
        sound_ = new SoundPlayer{this};
    }
    voice_->setPlayer(sound_);
    // The machine's own clock: sixty frames a second, and everything that moves moves
    // on it. A program that pokes the sound latch is serviced here, once a frame, which
    // is what a register with no interrupt line would have done.
    clock_ = new HbClock{this};
    connect(clock_, &HbClock::tick, this, [this] { tickFrame(); });
    clock_->start();

    // The disk is mounted before the banner, because the banner prints its volume
    // and the catalogue has to be readable the moment the machine is.
    QString disk_error;
    const bool disk_ok = dos_.open(&disk_error);
    // Boot the restored ROM after the selectors and settings exist, so the first
    // banner and the combo cannot disagree.
    setLanguage(language_);
    if (!disk_ok) {
        appendConsole(QStringLiteral("?HB DOS: %1").arg(disk_error));
    }
}

RetroComputerWindow::~RetroComputerWindow() {
    if (prepared_) {
        saveCurrentProgram();
        saveSettings();
    }
    stopRunner();
    stopRepl();
}

void RetroComputerWindow::buildUi() {
    auto* central = new QWidget{this};
    central->setObjectName(QStringLiteral("retroComputerCentral"));
    setCentralWidget(central);

    auto* column = new QVBoxLayout{central};
    column->setContentsMargins(10, 10, 10, 10);
    column->setSpacing(7);

    buildHeader(column);
    buildScreen(column);
    buildPrompt(column);
    buildStatus(column);

    applyMachineLook();

    // The machine's own keys. Window-scoped, so they work while the computer has
    // focus and cannot steal F9 from the IDE's editor behind it.
    auto* run = new QShortcut{QKeySequence{Qt::Key_F9}, this};
    run->setContext(Qt::WindowShortcut);
    connect(run, &QShortcut::activated, this, &RetroComputerWindow::runProgram);
    auto* stop = new QShortcut{timewarp::core::keySequence(Qt::SHIFT, Qt::Key_F9), this};
    stop->setContext(Qt::WindowShortcut);
    connect(stop, &QShortcut::activated, this, &RetroComputerWindow::stopProgram);
    auto* cls = new QShortcut{timewarp::core::keySequence(Qt::CTRL, Qt::Key_L), this};
    cls->setContext(Qt::WindowShortcut);
    connect(cls, &QShortcut::activated, this, &RetroComputerWindow::clearScreenAndCanvas);
    auto* configure = new QShortcut{timewarp::core::keySequence(Qt::CTRL, Qt::Key_Comma), this};
    configure->setContext(Qt::WindowShortcut);
    connect(configure, &QShortcut::activated, this,
            [this] { openSettings(); });
}

void RetroComputerWindow::buildHeader(QVBoxLayout* column) {
    auto* header = new QFrame{this};
    header->setObjectName(QStringLiteral("retroHeader"));
    header->setFrameShape(QFrame::NoFrame);
    auto* row = new QHBoxLayout{header};
    row->setContentsMargins(14, 8, 14, 8);
    row->setSpacing(10);

    auto* identity = new QVBoxLayout;
    identity->setSpacing(0);
    machine_label_ = new QLabel{QStringLiteral("HB-1"), header};
    machine_label_->setObjectName(QStringLiteral("retroMachineLabel"));
    identity->addWidget(machine_label_);
    tagline_label_ = new QLabel{QStringLiteral("PERSONAL COMPUTER SYSTEM  \u00b7  64K RAM"),
                                header};
    tagline_label_->setObjectName(QStringLiteral("retroTaglineLabel"));
    identity->addWidget(tagline_label_);
    row->addLayout(identity);
    row->addStretch(1);

    auto* rom_box = new QVBoxLayout;
    rom_box->setSpacing(1);
    auto* rom_caption = new QLabel{QStringLiteral("ROM"), header};
    rom_caption->setObjectName(QStringLiteral("retroCaption"));
    rom_box->addWidget(rom_caption);
    language_selector_ = new QComboBox{header};
    language_selector_->setObjectName(QStringLiteral("retroLanguageSelector"));
    language_selector_->setAccessibleName(tr("Language ROM"));
    for (const Language language : allLanguages()) {
        language_selector_->addItem(displayName(language), static_cast<int>(language));
    }
    rom_box->addWidget(language_selector_);
    row->addLayout(rom_box);

    auto* mode_box = new QVBoxLayout;
    mode_box->setSpacing(1);
    auto* mode_caption = new QLabel{QStringLiteral("SCREEN"), header};
    mode_caption->setObjectName(QStringLiteral("retroCaption"));
    mode_box->addWidget(mode_caption);
    mode_selector_ = new QComboBox{header};
    mode_selector_->setObjectName(QStringLiteral("retroModeSelector"));
    mode_selector_->setAccessibleName(tr("Screen mode"));
    mode_selector_->addItem(QStringLiteral("TEXT"),
                            static_cast<int>(ScreenMode::Text));
    mode_selector_->addItem(QStringLiteral("SPLIT"),
                            static_cast<int>(ScreenMode::Split));
    mode_selector_->addItem(QStringLiteral("GRAPHICS"),
                            static_cast<int>(ScreenMode::Graphics));
    mode_box->addWidget(mode_selector_);
    row->addLayout(mode_box);

    lamp_label_ = new QLabel{QStringLiteral("\u25cf POWER"), header};
    lamp_label_->setObjectName(QStringLiteral("retroPowerLamp"));
    lamp_label_->setCursor(Qt::PointingHandCursor);
    lamp_label_->setToolTip(tr("Power. Click to switch the machine off and on."));
    lamp_label_->setAccessibleName(tr("Power switch"));
    lamp_label_->installEventFilter(this);

    config_button_ = new QPushButton{QStringLiteral("CONFIG"), header};
    config_button_->setObjectName(QStringLiteral("retroConfigButton"));
    config_button_->setToolTip(tr("Change what the machine boots into and how it "
                                  "looks (Ctrl+,)"));
    connect(config_button_, &QPushButton::clicked, this, [this] { openSettings(); });
    row->addWidget(config_button_);
#ifdef TIME_WARP_HAVE_AI
    // The machine keeps its 1982 face: the assistant is a dialog opened from a
    // button, not a column of panels beside the tube.
    auto* assistant = new QPushButton{QStringLiteral("\U0001F916"), header};
    assistant->setObjectName(QStringLiteral("retroAssistantButton"));
    assistant->setToolTip(tr("Ask the assistant about the program in this machine"));
    assistant->setAccessibleName(tr("Assistant"));
    connect(assistant, &QPushButton::clicked, this, [this] { openAssistant(); });
    row->addWidget(assistant);
#endif
    row->addWidget(lamp_label_);

    column->addWidget(header);

    connect(language_selector_, QOverload<int>::of(&QComboBox::currentIndexChanged), this, [this](int index) {
        if (!prepared_ || index < 0) {
            return;
        }
        const auto language =
            static_cast<Language>(language_selector_->itemData(index).toInt());
        setLanguage(language);
    });
    connect(mode_selector_, QOverload<int>::of(&QComboBox::currentIndexChanged), this, [this](int index) {
        if (!prepared_ || index < 0) {
            return;
        }
        setScreenMode(static_cast<ScreenMode>(mode_selector_->itemData(index).toInt()));
    });
}

void RetroComputerWindow::buildScreen(QVBoxLayout* column) {
    auto* frame = new QFrame{this};
    frame->setObjectName(QStringLiteral("retroScreenFrame"));
    frame->setFrameShape(QFrame::NoFrame);
    auto* layout = new QVBoxLayout{frame};
    layout->setContentsMargins(3, 3, 3, 3);
    layout->setSpacing(0);

    screen_splitter_ = new QSplitter{Qt::Horizontal, frame};
    screen_splitter_->setObjectName(QStringLiteral("retroScreenSplitter"));
    screen_splitter_->setChildrenCollapsible(false);
    screen_splitter_->setHandleWidth(6);

    text_tabs_ = new QTabWidget{screen_splitter_};
    text_tabs_->setObjectName(QStringLiteral("retroTextTabs"));
    text_tabs_->setDocumentMode(true);
    text_tabs_->setTabPosition(QTabWidget::South);

    program_ = new CodeEditor{text_tabs_};
    program_->setObjectName(QStringLiteral("retroProgramEditor"));
    program_->setAccessibleName(tr("Computer Program Editor"));
    // Its font, tab stops and palette come from applyMachineLook, which has the
    // settings; setting them here too would leave two answers to "how big is the
    // program text".
    text_tabs_->addTab(program_, QStringLiteral("PROGRAM"));

    tube_ = new HbTube{text_tabs_};
    tube_->setObjectName(QStringLiteral("retroConsole"));
    tube_->setMemory(&memory_);
    text_tabs_->addTab(tube_, QStringLiteral("SCREEN"));
    screen_splitter_->addWidget(text_tabs_);
    screen_splitter_->setStretchFactor(0, kScreenTextStretch);

    auto* graphics_pane = new QWidget{screen_splitter_};
    graphics_pane->setObjectName(QStringLiteral("retroGraphicsPane"));
    auto* graphics_layout = new QVBoxLayout{graphics_pane};
    graphics_layout->setContentsMargins(0, 0, 0, 0);
    graphics_layout->setSpacing(0);
    canvas_ = new TurtleCanvas{graphics_pane};
    canvas_->setObjectName(QStringLiteral("retroCanvas"));
    canvas_->setGridVisible(false);
    graphics_layout->addWidget(canvas_, 1);
    screen_splitter_->addWidget(graphics_pane);
    screen_splitter_->setStretchFactor(1, kScreenGraphicsStretch);
    screen_splitter_->setSizes({420, 420});

    layout->addWidget(screen_splitter_, 1);
    column->addWidget(frame, 1);

    // The picture the ROM draws and the text it prints share one tube. The
    // overlay is the tube's glass, not a second surface: transparent to input, so
    // the editor and canvas underneath keep working.
    overlay_ = new CrtOverlay{frame};
    CrtOverlay::Settings crt;
    crt.scanlines = 13;
    crt.vignette = 7;
    crt.curvature = 3;
    crt.flicker = 0;
    crt.phosphor = 8;
    crt.phosphor_colour = QColor{51, 255, 102};
    overlay_->setSettings(crt);
    overlay_->show();
    overlay_->raise();

    queue_ = std::make_unique<timewarp::canvas::CanvasCommandQueue>();
    bridge_ = std::make_unique<timewarp::canvas::qt::CanvasBridge>(*queue_, this);
    bridge_->set_sink([this](timewarp::canvas::CanvasCommandView view) {
        if (canvas_ != nullptr) {
            canvas_->apply(view);
        }
    });

    connect(canvas_, &TurtleCanvas::contentChanged, this, [this] {
        if (canvas_->frameCount() > 0 && mode_ == ScreenMode::Text &&
            !auto_shown_graphics_ && settings_.switch_to_split) {
            auto_shown_graphics_ = true;
            setScreenMode(ScreenMode::Split);
        }
        updateStatus();
    });
    connect(program_, &QPlainTextEdit::cursorPositionChanged, this,
            [this] { updateStatus(); });
    connect(text_tabs_, &QTabWidget::currentChanged, this, [this](int) {
        updateStatus();
    });
}

void RetroComputerWindow::buildPrompt(QVBoxLayout* column) {
    auto* bar = new QFrame{this};
    bar->setObjectName(QStringLiteral("retroPromptBar"));
    bar->setFrameShape(QFrame::NoFrame);
    auto* row = new QHBoxLayout{bar};
    row->setContentsMargins(8, 5, 8, 5);
    row->setSpacing(8);

    prompt_label_ = new QLabel{QStringLiteral("READY."), bar};
    prompt_label_->setObjectName(QStringLiteral("retroPromptLabel"));
    prompt_label_->setMinimumWidth(70);
    prompt_label_->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    row->addWidget(prompt_label_);

    input_ = new QLineEdit{bar};
    input_->setObjectName(QStringLiteral("retroInput"));
    input_->setAccessibleName(tr("Computer Prompt"));
    input_->setClearButtonEnabled(false);
    input_->installEventFilter(this);
    connect(input_, &QLineEdit::returnPressed, this,
            &RetroComputerWindow::handlePromptLine);
    row->addWidget(input_, 1);

    run_button_ = new QPushButton{QStringLiteral("RUN"), bar};
    run_button_->setObjectName(QStringLiteral("retroRunButton"));
    run_button_->setToolTip(tr("Run the program in the editor (F9)"));
    connect(run_button_, &QPushButton::clicked, this, &RetroComputerWindow::runProgram);
    row->addWidget(run_button_);

    stop_button_ = new QPushButton{QStringLiteral("STOP"), bar};
    stop_button_->setObjectName(QStringLiteral("retroStopButton"));
    stop_button_->setToolTip(tr("Stop a running program (Shift+F9)"));
    connect(stop_button_, &QPushButton::clicked, this,
            &RetroComputerWindow::stopProgram);
    row->addWidget(stop_button_);

    auto* clear = new QPushButton{QStringLiteral("CLS"), bar};
    clear->setObjectName(QStringLiteral("retroClearButton"));
    clear->setToolTip(tr("Clear the screen and the drawing (Ctrl+L)"));
    connect(clear, &QPushButton::clicked, this,
            &RetroComputerWindow::clearScreenAndCanvas);
    row->addWidget(clear);

    column->addWidget(bar);
}

void RetroComputerWindow::buildStatus(QVBoxLayout* column) {
    status_label_ = new QLabel{this};
    status_label_->setObjectName(QStringLiteral("retroStatusLabel"));
    status_label_->setTextInteractionFlags(Qt::TextSelectableByMouse);
    column->addWidget(status_label_);
}

void RetroComputerWindow::applyMachineLook() {
    const RetroPalette palette = retroPalette(settings_.tint);

    QFont machine_font{QStringLiteral("Courier New")};
    machine_font.setStyleHint(QFont::Monospace);
    machine_font.setPointSize(11);
    setFont(machine_font);

    QFont screen_font = machine_font;
    screen_font.setPointSize(settings_.screen_text_size);
    if (tube_ != nullptr) {
        const RetroPalette colours = retroPalette(settings_.tint);
        TubePalette tube;
        tube.background = colours.canvas;
        tube.border = colours.border;
        tube.bright = colours.bright;
        tube.dim = colours.dim;
        tube_->setPalette(tube);
        tube_->setTextSize(settings_.screen_text_size);
    }
    if (input_ != nullptr) {
        input_->setFont(screen_font);
    }

    setStyleSheet(machineStyleSheet(palette));

    if (program_ != nullptr) {
        EditorSettings editor;
        editor.font_family = QStringLiteral("Courier New");
        editor.font_point_size = settings_.editor_text_size;
        editor.show_line_numbers = true;
        editor.highlight_current_line = true;
        editor.word_wrap = false;
        editor.auto_indent = true;
        editor.completion = false;
        editor.lint = true;
        editor.show_whitespace = false;
        editor.minimap = false;
        editor.autosave = false;
        program_->applySettings(editor);
        // The editor paints its gutter from the palette, not from the stylesheet.
        // Without this the machine is one colour except for two lighter margins.
        QPalette editor_palette = program_->palette();
        editor_palette.setColor(QPalette::Base, palette.canvas);
        editor_palette.setColor(QPalette::AlternateBase, palette.panel);
        editor_palette.setColor(QPalette::Text, palette.text);
        editor_palette.setColor(QPalette::Highlight, palette.selection);
        editor_palette.setColor(QPalette::HighlightedText, palette.bright);
        program_->setPalette(editor_palette);
    }
    if (canvas_ != nullptr) {
        canvas_->setSurfaceColor(palette.canvas);
    }
    if (repl_ != nullptr) {
        repl_->setSurfaceColor(toRgb(palette.canvas));
    }
    if (overlay_ != nullptr) {
        // The glass is the same type the editor's CRT uses; only its colour and
        // its five strengths come from this machine's settings.
        CrtOverlay::Settings crt = settings_.crt;
        crt.phosphor_colour = palette.text;
        overlay_->setSettings(crt);
    }
}
void RetroComputerWindow::appendConsole(const QString& text) {
    if (tube_ == nullptr) {
        return;
    }
    // One sink, and since 20.1 it writes *into screen memory*: the row lands where the
    // cursor register says, with the ink and paper registers' colours, and a full tube
    // scrolls by moving cells. Everything the machine says -- banner, prompt echo,
    // program output, errors, listings, HB DOS -- goes through here.
    tube_->append(text);
}

void RetroComputerWindow::appendBootBanner() {
    if (settings_.banner == BootBanner::Off) {
        return;
    }
    const RomInfo rom = romFor(language_);
    appendConsole(QStringLiteral("*** HB-1 ***"));
    appendConsole(QStringLiteral("%1 %2 ROM  \u00b7  64K RAM SYSTEM")
                      .arg(displayName(language_), QString::fromUtf8(rom.version)));
    if (settings_.banner == BootBanner::Full) {
        appendConsole(QStringLiteral("%1  \u00b7  %2")
                          .arg(QString::fromUtf8(kBootCopyright),
                               QString::fromUtf8(rom.note)));
        appendConsole(QStringLiteral("HB DOS 1.0  \u00b7  VOLUME %1").arg(dos_.volumeName()));
        appendConsole(QStringLiteral("SYSTEM READY"));
    }
    appendConsole(QStringLiteral("READY."));
}

void RetroComputerWindow::updatePrompt() {
    const RomInfo rom = romFor(language_);
    if (prompt_label_ != nullptr) {
        // On the line the prompt is the board's, not the ROM's: a machine that says
        // BASIC> while it is on a bulletin board is telling the caller something
        // untrue about where they are.
        prompt_label_->setText(online_ == OnlineState::Off
                                   ? QString::fromUtf8(rom.prompt)
                                   : QStringLiteral("BBS>"));
    }
    if (input_ != nullptr) {
        input_->setPlaceholderText(
            tr("Type a command, or a numbered line to enter a program"));
    }
}

void RetroComputerWindow::updateModeControls() {
    if (mode_selector_ != nullptr) {
        const QSignalBlocker blocker{mode_selector_};
        const int index = mode_selector_->findData(static_cast<int>(mode_));
        if (index >= 0) {
            mode_selector_->setCurrentIndex(index);
        }
    }
    if (text_tabs_ != nullptr) {
        text_tabs_->setVisible(mode_ != ScreenMode::Graphics);
    }
    if (canvas_ != nullptr && canvas_->parentWidget() != nullptr) {
        canvas_->parentWidget()->setVisible(mode_ != ScreenMode::Text);
    }
    if (screen_splitter_ != nullptr) {
        if (mode_ == ScreenMode::Split) {
            screen_splitter_->setSizes({420, 420});
        } else if (mode_ == ScreenMode::Text) {
            screen_splitter_->setSizes({840, 0});
        } else {
            screen_splitter_->setSizes({0, 840});
        }
    }
}

void RetroComputerWindow::updateStatus() {
    if (status_label_ == nullptr) {
        return;
    }
    const RomInfo rom = romFor(language_);
    // `MEM` is a reading, not a badge: the free bytes of the address space this
    // machine actually has, which is what stopped being a string literal in 20.1.
    QString line = tr("ROM %1 %2   MEM %4K   MODE %3")
                       .arg(displayName(language_), QString::fromUtf8(rom.version),
                            modeName(mode_))
                       .arg(HbMemory::kUserRamSize / 1024);
    if (program_ != nullptr) {
        const QTextCursor cursor = program_->textCursor();
        line += tr("   LINE %1:%2")
                    .arg(cursor.blockNumber() + 1)
                    .arg(cursor.positionInBlock() + 1);
    }
    line += tr("   HB DOS %1F").arg(dos_.files().size());
    line += runner_ != nullptr ? tr("   RUNNING") : tr("   READY");
    status_label_->setText(line);
}

}  // namespace timewarp::ui
