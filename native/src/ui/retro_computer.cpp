// SPDX-License-Identifier: MIT

#include "timewarp/core/qt_gui_compat.hpp"
#include "timewarp/ui/retro_computer.hpp"

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
#include "timewarp/ui/code_editor.hpp"
#include "timewarp/ui/native_runner.hpp"
#include "timewarp/ui/panels.hpp"
#include "timewarp/ui/repl_runner.hpp"
#include "timewarp/ui/sound_player.hpp"
#include "timewarp/ui/turtle_canvas.hpp"

namespace timewarp::ui {

namespace {

constexpr int kScreenTextStretch = 1;
constexpr int kScreenGraphicsStretch = 1;
constexpr int kConsoleBlockLimit = 4000;
constexpr auto kBootCopyright = "(C) 1982 HB SYSTEMS";

/// The part of a ROM a user sees and types at.
struct RomInfo {
    const char* version = "V1.0";
    const char* prompt = ">";
    const char* note = "";
};

[[nodiscard]] RomInfo romFor(Language language) {
    switch (language) {
        case Language::Basic:
            return {"V2.0", "READY.", "BASIC INTERPRETER"};
        case Language::Pilot:
            return {"V1.0", "PILOT:", "PROGRAMMED INSTRUCTION"};
        case Language::Logo:
            return {"V1.0", "?", "TURTLE LOGO"};
        case Language::Comal:
            return {"V2.1", "]", "STRUCTURED BASIC"};
        case Language::Rexx:
            return {"V3.0", "REXX>", "SCRIPT PROCESSOR"};
        case Language::Scheme:
            return {"V84", ">", "LISP MACHINE"};
        case Language::Focal:
            return {"V1969", "*", "FORMULA CALCULATOR"};
        case Language::Chip8:
            return {"V1.0", "CHIP8>", "VIRTUAL CONSOLE"};
        case Language::Apl:
            return {"V1.0", "APL>", "ARRAY PROCESSOR"};
        case Language::Smalltalk:
            return {"V80", "ST>", "OBJECT WORKBENCH"};
        case Language::Unknown:
            break;
    }
    return {};
}

[[nodiscard]] QString modeName(RetroComputerWindow::ScreenMode mode) {
    switch (mode) {
        case RetroComputerWindow::ScreenMode::Text:
            return QStringLiteral("TEXT");
        case RetroComputerWindow::ScreenMode::Graphics:
            return QStringLiteral("GRAPHICS");
        case RetroComputerWindow::ScreenMode::Split:
            return QStringLiteral("SPLIT");
    }
    return QStringLiteral("SPLIT");
}

/// The first word of a statement, uppercased. The machine's own words are
/// matched on this rather than on the whole line so `CLS` and `cls` agree.
[[nodiscard]] QString headWord(const QString& command) {
    const QString trimmed = command.trimmed();
    const int space = trimmed.indexOf(QRegularExpression(QStringLiteral("\\s")));
    return (space < 0 ? trimmed : trimmed.left(space)).toUpper();
}

/// The machine's stylesheet for one palette. Written with tokens and replaced
/// rather than `.arg()`, because a 100-line stylesheet with twenty placeholders is
/// a place where a missing argument silently shifts every colour after it.
[[nodiscard]] QString machineStyleSheet(const RetroPalette& palette) {
    QString sheet = QStringLiteral(R"(
        QWidget#retroComputerCentral {
            background-color: @background@;
        }
        QFrame#retroHeader {
            background-color: @panel@;
            border: 1px solid @border@;
            border-radius: 3px;
        }
        QLabel#retroMachineLabel {
            color: @bright@;
            font-family: "Courier New", monospace;
            font-size: 24px;
            font-weight: bold;
            letter-spacing: 2px;
        }
        QLabel#retroTaglineLabel, QLabel#retroCaption {
            color: @dim@;
            font-family: "Courier New", monospace;
            font-size: 10px;
            letter-spacing: 1px;
        }
        QLabel#retroPowerLamp {
            color: @accent@;
            font-weight: bold;
            padding-left: 8px;
        }
        QFrame#retroScreenFrame {
            background-color: @canvas@;
            border: 2px solid @border@;
            border-radius: 5px;
        }
        QFrame#retroPromptBar {
            background-color: @panel@;
            border: 1px solid @border@;
            border-radius: 3px;
        }
        QLabel#retroPromptLabel {
            color: @accent@;
            font-family: "Courier New", monospace;
            font-size: 13px;
            font-weight: bold;
        }
        QLabel#retroStatusLabel {
            color: @dim@;
            font-family: "Courier New", monospace;
            font-size: 10px;
        }
        QComboBox {
            background-color: @background@;
            color: @bright@;
            border: 1px solid @border@;
            padding: 2px 7px;
            min-width: 92px;
        }
        QComboBox QAbstractItemView {
            background-color: @background@;
            color: @bright@;
            selection-background-color: @selection@;
        }
        QLineEdit, QPlainTextEdit {
            background-color: @background@;
            color: @text@;
            border: 1px solid @border@;
            selection-background-color: @text@;
            selection-color: @background@;
        }
        QPlainTextEdit#retroProgramEditor {
            background-color: @background@;
            color: @text@;
        }
        QPushButton {
            background-color: @panel@;
            color: @bright@;
            border: 1px solid @border@;
            padding: 3px 11px;
            font-weight: bold;
        }
        QPushButton:hover {
            background-color: @selection@;
        }
        QPushButton:disabled {
            color: @dim@;
            border-color: @border@;
        }
        QTabWidget::pane {
            border: 1px solid @border@;
        }
        QTabBar::tab {
            background-color: @background@;
            color: @dim@;
            border: 1px solid @border@;
            padding: 2px 12px;
        }
        QTabBar::tab:selected {
            background-color: @selection@;
            color: @bright@;
        }
        QScrollBar:vertical {
            background: @background@;
            width: 12px;
        }
        QScrollBar::handle:vertical {
            background: @border@;
            min-height: 20px;
        }
    )");

    const auto paint = [&sheet](const char* token, const QColor& colour) {
        sheet.replace(QString::fromLatin1(token), colour.name());
    };
    paint("@background@", palette.background);
    paint("@panel@", palette.panel);
    paint("@border@", palette.border);
    paint("@text@", palette.text);
    paint("@bright@", palette.bright);
    paint("@dim@", palette.dim);
    paint("@accent@", palette.accent);
    paint("@selection@", palette.selection);
    paint("@canvas@", palette.canvas);
    return sheet;
}

/// The canvas and the native engines take a surface colour as a plain triplet.
[[nodiscard]] timewarp::lang::Rgb toRgb(const QColor& colour) {
    return timewarp::lang::Rgb{colour.red(), colour.green(), colour.blue()};
}

/// Everything after the command word, split on commas outside quotes, with the
/// quotes removed: `RENAME "A","B"` and `RENAME A,B` are the same request.
[[nodiscard]] QStringList dosArguments(const QString& command) {
    const QString trimmed = command.trimmed();
    const int space = trimmed.indexOf(QRegularExpression(QStringLiteral("\\s")));
    const QString rest = space < 0 ? QString{} : trimmed.mid(space + 1);

    QStringList arguments;
    QString current;
    bool quoted = false;
    for (const QChar character : rest) {
        if (character == QLatin1Char('"') || character == QLatin1Char('\'')) {
            quoted = !quoted;
            continue;
        }
        if (character == QLatin1Char(',') && !quoted) {
            arguments.append(current.trimmed());
            current.clear();
            continue;
        }
        current.append(character);
    }
    if (!current.trimmed().isEmpty() || !arguments.isEmpty()) {
        arguments.append(current.trimmed());
    }
    return arguments;
}

/// The first argument of a command, or an empty string: `SAVE "X"` and `SAVE X`
/// are the same request.
[[nodiscard]] QString firstArgument(const QString& command) {
    const QStringList arguments = dosArguments(command);
    return arguments.isEmpty() ? QString{} : arguments.first();
}

/// The name at the end of `SAVE MACHINE "N"` or `SAVEMACHINE "N"`.
///
/// `dosArguments` reads from the second word, which for the two-word spelling is
/// `MACHINE` itself -- so the volume this wrote was called `MACHINE.hb1` and the
/// name that was typed went nowhere.
[[nodiscard]] QString machineArgument(const QString& command) {
    const QString trimmed = command.trimmed();
    QString rest = trimmed.mid(headWord(trimmed).size()).trimmed();
    const QString machine = QStringLiteral("MACHINE");
    if (rest.toUpper().startsWith(machine)) {
        rest = rest.mid(machine.size()).trimmed();
    }
    return firstArgument(QStringLiteral("X ") + rest);
}

/// The catalogue's type column: the ROM the file belongs to, in capitals.
[[nodiscard]] QString dosTypeName(const QString& language_key) {
    const Language language = languageFromKey(language_key);
    if (language == Language::Unknown) {
        return language_key.toUpper();
    }
    return displayName(language).toUpper();
}

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

    config_button_ = new QPushButton{QStringLiteral("CONFIG"), header};
    config_button_->setObjectName(QStringLiteral("retroConfigButton"));
    config_button_->setToolTip(tr("Change what the machine boots into and how it "
                                  "looks (Ctrl+,)"));
    connect(config_button_, &QPushButton::clicked, this, [this] { openSettings(); });
    row->addWidget(config_button_);
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

    console_ = new QPlainTextEdit{text_tabs_};
    console_->setObjectName(QStringLiteral("retroConsole"));
    console_->setAccessibleName(tr("Computer Screen"));
    console_->setReadOnly(true);
    console_->setLineWrapMode(QPlainTextEdit::NoWrap);
    console_->setMaximumBlockCount(kConsoleBlockLimit);
    text_tabs_->addTab(console_, QStringLiteral("OUTPUT"));
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
    if (console_ != nullptr) {
        console_->setFont(screen_font);
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
    if (console_ == nullptr) {
        return;
    }
    QString body = text;
    while (body.endsWith(QLatin1Char('\n')) || body.endsWith(QLatin1Char('\r'))) {
        body.chop(1);
    }
    if (!body.isEmpty()) {
        console_->appendPlainText(body);
    }
    if (console_->verticalScrollBar() != nullptr) {
        console_->verticalScrollBar()->setValue(console_->verticalScrollBar()->maximum());
    }
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
        prompt_label_->setText(QString::fromUtf8(rom.prompt));
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
    QString line = tr("ROM %1 %2   MEM 64K   MODE %3")
                       .arg(displayName(language_), QString::fromUtf8(rom.version),
                            modeName(mode_));
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

void RetroComputerWindow::handlePromptLine() {
    if (input_ == nullptr) {
        return;
    }
    const QString command = input_->text();
    input_->clear();
    input_->setFocus(Qt::ShortcutFocusReason);
    submitCommand(command);
}

void RetroComputerWindow::submitCommand(const QString& command) {
    const QString trimmed = command.trimmed();
    if (trimmed.isEmpty()) {
        return;
    }

    if (history_.isEmpty() || history_.last() != trimmed) {
        history_.append(trimmed);
    }
    applyHistoryDepth();
    history_index_ = static_cast<int>(history_.size());

    // Saying anything else after INIT's warning is a change of mind, whether it is
    // a machine word or a language command.
    if (headWord(trimmed) != QStringLiteral("INIT")) {
        init_pending_ = false;
    }

    const RomInfo rom = romFor(language_);
    appendConsole(QStringLiteral("%1 %2")
                      .arg(QString::fromUtf8(rom.prompt), trimmed));

    if (language_ == Language::Basic && handleBasicLine(trimmed)) {
        emit commandFinished();
        return;
    }
    if (handleMachineCommand(trimmed)) {
        emit commandFinished();
        return;
    }

    ensureRepl();
    if (repl_ != nullptr) {
        repl_->submit(trimmed);
    }
}

bool RetroComputerWindow::handleBasicLine(const QString& command) {
    static const QRegularExpression numbered{
        QStringLiteral(R"(^\s*(\d+)\s*(.*)$)")};
    const QRegularExpressionMatch match = numbered.match(command);
    if (!match.hasMatch()) {
        return false;
    }

    const QString number = match.captured(1);
    const QString body = match.captured(2).trimmed();
    // A bare `2 + 2` starts with a digit, but it is an expression, not line 2.
    // A program line starts with a keyword, the `?` shorthand or a comment mark.
    if (!body.isEmpty()) {
        const QChar first = body.front();
        if (!(first.isLetter() || first == QLatin1Char('?') ||
              first == QLatin1Char('\'') || first == QLatin1Char('#'))) {
            return false;
        }
    }

    QStringList lines = program_->toPlainText().split(QLatin1Char('\n'));
    int existing = -1;
    int insert_at = -1;
    const int wanted = number.toInt();
    for (int index = 0; index < lines.size(); ++index) {
        const QRegularExpressionMatch other = numbered.match(lines.at(index));
        if (!other.hasMatch()) {
            continue;
        }
        const int other_number = other.captured(1).toInt();
        if (other_number == wanted) {
            existing = index;
        }
        if (insert_at < 0 && other_number > wanted) {
            insert_at = index;
        }
    }

    if (body.isEmpty()) {
        if (existing < 0) {
            status_label_->setText(tr("?LINE %1 NOT FOUND").arg(number));
            return true;
        }
        lines.removeAt(existing);
        program_->setPlainText(lines.join(QLatin1Char('\n')));
        program_->document()->setModified(true);
        text_tabs_->setCurrentWidget(program_);
        status_label_->setText(tr("LINE %1 DELETED").arg(number));
        return true;
    }

    const QString stored = QStringLiteral("%1 %2").arg(number, body);
    if (existing >= 0) {
        lines[existing] = stored;
        insert_at = existing;
    } else {
        if (insert_at < 0) {
            insert_at = static_cast<int>(lines.size());
        }
        if (lines.size() == 1 && lines.front().trimmed().isEmpty()) {
            lines.clear();
            insert_at = 0;
        }
        lines.insert(insert_at, stored);
    }

    program_->setPlainText(lines.join(QLatin1Char('\n')));
    program_->document()->setModified(true);
    QTextCursor cursor{program_->document()};
    cursor.movePosition(QTextCursor::Start);
    for (int line = 0; line < insert_at; ++line) {
        cursor.movePosition(QTextCursor::Down);
    }
    program_->setTextCursor(cursor);
    text_tabs_->setCurrentWidget(program_);
    status_label_->setText(tr("LINE %1 STORED").arg(number));
    return true;
}

bool RetroComputerWindow::handleMachineCommand(const QString& command) {
    const QString word = headWord(command);
    if (word == QStringLiteral("INIT")) {
        // The one word that erases a disk. It has to be said twice, and saying
        // anything else in between is a change of mind.
        if (!init_pending_) {
            init_pending_ = true;
            appendConsole(QStringLiteral("?INIT ERASES EVERY FILE ON THE DISK"));
            appendConsole(QStringLiteral("TYPE INIT AGAIN TO GO AHEAD"));
            return true;
        }
        init_pending_ = false;
        formatDisk();
        return true;
    }
    if (word == QStringLiteral("RUN")) {
        const QStringList arguments = dosArguments(command);
        if (arguments.isEmpty() || arguments.first().isEmpty()) {
            runProgram();
        } else {
            runFromDisk(arguments.first());
        }
        return true;
    }
    if (word == QStringLiteral("LIST")) {
        text_tabs_->setCurrentWidget(console_);
        const QString source = program_->toPlainText();
        if (source.trimmed().isEmpty()) {
            appendConsole(QStringLiteral("?NO PROGRAM"));
        } else {
            const QStringList lines = source.split(QLatin1Char('\n'));
            for (const QString& line : lines) {
                appendConsole(line);
            }
        }
        return true;
    }
    if (word == QStringLiteral("NEW")) {
        stopRepl();
        program_->clear();
        programs_.remove(static_cast<int>(language_));
        clearScreenAndCanvas();
        appendConsole(QStringLiteral("READY."));
        updateStatus();
        return true;
    }
    if (word == QStringLiteral("CLS") || word == QStringLiteral("CLEAR")) {
        clearScreenAndCanvas();
        return true;
    }
    if (word == QStringLiteral("CONFIG") || word == QStringLiteral("SETUP")) {
        openSettings();
        return true;
    }
    if (word == QStringLiteral("TEXT")) {
        setScreenMode(ScreenMode::Text);
        return true;
    }
    if (word == QStringLiteral("GRAPHICS") || word == QStringLiteral("GFX")) {
        setScreenMode(ScreenMode::Graphics);
        return true;
    }
    if (word == QStringLiteral("SPLIT")) {
        setScreenMode(ScreenMode::Split);
        return true;
    }
    if (word == QStringLiteral("EDIT")) {
        if (mode_ == ScreenMode::Graphics) {
            setScreenMode(ScreenMode::Split);
        }
        text_tabs_->setCurrentWidget(program_);
        program_->setFocus(Qt::ShortcutFocusReason);
        return true;
    }
    if (word == QStringLiteral("ROMS")) {
        text_tabs_->setCurrentWidget(console_);
        for (const Language language : allLanguages()) {
            appendConsole(QStringLiteral("%1 %2")
                              .arg(language == language_ ? QLatin1Char('>')
                                                         : QLatin1Char(' '))
                              .arg(displayName(language)));
        }
        return true;
    }
    if (word == QStringLiteral("CATALOG") || word == QStringLiteral("CAT")) {
        listDisk();
        return true;
    }
    if (word == QStringLiteral("STATUS")) {
        showDiskStatus();
        return true;
    }
    // `SAVE MACHINE "N` and `LOAD MACHINE "N` are checked before `SAVE`/`LOAD`,
    // because the word splitter would otherwise read them as those two words with a
    // file called MACHINE.
    if (word == QStringLiteral("SAVEMACHINE") ||
        command.trimmed().startsWith(QStringLiteral("SAVE MACHINE"))) {
        saveMachineTo(machineArgument(command));
        return true;
    }
    if (word == QStringLiteral("LOADMACHINE") ||
        command.trimmed().startsWith(QStringLiteral("LOAD MACHINE"))) {
        loadMachineFrom(machineArgument(command));
        return true;
    }
    if (word == QStringLiteral("SAVE")) {
        saveToDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("LOAD")) {
        loadFromDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("DELETE") || word == QStringLiteral("ERA")) {
        eraseFromDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("RENAME") || word == QStringLiteral("REN")) {
        const QStringList arguments = dosArguments(command);
        renameOnDisk(arguments.value(0), arguments.value(1));
        return true;
    }
    if (word == QStringLiteral("LOCK")) {
        lockOnDisk(firstArgument(command), true);
        return true;
    }
    if (word == QStringLiteral("UNLOCK")) {
        lockOnDisk(firstArgument(command), false);
        return true;
    }
    if (word == QStringLiteral("VERIFY") || word == QStringLiteral("VER")) {
        verifyOnDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("TYPE")) {
        typeFromDisk(firstArgument(command));
        return true;
    }
    if (word == QStringLiteral("HELP") || command.trimmed() == QStringLiteral("?")) {
        text_tabs_->setCurrentWidget(console_);
        appendConsole(QStringLiteral("RUN [\"N\"]  run the program; with a name, load N and run it"));
        appendConsole(QStringLiteral("LIST         list the program"));
        appendConsole(QStringLiteral("NEW          erase the program and the variables"));
        appendConsole(QStringLiteral("EDIT         move to the program editor"));
        appendConsole(QStringLiteral("TEXT / GFX / SPLIT   change the screen"));
        appendConsole(QStringLiteral("CLS          clear the screen and the drawing"));
        appendConsole(QStringLiteral("ROMS         list the language ROMs"));
        appendConsole(QStringLiteral("CONFIG       change the machine's settings"));
        if (language_ == Language::Basic) {
            appendConsole(QStringLiteral("10 ...       store a program line;  10  deletes it"));
        }
        appendConsole(QString());
        appendConsole(QStringLiteral("HB DOS 1.0"));
        appendConsole(QStringLiteral("CATALOG      list the disk"));
        appendConsole(QStringLiteral("SAVE \"N\"     save the program as N"));
        appendConsole(QStringLiteral("LOAD \"N\"     load N into memory"));
        appendConsole(QStringLiteral("DELETE \"N\"   erase N (LOCK protects it)"));
        appendConsole(QStringLiteral("RENAME \"A\",\"B\"  rename a file"));
        appendConsole(QStringLiteral("LOCK / UNLOCK \"N\"  protect a file"));
        appendConsole(QStringLiteral("VERIFY \"N\"   compare memory with the disk"));
        appendConsole(QStringLiteral("TYPE \"N\"     print a file"));
        appendConsole(QStringLiteral("INIT         erase the whole disk (asks twice)"));
        appendConsole(QString());
        appendConsole(QStringLiteral("SAVE MACHINE \"N\"  write the whole machine to N.hb1"));
        appendConsole(QStringLiteral("LOAD MACHINE \"N\"  boot from N.hb1"));
        appendConsole(QStringLiteral("STATUS       volume and free space"));
        appendConsole(QString());
        appendConsole(QStringLiteral("F9 runs; Shift+F9 stops; Ctrl+L clears; Ctrl+, configures."));
        return true;
    }
    return false;
}

void RetroComputerWindow::saveMachineTo(const QString& name) {
    const QString path = MachineImage::resolvePath(name);
    if (path.isEmpty()) {
        appendConsole(QStringLiteral("?SAVE MACHINE \"NAME\""));
        return;
    }
    QString error;
    if (!saveMachine(path, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error.toUpper()));
        return;
    }
    appendConsole(QStringLiteral("MACHINE SAVED: %1").arg(path));
    appendConsole(QStringLiteral("%1 PROGRAM(S), %2 FILE(S) ON THE DISK")
                      .arg(machineImage().programs.size())
                      .arg(dos_.files().size()));
}

void RetroComputerWindow::loadMachineFrom(const QString& name) {
    const QString path = MachineImage::resolvePath(name);
    if (path.isEmpty()) {
        appendConsole(QStringLiteral("?LOAD MACHINE \"NAME\""));
        return;
    }
    if (!QFileInfo::exists(path)) {
        appendConsole(QStringLiteral("?NO MACHINE IMAGE: %1").arg(path.toUpper()));
        return;
    }
    // The console is cleared by the reboot that follows, so the line that says what
    // was loaded is printed by `reboot`'s banner path instead of here.
    QString error;
    if (!loadMachine(path, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error.toUpper()));
        return;
    }
    appendConsole(QStringLiteral("MACHINE LOADED: %1").arg(path));
}

void RetroComputerWindow::ensureRepl() {
    if (repl_ != nullptr) {
        return;
    }
    const std::optional<timewarp::lang::Language> native = nativeLanguage(language_);
    if (!native.has_value() || queue_ == nullptr) {
        appendConsole(QStringLiteral("?NO LANGUAGE ROM"));
        return;
    }

    repl_ = new ReplRunner{*native, *queue_, this};
    repl_->setWakeup([this] {
        if (bridge_ != nullptr) {
            bridge_->wake();
        }
    });
    repl_->setSurfaceColor(toRgb(retroPalette(settings_.tint).canvas));
    connect(repl_, &ReplRunner::outputAvailable, this,
            [this](const QString& text) { appendConsole(text); });
    connect(repl_, &ReplRunner::commandFinished, this,
            [this](bool /*ok*/, const QString& error) {
                if (runner_ == nullptr && canvas_ != nullptr) {
                    if (bridge_ != nullptr) {
                        bridge_->drainNow();
                    }
                    canvas_->fitToContent();
                }
                if (!error.isEmpty()) {
                    appendConsole(QStringLiteral("?%1").arg(error));
                }
                updateStatus();
                // A prompt command has returned, so the next one can be typed.
                emit commandFinished();
            });
}

void RetroComputerWindow::stopRepl() {
    if (repl_ == nullptr) {
        return;
    }
    repl_->requestStop();
    repl_->wait();
    delete repl_;
    repl_ = nullptr;
}

void RetroComputerWindow::stopRunner() {
    if (runner_ == nullptr) {
        return;
    }
    NativeRunner* runner = runner_;
    runner->request_stop();
    runner->wait();
    if (runner_ == runner) {
        runner_ = nullptr;
        runner->deleteLater();
    }
}

void RetroComputerWindow::runProgram() {
    if (runner_ != nullptr) {
        return;
    }
    const QString source = program_ != nullptr ? program_->toPlainText() : QString{};
    if (source.trimmed().isEmpty()) {
        appendConsole(QStringLiteral("?NO PROGRAM"));
        return;
    }
    const std::optional<timewarp::lang::Language> native = nativeLanguage(language_);
    if (!native.has_value() || queue_ == nullptr) {
        appendConsole(QStringLiteral("?NO LANGUAGE ROM"));
        return;
    }

    // One engine at a time: a REPL command in flight would be a second program
    // drawing into the same tube.
    stopRepl();
    text_tabs_->setCurrentWidget(console_);
    appendConsole(QStringLiteral("* RUN *"));
    if (bridge_ != nullptr) {
        bridge_->drainNow();
    }
    canvas_->reset();
    auto_shown_graphics_ = false;

    auto* runner = new NativeRunner{*native, *queue_, this};
    runner_ = runner;
    runner->set_wakeup([this] {
        if (bridge_ != nullptr) {
            bridge_->wake();
        }
    });
    runner->set_surface_color(toRgb(retroPalette(settings_.tint).canvas));
    if (sound_ == nullptr) {
        sound_ = new SoundPlayer{this};
    }
    runner->set_sound_player(sound_);
    runner->load_program(source);

    connect(runner, &NativeRunner::outputAvailable, this,
            [this, runner](const QString& text) {
                if (runner_ != runner) {
                    return;
                }
                appendConsole(text);
            });
    connect(runner, &NativeRunner::inputRequested, this,
            [this, runner](const QString& prompt) {
                if (runner_ != runner) {
                    return;
                }
                bool accepted = false;
                const QString answer = QInputDialog::getText(
                    this, tr("The program is waiting for input"), prompt,
                    QLineEdit::Normal, QString{}, &accepted);
                runner->answerInput(accepted ? answer : QString{});
            });
    connect(runner, &NativeRunner::run_finished, this,
            [this, runner](bool completed, const QString& error) {
                if (runner_ != runner) {
                    return;
                }
                if (bridge_ != nullptr) {
                    bridge_->drainNow();
                }
                if (canvas_ != nullptr) {
                    canvas_->fitToContent();
                }
                if (!completed) {
                    appendConsole(QStringLiteral("?STOPPED"));
                }
                if (!error.isEmpty()) {
                    appendConsole(QStringLiteral("?%1").arg(error));
                }
                appendConsole(QStringLiteral("READY."));
                runner_ = nullptr;
                runner->deleteLater();
                if (canvas_ != nullptr && canvas_->frameCount() > 0 &&
                    mode_ == ScreenMode::Text && settings_.switch_to_split) {
                    setScreenMode(ScreenMode::Split);
                }
                updateStatus();
                emit runFinished();
            });

    updateStatus();
    runner->start();
}

void RetroComputerWindow::stopProgram() {
    if (runner_ == nullptr) {
        appendConsole(QStringLiteral("?NOT RUNNING"));
        return;
    }
    // The runner reports the stop through `run_finished`, so the console and the
    // status line are updated in one place whether the program stopped itself or
    // the user did.
    runner_->request_stop();
}

void RetroComputerWindow::clearScreenAndCanvas() {
    if (console_ != nullptr) {
        console_->clear();
    }
    if (bridge_ != nullptr) {
        bridge_->drainNow();
    }
    if (canvas_ != nullptr) {
        canvas_->reset();
    }
    auto_shown_graphics_ = false;
    updateStatus();
}

// ---------------------------------------------------------------------------
// HB DOS
//
// The volume's own behaviour lives in `HbDos`; this is the prompt's half: parse
// what was typed, print what happened, and keep the status line's file count in
// step. Every refusal is printed the way the machine refuses everything else --
// a question mark and the reason -- because that is what a learner reads.
// ---------------------------------------------------------------------------

void RetroComputerWindow::listDisk() {
    text_tabs_->setCurrentWidget(console_);
    appendConsole(QStringLiteral("DISK VOLUME %1   %2   %3 SECTORS")
                      .arg(HbDos::kVolumeNumber)
                      .arg(dos_.volumeName())
                      .arg(HbDos::kTotalSectors));
    appendConsole(QString());
    if (dos_.files().isEmpty()) {
        appendConsole(QStringLiteral(" NO FILES"));
    } else {
        for (const DosFile& file : dos_.files()) {
            appendConsole(QStringLiteral("%1%2 %3  %4")
                              .arg(file.locked ? QLatin1Char('*') : QLatin1Char(' '))
                              .arg(dosTypeName(file.language).leftJustified(10))
                              .arg(HbDos::sectorsFor(file.source), 3, 10,
                                   QLatin1Char('0'))
                              .arg(file.name));
        }
    }
    appendConsole(QString());
    appendConsole(QStringLiteral("%1 FILES   %2 SECTORS USED   %3 FREE")
                      .arg(dos_.files().size())
                      .arg(dos_.usedSectors())
                      .arg(dos_.freeSectors()));
}

void RetroComputerWindow::showDiskStatus() {
    text_tabs_->setCurrentWidget(console_);
    appendConsole(QStringLiteral("HB DOS 1.0   VOLUME %1").arg(dos_.volumeName()));
    appendConsole(QStringLiteral("%1 SECTORS   %2 USED   %3 FREE")
                      .arg(HbDos::kTotalSectors)
                      .arg(dos_.usedSectors())
                      .arg(dos_.freeSectors()));
    appendConsole(QStringLiteral("%1 FILES").arg(dos_.files().size()));
    appendConsole(QStringLiteral("IMAGE %1").arg(dos_.imagePath()));
}

void RetroComputerWindow::saveToDisk(const QString& name) {
    if (name.isEmpty()) {
        appendConsole(QStringLiteral("?FILENAME NEEDED"));
        return;
    }
    if (program_ == nullptr || program_->toPlainText().trimmed().isEmpty()) {
        appendConsole(QStringLiteral("?NO PROGRAM"));
        return;
    }

    const QString source = program_->toPlainText();
    QString error;
    if (!dos_.write(name, settingsKey(language_), source, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 SAVED   %2 SECTORS")
                      .arg(HbDos::normaliseName(name))
                      .arg(HbDos::sectorsFor(source)));
    updateStatus();
}

void RetroComputerWindow::loadFromDisk(const QString& name) {
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    const Language language = languageFromKey(file.language);
    if (language != Language::Unknown && language != language_) {
        // The file knows which ROM can run it, so loading it can cold-boot.
        setLanguage(language);
    }
    loadProgram(file.source);
    text_tabs_->setCurrentWidget(console_);
    appendConsole(QStringLiteral("%1 LOADED").arg(file.name));
    updateStatus();
}

void RetroComputerWindow::runFromDisk(const QString& name) {
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    const Language language = languageFromKey(file.language);
    if (language != Language::Unknown && language != language_) {
        setLanguage(language);
    }
    loadProgram(file.source);
    runProgram();
}

void RetroComputerWindow::eraseFromDisk(const QString& name) {
    if (name.isEmpty()) {
        appendConsole(QStringLiteral("?FILENAME NEEDED"));
        return;
    }
    const QString erased = HbDos::normaliseName(name);
    QString error;
    if (!dos_.erase(name, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 DELETED").arg(erased));
    updateStatus();
}

void RetroComputerWindow::renameOnDisk(const QString& from, const QString& to) {
    if (from.isEmpty() || to.isEmpty()) {
        appendConsole(QStringLiteral("?RENAME \"OLD\",\"NEW\""));
        return;
    }
    QString error;
    if (!dos_.rename(from, to, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 RENAMED TO %2")
                      .arg(HbDos::normaliseName(from), HbDos::normaliseName(to)));
}

void RetroComputerWindow::lockOnDisk(const QString& name, bool locked) {
    if (name.isEmpty()) {
        appendConsole(QStringLiteral("?FILENAME NEEDED"));
        return;
    }
    QString error;
    if (!dos_.setLocked(name, locked, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("%1 %2")
                      .arg(HbDos::normaliseName(name),
                           locked ? tr("LOCKED") : tr("UNLOCKED")));
}

void RetroComputerWindow::verifyOnDisk(const QString& name) {
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    // VERIFY compares the disk with memory, as it did on the machines this
    // imitates: load a file, edit it, and VERIFY is how you find out.
    if (program_ != nullptr && program_->toPlainText() == file.source) {
        appendConsole(QStringLiteral("%1 VERIFIED").arg(file.name));
    } else {
        appendConsole(QStringLiteral("?%1 DIFFERS FROM MEMORY").arg(file.name));
    }
}

void RetroComputerWindow::typeFromDisk(const QString& name) {
    DosFile file;
    QString error;
    if (!dos_.read(name, &file, &error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    text_tabs_->setCurrentWidget(console_);
    appendConsole(QStringLiteral("%1  %2  %3 SECTORS")
                      .arg(file.name, dosTypeName(file.language))
                      .arg(HbDos::sectorsFor(file.source), 3, 10, QLatin1Char('0')));
    const QStringList lines = file.source.split(QLatin1Char('\n'));
    for (const QString& line : lines) {
        appendConsole(line);
    }
}

void RetroComputerWindow::formatDisk() {
    QString error;
    if (!dos_.format(&error)) {
        appendConsole(QStringLiteral("?%1").arg(error));
        return;
    }
    appendConsole(QStringLiteral("HB DOS 1.0   VOLUME %1").arg(dos_.volumeName()));
    appendConsole(QStringLiteral("ALL FILES ERASED"));
    updateStatus();
}

void RetroComputerWindow::setLanguage(Language language) {
    if (prepared_ && booted_ && language == language_) {
        return;
    }
    if (booted_) {
        saveCurrentProgram();
    }
    stopRunner();
    stopRepl();

    language_ = language;
    if (language_selector_ != nullptr) {
        const QSignalBlocker blocker{language_selector_};
        const int index = language_selector_->findData(static_cast<int>(language_));
        if (index >= 0) {
            language_selector_->setCurrentIndex(index);
        }
    }
    if (program_ != nullptr) {
        program_->setLanguage(language_);
        program_->setPlainText(programs_.value(static_cast<int>(language_)));
    }
    updatePrompt();
    if (console_ != nullptr) {
        console_->clear();
    }
    if (bridge_ != nullptr) {
        bridge_->drainNow();
    }
    if (canvas_ != nullptr) {
        canvas_->reset();
    }
    auto_shown_graphics_ = false;
    appendBootBanner();
    text_tabs_->setCurrentWidget(console_);
    booted_ = true;
    prepared_ = true;
    updateModeControls();
    updateStatus();
    setWindowTitle(tr("HB-1 \u2014 %1").arg(displayName(language_)));
}

void RetroComputerWindow::setScreenMode(ScreenMode mode) {
    mode_ = mode;
    updateModeControls();
    updateStatus();
}

MachineImage RetroComputerWindow::machineImage() const {
    MachineImage image;
    image.name = machine_label_ != nullptr ? machine_label_->text() : QStringLiteral("HB-1");
    image.language = settingsKey(language_);
    image.mode = static_cast<int>(mode_);
    // The program in the editor is the current ROM's, and it is the newest thing
    // the machine knows: the copy in `programs_` is what was there when the ROM was
    // last switched away from.
    QHash<int, QString> programs = programs_;
    if (program_ != nullptr) {
        programs.insert(static_cast<int>(language_), program_->toPlainText());
    }
    for (auto it = programs.cbegin(); it != programs.cend(); ++it) {
        const QString key = settingsKey(static_cast<Language>(it.key()));
        if (!it.value().isEmpty()) {
            image.programs.insert(key, it.value());
        }
    }
    image.config = settings_;
    image.volume = dos_.toJson();
    return image;
}

void RetroComputerWindow::applyMachineImage(const MachineImage& image, bool boot) {
    const std::optional<Language> language = languageFromKey(image.language);
    if (!language.has_value()) {
        // A ROM this build does not have: keep the one that is in the socket rather
        // than emptying it. The image is still applied in every other respect, so a
        // machine from a build with one more language is usable rather than refused.
        appendConsole(QStringLiteral("?UNKNOWN ROM IN IMAGE: %1").arg(image.language));
    }

    programs_.clear();
    for (auto it = image.programs.cbegin(); it != image.programs.cend(); ++it) {
        const std::optional<Language> rom = languageFromKey(it.key());
        if (rom.has_value()) {
            programs_.insert(static_cast<int>(*rom), it.value());
        }
    }

    // The configuration first, because the boot ROM and the boot screen in it decide
    // what a cold boot does -- and a machine that is being *booted* has to be set up
    // before it starts.
    applySettings(image.config, false);

    if (booted_) {
        saveCurrentProgram();
    }
    if (language.has_value()) {
        language_ = *language;
        if (language_selector_ != nullptr) {
            const QSignalBlocker blocker{language_selector_};
            const int index = language_selector_->findData(static_cast<int>(language_));
            if (index >= 0) {
                language_selector_->setCurrentIndex(index);
            }
        }
        if (program_ != nullptr) {
            program_->setLanguage(*language);
            program_->setPlainText(programs_.value(static_cast<int>(*language)));
        }
    }
    setScreenMode(static_cast<ScreenMode>(std::clamp(image.mode, 0, 2)));

    QString volume_error;
    if (!dos_.fromJson(image.volume, &volume_error)) {
        appendConsole(QStringLiteral("?%1").arg(volume_error));
    } else if (!dos_.commit(&volume_error)) {
        // A machine whose disk could not be written still works; the volume is in
        // memory and the next `SAVE` tries again. Saying so is the point.
        appendConsole(QStringLiteral("?%1").arg(volume_error));
    }

    if (boot) {
        if (booted_) {
            stopRunner();
            stopRepl();
        }
        reboot();
    }
    updatePrompt();
    updateStatus();
}

bool RetroComputerWindow::saveMachine(const QString& path, QString* error) const {
    return machineImage().save(path, error);
}

bool RetroComputerWindow::loadMachine(const QString& path, QString* error) {
    MachineImage image;
    if (!MachineImage::load(path, &image, error)) {
        return false;
    }
    applyMachineImage(image, true);
    return true;
}

QString RetroComputerWindow::program() const {
    return program_ != nullptr ? program_->toPlainText() : QString{};
}

void RetroComputerWindow::showProgramPage() {
    if (text_tabs_ != nullptr && program_ != nullptr) {
        text_tabs_->setCurrentWidget(program_);
    }
}

void RetroComputerWindow::loadProgram(const QString& source) {
    if (program_ == nullptr) {
        return;
    }
    program_->setPlainText(source);
    programs_[static_cast<int>(language_)] = source;
    program_->document()->setModified(false);
    updateStatus();
}

void RetroComputerWindow::reboot() {
    stopRunner();
    stopRepl();
    if (console_ != nullptr) {
        console_->clear();
    }
    if (bridge_ != nullptr) {
        bridge_->drainNow();
    }
    if (canvas_ != nullptr) {
        canvas_->reset();
    }
    auto_shown_graphics_ = false;
    appendBootBanner();
    text_tabs_->setCurrentWidget(console_);
    updateStatus();
}

void RetroComputerWindow::saveCurrentProgram() {
    if (program_ != nullptr) {
        programs_[static_cast<int>(language_)] = program_->toPlainText();
    }
}

void RetroComputerWindow::saveSettings() {
    QSettings settings;
    settings.beginGroup(QStringLiteral("retroComputer"));
    settings.setValue(QStringLiteral("geometry"), saveGeometry());
    settings.setValue(QStringLiteral("language"), settingsKey(language_));
    settings.setValue(QStringLiteral("mode"), static_cast<int>(mode_));
    if (settings_.remember_programs) {
        for (auto it = programs_.cbegin(); it != programs_.cend(); ++it) {
            settings.setValue(
                QStringLiteral("programs/%1")
                    .arg(settingsKey(static_cast<Language>(it.key()))),
                it.value());
        }
    } else {
        // Switching the memory off has to erase what it already kept, or the next
        // start would show programs the setting says are not remembered.
        settings.remove(QStringLiteral("programs"));
    }
    settings.endGroup();
}

void RetroComputerWindow::restoreSettings() {
    if (settings_restored_) {
        return;
    }
    settings_restored_ = true;

    // Configuration first: the boot ROM and boot screen decide what this session
    // starts in, so they have to be known before the session's own state is read.
    settings_ = RetroComputerSettings::load();

    QSettings settings;
    settings.beginGroup(QStringLiteral("retroComputer"));
    const QByteArray geometry = settings.value(QStringLiteral("geometry")).toByteArray();
    if (!geometry.isEmpty()) {
        restoreGeometry(geometry);
    }
    const Language saved =
        languageFromKey(settings.value(QStringLiteral("language")).toString());
    const int saved_mode =
        settings.value(QStringLiteral("mode"), static_cast<int>(ScreenMode::Split)).toInt();
    if (settings_.remember_programs) {
        for (const Language language : allLanguages()) {
            const QString source =
                settings.value(QStringLiteral("programs/%1").arg(settingsKey(language)))
                    .toString();
            if (!source.isEmpty()) {
                programs_[static_cast<int>(language)] = source;
            }
        }
    }
    settings.endGroup();

    if (settings_.boot_language != RetroComputerSettings::kLastUsed) {
        const auto wanted = static_cast<Language>(settings_.boot_language);
        if (allLanguages().contains(wanted)) {
            language_ = wanted;
        }
    } else if (saved != Language::Unknown) {
        language_ = saved;
    }
    if (settings_.boot_mode != RetroComputerSettings::kLastUsed) {
        mode_ = static_cast<ScreenMode>(std::clamp(settings_.boot_mode, 0, 2));
    } else {
        mode_ = static_cast<ScreenMode>(std::clamp(saved_mode, 0, 2));
    }

    if (language_selector_ != nullptr) {
        const QSignalBlocker blocker{language_selector_};
        const int index = language_selector_->findData(static_cast<int>(language_));
        if (index >= 0) {
            language_selector_->setCurrentIndex(index);
        }
    }
    updateModeControls();
}

void RetroComputerWindow::setSettings(const RetroComputerSettings& settings) {
    applySettings(settings, true);
}

void RetroComputerWindow::applySettings(const RetroComputerSettings& settings, bool remember) {
    RetroComputerSettings next = settings;
    next.history_depth = std::clamp(next.history_depth, 0, 500);
    next.screen_text_size = std::clamp(next.screen_text_size, 8, 24);
    next.editor_text_size = std::clamp(next.editor_text_size, 8, 24);
    next.crt.scanlines = std::clamp(next.crt.scanlines, 0, 100);
    next.crt.vignette = std::clamp(next.crt.vignette, 0, 100);
    next.crt.curvature = std::clamp(next.crt.curvature, 0, 100);
    next.crt.flicker = std::clamp(next.crt.flicker, 0, 100);
    next.crt.phosphor = std::clamp(next.crt.phosphor, 0, 100);
    // The phosphor's colour is the palette's, so a settings file cannot pair a
    // green palette with an amber glow.
    next.crt.phosphor_colour = retroPalette(next.tint).text;
    settings_ = next;

    applyMachineLook();
    applyHistoryDepth();
    if (remember) {
        // Session state and configuration are written together: the remember
        // flag decides whether the programs are kept, so the two cannot be saved
        // apart without one of them becoming a lie.
        saveSettings();
        settings_.save();
    }
}

void RetroComputerWindow::applyHistoryDepth() {
    if (settings_.history_depth <= 0) {
        history_.clear();
        history_index_ = 0;
        return;
    }
    while (history_.size() > settings_.history_depth) {
        history_.removeFirst();
    }
    history_index_ = std::min(history_index_, static_cast<int>(history_.size()));
}

RetroComputerSettingsDialog* RetroComputerWindow::openSettings() {
    settings_before_dialog_ = settings_;
    auto* dialog = new RetroComputerSettingsDialog{settings_, this};
    dialog->setAttribute(Qt::WA_DeleteOnClose);
    connect(dialog, &RetroComputerSettingsDialog::previewRequested, this,
            [this](const RetroComputerSettings& preview) {
                // Preview only: the machine repaints, but nothing is written until
                // the dialog is accepted.
                applySettings(preview, false);
            });
    connect(dialog, &QDialog::finished, this, [this, dialog](int result) {
        if (result == QDialog::Accepted) {
            setSettings(dialog->settings());
        } else {
            applySettings(settings_before_dialog_, false);
        }
    });
    dialog->open();
    return dialog;
}

bool RetroComputerWindow::eventFilter(QObject* watched, QEvent* event) {
    if (watched == input_ && event->type() == QEvent::KeyPress) {
        auto* key = static_cast<QKeyEvent*>(event);
        if (key->key() == Qt::Key_Up && !history_.isEmpty()) {
            history_index_ = std::max(0, history_index_ - 1);
            input_->setText(history_.at(history_index_));
            input_->setCursorPosition(input_->text().size());
            return true;
        }
        if (key->key() == Qt::Key_Down) {
            history_index_ = std::min(static_cast<int>(history_.size()), history_index_ + 1);
            input_->setText(history_index_ >= history_.size()
                                ? QString{}
                                : history_.at(history_index_));
            input_->setCursorPosition(input_->text().size());
            return true;
        }
    }
    return QMainWindow::eventFilter(watched, event);
}

void RetroComputerWindow::closeEvent(QCloseEvent* event) {
    saveCurrentProgram();
    saveSettings();
    stopRunner();
    stopRepl();
    QMainWindow::closeEvent(event);
}

}  // namespace timewarp::ui
