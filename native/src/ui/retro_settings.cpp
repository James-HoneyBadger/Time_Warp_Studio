// SPDX-License-Identifier: MIT

#include "timewarp/ui/retro_settings.hpp"

#include <algorithm>

#include <QCheckBox>
#include <QComboBox>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSettings>
#include <QSignalBlocker>
#include <QSlider>
#include <QTabWidget>
#include <QVBoxLayout>
#include <QWidget>

#include "timewarp/ui/language.hpp"
#include "timewarp/ui/retro_computer.hpp"

namespace timewarp::ui {

namespace {

constexpr int kTextSizeMinimum = 8;
constexpr int kTextSizeMaximum = 24;
constexpr int kHistoryMaximum = 500;
constexpr int kCrtMaximum = 100;

QString configGroup(const char* name) {
    return QStringLiteral("retroComputer/config/%1").arg(QString::fromLatin1(name));
}

/// Selects the item carrying `value`, leaving the selection alone when there is
/// no such row (a settings file from a build with more ROMs, say).
void selectData(QComboBox* box, int value) {
    const int index = box->findData(value);
    if (index >= 0) {
        box->setCurrentIndex(index);
    }
}

int clampTo(int value, int lowest, int highest) {
    return std::clamp(value, lowest, highest);
}

QString sizeLabel(int value, const QString& suffix) {
    return QStringLiteral("%1%2").arg(value).arg(suffix);
}

}  // namespace

RetroPalette retroPalette(PhosphorTint tint) {
    switch (tint) {
        case PhosphorTint::Green:
            return {QColor{7, 11, 7},     QColor{11, 19, 11},    QColor{29, 90, 47},
                    QColor{102, 255, 153}, QColor{141, 255, 176}, QColor{47, 155, 77},
                    QColor{255, 176, 0},   QColor{20, 51, 29},    QColor{4, 8, 4}};
        case PhosphorTint::Amber:
            return {QColor{11, 8, 3},      QColor{23, 16, 4},     QColor{107, 69, 8},
                    QColor{255, 176, 0},   QColor{255, 217, 138}, QColor{168, 112, 0},
                    QColor{255, 233, 176}, QColor{61, 38, 3},     QColor{10, 6, 2}};
        case PhosphorTint::White:
            return {QColor{11, 11, 13},    QColor{20, 20, 24},    QColor{74, 74, 85},
                    QColor{230, 230, 230}, QColor{255, 255, 255}, QColor{154, 154, 165},
                    QColor{207, 224, 255}, QColor{51, 51, 61},    QColor{8, 8, 10}};
        case PhosphorTint::Ice:
            return {QColor{5, 10, 16},     QColor{10, 20, 32},    QColor{42, 74, 107},
                    QColor{143, 208, 255}, QColor{214, 236, 255}, QColor{79, 127, 159},
                    QColor{183, 228, 255}, QColor{23, 54, 79},    QColor{4, 8, 13}};
    }
    return retroPalette(PhosphorTint::Green);
}

RetroComputerSettings RetroComputerSettings::load() {
    RetroComputerSettings result;
    QSettings settings;

    result.boot_language =
        settings.value(configGroup("bootLanguage"), result.boot_language).toInt();
    result.boot_mode = settings.value(configGroup("bootScreen"), result.boot_mode).toInt();
    result.banner = static_cast<BootBanner>(clampTo(
        settings.value(configGroup("banner"), static_cast<int>(result.banner)).toInt(),
        static_cast<int>(BootBanner::Full), static_cast<int>(BootBanner::Off)));
    result.remember_programs =
        settings.value(configGroup("rememberPrograms"), result.remember_programs).toBool();
    result.switch_to_split =
        settings.value(configGroup("switchToSplit"), result.switch_to_split).toBool();
    result.history_depth = clampTo(
        settings.value(configGroup("historyDepth"), result.history_depth).toInt(), 0,
        kHistoryMaximum);
    result.tint = static_cast<PhosphorTint>(clampTo(
        settings.value(configGroup("phosphor"), static_cast<int>(result.tint)).toInt(),
        static_cast<int>(PhosphorTint::Green), static_cast<int>(PhosphorTint::Ice)));
    result.screen_text_size = clampTo(
        settings.value(configGroup("screenTextSize"), result.screen_text_size).toInt(),
        kTextSizeMinimum, kTextSizeMaximum);
    result.editor_text_size = clampTo(
        settings.value(configGroup("editorTextSize"), result.editor_text_size).toInt(),
        kTextSizeMinimum, kTextSizeMaximum);

    result.crt.scanlines = clampTo(
        settings.value(configGroup("crtScanlines"), result.crt.scanlines).toInt(), 0,
        kCrtMaximum);
    result.crt.vignette =
        clampTo(settings.value(configGroup("crtVignette"), result.crt.vignette).toInt(), 0,
                kCrtMaximum);
    result.crt.curvature =
        clampTo(settings.value(configGroup("crtCurvature"), result.crt.curvature).toInt(), 0,
                kCrtMaximum);
    result.crt.flicker =
        clampTo(settings.value(configGroup("crtFlicker"), result.crt.flicker).toInt(), 0,
                kCrtMaximum);
    result.crt.phosphor =
        clampTo(settings.value(configGroup("crtTint"), result.crt.phosphor).toInt(), 0,
                kCrtMaximum);
    // The tint's colour is derived, not stored: there is one source for "what an
    // amber machine looks like", and it is the palette table above.
    result.crt.phosphor_colour = retroPalette(result.tint).text;
    return result;
}

void RetroComputerSettings::save() const {
    QSettings settings;
    settings.setValue(configGroup("bootLanguage"), boot_language);
    settings.setValue(configGroup("bootScreen"), boot_mode);
    settings.setValue(configGroup("banner"), static_cast<int>(banner));
    settings.setValue(configGroup("rememberPrograms"), remember_programs);
    settings.setValue(configGroup("switchToSplit"), switch_to_split);
    settings.setValue(configGroup("historyDepth"), history_depth);
    settings.setValue(configGroup("phosphor"), static_cast<int>(tint));
    settings.setValue(configGroup("screenTextSize"), screen_text_size);
    settings.setValue(configGroup("editorTextSize"), editor_text_size);
    settings.setValue(configGroup("crtScanlines"), crt.scanlines);
    settings.setValue(configGroup("crtVignette"), crt.vignette);
    settings.setValue(configGroup("crtCurvature"), crt.curvature);
    settings.setValue(configGroup("crtFlicker"), crt.flicker);
    settings.setValue(configGroup("crtTint"), crt.phosphor);
}

RetroComputerSettingsDialog::RetroComputerSettingsDialog(const RetroComputerSettings& start,
                                                         QWidget* parent)
    : QDialog(parent), settings_(start) {
    setObjectName(QStringLiteral("retroSettingsDialog"));
    setWindowTitle(tr("HB-1 Settings"));
    setModal(true);
    buildUi();
    applyToControls();
    applyStyle();
}

void RetroComputerSettingsDialog::applyStyle() {
    const RetroPalette colour = retroPalette(settings_.tint);
    QString sheet = QStringLiteral(R"(
        QDialog#retroSettingsDialog {
            background-color: @panel@;
        }
        QLabel, QCheckBox {
            color: @text@;
        }
        QTabWidget::pane {
            border: 1px solid @border@;
        }
        QTabBar::tab {
            background-color: @background@;
            color: @dim@;
            border: 1px solid @border@;
            padding: 3px 14px;
        }
        QTabBar::tab:selected {
            background-color: @selection@;
            color: @bright@;
        }
        QComboBox {
            background-color: @background@;
            color: @bright@;
            border: 1px solid @border@;
            padding: 3px 6px;
        }
        QComboBox QAbstractItemView {
            background-color: @background@;
            color: @bright@;
            selection-background-color: @selection@;
        }
        QSlider::groove:horizontal {
            height: 4px;
            background-color: @background@;
            border: 1px solid @border@;
        }
        QSlider::sub-page:horizontal {
            background-color: @text@;
        }
        QSlider::handle:horizontal {
            width: 12px;
            margin: -6px 0;
            background-color: @bright@;
            border: 1px solid @border@;
        }
        QPushButton {
            background-color: @panel@;
            color: @bright@;
            border: 1px solid @border@;
            padding: 4px 12px;
            font-weight: bold;
        }
        QPushButton:hover {
            background-color: @selection@;
        }
    )");
    const auto paint = [&sheet](const char* token, const QColor& ink) {
        sheet.replace(QString::fromLatin1(token), ink.name());
    };
    paint("@panel@", colour.panel);
    paint("@background@", colour.background);
    paint("@border@", colour.border);
    paint("@text@", colour.text);
    paint("@bright@", colour.bright);
    paint("@dim@", colour.dim);
    paint("@selection@", colour.selection);
    setStyleSheet(sheet);

    // The stylesheet colours the widgets it names; the tick inside a checkbox is
    // drawn by the style from the palette, so a dark palette is what makes it
    // visible rather than invisible on dark.
    QPalette controls;
    controls.setColor(QPalette::Window, colour.panel);
    controls.setColor(QPalette::WindowText, colour.text);
    controls.setColor(QPalette::Base, colour.background);
    controls.setColor(QPalette::Text, colour.text);
    controls.setColor(QPalette::Button, colour.panel);
    controls.setColor(QPalette::ButtonText, colour.bright);
    controls.setColor(QPalette::Highlight, colour.selection);
    controls.setColor(QPalette::HighlightedText, colour.bright);
    setPalette(controls);
}

void RetroComputerSettingsDialog::buildUi() {
    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(8);

    auto* pages = new QTabWidget{this};
    pages->setObjectName(QStringLiteral("retroSettingsPages"));

    // A labelled slider with its value beside it: a slider without its number is
    // a control the user cannot describe to anybody else.
    const auto addSliderRow = [this](QFormLayout* form, QWidget* parent, const QString& label,
                                     const QString& object, int minimum, int maximum,
                                     int value, const QString& suffix) {
        auto* row = new QWidget{parent};
        auto* row_layout = new QHBoxLayout{row};
        row_layout->setContentsMargins(0, 0, 0, 0);
        row_layout->setSpacing(8);
        auto* slider = new QSlider{Qt::Horizontal, row};
        slider->setObjectName(object);
        slider->setRange(minimum, maximum);
        slider->setValue(value);
        row_layout->addWidget(slider, 1);
        auto* number = new QLabel{sizeLabel(value, suffix), row};
        number->setObjectName(object + QStringLiteral("Value"));
        number->setMinimumWidth(46);
        number->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        row_layout->addWidget(number);
        form->addRow(label, row);
        slider_rows_.append(SliderRow{slider, number, suffix});
        connect(slider, &QSlider::valueChanged, this, [this, number, suffix](int next) {
            number->setText(sizeLabel(next, suffix));
            publish();
        });
        return slider;
    };

    // -- Machine ----------------------------------------------------------
    auto* machine = new QWidget{pages};
    auto* machine_form = new QFormLayout{machine};
    machine_form->setContentsMargins(12, 12, 12, 12);
    machine_form->setSpacing(8);

    boot_language_ = new QComboBox{machine};
    boot_language_->setObjectName(QStringLiteral("retroBootLanguage"));
    boot_language_->addItem(tr("Last used ROM"), RetroComputerSettings::kLastUsed);
    for (const Language language : allLanguages()) {
        boot_language_->addItem(displayName(language), static_cast<int>(language));
    }
    machine_form->addRow(tr("Boot ROM"), boot_language_);

    boot_screen_ = new QComboBox{machine};
    boot_screen_->setObjectName(QStringLiteral("retroBootScreen"));
    boot_screen_->addItem(tr("Last used screen"), RetroComputerSettings::kLastUsed);
    boot_screen_->addItem(tr("TEXT"), static_cast<int>(RetroComputerWindow::ScreenMode::Text));
    boot_screen_->addItem(tr("SPLIT"),
                          static_cast<int>(RetroComputerWindow::ScreenMode::Split));
    boot_screen_->addItem(tr("GRAPHICS"),
                          static_cast<int>(RetroComputerWindow::ScreenMode::Graphics));
    machine_form->addRow(tr("Boot screen"), boot_screen_);

    boot_banner_ = new QComboBox{machine};
    boot_banner_->setObjectName(QStringLiteral("retroBootBanner"));
    boot_banner_->addItem(tr("Full start-up message"), static_cast<int>(BootBanner::Full));
    boot_banner_->addItem(tr("Short start-up message"), static_cast<int>(BootBanner::Short));
    boot_banner_->addItem(tr("No start-up message"), static_cast<int>(BootBanner::Off));
    machine_form->addRow(tr("Start-up message"), boot_banner_);

    history_depth_ = new QComboBox{machine};
    history_depth_->setObjectName(QStringLiteral("retroHistoryDepth"));
    for (const int depth : {0, 25, 50, 100, 250, 500}) {
        history_depth_->addItem(depth == 0 ? tr("Off") : sizeLabel(depth, tr(" lines")),
                                depth);
    }
    if (history_depth_->findData(settings_.history_depth) < 0) {
        // A value from a hand-edited settings file is still offered, rather than
        // silently replaced by the nearest preset.
        history_depth_->addItem(sizeLabel(settings_.history_depth, tr(" lines")),
                                settings_.history_depth);
    }
    machine_form->addRow(tr("Prompt history"), history_depth_);

    remember_programs_ = new QCheckBox{tr("Remember each ROM's program"), machine};
    remember_programs_->setObjectName(QStringLiteral("retroRememberPrograms"));
    machine_form->addRow(QString(), remember_programs_);

    switch_to_split_ = new QCheckBox{tr("Switch to SPLIT when a program draws"), machine};
    switch_to_split_->setObjectName(QStringLiteral("retroSwitchToSplit"));
    machine_form->addRow(QString(), switch_to_split_);

    auto* boot_note = new QLabel{
        tr("The boot ROM and boot screen apply the next time the machine is "
           "switched on. Everything else takes effect at once."),
        machine};
    boot_note->setObjectName(QStringLiteral("retroBootNote"));
    boot_note->setWordWrap(true);
    machine_form->addRow(boot_note);

    pages->addTab(machine, tr("Machine"));

    // -- Screen -----------------------------------------------------------
    auto* screen = new QWidget{pages};
    auto* screen_form = new QFormLayout{screen};
    screen_form->setContentsMargins(12, 12, 12, 12);
    screen_form->setSpacing(8);

    phosphor_ = new QComboBox{screen};
    phosphor_->setObjectName(QStringLiteral("retroPhosphor"));
    phosphor_->addItem(tr("Green phosphor"), static_cast<int>(PhosphorTint::Green));
    phosphor_->addItem(tr("Amber phosphor"), static_cast<int>(PhosphorTint::Amber));
    phosphor_->addItem(tr("White phosphor"), static_cast<int>(PhosphorTint::White));
    phosphor_->addItem(tr("Ice phosphor"), static_cast<int>(PhosphorTint::Ice));
    screen_form->addRow(tr("Monitor"), phosphor_);

    screen_text_size_ =
        addSliderRow(screen_form, screen, tr("Screen text size"),
                     QStringLiteral("retroScreenTextSize"), kTextSizeMinimum,
                     kTextSizeMaximum, settings_.screen_text_size, tr(" pt"));
    editor_text_size_ =
        addSliderRow(screen_form, screen, tr("Program text size"),
                     QStringLiteral("retroEditorTextSize"), kTextSizeMinimum,
                     kTextSizeMaximum, settings_.editor_text_size, tr(" pt"));

    auto* screen_note = new QLabel{
        tr("The screen text size sets OUTPUT and the prompt line; the program size "
           "sets the PROGRAM page."),
        screen};
    screen_note->setObjectName(QStringLiteral("retroScreenNote"));
    screen_note->setWordWrap(true);
    screen_form->addRow(screen_note);

    pages->addTab(screen, tr("Screen"));

    // -- Glass ------------------------------------------------------------
    auto* glass = new QWidget{pages};
    auto* glass_form = new QFormLayout{glass};
    glass_form->setContentsMargins(12, 12, 12, 12);
    glass_form->setSpacing(8);

    crt_scanlines_ = addSliderRow(glass_form, glass, tr("Scanlines"),
                                  QStringLiteral("retroCrtScanlines"), 0, kCrtMaximum,
                                  settings_.crt.scanlines, tr(" %"));
    crt_vignette_ =
        addSliderRow(glass_form, glass, tr("Vignette"), QStringLiteral("retroCrtVignette"),
                     0, kCrtMaximum, settings_.crt.vignette, tr(" %"));
    crt_curvature_ =
        addSliderRow(glass_form, glass, tr("Curvature"), QStringLiteral("retroCrtCurvature"),
                     0, kCrtMaximum, settings_.crt.curvature, tr(" %"));
    crt_flicker_ =
        addSliderRow(glass_form, glass, tr("Flicker"), QStringLiteral("retroCrtFlicker"), 0,
                     kCrtMaximum, settings_.crt.flicker, tr(" %"));
    crt_tint_ = addSliderRow(glass_form, glass, tr("Phosphor glow"),
                             QStringLiteral("retroCrtTint"), 0, kCrtMaximum,
                             settings_.crt.phosphor, tr(" %"));

    auto* glass_note = new QLabel{
        tr("Zero switches an effect off. The glass is drawn over the screen and "
           "never takes the keyboard."),
        glass};
    glass_note->setObjectName(QStringLiteral("retroGlassNote"));
    glass_note->setWordWrap(true);
    glass_form->addRow(glass_note);

    pages->addTab(glass, tr("Glass"));
    layout->addWidget(pages, 1);

    auto* buttons =
        new QDialogButtonBox{QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this};
    buttons->setObjectName(QStringLiteral("retroSettingsButtons"));
    auto* defaults = buttons->addButton(tr("Restore Defaults"), QDialogButtonBox::ResetRole);
    defaults->setObjectName(QStringLiteral("retroSettingsDefaults"));
    connect(defaults, &QPushButton::clicked, this,
            &RetroComputerSettingsDialog::restoreDefaults);
    connect(buttons, &QDialogButtonBox::accepted, this, [this] {
        collect();
        accept();
    });
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    layout->addWidget(buttons);

    connect(boot_language_, &QComboBox::currentIndexChanged, this, [this] { publish(); });
    connect(boot_screen_, &QComboBox::currentIndexChanged, this, [this] { publish(); });
    connect(boot_banner_, &QComboBox::currentIndexChanged, this, [this] { publish(); });
    connect(phosphor_, &QComboBox::currentIndexChanged, this, [this] { publish(); });
    connect(history_depth_, &QComboBox::currentIndexChanged, this, [this] { publish(); });
    connect(remember_programs_, &QCheckBox::toggled, this, [this] { publish(); });
    connect(switch_to_split_, &QCheckBox::toggled, this, [this] { publish(); });
}

void RetroComputerSettingsDialog::applyToControls() {
    const QSignalBlocker block_language{boot_language_};
    const QSignalBlocker block_screen{boot_screen_};
    const QSignalBlocker block_banner{boot_banner_};
    const QSignalBlocker block_phosphor{phosphor_};
    const QSignalBlocker block_history{history_depth_};
    const QSignalBlocker block_screen_size{screen_text_size_};
    const QSignalBlocker block_editor_size{editor_text_size_};
    const QSignalBlocker block_remember{remember_programs_};
    const QSignalBlocker block_split{switch_to_split_};
    const QSignalBlocker block_scanlines{crt_scanlines_};
    const QSignalBlocker block_vignette{crt_vignette_};
    const QSignalBlocker block_curvature{crt_curvature_};
    const QSignalBlocker block_flicker{crt_flicker_};
    const QSignalBlocker block_tint{crt_tint_};

    selectData(boot_language_, settings_.boot_language);
    selectData(boot_screen_, settings_.boot_mode);
    selectData(boot_banner_, static_cast<int>(settings_.banner));
    selectData(phosphor_, static_cast<int>(settings_.tint));
    selectData(history_depth_, settings_.history_depth);
    screen_text_size_->setValue(settings_.screen_text_size);
    editor_text_size_->setValue(settings_.editor_text_size);
    remember_programs_->setChecked(settings_.remember_programs);
    switch_to_split_->setChecked(settings_.switch_to_split);
    crt_scanlines_->setValue(settings_.crt.scanlines);
    crt_vignette_->setValue(settings_.crt.vignette);
    crt_curvature_->setValue(settings_.crt.curvature);
    crt_flicker_->setValue(settings_.crt.flicker);
    crt_tint_->setValue(settings_.crt.phosphor);
    refreshValueLabels();
}

void RetroComputerSettingsDialog::refreshValueLabels() {
    for (const SliderRow& row : slider_rows_) {
        if (row.slider != nullptr && row.label != nullptr) {
            row.label->setText(sizeLabel(row.slider->value(), row.suffix));
        }
    }
}

void RetroComputerSettingsDialog::collect() {
    settings_.boot_language = boot_language_->currentData().toInt();
    settings_.boot_mode = boot_screen_->currentData().toInt();
    settings_.banner = static_cast<BootBanner>(boot_banner_->currentData().toInt());
    settings_.history_depth = history_depth_->currentData().toInt();
    settings_.screen_text_size = screen_text_size_->value();
    settings_.editor_text_size = editor_text_size_->value();
    settings_.remember_programs = remember_programs_->isChecked();
    settings_.switch_to_split = switch_to_split_->isChecked();
    settings_.tint = static_cast<PhosphorTint>(phosphor_->currentData().toInt());
    settings_.crt.scanlines = crt_scanlines_->value();
    settings_.crt.vignette = crt_vignette_->value();
    settings_.crt.curvature = crt_curvature_->value();
    settings_.crt.flicker = crt_flicker_->value();
    settings_.crt.phosphor = crt_tint_->value();
    settings_.crt.phosphor_colour = retroPalette(settings_.tint).text;
}

void RetroComputerSettingsDialog::publish() {
    collect();
    // The dialog is part of the machine, so it changes phosphor with it.
    applyStyle();
    emit previewRequested(settings_);
}

void RetroComputerSettingsDialog::restoreDefaults() {
    settings_ = RetroComputerSettings{};
    applyToControls();
    publish();
}

}  // namespace timewarp::ui
