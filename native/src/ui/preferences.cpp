// SPDX-License-Identifier: MIT

#include "timewarp/ui/preferences.hpp"

#include <QCheckBox>
#include <QDialogButtonBox>
#include <QFontComboBox>
#include <QFormLayout>
#include <QGroupBox>
#include <QLabel>
#include <QPushButton>
#include <QSpinBox>
#include <QVBoxLayout>

namespace timewarp::ui {

namespace {

/// The sample the preview shows.
///
/// Deliberately BASIC with two deliberate mistakes in it: a `FOR` with no `NEXT`
/// and a keyword typed one letter short. A preview of correct code shows the
/// colours; a preview of almost-correct code shows the *checking*, which is the
/// feature a user is most likely to switch off if it annoys them.
constexpr auto kSample = R"(REM a sample, with two deliberate mistakes
FOR I = 1 TO 4
PRNT "size "; I * 10
NEXT I
)";

}  // namespace

PreferencesDialog::PreferencesDialog(const EditorSettings& settings, QWidget* parent)
    : QDialog(parent) {
    setObjectName(QStringLiteral("preferencesDialog"));
    setWindowTitle(tr("Settings"));
    setModal(true);
    resize(560, 560);
    buildUi(settings);
}

void PreferencesDialog::buildUi(const EditorSettings& settings) {
    settings_ = settings;

    auto* layout = new QVBoxLayout{this};
    layout->setContentsMargins(12, 12, 12, 12);
    layout->setSpacing(10);

    auto* editor_group = new QGroupBox{tr("Editor"), this};
    auto* form = new QFormLayout{editor_group};

    system_font_ = new QCheckBox{tr("Use the system fixed-width font"), editor_group};
    system_font_->setObjectName(QStringLiteral("systemFontBox"));
    system_font_->setChecked(settings.font_family.isEmpty());
    form->addRow(system_font_);

    font_family_ = new QFontComboBox{editor_group};
    font_family_->setObjectName(QStringLiteral("fontFamilyCombo"));
    if (!settings.font_family.isEmpty()) {
        font_family_->setCurrentFont(QFont{settings.font_family});
    }
    form->addRow(tr("Font:"), font_family_);

    font_size_ = new QSpinBox{editor_group};
    font_size_->setObjectName(QStringLiteral("fontSizeSpin"));
    font_size_->setRange(6, 32);
    font_size_->setValue(settings.font_point_size);
    font_size_->setSuffix(tr(" pt"));
    form->addRow(tr("Size:"), font_size_);

    tab_width_ = new QSpinBox{editor_group};
    tab_width_->setObjectName(QStringLiteral("tabWidthSpin"));
    tab_width_->setRange(1, 8);
    tab_width_->setValue(settings.tab_width);
    tab_width_->setSuffix(tr(" spaces"));
    form->addRow(tr("Tab width:"), tab_width_);
    layout->addWidget(editor_group);

    auto* behaviour = new QGroupBox{tr("While typing"), this};
    auto* behaviour_layout = new QVBoxLayout{behaviour};
    auto check = [behaviour, &behaviour_layout](const QString& name,
                                                const QString& text, bool checked) {
        auto* box = new QCheckBox{text, behaviour};
        box->setObjectName(name);
        box->setChecked(checked);
        behaviour_layout->addWidget(box);
        return box;
    };
    line_numbers_ = check(QStringLiteral("lineNumbersBox"), tr("Show line numbers"),
                          settings.show_line_numbers);
    current_line_ = check(QStringLiteral("currentLineBox"),
                          tr("Highlight the current line"), settings.highlight_current_line);
    word_wrap_ = check(QStringLiteral("wordWrapBox"), tr("Wrap long lines"),
                       settings.word_wrap);
    auto_indent_ = check(QStringLiteral("autoIndentBox"), tr("Indent new lines"),
                         settings.auto_indent);
    completion_ = check(QStringLiteral("completionBox"),
                        tr("Suggest words as I type (Ctrl+Space asks anyway)"),
                        settings.completion);
    whitespace_ = check(QStringLiteral("whitespaceBox"),
                        tr("Show spaces and tabs"), settings.show_whitespace);
    minimap_ = check(QStringLiteral("minimapBox"), tr("Show the minimap"),
                     settings.minimap);
    autosave_ = check(QStringLiteral("autosaveBox"),
                      tr("Keep a copy of unsaved work"), settings.autosave);
    autosave_seconds_ = new QSpinBox{editor_group};
    autosave_seconds_->setObjectName(QStringLiteral("autosaveSecondsSpin"));
    autosave_seconds_->setRange(10, 3600);
    autosave_seconds_->setValue(settings.autosave_seconds);
    autosave_seconds_->setSuffix(tr(" s"));
    autosave_seconds_->setEnabled(settings.autosave);
    connect(autosave_, &QCheckBox::toggled, autosave_seconds_, &QWidget::setEnabled);
    form->addRow(tr("Autosave every:"), autosave_seconds_);
    lint_ = check(QStringLiteral("lintBox"),
                  tr("Check the program as I type and underline what is wrong"),
                  settings.lint);
    layout->addWidget(behaviour);

    auto* preview_group = new QGroupBox{tr("Preview"), this};
    auto* preview_layout = new QVBoxLayout{preview_group};
    preview_ = new CodeEditor{preview_group};
    preview_->setObjectName(QStringLiteral("previewEditor"));
    preview_->setPlainText(QString::fromUtf8(kSample));
    preview_->setMinimumHeight(140);
    preview_layout->addWidget(preview_);
    auto* hint = new QLabel{tr("The sample has a missing NEXT and a mistyped PRINT: "
                               "that is what the checking looks like."),
                            preview_group};
    hint->setObjectName(QStringLiteral("previewHint"));
    hint->setWordWrap(true);
    preview_layout->addWidget(hint);
    layout->addWidget(preview_group, 1);

    auto* buttons = new QDialogButtonBox{
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel | QDialogButtonBox::Apply, this};
    buttons->setObjectName(QStringLiteral("preferencesButtons"));
    layout->addWidget(buttons);

    // Every control re-collects, which re-applies to the preview: the point of a
    // preview is that it answers before the dialog closes.
    const auto changed = [this] {
        collect();
        if (preview_ != nullptr) {
            preview_->applySettings(settings_);
        }
    };
    connect(system_font_, &QCheckBox::toggled, this, [this, changed] {
        updateFontControls();
        changed();
    });
    connect(font_family_, &QFontComboBox::currentFontChanged, this, [changed] { changed(); });
    connect(font_size_, &QSpinBox::valueChanged, this, [changed] { changed(); });
    connect(tab_width_, &QSpinBox::valueChanged, this, [changed] { changed(); });
    for (QCheckBox* box : {line_numbers_, current_line_, word_wrap_, auto_indent_,
                           completion_, lint_, whitespace_, minimap_, autosave_}) {
        connect(box, &QCheckBox::toggled, this, [changed] { changed(); });
    }

    connect(buttons, &QDialogButtonBox::accepted, this, [this] {
        collect();
        emit applied(settings_);
        emit accepted(settings_);
        accept();
    });
    connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
    connect(buttons->button(QDialogButtonBox::Apply), &QPushButton::clicked, this, [this] {
        collect();
        emit applied(settings_);
    });

    collect();
    updateFontControls();
    if (preview_ != nullptr) {
        preview_->applySettings(settings_);
    }
}

void PreferencesDialog::updateFontControls() {
    if (font_family_ != nullptr) {
        font_family_->setEnabled(system_font_ == nullptr || !system_font_->isChecked());
    }
}

void PreferencesDialog::collect() {
    settings_.font_family =
        system_font_ != nullptr && system_font_->isChecked()
            ? QString{}
            : font_family_->currentFont().family();
    settings_.font_point_size = font_size_->value();
    settings_.tab_width = tab_width_->value();
    settings_.show_line_numbers = line_numbers_->isChecked();
    settings_.highlight_current_line = current_line_->isChecked();
    settings_.word_wrap = word_wrap_->isChecked();
    settings_.auto_indent = auto_indent_->isChecked();
    settings_.completion = completion_->isChecked();
    settings_.show_whitespace = whitespace_->isChecked();
    settings_.minimap = minimap_->isChecked();
    settings_.autosave = autosave_->isChecked();
    settings_.autosave_seconds = autosave_seconds_->value();
    settings_.lint = lint_->isChecked();
}

EditorSettings PreferencesDialog::settings() const { return settings_; }

}  // namespace timewarp::ui
