// SPDX-License-Identifier: MIT

#include "timewarp/ui/panels.hpp"


#include <iterator>
#include <utility>

#include <QComboBox>
#include <QHBoxLayout>
#include <QPushButton>

namespace timewarp::ui {

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

}  // namespace timewarp::ui
