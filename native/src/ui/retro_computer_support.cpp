// SPDX-License-Identifier: MIT
//
// The HB-1 window's file-local helpers, moved out of the window's own file so the
// split parts can share them.

#include "retro_computer_internal.hpp"

#include <QRegularExpressionMatch>

namespace timewarp::ui {
namespace retro_detail {

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
        case Language::Forth:
            // Forth's prompt is the word every Forth book starts with, and the note
            // says what the ROM is: a stack machine in a socket.
            return {"V1.0", "ok", "STACK LANGUAGE"};
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

/// One line of a listing, matched the same way the machine stores one.
[[nodiscard]] QRegularExpression numberedLine() {
    static const QRegularExpression pattern{QStringLiteral(R"(^\s*(\d+)\s*(.*)$)")};
    return pattern;
}

[[nodiscard]] LineRange parseLineRange(const QString& text) {
    static const QRegularExpression single{QStringLiteral(R"(^\s*(\d+)\s*$)")};
    static const QRegularExpression span{
        QStringLiteral(R"(^\s*(\d*)\s*-\s*(\d*)\s*$)")};
    LineRange range;
    const QRegularExpressionMatch one = single.match(text);
    if (one.hasMatch()) {
        range.valid = true;
        range.first = one.captured(1).toInt();
        range.last = range.first;
        return range;
    }
    const QRegularExpressionMatch both = span.match(text);
    if (both.hasMatch() && (!both.captured(1).isEmpty() || !both.captured(2).isEmpty())) {
        range.valid = true;
        range.first = both.captured(1).isEmpty() ? 0 : both.captured(1).toInt();
        range.last = both.captured(2).isEmpty() ? std::numeric_limits<int>::max()
                                                : both.captured(2).toInt();
    }
    return range;
}

/// True for the words whose following number is a *line number*.
[[nodiscard]] bool isTargetWord(const QString& word) {
    return word == QLatin1String("GOTO") || word == QLatin1String("GOSUB") ||
           word == QLatin1String("THEN") || word == QLatin1String("ELSE") ||
           word == QLatin1String("RESTORE");
}

/// Rewrites the line numbers a statement jumps to, leaving everything else alone.
///
/// String literals are copied verbatim and a comment stops the rewrite, because
/// `PRINT "GOTO 10"` and `REM GOTO 10` are not jumps. A number that names a line
/// the program does not have is counted in `dangling` and left as it is.
[[nodiscard]] QString renumberTargets(const QString& body, const QMap<int, int>& map,
                                      int* dangling) {
    QString out;
    auto readTargets = [&](int& index) {
        while (true) {
            const int before = index;
            while (index < body.size() && body[index] == QLatin1Char(' ')) {
                ++index;
            }
            out += body.mid(before, index - before);
            const int digits = index;
            while (index < body.size() && body[index].isDigit()) {
                ++index;
            }
            if (index == digits) {
                return;
            }
            const int found = body.mid(digits, index - digits).toInt();
            const auto renamed = map.constFind(found);
            if (renamed == map.constEnd()) {
                ++(*dangling);
                out += body.mid(digits, index - digits);
            } else {
                out += QString::number(*renamed);
            }
            // `ON X GOTO 10, 20, 30` is one keyword and a list.
            int look = index;
            while (look < body.size() && body[look] == QLatin1Char(' ')) {
                ++look;
            }
            if (look < body.size() && body[look] == QLatin1Char(',')) {
                out += body.mid(index, look + 1 - index);
                index = look + 1;
                continue;
            }
            return;
        }
    };

    int index = 0;
    while (index < body.size()) {
        const QChar character = body[index];
        if (character == QLatin1Char('"')) {
            const int start = index++;
            while (index < body.size() && body[index] != QLatin1Char('"')) {
                ++index;
            }
            if (index < body.size()) {
                ++index;
            }
            out += body.mid(start, index - start);
            continue;
        }
        if (character == QLatin1Char('\'')) {
            out += body.mid(index);
            break;
        }
        if (!character.isLetter()) {
            out += character;
            ++index;
            continue;
        }
        const int start = index;
        while (index < body.size() &&
               (body[index].isLetterOrNumber() || body[index] == QLatin1Char('$') ||
                body[index] == QLatin1Char('_'))) {
            ++index;
        }
        const QString word = body.mid(start, index - start);
        const QString upper = word.toUpper();
        out += word;
        if (upper == QLatin1String("REM")) {
            out += body.mid(index);
            break;
        }
        if (isTargetWord(upper)) {
            readTargets(index);
            continue;
        }
        // `GO TO` and `GO SUB` are the two-word spellings of the same jump.
        if (upper == QLatin1String("GO")) {
            int look = index;
            while (look < body.size() && body[look] == QLatin1Char(' ')) {
                ++look;
            }
            const int wordStart = look;
            while (look < body.size() && body[look].isLetter()) {
                ++look;
            }
            const QString second = body.mid(wordStart, look - wordStart).toUpper();
            if (second == QLatin1String("TO") || second == QLatin1String("SUB")) {
                out += body.mid(index, look - index);
                index = look;
                readTargets(index);
            }
        }
    }
    return out;
}

}  // namespace retro_detail
}  // namespace timewarp::ui
