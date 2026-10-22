// SPDX-License-Identifier: MIT
//
// See `inventory.hpp`. This is the port of the Python inventory, regex for regex:
// the shapes it harvests (`cmd == "PRINT"`, `starts_with_upper(line, "TO ")`, a
// brace list of quoted literals, Smalltalk's `add(selector, "isKindOf:", ...)`,
// Forth's `{"DUP", Primitive::Dup}` pair table) and the list of literals that look
// like commands and are not.

#include "inventory.hpp"

#include "gates.hpp"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QRegularExpression>

namespace timewarp::gates::inventory {

namespace {

/// Which source files belong to which language. Scheme is not in `src/lang/`: it is
/// the separate machine in `src/scheme/`, and its dispatch is the special-form
/// table there.
const QMap<QString, QStringList>& sources() {
    static const QMap<QString, QStringList> map{
        {QStringLiteral("basic"),
         {QStringLiteral("basic.cpp"), QStringLiteral("basic_statements.cpp"),
          QStringLiteral("basic_text.cpp"), QStringLiteral("basic_features.cpp"),
          QStringLiteral("basic_library.cpp")}},
        {QStringLiteral("pilot"), {QStringLiteral("pilot.cpp")}},
        {QStringLiteral("logo"), {QStringLiteral("logo.cpp")}},
        {QStringLiteral("comal"), {QStringLiteral("comal.cpp")}},
        {QStringLiteral("rexx"), {QStringLiteral("rexx.cpp")}},
        {QStringLiteral("focal"), {QStringLiteral("focal.cpp")}},
        {QStringLiteral("apl"), {QStringLiteral("apl.cpp")}},
        {QStringLiteral("smalltalk"),
         {QStringLiteral("smalltalk.cpp"), QStringLiteral("smalltalk_library.cpp"),
          QStringLiteral("smalltalk_driver.cpp")}},
        {QStringLiteral("chip8"), {QStringLiteral("chip8.cpp")}},
        {QStringLiteral("forth"), {QStringLiteral("forth.cpp")}},
    };
    return map;
}

/// Names that hold "the thing being dispatched on" in the executors.
const QSet<QString>& commandVariables() {
    static const QSet<QString> names{
        QStringLiteral("cmd"),       QStringLiteral("cmd_name"),
        QStringLiteral("cmd_type"),  QStringLiteral("command"),
        QStringLiteral("command_type"), QStringLiteral("kw"),
        QStringLiteral("keyword"),
        // APL dispatches on the statement's leading glyph or word; CHIP-8 on the
        // mnemonic the assembler split off; Smalltalk compares a whole send against
        // a selector name; COMAL upper-cases the statement into `up`.
        QStringLiteral("glyph"),    QStringLiteral("mnemonic"),
        QStringLiteral("up"),        QStringLiteral("name"),
        QStringLiteral("op"),        QStringLiteral("stmt"),
        QStringLiteral("statement"), QStringLiteral("token"),
        QStringLiteral("unit"),      QStringLiteral("verb"),
    };
    return names;
}

/// Literals a dispatcher compares against that are not commands. Kept as an
/// explicit list so the completeness figure counts commands, and so "not a command"
/// is a written decision rather than a silent skip.
const QSet<QString>& notCommands() {
    static const QSet<QString> literals{
        QString::fromUtf8("\u274C"),  // the error prefix every executor returns
        QStringLiteral("ENDTYPE"),    // no-space spelling of END TYPE, which has a row
        QStringLiteral("TYPE$"),      // only ever a guard, never a dispatch
        QStringLiteral("TYPE$("),     // the same guard with its parenthesis attached
        QStringLiteral("undefined variable:"),
        QStringLiteral("#t"),  // Scheme booleans: reader syntax, not commands
        QStringLiteral("#f"),
        QStringLiteral("#true"),
        QStringLiteral("#false"),
        // Smalltalk's lexer token kinds and its three literals: the parser asking
        // about shapes, not dispatch on a selector.
        QStringLiteral("binary"),
        QStringLiteral("char"),
        QStringLiteral("hash-array"),
        QStringLiteral("ident"),
        QStringLiteral("keyword"),
        QStringLiteral("number"),
        QStringLiteral("self"),
        QStringLiteral("super"),
        QStringLiteral("symbol"),
        QStringLiteral("true"),
        QStringLiteral("false"),
        QStringLiteral("nil"),
        // The CHIP-8 number prefix, which `_number` tests for so that 0x41 is 65.
        QStringLiteral("0x"),
    };
    return literals;
}

/// Upper-cased once, because `isCommandLiteral` compares against the upper form and
/// a lowercase entry would read as a decision while doing nothing.
const QSet<QString>& notCommandsUpper() {
    static const QSet<QString> upper = [] {
        QSet<QString> result;
        for (const QString& name : notCommands()) {
            result.insert(name.toUpper());
        }
        return result;
    }();
    return upper;
}

/// Whether a harvested literal names a command rather than punctuation.
///
/// Punctuation and operators (`"`, `#`, `+`, `<=`) are what a token comparison looks
/// like; a command has a letter in it. APL's commands *are* symbols, so anything
/// outside ASCII is taken as a word rather than as punctuation.
bool isCommandLiteral(const QString& literal) {
    const QString stripped = literal.trimmed();
    if (stripped.isEmpty() || notCommandsUpper().contains(stripped.toUpper())) {
        return false;
    }
    QString text = stripped;
    while (text.startsWith(QLatin1Char(':'))) {
        text = text.mid(1).trimmed();
    }
    if (text.isEmpty() || notCommandsUpper().contains(text.toUpper())) {
        return false;
    }
    for (const QChar character : text) {
        if (character.isLetter()) {
            return true;
        }
    }
    for (const QChar character : text) {
        if (character.unicode() > 127) {
            return true;
        }
    }
    return false;
}

struct Scan {
    QSet<QString> equality;
    QSet<QString> prefix;
    QMap<QString, QSet<QString>> tables;
};

Scan scanSource(const QString& text) {
    Scan scan;

    // `cmd == "PRINT"`, `name == "FORWARD"`, `op != "<="`, either way round. The
    // left side has to be one of the command variables so that `status == "ok"` is
    // not a candidate.
    static const QRegularExpression equality{
        QString::fromLatin1(R"RE((?:\b(\w+)\s*[!=]=\s*"([^"\n]{1,40})")|(?:"([^"\n]{1,40})"\s*[!=]=\s*(\w+)\b))RE")};
    auto matches = equality.globalMatch(text);
    while (matches.hasNext()) {
        const QRegularExpressionMatch match = matches.next();
        const QString variable = match.captured(1);
        if (!variable.isEmpty()) {
            if (commandVariables().contains(variable)) {
                scan.equality.insert(match.captured(2));
            }
        } else if (commandVariables().contains(match.captured(4))) {
            scan.equality.insert(match.captured(3));
        }
    }

    // `starts_with_upper(line, "TO ")` and its bare sibling.
    static const QRegularExpression prefix{
        QString::fromLatin1(R"RE(\b(?:starts_with_upper|starts_with)\s*\(\s*[^,()]+,\s*"([^"\n]{1,40})")RE")};
    matches = prefix.globalMatch(text);
    while (matches.hasNext()) {
        scan.prefix.insert(matches.next().captured(1));
    }

    // Smalltalk registers a message send as the first string argument of a call to
    // `add`, because its commands are selectors in a class library.
    static const QRegularExpression registration{
        QString::fromLatin1(R"RE(\badd\s*\(\s*[^,()]+,\s*"([^"\n]{1,40})")RE")};
    matches = registration.globalMatch(text);
    while (matches.hasNext()) {
        scan.equality.insert(matches.next().captured(1));
    }

    // Forth's dictionary is a table of `{name, Primitive::X}` pairs rather than a
    // switch on a string, so the equality scan sees only the control words. One
    // dedicated pattern reads the primitive table; the pairs interleave identifiers
    // with the quotes, which is why the brace-list pattern cannot.
    static const QRegularExpression forthPair{
        QString::fromLatin1(R"RE(\{"([^"\n]{1,40})",\s*Primitive::\w+\})RE")};
    matches = forthPair.globalMatch(text);
    while (matches.hasNext()) {
        scan.equality.insert(matches.next().captured(1));
    }

    // A brace list made *only* of quoted literals: `{"FORWARD", "FD", "BACK"}`.
    // The stricter shape matters: a code block whose message clauses are quoted
    // strings would otherwise be harvested as commands.
    static const QRegularExpression table{
        QString::fromLatin1(R"RE(\{\s*((?:"[^"\n]{1,40}"\s*,?\s*){3,})\})RE")};
    static const QRegularExpression quoted{QString::fromLatin1(R"RE("([^"\n]{1,40})")RE")};
    matches = table.globalMatch(text);
    while (matches.hasNext()) {
        const QString body = matches.next().captured(1);
        QStringList found;
        auto inner = quoted.globalMatch(body);
        while (inner.hasNext()) {
            found.append(inner.next().captured(1));
        }
        // Three or more in one brace list is a word table; two is a pair of
        // alternatives written inline, which the equality scan already found.
        if (found.size() >= 3) {
            QSet<QString> literals;
            for (const QString& literal : found) {
                literals.insert(literal);
            }
            scan.tables.insert(found.first(), literals);
        }
    }
    return scan;
}

}  // namespace

QMap<QString, Entry> collect() {
    QMap<QString, Entry> inventory;
    const QDir lang_dir{repositoryRoot() + QStringLiteral("/native/src/lang")};
    for (auto it = sources().cbegin(); it != sources().cend(); ++it) {
        QSet<QString> equality;
        QSet<QString> prefix;
        QSet<QString> from_tables;
        for (const QString& filename : it.value()) {
            QFile file{lang_dir.filePath(filename)};
            if (!file.open(QIODevice::ReadOnly)) {
                continue;
            }
            const Scan scan = scanSource(QString::fromUtf8(file.readAll()));
            equality |= scan.equality;
            prefix |= scan.prefix;
            for (const QSet<QString>& values : scan.tables) {
                from_tables |= values;
            }
        }
        Entry entry;
        for (const QString& literal : equality | prefix) {
            if (literal.isEmpty()) {
                continue;
            }
            if (isCommandLiteral(literal)) {
                entry.dispatchLiterals.insert(literal.trimmed());
            } else {
                entry.notCommands.insert(literal.trimmed());
            }
        }
        // A brace list is not automatically a command table: Logo's operator table
        // holds "%", "+" and "<=", which are not commands.
        for (const QString& literal : from_tables) {
            if (isCommandLiteral(literal)) {
                entry.tableCommands.insert(literal.trimmed());
            }
        }
        inventory.insert(it.key(), entry);
    }
    return inventory;
}

}  // namespace timewarp::gates::inventory
