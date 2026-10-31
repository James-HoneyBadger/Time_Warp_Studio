// SPDX-License-Identifier: MIT

#include "timewarp/ui/syntax_highlighter.hpp"

#include <cstddef>
#include <initializer_list>

#include <QFont>
#include <QPair>
#include <QTextBlock>
#include <QTextDocument>
#include <QTextLayout>

namespace timewarp::ui {

namespace {

/// Block state carried from one line to the next. Any non-zero value means "the
/// previous line ended inside a comment", which is the only construct in these
/// languages that can outlive a line.
constexpr int kInComment = 1;

/// The pattern every language uses for a decimal number unless it says
/// otherwise. The lookarounds are what keep `V0` and `0x1F` from being read as
/// three numbers and a letter.
constexpr auto kDecimalNumber =
    R"((?<![A-Za-z0-9_])\d+(?:\.\d+)?(?:[eE][+-]?\d+)?(?![A-Za-z0-9_]))";

/// CHIP-8 writes addresses as `0x1F` or `$1F`, and small literal numbers bare.
constexpr auto kHexOrDecimal = R"((?<![A-Za-z0-9_])(?:0[xX][0-9A-Fa-f]+|\$[0-9A-Fa-f]+|\d+)(?![A-Za-z0-9_]))";

/// Builds a word list from ASCII literals, which is how every keyword table
/// below is written.
[[nodiscard]] QStringList words(std::initializer_list<const char*> items) {
    QStringList list;
    list.reserve(static_cast<qsizetype>(items.size()));
    for (const char* item : items) {
        list.append(QString::fromLatin1(item));
    }
    return list;
}

/// Everything that decides how one language is coloured.
struct LanguageRules {
    QStringList keywords;
    bool case_insensitive = false;
    /// Literal prefixes that comment out the rest of the line.
    QStringList line_comments;
    /// Words that do the same, matched at a word boundary: BASIC's `REM`.
    QStringList comment_words;
    /// A comment that spans lines, e.g. REXX's `/* ... */`.
    QString block_open;
    QString block_close;
    /// String delimiters. Smalltalk's are `'`, and its `"` is a comment.
    QStringList strings;
    /// Overrides the default decimal pattern when a language needs to.
    QString numbers;
    /// Whole-line shapes: PILOT's `T:`, CHIP-8's labels, Scheme's `#t`.
    QStringList directives;
};

/// The rule table for a language.
///
/// The keyword lists are the engine's own command names -- the inventory in
/// `native/tools/gates/inventory.cpp` extracts them from the executors' dispatch
/// tables -- plus the words a *statement* needs that a dispatcher never sees
/// (BASIC's `THEN`, COMAL's `TO`), and the registers or class names a listing
/// names out loud. Each addition is marked where it is not from the inventory.
[[nodiscard]] LanguageRules rulesFor(Language language) {
    LanguageRules rules;
    switch (language) {
        case Language::Basic:
            rules.case_insensitive = true;
            rules.strings = words({"\""});
            // `:` separates statements, so it is not a comment marker; the
            // inventory spells the comment form `: REM`.
            rules.line_comments = words({"'"});
            rules.comment_words = words({"REM"});
            rules.keywords = words({
                "BEEP", "CALL", "CASE", "CIRCLE", "CLS", "COLOR", "CONST", "CYCLE",
                "DATA", "DIM", "DO", "ELSE", "END", "END FUNCTION", "END IF",
                "END SELECT", "END SUB", "END TYPE", "ENDIF", "ERASE", "FOR",
                "FRACTAL", "FUNCTION", "GET", "GOSUB", "GOTO", "IF", "INPUT",
                "LET", "LINE", "LINE INPUT", "LOCATE", "LOOP", "MAKE",
                "ON ERROR GOTO", "ON TIMER", "OPEN", "OUT", "PAINT", "PALETTE",
                "PARTICLE", "PEEK", "PLAY", "POKE", "PRESET", "PRINT",
                "PRINT USING", "PSET", "PUT", "READ", "RESUME NEXT", "RETURN",
                "SAY", "SCREEN", "SELECT", "SETXY", "SHAPE", "SHELL", "SOUND",
                "SPEED", "SPRITE", "SUB", "SWAP", "TIMER", "TIMER OFF",
                "TIMER ON", "TYPE", "UNTIL", "WEND", "WHILE", "WIDTH", "WRITE",
                // Connectives: the statement parser consumes these, so they are
                // in no dispatch table.
                "THEN", "TO", "STEP", "NEXT", "AS", "AND", "OR", "NOT", "MOD",
                "XOR", "DEF", "FN", "DEFINT", "DEFSNG", "DEFDBL", "DEFSTR",
                "DEFLNG", "SHARED", "STATIC", "RANDOMIZE", "USING", "OFF", "ON",
                "ERROR", "BASE",
            });
            break;

        case Language::Pilot:
            rules.case_insensitive = true;
            rules.strings = words({"\""});
            rules.line_comments = words({";"});
            rules.comment_words = words({"REM", "REMARK"});
            // The unit letters are a shape at the start of a line, not a word.
            rules.directives = words({R"(^\s*[A-Za-z]:)", R"(^\s*\*[A-Za-z0-9_]*)"});
            rules.keywords = words({
                "ARC", "BACK", "BK", "CIRCLE", "CLEAR", "CS", "DOT", "FD", "FILL",
                "FORWARD", "HIDETURTLE", "HOME", "HT", "LEFT", "LT", "MAKE", "PD",
                "PENDOWN", "PENUP", "PU", "RIGHT", "RT", "SETBGCOLOR", "SETH",
                "SETHEADING", "SETPENCOLOR", "SETPENWIDTH", "SETXY", "SHOWTURTLE",
                "SPEED", "ST", "STAMP", "TEXT", "TU",
            });
            break;

        case Language::Logo:
            rules.case_insensitive = true;
            rules.strings = words({"\""});
            rules.line_comments = words({";"});
            rules.keywords = words({
                "2DON", "3DOFF", "3DON", "ARC", "BACK", "BACKWARD", "BK",
                "BUTFIRST", "BUTLAST", "BYE", "CLEAN", "CLEAR", "CLEARSCREEN",
                "COUNT", "CS", "DIFFERENCE", "DISABLE3D", "DOT", "DOWN",
                "DOWNWARD", "ENABLE3D", "END", "FD", "FENCE", "FILLED", "FILTER",
                "FIRST", "FOREACH", "FOREVER", "FORWARD", "HIDETURTLE", "HOME",
                "HT", "IF", "IFELSE", "ITEM", "LABEL", "LAST", "LEFT", "LIST",
                "LOCAL", "LT", "MAKE", "MAP", "OP", "OUTPUT", "PD", "PE",
                "PENDOWN", "PENERASE", "PENMODE", "PENPAINT", "PENREVERSE",
                "PENSIZE", "PENUP", "PENWIDTH", "PITCH", "PPT", "PRINT",
                "PRODUCT", "PU", "PX", "QUOTIENT", "RANDOM", "REDUCE", "REPCOUNT",
                "REPEAT", "RIGHT", "ROLL", "RT", "SENTENCE", "SETBG",
                "SETBGCOLOR", "SETCOLOR", "SETH", "SETHEADING", "SETLABELHEIGHT",
                "SETPC", "SETPENCOLOR", "SETPENSIZE", "SETPENWIDTH", "SETPOS",
                "SETPOSITION", "SETPW", "SETX", "SETXY", "SETY", "SHOW",
                "SHOWTURTLE", "ST", "STAMP", "STAMPCLEAN", "STOP", "SUM",
                "TEXTSIZE", "THING", "TILTDOWN", "TILTROLL", "TILTUP", "TO",
                "TYPE", "UNTIL", "UP", "UPWARD", "WAIT", "WHILE", "WINDOW",
                "WORD", "WRAP", "YAW",
            });
            break;

        case Language::Comal:
            rules.case_insensitive = true;
            rules.strings = words({"\""});
            rules.line_comments = words({"//"});
            rules.comment_words = words({"REM"});
            rules.keywords = words({
                "CALL", "DATA", "DIM", "ELIF", "ENDIF", "FOR", "FUNC", "GOSUB",
                "GOTO", "HOME", "IF", "INPUT", "MOVE", "NEXT", "PENDOWN", "PENUP",
                "PLOT", "PRINT", "PROC", "READ", "REPEAT", "RETURN", "TURN",
                "UNTIL", "WHILE",
                // Connectives and block terms the dispatcher never sees.
                "THEN", "TO", "STEP", "ELSE", "CASE", "WHEN", "OTHERWISE",
                "ENDCASE", "ENDPROC", "ENDFUNC", "ENDWHILE", "LOCAL", "EXEC",
                "SET", "OPEN", "CLOSE", "AT", "ZONE", "TRACE",
            });
            break;

        case Language::Rexx:
            rules.case_insensitive = true;
            rules.strings = words({"\"", "'"});
            rules.line_comments = words({"--"});
            rules.block_open = QStringLiteral("/*");
            rules.block_close = QStringLiteral("*/");
            rules.keywords = words({
                "ABS", "ARG", "BACK", "BK", "CALL", "COPIES", "DATATYPE", "DO",
                "FD", "FOREVER", "FORMAT", "FORWARD", "HOME", "IF", "INDEX",
                "LEFT", "LENGTH", "LOWER", "LT", "MAX", "MIN", "MOVE",
                "OTHERWISE", "PARSE", "PENDOWN", "PENUP", "PULL", "RETURN",
                "REVERSE", "RIGHT", "RT", "SAY", "SIGN", "SPACE", "STRIP",
                "SUBSTR", "TRUNC", "TURN", "UNTIL", "UPPER", "WHEN", "WHILE",
                "WORD", "WORDS",
                // Connectives and clause terms.
                "THEN", "ELSE", "END", "SELECT", "EXIT", "LEAVE", "ITERATE",
                "SIGNAL", "INTERPRET", "PROCEDURE", "EXPOSE", "DROP", "NOP",
                "ADDRESS", "OPTIONS", "TRACE", "PUSH", "QUEUE", "BY", "FOR",
                "TO", "OVER", "AND", "OR", "NOT",
            });
            break;

        case Language::Focal:
            rules.case_insensitive = true;
            rules.strings = words({"\""});
            // FOCAL has no comment syntax on purpose: an unrecognised command is
            // an error, which is how a mistyped line is caught rather than being
            // silently swallowed as a comment.
            rules.keywords = words({
                "ASK", "COMMENT", "DO", "ERASE", "FOR", "FPI", "FRAN",
                "GOTO", "IF", "QUIT", "RETURN", "SET", "TYPE",
            });
            break;

        case Language::Chip8:
            rules.case_insensitive = true;
            rules.line_comments = words({";"});
            rules.numbers = QString::fromLatin1(kHexOrDecimal);
            rules.directives = words({R"(^\s*[A-Za-z_][A-Za-z0-9_]*:)"});
            rules.keywords = words({
                "ADD", "AND", "CALL", "CLS", "DB", "DRW", "DW", "HALT", "JP",
                "LD", "OR", "RET", "RND", "SE", "SHL", "SHR", "SKNP", "SKP",
                "SNE", "SUB", "SUBN", "XOR",
                // The registers: a listing names them, so they are coloured even
                // though no dispatch table holds them.
                "V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9",
                "VA", "VB", "VC", "VD", "VE", "VF", "I", "DT", "ST", "PC", "SP",
            });
            break;

        case Language::Apl:
            rules.strings = words({"'", "\""});
            // APL's comment is its own symbol, and it is not ASCII.
            rules.line_comments = QStringList{QStringLiteral("\u235D")};
            rules.keywords = words({
                "ABS", "AND", "BK", "CATENATE", "CEILING", "CS", "DIVIDE", "DROP",
                "FD", "FLOOR", "FORMAT", "GRADEDOWN", "GRADEUP", "HOME", "IOTA",
                "LOG", "LT", "MAX", "MEMBERSHIP", "MIN", "NAND", "NOR", "NOT",
                "OR", "PD", "POWER", "PU", "RAVEL", "RECIPROCAL", "REDUCE",
                "RESIDUE", "REVERSE", "RHO", "ROLL", "ROTATE", "RT", "SCAN",
                "SIGNUM", "TAKE", "TIMES", "TRANSPOSE",
            });
            break;

        case Language::Forth: {
            // `( ... )` is a comment and `\` comments to the end of the line; `."` is
            // a string that is printed. One quote closes it, and the stack words are
            // the keywords a reader scans for.
            rules.block_open = QStringLiteral("(");
            rules.block_close = QStringLiteral(")");
            rules.strings = words({"\""});
            rules.keywords = words({
                ":", ";", "IF", "ELSE", "THEN", "BEGIN", "UNTIL", "WHILE", "REPEAT",
                "DO", "LOOP", "+LOOP", "LEAVE", "EXIT", "RECURSE", "VARIABLE",
                "CONSTANT", "DUP", "DROP", "SWAP", "OVER", "ROT", "NIP", "TUCK",
                "DEPTH", "PICK", "ROLL", "+", "-", "*", "/", "MOD", "/MOD", "NEGATE",
                "ABS", "MIN", "MAX", "1+", "1-", "=", "<>", "<", ">", "<=", ">=",
                "0=", "0<", "0>", "AND", "OR", "XOR", "INVERT", "LSHIFT", "RSHIFT",
                ".", ".S", "EMIT", "CR", "SPACE", "SPACES", "WORDS", "@", "!", "C@",
                "C!", "+!", "I", "J", "FD", "BK", "RT", "LT", "PU", "PD", "HOME",
                "CS", "SETPC", "SETPW", "ST", "HT", "BYE",
            });
            break;
        }

        case Language::Smalltalk:
            // A comment is `"..."` and a string is `'...'`, which is the reverse
            // of every other language in this list.
            rules.block_open = QStringLiteral("\"");
            rules.block_close = QStringLiteral("\"");
            rules.strings = words({"'"});
            rules.keywords = words({
                "back:", "clear", "cr", "forward:", "home", "initialize", "left:",
                "new", "penDown", "penUp", "right:", "show:",
                "subclass:instanceVariableNames:",
                // The words a message send is built from.
                "self", "super", "true", "false", "nil", "thisContext",
                "ifTrue:", "ifFalse:", "whileTrue:", "to:do:", "timesRepeat:",
                "printString", "displayString", "value", "value:", "yourself",
                "at:", "at:put:", "do:", "collect:", "select:", "inject:into:",
                "isNil", "notNil", "class", "Transcript", "Object", "String",
                "Symbol", "Array", "Turtle",
            });
            break;

        case Language::Scheme:
            rules.strings = words({"\""});
            rules.line_comments = words({";"});
            // `#t` and `#f`, which are literals rather than calls.
            rules.directives = words({R"(#[tf])"});
            rules.keywords = words({
                // The special forms the machine dispatches on.
                "and", "begin", "case", "cond", "define", "defun", "do", "else",
                "if", "lambda", "let", "let*", "letrec", "or", "quasiquote",
                "quote", "set!", "unless", "when",
                // The builtins, as `native/src/scheme/builtins.cpp` registers
                // them, turtle verbs included.
                "abs", "add", "append", "apply", "assoc", "back", "backward",
                "bk", "boolean?", "caar", "cadr", "car", "cdar", "cddr", "cdr",
                "ceiling", "circle", "clear-canvas", "color", "cons", "display",
                "eq?", "equal?", "eqv?", "even?", "exact->inexact", "expt", "fd",
                "filter", "floor", "foldl", "for-each", "forward", "goto",
                "hide-turtle", "home", "inexact->exact", "integer?", "left",
                "length", "list", "list->vector", "list-ref", "list-tail",
                "list?", "lt", "make-vector", "map", "max", "member", "memq",
                "min", "modulo", "mul", "negative?", "newline", "not", "null?",
                "number->string", "number?", "odd?", "pair?", "pd", "pendown",
                "pensize", "penup", "positive?", "procedure?", "pu", "quotient",
                "remainder", "reverse", "right", "round", "rt", "setheading",
                "setpos", "show-turtle", "sqrt", "string->list", "string->number",
                "string->symbol", "string-append", "string-ci=?", "string-downcase",
                "string-length", "string-upcase", "string<?", "string=?",
                "string?", "sub", "substring", "symbol->string", "symbol?",
                "truncate", "vector", "vector->list", "vector-length",
                "vector-ref", "vector?", "width", "write", "zero?",
            });
            break;

        case Language::Unknown:
            break;
    }
    return rules;
}

/// The regex that matches any of `words`, bounded so that `PRINT` inside
/// `PRINTER` is not a keyword.
///
/// `\b` is not enough: Scheme's `set!`, `string=?` and `list->vector` end in a
/// character that is not a word character, and `\b` after one matches nothing.
/// Hence the explicit lookarounds, which also let a multi-word command such as
/// BASIC's `PRINT USING` be a single alternative.
[[nodiscard]] QRegularExpression wordPattern(const QStringList& words,
                                             bool caseInsensitive) {
    QStringList escaped;
    escaped.reserve(words.size());
    for (const QString& word : words) {
        escaped.append(QRegularExpression::escape(word));
    }
    const QString expression = QStringLiteral(R"((?<![A-Za-z0-9_])(?:%1)(?![A-Za-z0-9_]))")
                                   .arg(escaped.join(QLatin1Char('|')));
    QRegularExpression pattern{
        expression, caseInsensitive ? QRegularExpression::CaseInsensitiveOption
                                    : QRegularExpression::NoPatternOption};
    pattern.optimize();
    return pattern;
}

/// The pattern that matches a string literal delimited by `delimiter` with
/// backslash escapes. A newline cannot appear inside one: `highlightBlock` sees
/// a single line, so a match can never span blocks.
[[nodiscard]] QRegularExpression stringPattern(const QString& delimiter) {
    const QString escaped = QRegularExpression::escape(delimiter);
    const QString body = QStringLiteral("[^%1\\\\]|\\\\.").arg(escaped);
    QRegularExpression pattern{QStringLiteral("%1(?:%2)*%1").arg(escaped, body)};
    pattern.optimize();
    return pattern;
}

/// True when `[start, start + length)` overlaps any recorded range.
[[nodiscard]] bool overlaps(const QVector<QPair<int, int>>& ranges, int start,
                            int length) {
    const int end = start + length;
    for (const QPair<int, int>& range : ranges) {
        if (start < range.second && range.first < end) {
            return true;
        }
    }
    return false;
}

}  // namespace

QStringList keywordsOf(Language language) { return rulesFor(language).keywords; }

LanguageSyntax syntaxOf(Language language) {
    const LanguageRules rules = rulesFor(language);
    LanguageSyntax syntax;
    syntax.line_comments = rules.line_comments;
    syntax.comment_words = rules.comment_words;
    syntax.block_open = rules.block_open;
    syntax.block_close = rules.block_close;
    syntax.strings = rules.strings;
    syntax.case_insensitive = rules.case_insensitive;
    return syntax;
}

SyntaxColors SyntaxColors::readableOnAnyBase() {
    SyntaxColors colors;
    // Chosen against `palette(base)`, which `ui_style.cpp` leaves to the system:
    // mid-tone hues rather than black-and-bright, so they keep contrast on a
    // light base and are still legible on a dark one. `docs/ROADMAP.md` has the
    // themes that will replace these.
    colors.keyword = QColor{0x1F, 0x6F, 0xEB};
    colors.comment = QColor{0x6E, 0x77, 0x81};
    colors.string = QColor{0x0A, 0x7D, 0x33};
    colors.number = QColor{0xB0, 0x5A, 0x00};
    colors.directive = QColor{0x82, 0x50, 0xDF};
    return colors;
}

SyntaxHighlighter::SyntaxHighlighter(QTextDocument* document, Language language)
    : QSyntaxHighlighter(document) {
    const SyntaxColors colors = SyntaxColors::readableOnAnyBase();

    keyword_format_.setForeground(colors.keyword);
    keyword_format_.setFontWeight(QFont::DemiBold);
    comment_format_.setForeground(colors.comment);
    comment_format_.setFontItalic(true);
    string_format_.setForeground(colors.string);
    number_format_.setForeground(colors.number);
    directive_format_.setForeground(colors.directive);

    setLanguage(language);
}

void SyntaxHighlighter::setColors(const SyntaxColors& colors) {
    keyword_format_.setForeground(colors.keyword);
    comment_format_.setForeground(colors.comment);
    string_format_.setForeground(colors.string);
    number_format_.setForeground(colors.number);
    directive_format_.setForeground(colors.directive);
    // The rule table holds *copies* of these formats, so replacing the formats
    // alone leaves every rule painting the previous theme's colour. Rebuilding
    // the rules is what actually recolours the document -- and `setLanguage`
    // ends in `rehighlight()`.
    setLanguage(language_);
}

void SyntaxHighlighter::setLanguage(Language language) {
    language_ = language;

    const LanguageRules rules = rulesFor(language);
    case_insensitive_ = rules.case_insensitive;

    rules_.clear();
    string_patterns_.clear();
    line_comments_.clear();
    comment_words_.clear();
    block_open_.clear();
    block_close_.clear();
    if (language == Language::Unknown) {
        // Nothing at all, rather than a lone number rule: a document whose
        // extension means nothing to this build is left alone until the user
        // says what it is.
        rehighlight();
        return;
    }

    if (!rules.keywords.isEmpty()) {
        rules_.append(Rule{wordPattern(rules.keywords, rules.case_insensitive),
                           keyword_format_});
    }
    const QString number_source = rules.numbers.isEmpty()
                                      ? QString::fromLatin1(kDecimalNumber)
                                      : rules.numbers;
    QRegularExpression number_pattern{number_source};
    number_pattern.optimize();
    rules_.append(Rule{number_pattern, number_format_});
    for (const QString& directive : rules.directives) {
        QRegularExpression pattern{directive};
        pattern.optimize();
        rules_.append(Rule{pattern, directive_format_});
    }

    string_patterns_.clear();
    for (const QString& delimiter : rules.strings) {
        string_patterns_.append(stringPattern(delimiter));
    }

    line_comments_ = rules.line_comments;
    comment_words_.clear();
    if (!rules.comment_words.isEmpty()) {
        comment_words_.append(wordPattern(rules.comment_words, rules.case_insensitive));
    }
    block_open_ = rules.block_open;
    block_close_ = rules.block_close;

    rehighlight();
}

void SyntaxHighlighter::highlightBlock(const QString& text) {
    const int length = text.size();
    int start = 0;

    /// Everything already claimed: strings, and the block comments found below.
    /// The word rules skip these rather than truncating at the first one, which
    /// is what lets Smalltalk's `"note" Transcript show: 'x'` keep its keyword
    /// *after* a comment on the same line.
    QVector<QPair<int, int>> claimed;

    // A block comment that started on an earlier line and has not closed yet.
    if (previousBlockState() == kInComment && !block_open_.isEmpty()) {
        const int close = text.indexOf(block_close_);
        if (close == -1) {
            setFormat(0, length, comment_format_);
            setCurrentBlockState(kInComment);
            return;
        }
        const int end = close + block_close_.size();
        setFormat(0, end, comment_format_);
        claimed.append({0, end});
        start = end;
    }

    // Strings come next, because they beat everything else: a comment marker
    // inside one is not a comment, and a keyword inside one is not a keyword.
    for (const QRegularExpression& pattern : string_patterns_) {
        QRegularExpressionMatchIterator matches = pattern.globalMatch(text, start);
        while (matches.hasNext()) {
            const QRegularExpressionMatch match = matches.next();
            setFormat(match.capturedStart(), match.capturedLength(), string_format_);
            claimed.append({match.capturedStart(),
                            match.capturedStart() + match.capturedLength()});
        }
    }

    // The comments. A line comment claims the rest of the line; a block comment
    // claims only itself, so the line continues afterwards.
    int line_stop = length;
    int cursor = start;
    while (cursor < length) {
        int at = -1;
        bool is_block = false;

        for (const QString& prefix : line_comments_) {
            for (int found = text.indexOf(prefix, cursor); found != -1;
                 found = text.indexOf(prefix, found + 1)) {
                if (!overlaps(claimed, found, prefix.size())) {
                    if (at == -1 || found < at) {
                        at = found;
                        is_block = false;
                    }
                    break;
                }
            }
        }
        for (const QRegularExpression& pattern : comment_words_) {
            QRegularExpressionMatchIterator matches = pattern.globalMatch(text, cursor);
            while (matches.hasNext()) {
                const QRegularExpressionMatch match = matches.next();
                if (overlaps(claimed, match.capturedStart(), match.capturedLength())) {
                    continue;
                }
                if (at == -1 || match.capturedStart() < at) {
                    at = match.capturedStart();
                    is_block = false;
                }
                break;
            }
        }
        if (!block_open_.isEmpty()) {
            for (int found = text.indexOf(block_open_, cursor); found != -1;
                 found = text.indexOf(block_open_, found + 1)) {
                if (!overlaps(claimed, found, block_open_.size())) {
                    if (at == -1 || found < at) {
                        at = found;
                        is_block = true;
                    }
                    break;
                }
            }
        }

        if (at == -1) {
            break;
        }
        if (!is_block) {
            setFormat(at, length - at, comment_format_);
            line_stop = at;
            break;
        }

        const int close = text.indexOf(block_close_, at + block_open_.size());
        if (close == -1) {
            // The comment runs off the end of the line: the rest is comment and
            // the next line starts inside one.
            setFormat(at, length - at, comment_format_);
            setCurrentBlockState(kInComment);
            line_stop = at;
            break;
        }
        const int end = close + block_close_.size();
        setFormat(at, end - at, comment_format_);
        claimed.append({at, end});
        cursor = end;
    }

    // What is left is code: keywords, numbers and whole-line shapes.
    for (const Rule& rule : rules_) {
        QRegularExpressionMatchIterator matches = rule.pattern.globalMatch(text, start);
        while (matches.hasNext()) {
            const QRegularExpressionMatch match = matches.next();
            const int at = match.capturedStart();
            if (at >= line_stop) {
                break;
            }
            if (overlaps(claimed, at, match.capturedLength())) {
                continue;
            }
            setFormat(at, match.capturedLength(), rule.format);
        }
    }
}

}  // namespace timewarp::ui
