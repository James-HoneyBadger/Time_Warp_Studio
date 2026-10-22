// SPDX-License-Identifier: MIT

#include "timewarp/ui/lint.hpp"

#include <QPair>
#include <QRegularExpression>
#include <QSet>
#include <QStringList>

#include <algorithm>
#include <cstdlib>
#include <vector>

#include "timewarp/ui/syntax_highlighter.hpp"

namespace timewarp::ui {

namespace {

/// A word this far from a real keyword is a typo; further away it is a
/// procedure name the checker has no business guessing about.
constexpr int kMaxSuggestionDistance = 2;

/// How many edits to consider before giving up. Anything at or beyond this is
/// "not a typo" and the exact number stops mattering.
constexpr int kDistanceCap = kMaxSuggestionDistance + 1;

// ---------------------------------------------------------------------------
// A line with its comments and its string bodies blanked out
// ---------------------------------------------------------------------------

/// The code part of one line, at the same length and in the same columns as the
/// original, so a finding's column can be reported straight from it.
struct ScannedLine {
    QString code;
    /// The line ended inside a string. The language will refuse it.
    bool unterminated_string = false;
    int string_column = 1;
};

/// Strips comments and string *contents*, keeping every column where it was.
///
/// Blanking rather than removing is the whole trick: `PRINT "(" ` has no bracket
/// to match, and a checker that deletes the string cannot say which column the
/// next problem is in.
class LineScanner {
public:
    explicit LineScanner(LanguageSyntax syntax) : syntax_{std::move(syntax)} {}

    [[nodiscard]] ScannedLine scan(const QString& line) {
        ScannedLine result;
        QString code = line;

        const auto blank = [&code](int from, int count) {
            for (int index = from; index < from + count && index < code.size(); ++index) {
                code[index] = QLatin1Char(' ');
            }
        };
        const auto matches = [&line](int at, const QString& text) {
            return !text.isEmpty() && line.mid(at, text.size()) == text;
        };
        const auto at_word_start = [&line](int at) {
            return at == 0 || !line.at(at - 1).isLetterOrNumber();
        };
        const auto word_continues = [&line](int after) {
            return after < line.size() && line.at(after).isLetterOrNumber();
        };

        bool in_string = false;
        QChar delimiter;
        int open_string_column = 1;

        int index = 0;
        while (index < line.size()) {
            if (in_block_) {
                if (matches(index, syntax_.block_close)) {
                    blank(index, syntax_.block_close.size());
                    index += syntax_.block_close.size();
                    in_block_ = false;
                } else {
                    blank(index, 1);
                    ++index;
                }
                continue;
            }

            // A block comment that opens and closes on one line, or spans lines.
            if (matches(index, syntax_.block_open)) {
                blank(index, syntax_.block_open.size());
                index += syntax_.block_open.size();
                in_block_ = true;
                if (syntax_.block_open == syntax_.block_close) {
                    // Smalltalk: `"` opens and closes, so the opener is not
                    // itself the terminator to look for next.
                    continue;
                }
                continue;
            }

            if (!in_string) {
                bool commented = false;
                for (const QString& prefix : syntax_.line_comments) {
                    if (matches(index, prefix)) {
                        blank(index, line.size() - index);
                        commented = true;
                        break;
                    }
                }
                if (commented) {
                    break;
                }
                for (const QString& word : syntax_.comment_words) {
                    if (at_word_start(index) && matches(index, word) &&
                        !word_continues(index + word.size())) {
                        blank(index, line.size() - index);
                        commented = true;
                        break;
                    }
                }
                if (commented) {
                    break;
                }
            }

            if (in_string) {
                if (line.at(index) == QLatin1Char('\\') && index + 1 < line.size()) {
                    blank(index, 2);
                    index += 2;
                    continue;
                }
                if (line.at(index) == delimiter) {
                    // A doubled delimiter is an escaped quote, as BASIC writes it.
                    if (index + 1 < line.size() && line.at(index + 1) == delimiter) {
                        blank(index, 2);
                        index += 2;
                        continue;
                    }
                    in_string = false;
                    ++index;
                    continue;
                }
                blank(index, 1);
                ++index;
                continue;
            }

            bool opened = false;
            for (const QString& marker : syntax_.strings) {
                if (!marker.isEmpty() && line.at(index) == marker.at(0)) {
                    in_string = true;
                    delimiter = marker.at(0);
                    open_string_column = index + 1;
                    opened = true;
                    ++index;
                    break;
                }
            }
            if (opened) {
                continue;
            }
            ++index;
        }

        result.code = code;
        result.unterminated_string = in_string;
        result.string_column = open_string_column;
        return result;
    }

    [[nodiscard]] bool inside_block_comment() const noexcept { return in_block_; }

private:
    LanguageSyntax syntax_;
    bool in_block_ = false;
};

// ---------------------------------------------------------------------------
// A statement, ready to be compared with the keyword table
// ---------------------------------------------------------------------------

/// One line of code, reduced to the words a rule can act on.
struct Statement {
    QString first;    // folded first word
    QString second;   // folded second word
    QString pair;     // "FIRST SECOND", for the table's multi-word entries
    int column = 1;   // 1-based column of `first`
};

/// `text`, case-folded when the language ignores case.
[[nodiscard]] QString fold(const QString& text, bool insensitive) {
    return insensitive ? text.toUpper() : text;
}

/// The next word in `text` at or after `from`, with the folded spelling and the
/// index it starts at.
[[nodiscard]] QPair<QString, int> nextWord(const QString& text, int from, bool insensitive,
                                           int skip = 0) {
    int index = from;
    int seen = 0;
    while (index < text.size()) {
        while (index < text.size() && !text.at(index).isLetterOrNumber()) {
            ++index;
        }
        const int start = index;
        while (index < text.size() && text.at(index).isLetterOrNumber()) {
            ++index;
        }
        if (start == index) {
            break;
        }
        if (seen++ == skip) {
            return {fold(text.mid(start, index - start), insensitive), start};
        }
    }
    return {QString{}, -1};
}

/// Reads a line as a statement, skipping the parts that are not one: a BASIC or
/// FOCAL line number, a Logo or CHIP-8 label, indentation.
[[nodiscard]] Statement parseStatement(const QString& code, bool insensitive) {
    Statement statement;
    int offset = 0;

    static const QRegularExpression number_prefix{
        QStringLiteral(R"(^\s*(\d+(?:\.\d+)?)[ \t]+)")};
    const QRegularExpressionMatch number = number_prefix.match(code);
    if (number.hasMatch()) {
        offset = static_cast<int>(number.capturedEnd());
    } else {
        while (offset < code.size() && code.at(offset).isSpace()) {
            ++offset;
        }
    }

    // A label is `name:` at the start of the statement; the statement is what
    // follows it.
    static const QRegularExpression label_prefix{QStringLiteral(R"(^([A-Za-z_][A-Za-z0-9_]*)\s*:)")};
    const QRegularExpressionMatch label = label_prefix.match(code.mid(offset));
    if (label.hasMatch()) {
        offset += static_cast<int>(label.capturedEnd());
    }

    const QPair<QString, int> first = nextWord(code, offset, insensitive);
    if (first.second < 0) {
        return statement;
    }
    statement.first = first.first;
    statement.column = first.second + 1;
    const QPair<QString, int> second =
        nextWord(code, first.second + statement.first.size(), insensitive);
    statement.second = second.first;
    statement.pair = statement.second.isEmpty()
                         ? statement.first
                         : statement.first + QLatin1Char(' ') + statement.second;
    return statement;
}

/// Everything a rule needs to know about one language's vocabulary.
struct Vocabulary {
    QSet<QString> words;
    QStringList single_words;
    bool insensitive = false;

    bool knows(const QString& word) const { return !word.isEmpty() && words.contains(word); }
    bool knowsPair(const QString& pair) const { return !pair.isEmpty() && words.contains(pair); }
};

[[nodiscard]] Vocabulary vocabularyFor(Language language) {
    Vocabulary vocabulary;
    vocabulary.insensitive = syntaxOf(language).case_insensitive;
    for (const QString& keyword : keywordsOf(language)) {
        const QString folded = fold(keyword, vocabulary.insensitive);
        vocabulary.words.insert(folded);
        if (!folded.contains(QLatin1Char(' '))) {
            vocabulary.single_words.append(folded);
        }
    }
    return vocabulary;
}

// ---------------------------------------------------------------------------
// Edit distance, for "did you mean ...?"
// ---------------------------------------------------------------------------

/// Levenshtein distance, capped: anything at or past `kDistanceCap` comes back as
/// `kDistanceCap`, which is all a suggestion needs to know.
[[nodiscard]] int editDistance(const QString& left, const QString& right) {
    const int rows = static_cast<int>(left.size()) + 1;
    const int columns = static_cast<int>(right.size()) + 1;
    if (std::abs(rows - columns) > kDistanceCap) {
        return kDistanceCap;
    }
    std::vector<int> previous(static_cast<std::size_t>(columns));
    std::vector<int> current(static_cast<std::size_t>(columns));
    for (int column = 0; column < columns; ++column) {
        previous[static_cast<std::size_t>(column)] = column;
    }
    for (int row = 1; row < rows; ++row) {
        current[0] = row;
        for (int column = 1; column < columns; ++column) {
            const int substitution =
                previous[static_cast<std::size_t>(column - 1)] +
                (left.at(row - 1) == right.at(column - 1) ? 0 : 1);
            const int deletion = previous[static_cast<std::size_t>(column)] + 1;
            const int insertion = current[static_cast<std::size_t>(column - 1)] + 1;
            current[static_cast<std::size_t>(column)] =
                std::min({substitution, deletion, insertion, kDistanceCap});
        }
        previous.swap(current);
    }
    return previous[static_cast<std::size_t>(columns - 1)];
}

/// The closest keyword to `word`, or an empty string when nothing is close
/// enough to be worth suggesting.
[[nodiscard]] QString nearestKeyword(const QString& word, const Vocabulary& vocabulary) {
    if (word.size() < 3) {
        // Two letters is inside a typo of half the table; a suggestion would be a
        // guess.
        return {};
    }
    QString best;
    int best_distance = kMaxSuggestionDistance + 1;
    for (const QString& candidate : vocabulary.single_words) {
        // A length difference alone can rule a candidate out, cheaply.
        if (std::abs(static_cast<int>(candidate.size()) - static_cast<int>(word.size())) >
            kMaxSuggestionDistance) {
            continue;
        }
        const int distance = editDistance(word, candidate);
        if (distance < best_distance) {
            best_distance = distance;
            best = candidate;
            if (distance <= 1) {
                break;
            }
        }
    }
    return best_distance <= kMaxSuggestionDistance ? best : QString{};
}

// ---------------------------------------------------------------------------
// Findings
// ---------------------------------------------------------------------------

class Findings {
public:
    void add(int line, int column, int length, Diagnostic::Severity severity,
             QString message, QString rule) {
        Diagnostic diagnostic;
        diagnostic.line = line;
        diagnostic.column = std::max(1, column);
        diagnostic.length = std::max(1, length);
        diagnostic.severity = severity;
        diagnostic.message = std::move(message);
        diagnostic.rule = std::move(rule);
        diagnostics_.append(diagnostic);
    }

    [[nodiscard]] QList<Diagnostic> sorted() const {
        QList<Diagnostic> copy = diagnostics_;
        std::stable_sort(copy.begin(), copy.end(),
                         [](const Diagnostic& left, const Diagnostic& right) {
                             if (left.line != right.line) {
                                 return left.line < right.line;
                             }
                             return left.column < right.column;
                         });
        return copy;
    }

private:
    QList<Diagnostic> diagnostics_;
};

/// An opener waiting for its closer.
struct OpenBlock {
    QString opener;   // for the message
    QString closer;   // the word pair that closes it
    QString rule;     // the rule id to report
    int line = 1;
    int column = 1;
};

/// The block structure of the languages that have one, as opener/closer pairs.
struct BlockRule {
    const char* opener;
    const char* closer;
    const char* rule;
};

constexpr BlockRule kBasicBlocks[] = {
    {"FOR", "NEXT", "basic.for-without-next"},
    {"WHILE", "WEND", "basic.while-without-wend"},
    {"DO", "LOOP", "basic.do-without-loop"},
    {"SUB", "END SUB", "basic.sub-without-end-sub"},
    {"FUNCTION", "END FUNCTION", "basic.function-without-end-function"},
    {"TYPE", "END TYPE", "basic.type-without-end-type"},
    {"SELECT", "END SELECT", "basic.select-without-end-select"},
    {"IF", "END IF", "basic.if-without-end-if"},
};

constexpr BlockRule kComalBlocks[] = {
    {"FOR", "NEXT", "comal.for-without-next"},
    {"REPEAT", "UNTIL", "comal.repeat-without-until"},
    {"WHILE", "ENDWHILE", "comal.while-without-endwhile"},
    {"PROC", "ENDPROC", "comal.proc-without-endproc"},
    {"FUNC", "ENDFUNC", "comal.func-without-endfunc"},
    {"IF", "ENDIF", "comal.if-without-endif"},
};

constexpr BlockRule kRexxBlocks[] = {
    {"DO", "END", "rexx.do-without-end"},
    {"SELECT", "END", "rexx.select-without-end"},
};

/// Whether a closer word is one of `blocks`' closers, and which rule it belongs
/// to.
[[nodiscard]] const BlockRule* ruleForCloser(const QString& word,
                                             const BlockRule* blocks,
                                             std::size_t count) {
    for (std::size_t index = 0; index < count; ++index) {
        if (word == QLatin1String(blocks[index].closer)) {
            return &blocks[index];
        }
    }
    return nullptr;
}

/// Whether an opener word is one of `blocks`' openers.
[[nodiscard]] const BlockRule* ruleForOpener(const QString& word,
                                             const BlockRule* blocks,
                                             std::size_t count) {
    for (std::size_t index = 0; index < count; ++index) {
        if (word == QLatin1String(blocks[index].opener)) {
            return &blocks[index];
        }
    }
    return nullptr;
}

// ---------------------------------------------------------------------------
// Rules
// ---------------------------------------------------------------------------

/// Brackets, across the whole program.
///
/// Every one of the ten languages pairs them, and in every one of them a bracket
/// left open means the rest of the program is misread -- which is the single most
/// common beginner mistake and the one worth reporting at the bracket rather than
/// at the end of the file.
void checkBrackets(const QList<ScannedLine>& lines, Findings* findings) {
    struct Open {
        QChar symbol;
        int line;
        int column;
    };
    QList<Open> open;
    const QString pairs_open = QStringLiteral("([{");
    const QString pairs_close = QStringLiteral(")]}");

    for (int index = 0; index < lines.size(); ++index) {
        const QString& code = lines.at(index).code;
        for (int column = 0; column < code.size(); ++column) {
            const QChar character = code.at(column);
            const int open_index = static_cast<int>(pairs_open.indexOf(character));
            if (open_index >= 0) {
                open.append(Open{character, index + 1, column + 1});
                continue;
            }
            const int close_index = static_cast<int>(pairs_close.indexOf(character));
            if (close_index < 0) {
                continue;
            }
            if (open.isEmpty()) {
                findings->add(index + 1, column + 1, 1, Diagnostic::Severity::Error,
                              QStringLiteral("`%1` closes nothing \u2014 there is no "
                                             "open bracket before it")
                                  .arg(character),
                              QStringLiteral("brackets.unexpected-close"));
                continue;
            }
            const Open last = open.takeLast();
            if (pairs_open.indexOf(last.symbol) != close_index) {
                findings->add(index + 1, column + 1, 1, Diagnostic::Severity::Error,
                              QStringLiteral("`%1` closes `%2` from line %3")
                                  .arg(character)
                                  .arg(last.symbol)
                                  .arg(last.line),
                              QStringLiteral("brackets.mismatched"));
            }
        }
    }
    for (const Open& leftover : open) {
        findings->add(leftover.line, leftover.column, 1, Diagnostic::Severity::Error,
                      QStringLiteral("`%1` is never closed").arg(leftover.symbol),
                      QStringLiteral("brackets.unclosed"));
    }
}

/// Reports the openers still on the stack when the program ends.
void reportUnclosed(const QList<OpenBlock>& stack, Findings* findings) {
    for (const OpenBlock& block : stack) {
        findings->add(block.line, block.column,
                      static_cast<int>(block.opener.size()), Diagnostic::Severity::Error,
                      QStringLiteral("%1 is never closed with %2").arg(block.opener,
                                                                      block.closer),
                      block.rule);
    }
}

/// The block check for BASIC, COMAL and REXX: push openers, pop closers, and
/// report both kinds of orphan.
void checkBlocks(const QList<ScannedLine>& lines, const BlockRule* blocks,
                 std::size_t count, bool insensitive, const QString& language_id,
                 Findings* findings) {
    QList<OpenBlock> stack;
    for (int index = 0; index < lines.size(); ++index) {
        const Statement statement = parseStatement(lines.at(index).code, insensitive);
        if (statement.first.isEmpty()) {
            continue;
        }
        // `END IF` and `END SUB` are two words; a closer is matched on the pair
        // first, then on the single word (`NEXT`, `WEND`, `ENDWHILE`).
        const BlockRule* closer = ruleForCloser(statement.pair, blocks, count);
        QString closer_word = statement.pair;
        if (closer == nullptr) {
            closer = ruleForCloser(statement.first, blocks, count);
            closer_word = statement.first;
        }
        if (closer != nullptr) {
            if (stack.isEmpty()) {
                findings->add(index + 1, statement.column,
                              static_cast<int>(closer_word.size()), Diagnostic::Severity::Error,
                              QStringLiteral("%1 without a %2")
                                  .arg(closer_word, QLatin1String(closer->opener)),
                              QStringLiteral("blocks.%1-without-opener").arg(language_id));
            } else if (stack.last().closer != closer_word) {
                findings->add(index + 1, statement.column,
                              static_cast<int>(closer_word.size()), Diagnostic::Severity::Error,
                              QStringLiteral("%1 closes %2 from line %3, which needs %4")
                                  .arg(closer_word, stack.last().opener)
                                  .arg(stack.last().line)
                                  .arg(stack.last().closer),
                              QStringLiteral("blocks.%1-mismatched").arg(language_id));
                stack.removeLast();
            } else {
                stack.removeLast();
            }
            continue;
        }

        const BlockRule* opener = ruleForOpener(statement.first, blocks, count);
        if (opener == nullptr) {
            continue;
        }
        // `IF` opens a block only when nothing follows its THEN; `IF x THEN PRINT`
        // is a whole statement on one line.
        if (statement.first == QLatin1String("IF")) {
            const QString& code = lines.at(index).code;
            const int then_at = code.indexOf(QLatin1String("THEN"), statement.column);
            if (then_at >= 0) {
                const QString tail = code.mid(then_at + 4).trimmed();
                if (!tail.isEmpty()) {
                    continue;
                }
            }
        }
        stack.append(OpenBlock{statement.first, QLatin1String(opener->closer),
                               QLatin1String(opener->rule), index + 1, statement.column});
    }
    reportUnclosed(stack, findings);
}

/// A first word that is one letter from a real keyword.
///
/// Deliberately narrow. A checker that reports every word it does not recognise
/// would flag every procedure call a learner defines -- and in Logo, COMAL and
/// BASIC a call *is* just a name. Naming the word it was probably meant to be is
/// useful; claiming to know all the words is not.
void checkProbableTypo(int line, const Statement& statement, const Vocabulary& vocabulary,
                       const QString& language_id, Findings* findings) {
    if (statement.first.isEmpty() || vocabulary.knows(statement.first) ||
        vocabulary.knowsPair(statement.pair)) {
        return;
    }
    const QString suggestion = nearestKeyword(statement.first, vocabulary);
    if (suggestion.isEmpty()) {
        return;
    }
    findings->add(line, statement.column, static_cast<int>(statement.first.size()),
                  Diagnostic::Severity::Warning,
                  QStringLiteral("`%1` is not a %2 command \u2014 did you mean `%3`?")
                      .arg(statement.first, language_id.toUpper(), suggestion),
                  QStringLiteral("typo.%1").arg(language_id));
}

/// BASIC and COMAL: `IF` needs a `THEN`.
void checkIfNeedsThen(int line, const QString& code, const Statement& statement,
                      const QString& language_id, Findings* findings) {
    if (statement.first != QLatin1String("IF")) {
        return;
    }
    if (code.indexOf(QLatin1String("THEN"), statement.column) >= 0) {
        return;
    }
    findings->add(line, statement.column, 2, Diagnostic::Severity::Warning,
                  QStringLiteral("IF needs a THEN before the statement to run"),
                  QStringLiteral("%1.if-without-then").arg(language_id));
}

/// PILOT: every line is a unit letter and a colon, or a remark.
///
/// PILOT is the one language here whose line *shape* carries the structure, so a
/// line that does not start with `T:` (or `*remark`) is not PILOT at all and the
/// engine will refuse it.
void checkPilotShape(const QList<ScannedLine>& original, Findings* findings) {
    static const QRegularExpression unit{QStringLiteral(R"(^\s*([A-Za-z])\s*:)")};
    static const QRegularExpression remark{QStringLiteral(R"(^\s*\*)")};
    for (int index = 0; index < original.size(); ++index) {
        const QString& text = original.at(index).code;
        if (text.trimmed().isEmpty() || remark.match(text).hasMatch() ||
            unit.match(text).hasMatch()) {
            continue;
        }
        int column = 0;
        while (column < text.size() && text.at(column).isSpace()) {
            ++column;
        }
        findings->add(index + 1, column + 1,
                      std::max(1, static_cast<int>(text.trimmed().size())),
                      Diagnostic::Severity::Error,
                      QStringLiteral("PILOT lines start with a unit letter and a colon, "
                                     "e.g. `T:` or `A:`, or `*` for a remark"),
                      QStringLiteral("pilot.missing-unit"));
    }
}

/// CHIP-8: a listing is words and hex, so a word that is neither is a typo.
void checkChip8(const QList<ScannedLine>& lines, const Vocabulary& vocabulary,
                Findings* findings) {
    static const QRegularExpression hex{QStringLiteral(R"(^[0-9A-Fa-fxX$]+$)")};
    for (int index = 0; index < lines.size(); ++index) {
        const Statement statement = parseStatement(lines.at(index).code, true);
        if (statement.first.isEmpty()) {
            continue;
        }
        if (hex.match(statement.first).hasMatch() || vocabulary.knows(statement.first)) {
            continue;
        }
        const QString suggestion = nearestKeyword(statement.first, vocabulary);
        findings->add(index + 1, statement.column,
                      static_cast<int>(statement.first.size()),
                      Diagnostic::Severity::Warning,
                      suggestion.isEmpty()
                          ? QStringLiteral("`%1` is not a CHIP-8 opcode or a hex value")
                                .arg(statement.first)
                          : QStringLiteral("`%1` is not a CHIP-8 opcode \u2014 did you "
                                           "mean `%2`?")
                                .arg(statement.first, suggestion),
                      QStringLiteral("typo.chip8"));
    }
}

}  // namespace

QList<Diagnostic> lintText(Language language, const QString& text) {
    Findings findings;
    if (language == Language::Unknown || text.isEmpty()) {
        return findings.sorted();
    }

    const LanguageSyntax syntax = syntaxOf(language);
    const Vocabulary vocabulary = vocabularyFor(language);
    const QString language_id = settingsKey(language);

    // Split once: every rule walks the same lines.
    const QStringList raw = text.split(QLatin1Char('\n'));
    QList<ScannedLine> lines;
    lines.reserve(raw.size());

    LineScanner scanner{syntax};
    int block_comment_column = 1;
    int block_comment_line = 1;
    bool block_comment_open = false;

    for (int index = 0; index < raw.size(); ++index) {
        const QString line = raw.at(index).endsWith(QLatin1Char('\r'))
                                 ? raw.at(index).chopped(1)
                                 : raw.at(index);
        const ScannedLine scanned = scanner.scan(line);
        lines.append(scanned);

        if (scanned.unterminated_string) {
            findings.add(index + 1, scanned.string_column, 1, Diagnostic::Severity::Error,
                         QStringLiteral("this string is never closed"),
                         QStringLiteral("strings.unterminated"));
        }
        if (!block_comment_open && scanner.inside_block_comment()) {
            block_comment_open = true;
            block_comment_line = index + 1;
            block_comment_column = 1;
        } else if (block_comment_open && !scanner.inside_block_comment()) {
            block_comment_open = false;
        }
    }
    if (block_comment_open) {
        findings.add(block_comment_line, block_comment_column, 1, Diagnostic::Severity::Error,
                     QStringLiteral("this comment is never closed"),
                     QStringLiteral("comments.unterminated"));
    }

    checkBrackets(lines, &findings);

    // Language-specific structure.
    switch (language) {
        case Language::Basic:
            checkBlocks(lines, kBasicBlocks, std::size(kBasicBlocks), true, language_id,
                        &findings);
            break;
        case Language::Comal:
            checkBlocks(lines, kComalBlocks, std::size(kComalBlocks), true, language_id,
                        &findings);
            break;
        case Language::Rexx:
            checkBlocks(lines, kRexxBlocks, std::size(kRexxBlocks), true, language_id,
                        &findings);
            break;
        case Language::Pilot:
            checkPilotShape(lines, &findings);
            break;
        case Language::Chip8:
            checkChip8(lines, vocabulary, &findings);
            break;
        case Language::Logo:
        case Language::Scheme:
        case Language::Focal:
        case Language::Apl:
        case Language::Smalltalk:
        // Forth's checks would have to know which words the dictionary has at that
        // point, which is the interpreter's job and not a linter's.
        case Language::Forth:
        case Language::Unknown:
            break;
    }

    // Per-line checks shared by the languages whose statements are words.
    for (int index = 0; index < lines.size(); ++index) {
        const Statement statement = parseStatement(lines.at(index).code, vocabulary.insensitive);
        if (statement.first.isEmpty()) {
            continue;
        }
        if (language != Language::Pilot && language != Language::Chip8) {
            checkProbableTypo(index + 1, statement, vocabulary, language_id, &findings);
        }
        if (language == Language::Basic || language == Language::Comal) {
            checkIfNeedsThen(index + 1, lines.at(index).code, statement, language_id,
                             &findings);
        }
    }

    return findings.sorted();
}

int countAtLeast(const QList<Diagnostic>& diagnostics, Diagnostic::Severity severity) {
    int count = 0;
    for (const Diagnostic& diagnostic : diagnostics) {
        const bool is_error = diagnostic.severity == Diagnostic::Severity::Error;
        if (is_error || severity == Diagnostic::Severity::Warning) {
            ++count;
        }
    }
    return count;
}

}  // namespace timewarp::ui
