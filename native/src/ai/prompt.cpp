// SPDX-License-Identifier: MIT

#include "timewarp/ai/prompt.hpp"

#include <QStringList>

#include <algorithm>

namespace timewarp::ai {

namespace {

/// Enough for a long teaching example and short enough that the request fits a
/// small model's context along with the instructions and the conversation.
constexpr int kMaxContextCharacters = 12000;

/// The standing instructions. Every sentence here is a mistake the assistant
/// otherwise makes: a Python answer to a Logo question, an invented `CIRCLE` in
/// Logo, a `pip install`, or a reply that is 80% the user's own code quoted back.
constexpr auto kSystemPrompt = R"(You are the assistant built into Time Warp Studio, a desktop programming environment for retro computers. The user is working in one of the eleven interpreters it ships, all of which run natively inside the application: BASIC, PILOT, Logo, COMAL, REXX, Scheme, FOCAL, CHIP-8, APL, Smalltalk and Forth.

What to keep in mind:
- Answer in the language the user is working in. Never suggest an import, a module, a package manager or an external library: every program is a single self-contained file run by an interpreter written between 1964 and 1980.
- Do not invent commands. When you are not certain a command exists in this dialect, say so, and offer a way to do it with commands you are certain of.
- Programs are plain text. BASIC, COMAL and FOCAL are written as numbered lines; PILOT uses letter-coded units (T:, A:, M:, Y:, N:, ...); Logo, Scheme, Smalltalk, Forth and CHIP-8 are whole programs; APL and REXX are line-oriented.
- Turtle graphics draw on the canvas beside the editor. The Logo words are FORWARD/FD, BACK/BK, RIGHT/RT, LEFT/LT, HOME, SETXY, SETHEADING, PENUP/PU, PENDOWN/PD, SETPENCOLOR, SETPC, SETPENWIDTH and REPEAT ... [ ... ]; PENSTYLE and BEZIER are extensions of this canvas. BASIC accepts the same words as statements. Heading 0 is north and increases clockwise. Logo has no CIRCLE, RECTANGLE or POLYGON: shapes are built with REPEAT.
- CHIP-8 programs are assembly for the assembler built into this application, not a high-level language.
- When you give code, put the whole program in one fenced block labelled with the language key: basic, pilot, logo, comal, rexx, scheme, focal, chip8, apl, smalltalk or forth.
- Be brief and concrete: a few sentences of explanation, then the code. Do not repeat the user's program back unless you are changing it.)";

[[nodiscard]] QString capped(QString text) {
    if (text.size() <= kMaxContextCharacters) {
        return text;
    }
    text.truncate(kMaxContextCharacters);
    return text + QStringLiteral(
                      "\n... [the program was cut here: it is longer than one request carries]");
}

/// The fenced block that shows the program to the model. The label is the language
/// key, so the same text a model sees is what the editor would highlight.
[[nodiscard]] QString fenced(const QString& language, const QString& body) {
    QString label;
    for (const QChar character : language) {
        if (character.isLetterOrNumber()) {
            label.append(character.toLower());
        }
    }
    // "CHIP-8" becomes "chip8", which is the key; "BASIC" becomes "basic". A name
    // this does not match is left bare rather than guessed at.
    if (label == QLatin1String("chip")) {
        label = QStringLiteral("chip8");
    }
    return QStringLiteral("```%1\n%2\n```").arg(label, body);
}

[[nodiscard]] QString program_section(const Context& context) {
    if (!context.selection.trimmed().isEmpty()) {
        return QStringLiteral("Selected lines%1:\n%2")
            .arg(context.file_name.isEmpty() ? QString{}
                                             : QStringLiteral(" of %1").arg(context.file_name),
                 fenced(context.language, capped(context.selection)));
    }
    if (context.source.trimmed().isEmpty()) {
        return {};
    }
    return QStringLiteral("Program%1:\n%2")
        .arg(context.file_name.isEmpty() ? QString{}
                                         : QStringLiteral(" (%1)").arg(context.file_name),
             fenced(context.language, capped(context.source)));
}

[[nodiscard]] QString output_section(const Context& context) {
    if (context.last_output.trimmed().isEmpty()) {
        return {};
    }
    return QStringLiteral("What the program's last run reported:\n```\n%1\n```")
        .arg(capped(context.last_output));
}

/// Joins the pieces of a user turn, skipping the absent ones and keeping one blank
/// line between paragraphs.
[[nodiscard]] QString joined(const QStringList& pieces) {
    QStringList present;
    for (const QString& piece : pieces) {
        if (!piece.trimmed().isEmpty()) {
            present << piece;
        }
    }
    return present.join(QStringLiteral("\n\n"));
}

}  // namespace

QString task_key(Task task) {
    switch (task) {
        case Task::Question:
            return QStringLiteral("question");
        case Task::Explain:
            return QStringLiteral("explain");
        case Task::Fix:
            return QStringLiteral("fix");
        case Task::Comment:
            return QStringLiteral("comment");
        case Task::Translate:
            return QStringLiteral("translate");
        case Task::Write:
            return QStringLiteral("write");
    }
    return QStringLiteral("question");
}

QString task_label(Task task) {
    switch (task) {
        case Task::Question:
            return QStringLiteral("Ask");
        case Task::Explain:
            return QStringLiteral("Explain");
        case Task::Fix:
            return QStringLiteral("Fix");
        case Task::Comment:
            return QStringLiteral("Comment");
        case Task::Translate:
            return QStringLiteral("Translate");
        case Task::Write:
            return QStringLiteral("Write");
    }
    return QStringLiteral("Ask");
}

QString system_prompt() { return QString::fromUtf8(kSystemPrompt); }

int max_context_characters() { return kMaxContextCharacters; }

Conversation build_prompt(Task task, const Context& context, const QString& input) {
    Conversation conversation;
    conversation.system = system_prompt();

    const QString language =
        context.language.trimmed().isEmpty() ? QStringLiteral("this language")
                                             : context.language.trimmed();
    const QString words = input.trimmed();
    QString instruction;

    switch (task) {
        case Task::Question:
            instruction = words.isEmpty()
                              ? QStringLiteral("Answer a question about the %1 program "
                                               "below. If the answer needs code, give "
                                               "the exact lines or a complete program.")
                                    .arg(language)
                              : QStringLiteral("Answer this question about the %1 program "
                                               "below:\n\n%2")
                                    .arg(language, words);
            break;
        case Task::Explain:
            instruction = QStringLiteral(
                              "Explain what the following %1 program does, in plain "
                              "language and in the order it runs, for someone learning "
                              "the language. Say what each part does and what the reader "
                              "should notice. Do not rewrite the program.")
                              .arg(language);
            if (!words.isEmpty()) {
                instruction += QStringLiteral("\n\nThe reader also asks: %1").arg(words);
            }
            break;
        case Task::Fix:
            instruction =
                QStringLiteral("Find what is wrong with the following %1 program and "
                               "correct it. Keep the program's intent and its structure. "
                               "Return the corrected program in one fenced block, then "
                               "list the faults you fixed, shortest first.")
                    .arg(language);
            if (!words.isEmpty()) {
                instruction += QStringLiteral("\n\nThe reader says: %1").arg(words);
            }
            break;
        case Task::Comment:
            instruction =
                QStringLiteral("Add explanatory comments to the following %1 program for "
                               "someone reading it for the first time. Change no "
                               "behaviour and no output. Return the whole program in one "
                               "fenced block.")
                    .arg(language);
            break;
        case Task::Translate:
            instruction =
                words.isEmpty()
                    ? QStringLiteral("Rewrite the following %1 program in whichever of "
                                     "the eleven languages fits it best, and name that "
                                     "language in your first sentence. Return the whole "
                                     "program in one fenced block.")
                          .arg(language)
                    : QStringLiteral("Rewrite the following %1 program in %2. Translate "
                                     "the program's behaviour, not its syntax: keep what "
                                     "it prints and what it draws. Return the whole "
                                     "program in one fenced block labelled with the new "
                                     "language's key.")
                          .arg(language, words);
            break;
        case Task::Write:
            instruction =
                words.isEmpty()
                    ? QStringLiteral("Write a short %1 program for a beginner, and say in "
                                     "one sentence what to try next. Return the program "
                                     "in one fenced block.")
                          .arg(language)
                    : QStringLiteral("Write a %1 program that does this:\n\n%2\n\nReturn "
                                     "the program in one fenced block, then in a sentence "
                                     "or two say how to run it and what to look for.")
                          .arg(language, words);
            break;
    }

    // The document rides along only when the task is about it. "Write me a
    // program" does not want the current one in the request, and a question with
    // the document unticked sends nothing but the question.
    const bool about_the_document = task != Task::Write;
    const QString program = about_the_document ? program_section(context) : QString{};
    const QString output = about_the_document && task == Task::Fix ? output_section(context)
                                                                  : QString{};

    conversation.messages.append(
        Message{QStringLiteral("user"), joined({instruction, program, output})});
    return conversation;
}

QString extract_code(const QString& reply) {
    QString best;
    int at = 0;
    while (true) {
        const int open = reply.indexOf(QStringLiteral("```"), at);
        if (open < 0) {
            break;
        }
        // The opening fence's info string (`basic`) runs to the end of the line.
        int body = open + 3;
        while (body < reply.size() && reply.at(body) != QLatin1Char('\n')) {
            ++body;
        }
        if (body >= reply.size()) {
            break;
        }
        ++body;
        const int close = reply.indexOf(QStringLiteral("```"), body);
        if (close < 0) {
            break;
        }
        const QString block = reply.mid(body, close - body);
        // The longest block is the program. A model that shows a two-line fragment
        // and then the whole file has both, and the whole file is what Insert
        // means.
        if (block.size() > best.size()) {
            best = block;
        }
        at = close + 3;
    }
    return best.trimmed().isEmpty() ? reply.trimmed() : best.trimmed();
}

}  // namespace timewarp::ai
