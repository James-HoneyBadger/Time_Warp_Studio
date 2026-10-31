// SPDX-License-Identifier: MIT
//
// What the assistant is asked, and what it is told about the program on screen.
//
// Kept apart from the client on purpose: composing a request is a pure function of
// the task, the document and the user's words, so it can be read, tested and
// improved without a network, an API key or a window. The client's job is to carry
// the bytes; this file's job is to make sure the bytes are worth carrying.
//
// #### Why the system prompt is long
//
// A general assistant does not know that this IDE runs eleven specific
// interpreters, that BASIC has a turtle at all, or that a CHIP-8 program is
// assembly rather than a script. Asked cold, it will happily answer a Logo question
// with Python, and a learner who pastes that answer gets an error. So the prompt
// states the language list, the shapes of the programs, the turtle vocabulary the
// guide documents, and the reply format -- code in one fence, labelled with the
// language key -- once, where a test can pin it.

#pragma once

#include <QList>
#include <QString>

#include <optional>

namespace timewarp::ai {

/// What the user asked for. The panel has a button for the ones without free text;
/// every one of them is also reachable by typing.
enum class Task {
    /// A question, in the user's own words.
    Question,
    /// What does this program do?
    Explain,
    /// Find and correct what is wrong with it.
    Fix,
    /// Comment it, without changing what it does.
    Comment,
    /// Rewrite it in another language, named in the user's words.
    Translate,
    /// Write a new program from a description.
    Write,
    /// Explain one error the program reported, in the user's words carrying the
    /// error. Reached from the Console, where the failure is in front of the reader.
    Diagnose,
    /// The next lines of a program being typed. Never a button: the editor asks when
    /// a pause in typing says a suggestion would be useful.
    Continue,
};

/// One turn of a conversation, in the form every provider takes.
struct Message {
    /// `user` or `assistant`. A system turn travels separately: Anthropic's API
    /// will not accept one in the list, and both providers have a field for it.
    QString role;
    QString content;
};

/// A request ready to be serialised: one instruction block and the turns since.
struct Conversation {
    QString system;
    QList<Message> messages;
};

/// The program on screen, as the assistant should see it.
struct Context {
    /// The language's display name ("Logo", "CHIP-8"). Empty when unknown.
    QString language;
    /// The document's file name, or its untitled label.
    QString file_name;
    /// The whole document.
    QString source;
    /// The selected text, when there is a selection. It wins over `source`: a user
    /// who selected six lines is asking about six lines.
    QString selection;
    /// What the last run printed, including any error, when there was one.
    QString last_output;
};

/// The voice the assistant answers in.
///
/// A persona changes *how* a reply reads, never *what* is true: every one of them
/// carries the same standing rules, and each states that the voice is not licence
/// to invent a command. The assistant answering a 1974 question in a 1974 voice is
/// the point of an environment that is deliberately out of its own time; an
/// assistant that invents `CIRCLE` in Logo to sound period-correct is a bug in it.
enum class Persona {
    /// No era, no affect: the voice the assistant shipped with.
    Plain,
    /// A computer science lecturer in 1974: structured programming, listings, the
    /// machine, and the language's own specification.
    Professor,
    /// A bedroom programmer in 1984: terse, tape and disk, type it in and see.
    Hacker,
    /// A machine intelligence in 2026: precise, unhurried about its own nature.
    Machine,
};

/// `plain` / `professor` / `hacker` / `machine`: the settings value, which is also
/// what `TIME_WARP_AI_PERSONA` accepts.
[[nodiscard]] QString persona_key(Persona persona);
[[nodiscard]] std::optional<Persona> persona_from_key(const QString& key);
/// The name the settings dialog shows.
[[nodiscard]] QString persona_name(Persona persona);
/// One line under it: what choosing this one does to a reply.
[[nodiscard]] QString persona_description(Persona persona);
/// Every persona, in the order the dialog lists them.
[[nodiscard]] QList<Persona> personas();

/// The task's stable name (`explain`, `fix`, ...), which is what a settings file or
/// a script would say, and the label its button shows.
[[nodiscard]] QString task_key(Task task);
[[nodiscard]] QString task_label(Task task);

/// The standing instructions. Constant, so a test can assert what is in it.
[[nodiscard]] QString system_prompt();
/// The standing instructions with a voice appended: the rules are the same text, so
/// a test can assert that a persona adds to them rather than replacing them.
[[nodiscard]] QString system_prompt(Persona persona);

/// The most document characters that are sent. A 4,000-line program would cost
/// more tokens than most models accept, and a truncated program the model knows is
/// truncated is worth more than a 400 the service rejects.
[[nodiscard]] int max_context_characters();

/// Builds the conversation for one request. `input` is the user's words: the
/// question, the description for `Write`, or the target language for `Translate`.
[[nodiscard]] Conversation build_prompt(Task task, const Context& context, Persona persona,
                                        const QString& input);

/// The same request in the plain voice, which is what a caller that has no settings
/// (a test, a one-shot) wants.
[[nodiscard]] Conversation build_prompt(Task task, const Context& context,
                                        const QString& input);

/// The program inside a reply: the longest fenced block when there is one, else the
/// reply itself, trimmed. Models answer "here is the fix" and then the code, and a
/// user clicking **Insert** wants the code rather than the sentence -- this is what
/// makes that one button behave the way the user means.
[[nodiscard]] QString extract_code(const QString& reply);

}  // namespace timewarp::ai
