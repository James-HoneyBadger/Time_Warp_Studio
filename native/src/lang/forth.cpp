// SPDX-License-Identifier: MIT
//
// The Forth machine. See `forth.hpp` for what this is and is not.
//
// The shape is the classic one: a text interpreter that reads whitespace-separated
// tokens, pushes numbers, executes words, and *compiles* words into a colon
// definition while one is open. There is no syntax tree: a definition is a flat list
// of instructions and a branch is an index into it, which is how Forth has always
// been implemented and why it is small.

#include "timewarp/lang/forth.hpp"

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"

namespace timewarp::lang {

namespace {

struct ForthError {
    std::string message;
};

struct Instruction {
    enum class Op {
        Push,
        Call,
        JumpIfZero,
        Jump,
        PrintString,
        DoSetup,
        Loop,
        PlusLoop,
        Leave,
        Exit,
    };
    Op op = Op::Push;
    long long value = 0;
};

enum class Primitive {
    None,
    Dup, Drop, Swap, Over, Rot, MinusRot, Nip, Tuck, QuestionDup, Depth, Pick, Roll,
    TwoDup, TwoDrop, TwoSwap,
    Add, Subtract, Multiply, Divide, Mod, DivMod, Negate, Abs, Min, Max, Inc, Dec,
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    ZeroEqual, ZeroLess, ZeroGreater, And, Or, Xor, Invert, Lshift, Rshift,
    Dot, DotS, Emit, Cr, Space, Spaces, Words,
    Fetch, Store, CFetch, CStore, PlusStore,
    LoopIndex, OuterLoopIndex,
    Forward, Back, Right, Left, PenUp, PenDown, Home, ClearScreen, SetPenColour,
    SetPenWidth, ShowTurtle, HideTurtle, Bye,
    Poke, Peek,
};

struct Word {
    enum class Kind { Primitive, Colon, Variable, Constant };
    int id = 0;
    std::string name;
    Kind kind = Kind::Primitive;
    Primitive primitive = Primitive::None;
    std::vector<Instruction> code;
    long long constant = 0;
    long long address = 0;
};

struct Token {
    enum class Kind { Word, Number, String };
    Kind kind = Kind::Word;
    std::string text;
    int line = 1;
};

struct PrimitiveEntry {
    const char* name;
    Primitive primitive;
};

const PrimitiveEntry kPrimitives[] = {
    {"DUP", Primitive::Dup},          {"DROP", Primitive::Drop},
    {"SWAP", Primitive::Swap},        {"OVER", Primitive::Over},
    {"ROT", Primitive::Rot},          {"-ROT", Primitive::MinusRot},
    {"NIP", Primitive::Nip},          {"TUCK", Primitive::Tuck},
    {"?DUP", Primitive::QuestionDup}, {"DEPTH", Primitive::Depth},
    {"2DUP", Primitive::TwoDup},      {"2DROP", Primitive::TwoDrop},
    {"2SWAP", Primitive::TwoSwap},
    {"PICK", Primitive::Pick},        {"ROLL", Primitive::Roll},
    {"+", Primitive::Add},            {"-", Primitive::Subtract},
    {"*", Primitive::Multiply},       {"/", Primitive::Divide},
    {"MOD", Primitive::Mod},          {"/MOD", Primitive::DivMod},
    {"NEGATE", Primitive::Negate},    {"ABS", Primitive::Abs},
    {"MIN", Primitive::Min},          {"MAX", Primitive::Max},
    {"1+", Primitive::Inc},           {"1-", Primitive::Dec},
    {"=", Primitive::Equal},          {"<>", Primitive::NotEqual},
    {"<", Primitive::Less},           {">", Primitive::Greater},
    {"<=", Primitive::LessEqual},     {">=", Primitive::GreaterEqual},
    {"0=", Primitive::ZeroEqual},     {"0<", Primitive::ZeroLess},
    {"0>", Primitive::ZeroGreater},   {"AND", Primitive::And},
    {"OR", Primitive::Or},            {"XOR", Primitive::Xor},
    {"INVERT", Primitive::Invert},    {"LSHIFT", Primitive::Lshift},
    {"RSHIFT", Primitive::Rshift},    {".", Primitive::Dot},
    {".S", Primitive::DotS},          {"EMIT", Primitive::Emit},
    {"CR", Primitive::Cr},            {"SPACE", Primitive::Space},
    {"SPACES", Primitive::Spaces},    {"WORDS", Primitive::Words},
    {"@", Primitive::Fetch},          {"!", Primitive::Store},
    {"C@", Primitive::CFetch},        {"C!", Primitive::CStore},
    {"+!", Primitive::PlusStore},     {"I", Primitive::LoopIndex},
    {"J", Primitive::OuterLoopIndex},
    {"FD", Primitive::Forward},       {"FORWARD", Primitive::Forward},
    {"BK", Primitive::Back},          {"BACK", Primitive::Back},
    {"RT", Primitive::Right},         {"RIGHT", Primitive::Right},
    {"LT", Primitive::Left},          {"LEFT", Primitive::Left},
    {"PU", Primitive::PenUp},         {"PENUP", Primitive::PenUp},
    {"PD", Primitive::PenDown},       {"PENDOWN", Primitive::PenDown},
    {"HOME", Primitive::Home},        {"CS", Primitive::ClearScreen},
    {"CLEARSCREEN", Primitive::ClearScreen},
    {"SETPC", Primitive::SetPenColour}, {"SETPENCOLOR", Primitive::SetPenColour},
    {"SETPW", Primitive::SetPenWidth},
    {"ST", Primitive::ShowTurtle},    {"HT", Primitive::HideTurtle},
    {"BYE", Primitive::Bye},
    {"POKE", Primitive::Poke},        {"PEEK", Primitive::Peek},
};

[[nodiscard]] std::string upper(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char character) {
        return static_cast<char>(std::toupper(character));
    });
    return text;
}

class ForthMachine {
public:
    ForthMachine(Interpreter& interpreter, TurtleHost& turtle)
        : interpreter_(interpreter), turtle_(turtle) {
        for (const PrimitiveEntry& entry : kPrimitives) {
            Word word;
            word.name = entry.name;
            word.kind = Word::Kind::Primitive;
            word.primitive = entry.primitive;
            define(std::make_shared<Word>(std::move(word)));
        }
    }

    std::string run(std::string_view source);

    /// One `DO ... LOOP` frame, and one open control structure while a definition
    /// is being compiled. Both belong to the run rather than to a statement, so
    /// both travel in a checkpoint.
    struct LoopFrame {
        long long index = 0;
        long long limit = 0;
    };
    struct Control {
        enum class Kind { If, Else, Begin, While, Do } kind = Kind::If;
        std::size_t index = 0;
        std::vector<std::size_t> leaves;
    };

    /// Everything about the run that is not the token list: where it is, the two
    /// stacks, the dictionary and what has been printed. The program text is
    /// tokenised once and kept here, so only the position in it travels.
    ///
    /// The data space is copied only when a program has written to it -- a `!`,
    /// `C!` or `+!` -- because 64K per statement is a price a program that never
    /// pokes should not pay.
    struct Checkpoint {
        std::size_t pc = 0;
        std::vector<long long> stack;
        std::vector<LoopFrame> loops;
        std::vector<Control> control;
        std::map<std::string, std::shared_ptr<Word>> dictionary;
        std::map<int, std::shared_ptr<Word>> by_id;
        std::vector<std::string> names;
        int next_id = 1;
        std::shared_ptr<Word> current;
        bool compiling = false;
        std::string out;
        std::size_t here = 1;
        std::vector<std::string> strings;
        int line = 1;
        long long steps = 0;
        bool bye = false;
        bool has_memory = false;
        std::vector<std::uint8_t> memory;
    };

    [[nodiscard]] Checkpoint capture() const;
    void apply(const Checkpoint& state);

private:
    // -- reading ---------------------------------------------------------
    [[nodiscard]] std::vector<Token> tokenise(std::string_view source) const;
    static std::optional<long long> parse_number(const std::string& text);

    // -- the dictionary --------------------------------------------------
    void define(const std::shared_ptr<Word>& word);
    [[nodiscard]] std::shared_ptr<Word> find(const std::string& name) const;
    [[nodiscard]] std::shared_ptr<Word> find_by_id(int id) const;

    // -- interpretation --------------------------------------------------
    void interpret(const Token& token);
    bool compile_only(const std::string& name, const Token& token);
    void compile(const Token& token);
    void execute_word(const std::shared_ptr<Word>& word);
    void run_code(const std::vector<Instruction>& code);
    void run_primitive(Primitive primitive, const char* name);

    // -- stacks ----------------------------------------------------------
    void push(long long value) { stack_.push_back(value); }
    long long pop(const char* word);
    void underflow(const char* word) const;
    [[nodiscard]] std::size_t address_of(long long address, const char* word) const;

    // -- output ----------------------------------------------------------
    void write(const std::string& text) { out_ += text; }
    void write_number(long long value) { out_ += std::to_string(value) + " "; }

    /// The program, tokenised once, and the position in it -- which is what a
    /// rewind puts back.
    std::vector<Token> tokens_;
    std::size_t pc_ = 0;
    /// Set when a program writes to the data space, so a checkpoint knows whether
    /// the 64K is worth copying.
    bool memory_dirty_ = false;

    Interpreter& interpreter_;
    TurtleHost& turtle_;
    std::map<std::string, std::shared_ptr<Word>> dictionary_;
    std::map<int, std::shared_ptr<Word>> by_id_;
    std::vector<std::string> names_;
    int next_id_ = 1;

    std::vector<long long> stack_;
    std::vector<LoopFrame> loops_;
    std::string out_;

    bool compiling_ = false;
    std::shared_ptr<Word> current_;
    std::vector<Control> control_;

    /// Set by `BYE`: the interpreter stops between words and nothing after it runs.
    bool bye_ = false;
    std::vector<std::uint8_t> memory_ = std::vector<std::uint8_t>(0x10000, 0);
    std::size_t here_ = 1;
    std::vector<std::string> strings_;
    int line_ = 1;
    long long steps_ = 0;
    static constexpr long long kStepLimit = 4'000'000;
    static constexpr std::size_t kCallLimit = 4096;
};

// -- reading ---------------------------------------------------------------

std::optional<long long> ForthMachine::parse_number(const std::string& text) {
    if (text.empty()) {
        return std::nullopt;
    }
    std::size_t index = 0;
    bool negative = false;
    if (text[0] == '-') {
        negative = true;
        index = 1;
    }
    int base = 10;
    if (index < text.size()) {
        if (text[index] == '$') {
            base = 16;
            ++index;
        } else if (text[index] == '%') {
            base = 2;
            ++index;
        } else if (text[index] == '#') {
            ++index;
        }
    }
    if (index >= text.size()) {
        return std::nullopt;
    }
    long long value = 0;
    for (; index < text.size(); ++index) {
        const char character = text[index];
        int digit = -1;
        if (character >= '0' && character <= '9') {
            digit = character - '0';
        } else if (character >= 'a' && character <= 'f') {
            digit = character - 'a' + 10;
        } else if (character >= 'A' && character <= 'F') {
            digit = character - 'A' + 10;
        } else {
            return std::nullopt;
        }
        if (digit < 0 || digit >= base) {
            return std::nullopt;
        }
        value = value * base + digit;
        if (value > 0xFFFFFFFFLL) {
            return std::nullopt;
        }
    }
    return negative ? -value : value;
}

std::vector<Token> ForthMachine::tokenise(std::string_view source) const {
    std::vector<Token> tokens;
    std::size_t index = 0;
    int line = 1;
    while (index < source.size()) {
        const char character = source[index];
        if (character == '\n') {
            ++line;
            ++index;
            continue;
        }
        if (std::isspace(static_cast<unsigned char>(character)) != 0) {
            ++index;
            continue;
        }
        if (character == '\\') {
            while (index < source.size() && source[index] != '\n') {
                ++index;
            }
            continue;
        }
        if (character == '(') {
            while (index < source.size() && source[index] != ')') {
                if (source[index] == '\n') {
                    ++line;
                }
                ++index;
            }
            if (index < source.size()) {
                ++index;
            }
            continue;
        }
        if (character == '.' && index + 1 < source.size() && source[index + 1] == '"') {
            // `." text"` -- the space after the quote is Forth's delimiter, not the
            // first character of the text, so `." hello"` prints `hello`.
            index += 2;
            if (index < source.size() && source[index] == ' ') {
                ++index;
            }
            const std::size_t start = index;
            while (index < source.size() && source[index] != '"') {
                if (source[index] == '\n') {
                    ++line;
                }
                ++index;
            }
            Token token;
            token.kind = Token::Kind::String;
            token.text = std::string{source.substr(start, index - start)};
            token.line = line;
            tokens.push_back(std::move(token));
            if (index < source.size()) {
                ++index;
            }
            continue;
        }
        const std::size_t start = index;
        while (index < source.size() &&
               std::isspace(static_cast<unsigned char>(source[index])) == 0) {
            ++index;
        }
        Token token;
        token.text = std::string{source.substr(start, index - start)};
        token.line = line;
        token.kind = parse_number(token.text).has_value() ? Token::Kind::Number
                                                          : Token::Kind::Word;
        tokens.push_back(std::move(token));
    }
    return tokens;
}

// -- the dictionary --------------------------------------------------------

void ForthMachine::define(const std::shared_ptr<Word>& word) {
    if (word->id == 0) {
        word->id = next_id_++;
    }
    const bool is_new = dictionary_.find(word->name) == dictionary_.end();
    dictionary_[word->name] = word;
    by_id_[word->id] = word;
    if (is_new) {
        names_.push_back(word->name);
    }
}

std::shared_ptr<Word> ForthMachine::find(const std::string& name) const {
    const auto found = dictionary_.find(name);
    return found == dictionary_.end() ? nullptr : found->second;
}

std::shared_ptr<Word> ForthMachine::find_by_id(int id) const {
    const auto found = by_id_.find(id);
    return found == by_id_.end() ? nullptr : found->second;
}

// -- stacks ----------------------------------------------------------------

void ForthMachine::underflow(const char* word) const {
    throw ForthError{std::string{word} + ": stack underflow"};
}

long long ForthMachine::pop(const char* word) {
    if (stack_.empty()) {
        underflow(word);
    }
    const long long value = stack_.back();
    stack_.pop_back();
    return value;
}

std::size_t ForthMachine::address_of(long long address, const char* word) const {
    if (address < 0 || static_cast<std::size_t>(address) + 1 >= memory_.size()) {
        throw ForthError{std::string{word} + ": address " + std::to_string(address) +
                         " is outside the machine"};
    }
    return static_cast<std::size_t>(address);
}

// -- primitives ------------------------------------------------------------

void ForthMachine::run_primitive(Primitive primitive, const char* name) {
    // Forth's true is all bits set, which is -1 in a signed cell.
    const auto flag = [](bool value) -> long long { return value ? -1LL : 0LL; };
    switch (primitive) {
        case Primitive::Dup: {
            const long long top = pop(name);
            push(top);
            push(top);
            return;
        }
        case Primitive::Drop:
            (void)pop(name);
            return;
        case Primitive::Swap: {
            const long long b = pop(name);
            const long long a = pop(name);
            push(b);
            push(a);
            return;
        }
        case Primitive::Over: {
            const long long b = pop(name);
            const long long a = pop(name);
            push(a);
            push(b);
            push(a);
            return;
        }
        case Primitive::Rot: {
            const long long c = pop(name);
            const long long b = pop(name);
            const long long a = pop(name);
            push(b);
            push(c);
            push(a);
            return;
        }
        case Primitive::MinusRot: {
            const long long c = pop(name);
            const long long b = pop(name);
            const long long a = pop(name);
            push(c);
            push(a);
            push(b);
            return;
        }
        case Primitive::Nip: {
            const long long b = pop(name);
            (void)pop(name);
            push(b);
            return;
        }
        case Primitive::Tuck: {
            const long long b = pop(name);
            const long long a = pop(name);
            push(b);
            push(a);
            push(b);
            return;
        }
        case Primitive::TwoDup: {
            if (stack_.size() < 2) {
                underflow(name);
            }
            const long long a = stack_[stack_.size() - 2];
            const long long b = stack_.back();
            push(a);
            push(b);
            return;
        }
        case Primitive::TwoDrop:
            (void)pop(name);
            (void)pop(name);
            return;
        case Primitive::TwoSwap: {
            if (stack_.size() < 4) {
                underflow(name);
            }
            std::swap(stack_[stack_.size() - 1], stack_[stack_.size() - 3]);
            std::swap(stack_[stack_.size() - 2], stack_[stack_.size() - 4]);
            return;
        }
        case Primitive::QuestionDup:
            if (!stack_.empty() && stack_.back() != 0) {
                push(stack_.back());
            }
            return;
        case Primitive::Depth:
            push(static_cast<long long>(stack_.size()));
            return;
        case Primitive::Pick: {
            const long long depth = pop(name);
            if (depth < 0 || depth >= static_cast<long long>(stack_.size())) {
                underflow(name);
            }
            push(stack_[stack_.size() - 1 - static_cast<std::size_t>(depth)]);
            return;
        }
        case Primitive::Roll: {
            const long long depth = pop(name);
            if (depth < 0 || depth >= static_cast<long long>(stack_.size())) {
                underflow(name);
            }
            const auto index = stack_.size() - 1 - static_cast<std::size_t>(depth);
            const long long value = stack_[index];
            stack_.erase(stack_.begin() + static_cast<std::ptrdiff_t>(index));
            push(value);
            return;
        }
        case Primitive::Add: {
            const long long b = pop(name);
            push(pop(name) + b);
            return;
        }
        case Primitive::Subtract: {
            const long long b = pop(name);
            push(pop(name) - b);
            return;
        }
        case Primitive::Multiply: {
            const long long b = pop(name);
            push(pop(name) * b);
            return;
        }
        case Primitive::Divide: {
            const long long b = pop(name);
            const long long a = pop(name);
            if (b == 0) {
                throw ForthError{"/: division by zero"};
            }
            push(a / b);
            return;
        }
        case Primitive::Mod: {
            const long long b = pop(name);
            const long long a = pop(name);
            if (b == 0) {
                throw ForthError{"MOD: division by zero"};
            }
            push(a % b);
            return;
        }
        case Primitive::DivMod: {
            const long long b = pop(name);
            const long long a = pop(name);
            if (b == 0) {
                throw ForthError{"/MOD: division by zero"};
            }
            push(a % b);
            push(a / b);
            return;
        }
        case Primitive::Negate:
            push(-pop(name));
            return;
        case Primitive::Abs: {
            const long long value = pop(name);
            push(value < 0 ? -value : value);
            return;
        }
        case Primitive::Min: {
            const long long b = pop(name);
            push(std::min(pop(name), b));
            return;
        }
        case Primitive::Max: {
            const long long b = pop(name);
            push(std::max(pop(name), b));
            return;
        }
        case Primitive::Inc:
            push(pop(name) + 1);
            return;
        case Primitive::Dec:
            push(pop(name) - 1);
            return;
        case Primitive::Equal: {
            const long long b = pop(name);
            push(flag(pop(name) == b));
            return;
        }
        case Primitive::NotEqual: {
            const long long b = pop(name);
            push(flag(pop(name) != b));
            return;
        }
        case Primitive::Less: {
            const long long b = pop(name);
            push(flag(pop(name) < b));
            return;
        }
        case Primitive::Greater: {
            const long long b = pop(name);
            push(flag(pop(name) > b));
            return;
        }
        case Primitive::LessEqual: {
            const long long b = pop(name);
            push(flag(pop(name) <= b));
            return;
        }
        case Primitive::GreaterEqual: {
            const long long b = pop(name);
            push(flag(pop(name) >= b));
            return;
        }
        case Primitive::ZeroEqual:
            push(flag(pop(name) == 0));
            return;
        case Primitive::ZeroLess:
            push(flag(pop(name) < 0));
            return;
        case Primitive::ZeroGreater:
            push(flag(pop(name) > 0));
            return;
        case Primitive::And: {
            const long long b = pop(name);
            push(pop(name) & b);
            return;
        }
        case Primitive::Or: {
            const long long b = pop(name);
            push(pop(name) | b);
            return;
        }
        case Primitive::Xor: {
            const long long b = pop(name);
            push(pop(name) ^ b);
            return;
        }
        case Primitive::Invert:
            push(~pop(name));
            return;
        case Primitive::Lshift: {
            const long long count = pop(name);
            const long long value = pop(name);
            push(count < 0 || count > 62 ? value : value << count);
            return;
        }
        case Primitive::Rshift: {
            const long long count = pop(name);
            const long long value = pop(name);
            push(count < 0 || count > 62 ? value : value >> count);
            return;
        }
        case Primitive::Dot:
            write_number(pop(name));
            return;
        case Primitive::DotS:
            write("<" + std::to_string(stack_.size()) + "> ");
            for (const long long value : stack_) {
                write_number(value);
            }
            return;
        case Primitive::Emit:
            write(std::string(1, static_cast<char>(pop(name) & 0xFF)));
            return;
        case Primitive::Cr:
            write("\n");
            return;
        case Primitive::Space:
            write(" ");
            return;
        case Primitive::Spaces: {
            const long long count = pop(name);
            if (count > 0) {
                write(std::string(static_cast<std::size_t>(count), ' '));
            }
            return;
        }
        case Primitive::Words: {
            std::string line;
            for (const std::string& word_name : names_) {
                if (!line.empty() && line.size() + word_name.size() + 1 > 76) {
                    write(line + "\n");
                    line.clear();
                }
                if (!line.empty()) {
                    line += " ";
                }
                line += word_name;
            }
            if (!line.empty()) {
                write(line + "\n");
            }
            return;
        }
        case Primitive::Fetch: {
            const auto address = address_of(pop(name), name);
            push(static_cast<long long>(memory_[address]) |
                 (static_cast<long long>(memory_[address + 1]) << 8));
            return;
        }
        case Primitive::Store: {
            // `!` is `( x addr -- )`: the address is on top, which is the one thing
            // about Forth's memory words that a reader gets wrong once.
            const auto address = address_of(pop(name), name);
            const long long value = pop(name);
            memory_[address] = static_cast<std::uint8_t>(value & 0xFF);
            memory_[address + 1] = static_cast<std::uint8_t>((value >> 8) & 0xFF);
            memory_dirty_ = true;
            return;
        }
        case Primitive::CFetch: {
            const auto address = address_of(pop(name), name);
            push(memory_[address]);
            return;
        }
        case Primitive::CStore: {
            const auto address = address_of(pop(name), name);
            const long long value = pop(name);
            memory_[address] = static_cast<std::uint8_t>(value & 0xFF);
            memory_dirty_ = true;
            return;
        }
        case Primitive::PlusStore: {
            const auto address = address_of(pop(name), name);
            const long long value = pop(name);
            const long long current = static_cast<long long>(memory_[address]) |
                                      (static_cast<long long>(memory_[address + 1]) << 8);
            const long long next = current + value;
            memory_[address] = static_cast<std::uint8_t>(next & 0xFF);
            memory_[address + 1] = static_cast<std::uint8_t>((next >> 8) & 0xFF);
            memory_dirty_ = true;
            return;
        }
        case Primitive::LoopIndex:
            if (loops_.empty()) {
                throw ForthError{"I outside a DO loop"};
            }
            push(loops_.back().index);
            return;
        case Primitive::OuterLoopIndex:
            if (loops_.size() < 2) {
                throw ForthError{"J outside a nested DO loop"};
            }
            push(loops_[loops_.size() - 2].index);
            return;
        case Primitive::Forward:
            turtle_.forward(static_cast<double>(pop(name)));
            return;
        case Primitive::Back:
            turtle_.back(static_cast<double>(pop(name)));
            return;
        case Primitive::Right:
            turtle_.right(static_cast<double>(pop(name)));
            return;
        case Primitive::Left:
            turtle_.left(static_cast<double>(pop(name)));
            return;
        case Primitive::PenUp:
            turtle_.pen_up();
            return;
        case Primitive::PenDown:
            turtle_.pen_down();
            return;
        case Primitive::Home:
            turtle_.home();
            return;
        case Primitive::ClearScreen:
            turtle_.clear();
            return;
        case Primitive::SetPenColour: {
            const long long index = pop(name);
            turtle_.set_color(turtle_.resolve_color(static_cast<int>(index)));
            return;
        }
        case Primitive::SetPenWidth:
            turtle_.set_pen_width(static_cast<double>(pop(name)));
            return;
        case Primitive::ShowTurtle:
            turtle_.show_turtle();
            return;
        case Primitive::HideTurtle:
            turtle_.hide_turtle();
            return;
        case Primitive::Poke: {
            // `POKE ( value address -- )` reaches the *machine*: `128 32768 POKE` on
            // the HB-1 writes a character cell on the tube, as BASIC's POKE does.
            // With no machine attached the same word writes this program's own 64K,
            // so a poke and a peek always agree instead of the write vanishing.
            const long long address = pop(name);
            const long long value = pop(name);
            if (address < 0 || address > 0xFFFF) {
                throw ForthError{"POKE address must be 0-65535"};
            }
            if (value < 0 || value > 255) {
                throw ForthError{"POKE value must be 0-255"};
            }
            const auto byte = static_cast<std::uint8_t>(value);
            if (interpreter_.machine_attached()) {
                if (!interpreter_.machine_poke(static_cast<int>(address), byte)) {
                    throw ForthError{"POKE " + std::to_string(address) +
                                     " is ROM: it cannot be written"};
                }
                return;
            }
            memory_[static_cast<std::size_t>(address)] = byte;
            return;
        }
        case Primitive::Peek: {
            const long long address = pop(name);
            if (address < 0 || address > 0xFFFF) {
                throw ForthError{"PEEK address must be 0-65535"};
            }
            push(interpreter_.machine_attached()
                     ? static_cast<long long>(
                           interpreter_.machine_peek(static_cast<int>(address)))
                     : static_cast<long long>(memory_[static_cast<std::size_t>(address)]));
            return;
        }
        case Primitive::Bye:
            // `BYE` ends the program where it stands: whatever the words after it
            // would have printed is not printed, which is the whole point of it.
            bye_ = true;
            return;
        case Primitive::None:
            return;
    }
}

// -- execution -------------------------------------------------------------

void ForthMachine::execute_word(const std::shared_ptr<Word>& word) {
    switch (word->kind) {
        case Word::Kind::Primitive:
            run_primitive(word->primitive, word->name.c_str());
            return;
        case Word::Kind::Constant:
            push(word->constant);
            return;
        case Word::Kind::Variable:
            push(word->address);
            return;
        case Word::Kind::Colon:
            break;
    }
    run_code(word->code);
}

void ForthMachine::run_code(const std::vector<Instruction>& code) {
    struct Frame {
        const std::vector<Instruction>* code = nullptr;
        std::size_t ip = 0;
        std::size_t loop_depth = 0;
    };
    std::vector<Frame> frames;
    frames.push_back(Frame{&code, 0, loops_.size()});
    while (!frames.empty()) {
        // The frame is read through a reference but written back through its
        // *index*: a call pushes a frame, and a `std::vector` that reallocates moves
        // every frame with it. A reference taken before the push and written after it
        // is a use-after-free, and AddressSanitizer found exactly that in a lesson
        // program.
        const std::size_t depth = frames.size() - 1;
        const Frame& frame = frames[depth];
        if (bye_) {
            loops_.resize(frame.loop_depth);
            frames.clear();
            break;
        }
        if (frame.ip >= frame.code->size() || ++steps_ > kStepLimit) {
            if (steps_ > kStepLimit) {
                throw ForthError{"the program ran for " + std::to_string(kStepLimit) +
                                 " instructions without stopping"};
            }
            loops_.resize(frame.loop_depth);
            frames.pop_back();
            continue;
        }
        const Instruction& instruction = (*frame.code)[frame.ip];
        frames[depth].ip = frame.ip + 1;
        std::size_t ip = frames[depth].ip;
        switch (instruction.op) {
            case Instruction::Op::Push:
                push(instruction.value);
                break;
            case Instruction::Op::Call: {
                const std::shared_ptr<Word> callee =
                    find_by_id(static_cast<int>(instruction.value));
                if (callee == nullptr) {
                    throw ForthError{"call to a word that is not there"};
                }
                if (callee->kind != Word::Kind::Colon) {
                    execute_word(callee);
                    break;
                }
                if (frames.size() >= kCallLimit) {
                    throw ForthError{callee->name + ": too deep (endless recursion?)"};
                }
                frames.push_back(Frame{&callee->code, 0, loops_.size()});
                break;
            }
            case Instruction::Op::JumpIfZero:
                if (pop("IF") == 0) {
                    ip = static_cast<std::size_t>(instruction.value);
                }
                break;
            case Instruction::Op::Jump:
                ip = static_cast<std::size_t>(instruction.value);
                break;
            case Instruction::Op::PrintString:
                write(strings_[static_cast<std::size_t>(instruction.value)]);
                break;
            case Instruction::Op::DoSetup: {
                const long long start = pop("DO");
                const long long limit = pop("DO");
                loops_.push_back(LoopFrame{start, limit});
                break;
            }
            case Instruction::Op::Loop: {
                if (loops_.empty()) {
                    throw ForthError{"LOOP without DO"};
                }
                LoopFrame& loop = loops_.back();
                ++loop.index;
                if (loop.index < loop.limit) {
                    ip = static_cast<std::size_t>(instruction.value);
                } else {
                    loops_.pop_back();
                }
                break;
            }
            case Instruction::Op::PlusLoop: {
                if (loops_.empty()) {
                    throw ForthError{"+LOOP without DO"};
                }
                const long long step = pop("+LOOP");
                LoopFrame& loop = loops_.back();
                loop.index += step;
                if (step > 0 ? loop.index < loop.limit : loop.index >= loop.limit) {
                    ip = static_cast<std::size_t>(instruction.value);
                } else {
                    loops_.pop_back();
                }
                break;
            }
            case Instruction::Op::Leave:
                if (!loops_.empty()) {
                    loops_.pop_back();
                }
                ip = static_cast<std::size_t>(instruction.value);
                break;
            case Instruction::Op::Exit:
                ip = frame.code->size();
                break;
        }
        // A jump replaced the ip; otherwise the ip is already the next instruction.
        frames[depth].ip = ip;
    }
}

// -- the text interpreter --------------------------------------------------

void ForthMachine::compile(const Token& token) {
    std::shared_ptr<Word> word = current_;
    if (word == nullptr) {
        throw ForthError{"compiling with no definition open"};
    }
    if (token.kind == Token::Kind::Number) {
        Instruction instruction;
        instruction.op = Instruction::Op::Push;
        instruction.value = *parse_number(token.text);
        word->code.push_back(instruction);
        return;
    }
    if (token.kind == Token::Kind::String) {
        strings_.push_back(token.text);
        Instruction instruction;
        instruction.op = Instruction::Op::PrintString;
        instruction.value = static_cast<long long>(strings_.size() - 1);
        word->code.push_back(instruction);
        return;
    }
    const std::string name = upper(token.text);
    const std::shared_ptr<Word> found = find(name);
    if (found == nullptr) {
        throw ForthError{"undefined word: " + name};
    }
    Instruction call;
    call.op = Instruction::Op::Call;
    call.value = found->id;
    word->code.push_back(call);
}

bool ForthMachine::compile_only(const std::string& name, const Token& token) {
    (void)token;
    if (!compiling_) {
        return false;
    }
    std::shared_ptr<Word> word = current_;
    const auto emit = [&word](Instruction::Op op, long long value) {
        Instruction instruction;
        instruction.op = op;
        instruction.value = value;
        word->code.push_back(instruction);
        return word->code.size() - 1;
    };
    if (name == "IF") {
        control_.push_back(Control{Control::Kind::If, emit(Instruction::Op::JumpIfZero, 0), {}});
        return true;
    }
    if (name == "ELSE") {
        if (control_.empty() || control_.back().kind != Control::Kind::If) {
            throw ForthError{"ELSE without IF"};
        }
        const std::size_t jump = emit(Instruction::Op::Jump, 0);
        word->code[control_.back().index].value =
            static_cast<long long>(word->code.size());
        control_.back().kind = Control::Kind::Else;
        control_.back().index = jump;
        return true;
    }
    if (name == "THEN") {
        if (control_.empty() || (control_.back().kind != Control::Kind::If &&
                                 control_.back().kind != Control::Kind::Else)) {
            throw ForthError{"THEN without IF"};
        }
        word->code[control_.back().index].value =
            static_cast<long long>(word->code.size());
        control_.pop_back();
        return true;
    }
    if (name == "BEGIN") {
        control_.push_back(Control{Control::Kind::Begin, word->code.size(), {}});
        return true;
    }
    if (name == "UNTIL") {
        if (control_.empty() || control_.back().kind != Control::Kind::Begin) {
            throw ForthError{"UNTIL without BEGIN"};
        }
        emit(Instruction::Op::JumpIfZero, static_cast<long long>(control_.back().index));
        control_.pop_back();
        return true;
    }
    if (name == "WHILE") {
        if (control_.empty() || control_.back().kind != Control::Kind::Begin) {
            throw ForthError{"WHILE without BEGIN"};
        }
        const std::size_t jump = emit(Instruction::Op::JumpIfZero, 0);
        control_.push_back(Control{Control::Kind::While, jump, {}});
        return true;
    }
    if (name == "REPEAT") {
        if (control_.size() < 2 || control_.back().kind != Control::Kind::While ||
            control_[control_.size() - 2].kind != Control::Kind::Begin) {
            throw ForthError{"REPEAT without WHILE"};
        }
        const std::size_t while_jump = control_.back().index;
        control_.pop_back();
        const std::size_t begin = control_.back().index;
        control_.pop_back();
        emit(Instruction::Op::Jump, static_cast<long long>(begin));
        word->code[while_jump].value = static_cast<long long>(word->code.size());
        return true;
    }
    if (name == "DO") {
        control_.push_back(Control{Control::Kind::Do, emit(Instruction::Op::DoSetup, 0), {}});
        return true;
    }
    if (name == "LOOP" || name == "+LOOP") {
        if (control_.empty() || control_.back().kind != Control::Kind::Do) {
            throw ForthError{name + " without DO"};
        }
        const std::size_t target = control_.back().index + 1;
        emit(name == "LOOP" ? Instruction::Op::Loop : Instruction::Op::PlusLoop,
             static_cast<long long>(target));
        const std::size_t after = word->code.size();
        for (const std::size_t leave : control_.back().leaves) {
            word->code[leave].value = static_cast<long long>(after);
        }
        control_.pop_back();
        return true;
    }
    if (name == "LEAVE") {
        // `LEAVE` is written inside the loop body but is almost always inside an
        // `IF` as well, so the frame it belongs to is the innermost `DO`, not the
        // top of the control stack. `LOOP` patches every leave it was given.
        std::size_t loop = control_.size();
        while (loop > 0) {
            --loop;
            if (control_[loop].kind == Control::Kind::Do) {
                control_[loop].leaves.push_back(emit(Instruction::Op::Leave, 0));
                return true;
            }
        }
        throw ForthError{"LEAVE outside a DO loop"};
    }
    if (name == "RECURSE") {
        Instruction call;
        call.op = Instruction::Op::Call;
        call.value = word->id;
        word->code.push_back(call);
        return true;
    }
    if (name == "EXIT") {
        emit(Instruction::Op::Exit, 0);
        return true;
    }
    return false;
}

void ForthMachine::interpret(const Token& token) {
    line_ = token.line;
    if (token.kind == Token::Kind::Number) {
        if (compiling_) {
            compile(token);
        } else {
            push(*parse_number(token.text));
        }
        return;
    }
    if (token.kind == Token::Kind::String) {
        if (compiling_) {
            compile(token);
        } else {
            write(token.text);
        }
        return;
    }

    const std::string name = upper(token.text);
    if (name == ":") {
        if (compiling_) {
            throw ForthError{"`:` inside a definition"};
        }
        compiling_ = true;
        current_ = std::make_shared<Word>();
        current_->kind = Word::Kind::Colon;
        current_->id = next_id_++;
        control_.clear();
        return;
    }
    if (name == ";") {
        if (!compiling_) {
            throw ForthError{"; without a definition"};
        }
        if (!control_.empty()) {
            throw ForthError{"unfinished control structure in " + current_->name};
        }
        compiling_ = false;
        if (current_->name.empty()) {
            throw ForthError{"a definition with no name"};
        }
        // Re-defining a word replaces it, which is Forth's whole way of working.
        define(current_);
        current_.reset();
        return;
    }
    if (name == "IMMEDIATE") {
        // Accepted and ignored, before or after the `;`: this machine has no user
        // immediate words, and a listing copied from a book should not be rejected
        // for one line. The manual says so rather than leaving a reader to find out.
        return;
    }
    if (compiling_ && current_->name.empty()) {
        current_->name = name;
        // The word goes into the dictionary *now* so that `RECURSE` and a recursive
        // call by name both resolve while it is still being defined.
        define(current_);
        return;
    }
    if (compile_only(name, token)) {
        return;
    }
    if (compiling_) {
        compile(token);
        return;
    }
    const std::shared_ptr<Word> found = find(name);
    if (found == nullptr) {
        throw ForthError{"undefined word: " + name};
    }
    execute_word(found);
}

ForthMachine::Checkpoint ForthMachine::capture() const {
    Checkpoint state;
    state.pc = pc_;
    state.stack = stack_;
    state.loops = loops_;
    state.control = control_;
    state.dictionary = dictionary_;
    state.by_id = by_id_;
    state.names = names_;
    state.next_id = next_id_;
    state.current = current_;
    state.compiling = compiling_;
    state.out = out_;
    state.here = here_;
    state.strings = strings_;
    state.line = line_;
    state.steps = steps_;
    state.bye = bye_;
    state.has_memory = memory_dirty_;
    if (memory_dirty_) {
        state.memory = memory_;
    }
    return state;
}

void ForthMachine::apply(const Checkpoint& state) {
    pc_ = state.pc;
    stack_ = state.stack;
    loops_ = state.loops;
    control_ = state.control;
    dictionary_ = state.dictionary;
    by_id_ = state.by_id;
    names_ = state.names;
    next_id_ = state.next_id;
    current_ = state.current;
    compiling_ = state.compiling;
    out_ = state.out;
    here_ = state.here;
    strings_ = state.strings;
    line_ = state.line;
    steps_ = state.steps;
    bye_ = state.bye;
    memory_dirty_ = state.has_memory;
    if (state.has_memory) {
        memory_ = state.memory;
    }
}

std::string ForthMachine::run(std::string_view source) {
    if (tokens_.empty()) {
        tokens_ = tokenise(source);
    }
    try {
        while (pc_ < tokens_.size() && !bye_) {
            // The debugger's pause point, and where the position is handed over: a
            // token is the unit here, which is what a Forth programmer steps through.
            // A rewind comes back as a checkpoint to adopt, which is how the run
            // finds itself earlier in the token list with its stacks as they were.
            const Token& token = tokens_[pc_];
            const Interpreter::WholeProgramStep step =
                interpreter_.whole_program_pause(token.line, capture());
            if (!step.go_on) {
                break;
            }
            if (step.restored.has_value()) {
                apply(std::any_cast<const Checkpoint&>(*step.restored));
                continue;
            }
            const std::size_t index = pc_;
            const std::string name =
                token.kind == Token::Kind::Word ? upper(token.text) : std::string{};
            // The defining words take the *next* token as their name, which is why they
            // are read here rather than as ordinary words.
            if (!compiling_ && (name == "VARIABLE" || name == "CONSTANT")) {
                if (index + 1 >= tokens_.size()) {
                    throw ForthError{name + " needs a name"};
                }
                Word word;
                word.name = upper(tokens_[index + 1].text);
                word.id = next_id_++;
                if (name == "VARIABLE") {
                    word.kind = Word::Kind::Variable;
                    word.address = static_cast<long long>(here_);
                    here_ += 2;
                    if (here_ + 1 >= memory_.size()) {
                        throw ForthError{"VARIABLE: out of data space"};
                    }
                } else {
                    word.kind = Word::Kind::Constant;
                    word.constant = pop("CONSTANT");
                }
                define(std::make_shared<Word>(std::move(word)));
                pc_ += 2;
                continue;
            }
            interpret(token);
            ++pc_;
        }
    } catch (const ForthError& error) {
        // What the program printed before it failed is still printed: a Forth that
        // threw away its output on the first error would hide the evidence.
        return out_ + "❌ Forth, line " + std::to_string(line_) + ": " + error.message + "\n";
    }
    if (compiling_) {
        return out_ + "❌ Forth: the definition of " +
               (current_ != nullptr ? current_->name : std::string{"?"}) +
               " was never closed with `;`\n";
    }
    loops_.clear();
    return out_;
}

}  // namespace

std::string execute_forth(Interpreter& interpreter, std::string_view source,
                          TurtleHost& turtle) {
    ForthMachine machine{interpreter, turtle};
    return machine.run(source);
}

}  // namespace timewarp::lang
