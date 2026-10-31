// SPDX-License-Identifier: MIT

#include "timewarp/hb2/asm.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <charconv>
#include <map>
#include <optional>
#include <sstream>

namespace timewarp::hb2 {

namespace {

constexpr std::array<std::string_view, static_cast<std::size_t>(Op::Count)> kMnemonics{{
    "NOP",  "HALT", "MOV",  "ADD",  "SUB",   "AND",  "OR",   "XOR",  "NOT",  "CMP",
    "SHL",  "SHR",  "SAR",  "MUL",  "DIV",   "MOD",  "INC",  "DEC",  "NEG",  "TEST",
    "SWAP", "LOAD", "LOADB","STORE","STOREB","LEA",  "PUSH", "POP",  "CALL", "RET",
    "JMP",  "JZ",   "JNZ",  "JC",   "JNC",   "JN",   "JNS",  "JLT",  "JGT",  "JLE",
    "JGE",  "INT",  "IN",   "OUT",
}};

/// Base cost, in machine cycles. One per opcode, in opcode order, so a reader can
/// check a row of the manual's timing table against the same number. It is data
/// rather than a switch because the manual prints it.
constexpr std::array<std::uint32_t, static_cast<std::size_t>(Op::Count)> kCycles{{
    1,  1,  2,  2,  2,  2,  2,  2,  1,  2,  2,  2,  2,  12, 20, 20,
    1,  1,  1,  2,  2,  4,  3,  4,  3,  2,  3,  3,  6,  5,  3,  3,
    3,  3,  3,  3,  3,  3,  3,  3,  3,  8,  3,  3,
}};
static_assert(kCycles.size() == static_cast<std::size_t>(Op::Count),
              "one cycle entry per opcode, no more and no fewer");

std::string upper(std::string_view text) {
    std::string out{text};
    std::transform(out.begin(), out.end(), out.begin(),
                   [](unsigned char character) { return static_cast<char>(std::toupper(character)); });
    return out;
}

std::string trim(std::string_view text) {
    std::size_t begin = 0;
    std::size_t end = text.size();
    while (begin < end && std::isspace(static_cast<unsigned char>(text[begin])) != 0) {
        ++begin;
    }
    while (end > begin && std::isspace(static_cast<unsigned char>(text[end - 1])) != 0) {
        --end;
    }
    return std::string{text.substr(begin, end - begin)};
}

std::string hex(std::uint32_t value, int digits) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    std::string text(static_cast<std::size_t>(digits), '0');
    for (int index = digits - 1; index >= 0; --index) {
        text[static_cast<std::size_t>(index)] = kHex[value & 0xF];
        value >>= 4;
    }
    return text;
}

std::vector<std::string> splitOperands(std::string_view text) {
    std::vector<std::string> parts;
    std::string current;
    int depth = 0;
    bool quoted = false;
    for (const char character : text) {
        if (character == '"') {
            quoted = !quoted;
        }
        if (character == '[' && !quoted) {
            ++depth;
        } else if (character == ']' && !quoted) {
            --depth;
        }
        if (character == ',' && depth == 0 && !quoted) {
            parts.push_back(trim(current));
            current.clear();
            continue;
        }
        current.push_back(character);
    }
    const std::string tail = trim(current);
    if (!tail.empty()) {
        parts.push_back(tail);
    }
    return parts;
}

/// A register operand: `R0` to `R7`, in either case.
std::optional<std::uint8_t> registerNamed(std::string_view text) {
    const std::string name = upper(text);
    if (name.size() != 2 || name[0] != 'R') {
        return std::nullopt;
    }
    if (name[1] < '0' || name[1] > '7') {
        return std::nullopt;
    }
    return static_cast<std::uint8_t>(name[1] - '0');
}

struct ParsedLine {
    std::string label;
    std::string mnemonic;
    std::vector<std::string> operands;
    std::string text;
};

/// Splits a source line into its label, mnemonic and operands. A line that is only
/// a comment, or only blank, comes back with an empty mnemonic.
ParsedLine parseLine(std::string_view raw) {
    ParsedLine line;
    line.text = trim(raw);
    std::string body;
    // A comment runs to the end of the line: `;` is the machine's, as it is on every
    // assembler a reader of this tree has met.
    const std::size_t comment = line.text.find(';');
    body = trim(comment == std::string::npos ? line.text : line.text.substr(0, comment));
    if (body.empty()) {
        return line;
    }
    const std::size_t colon = body.find(':');
    if (colon != std::string::npos && body.find_first_of(" \t") > colon) {
        line.label = trim(body.substr(0, colon));
        body = trim(body.substr(colon + 1));
    }
    if (body.empty()) {
        return line;
    }
    std::size_t end = 0;
    while (end < body.size() && std::isspace(static_cast<unsigned char>(body[end])) == 0) {
        ++end;
    }
    line.mnemonic = upper(body.substr(0, end));
    line.operands = splitOperands(body.substr(end));
    return line;
}

/// A number, a character or a label, and sums and differences of them. That is all a
/// listing needs, and a bigger expression language would be a second interpreter.
class Expr {
public:
    Expr(std::string text, const std::map<std::string, std::uint32_t>& symbols, bool final)
        : text_(trim(text)), symbols_(symbols), final_(final) {}

    [[nodiscard]] std::optional<std::int64_t> value() {
        std::int64_t total = 0;
        int sign = 1;
        while (position_ < text_.size()) {
            skipSpace();
            if (position_ >= text_.size()) {
                break;
            }
            const char character = text_[position_];
            if (character == '+') {
                sign = 1;
                ++position_;
                continue;
            }
            if (character == '-') {
                sign = -1;
                ++position_;
                continue;
            }
            const std::optional<std::int64_t> term = readTerm();
            if (!term) {
                return std::nullopt;
            }
            total += sign * *term;
            sign = 1;
        }
        return total;
    }

private:
    void skipSpace() {
        while (position_ < text_.size() && std::isspace(static_cast<unsigned char>(text_[position_])) != 0) {
            ++position_;
        }
    }

    std::optional<std::int64_t> readTerm() {
        skipSpace();
        if (position_ >= text_.size()) {
            return std::nullopt;
        }
        if (text_[position_] == '\'') {
            if (position_ + 2 < text_.size() && text_[position_ + 2] == '\'') {
                const auto code = static_cast<std::int64_t>(
                    static_cast<unsigned char>(text_[position_ + 1]));
                position_ += 3;
                return code;
            }
            return std::nullopt;
        }
        if (text_[position_] == '$') {
            ++position_;
            const std::size_t start = position_;
            while (position_ < text_.size() &&
                   std::isxdigit(static_cast<unsigned char>(text_[position_])) != 0) {
                ++position_;
            }
            if (position_ == start) {
                return std::nullopt;
            }
            return std::stoll(text_.substr(start, position_ - start), nullptr, 16);
        }
        if (std::isdigit(static_cast<unsigned char>(text_[position_])) != 0) {
            const std::size_t start = position_;
            while (position_ < text_.size() &&
                   std::isdigit(static_cast<unsigned char>(text_[position_])) != 0) {
                ++position_;
            }
            return std::stoll(text_.substr(start, position_ - start));
        }
        const std::size_t start = position_;
        while (position_ < text_.size() &&
               (std::isalnum(static_cast<unsigned char>(text_[position_])) != 0 ||
                text_[position_] == '_' || text_[position_] == '.')) {
            ++position_;
        }
        if (position_ == start) {
            return std::nullopt;
        }
        const std::string name = text_.substr(start, position_ - start);
        const auto found = symbols_.find(name);
        if (found != symbols_.end()) {
            return static_cast<std::int64_t>(found->second);
        }
        // An unresolved label is only an error once every line has been read, which
        // is what the second pass is for.
        if (!final_) {
            return 0;
        }
        return std::nullopt;
    }

    std::string text_;
    const std::map<std::string, std::uint32_t>& symbols_;
    bool final_ = false;
    std::size_t position_ = 0;
};

std::string registerName(std::uint8_t index) { return "R" + std::to_string(index); }

/// Turns one parsed line into an instruction. Returns false with a sentence for a
/// line the machine cannot encode, because "line 4: STORE needs a bracketed
/// destination" is what a person needs and a byte offset is not.
bool encodeInstruction(const ParsedLine& line, const std::map<std::string, std::uint32_t>& symbols,
                       bool final, Instruction* out, std::string* error) {
    const auto fail = [error](std::string message) {
        if (error != nullptr) {
            *error = std::move(message);
        }
        return false;
    };
    Op op = Op::Nop;
    bool found = false;
    for (std::size_t index = 0; index < kMnemonics.size(); ++index) {
        if (kMnemonics[index] == line.mnemonic) {
            op = static_cast<Op>(index);
            found = true;
            break;
        }
    }
    if (!found) {
        return fail("unknown mnemonic '" + line.mnemonic + "'");
    }
    out->op = op;
    out->a = 0;
    out->b = 0;
    out->mode = Mode::Reg;
    out->operand = 0;

    const auto eval = [&symbols, final](const std::string& text) -> std::optional<std::int64_t> {
        Expr expr{text, symbols, final};
        return expr.value();
    };
    const auto valueOf = [&eval](const std::string& text) -> std::optional<std::uint32_t> {
        const std::optional<std::int64_t> value = eval(text);
        if (!value) {
            return std::nullopt;
        }
        return static_cast<std::uint32_t>(*value);
    };
    /// `[Rn]`, `[Rn+disp]` and `[addr]` are three modes and one syntax, so the three
    /// are decided in one place.
    const auto readBracket = [&](const std::string& text, bool pointer_in_a) -> bool {
        if (text.size() < 2 || text.front() != '[' || text.back() != ']') {
            return fail("expected a bracketed address or register in '" + text + "'");
        }
        const std::string inside = trim(text.substr(1, text.size() - 2));
        const std::optional<std::uint8_t> direct = registerNamed(inside);
        if (direct) {
            out->mode = Mode::RegInd;
            if (pointer_in_a) {
                out->a = *direct;
            } else {
                out->b = *direct;
            }
            return true;
        }
        const std::size_t sign = inside.find_first_of("+-");
        if (sign != std::string::npos) {
            const std::optional<std::uint8_t> base = registerNamed(trim(inside.substr(0, sign)));
            if (base) {
                const std::optional<std::int64_t> displacement = eval(inside.substr(sign));
                if (!displacement) {
                    return fail("cannot evaluate the displacement in '" + inside + "'");
                }
                out->mode = Mode::Indexed;
                if (pointer_in_a) {
                    out->a = *base;
                } else {
                    out->b = *base;
                }
                out->operand = static_cast<std::uint32_t>(static_cast<std::int32_t>(*displacement));
                return true;
            }
        }
        const std::optional<std::uint32_t> absolute = valueOf(inside);
        if (!absolute) {
            return fail("cannot evaluate '" + inside + "'");
        }
        out->mode = Mode::Abs;
        out->operand = *absolute;
        return true;
    };

    const std::vector<std::string>& operands = line.operands;
    if (op == Op::Nop || op == Op::Halt || op == Op::Ret) {
        if (!operands.empty()) {
            return fail(std::string{mnemonic(op)} + " takes no operands");
        }
        return true;
    }
    if (op == Op::Int) {
        if (operands.size() != 1) {
            return fail("INT takes a vector");
        }
        const std::optional<std::uint32_t> vector = valueOf(operands[0]);
        if (!vector) {
            return fail("cannot evaluate '" + operands[0] + "'");
        }
        out->mode = Mode::Imm;
        out->operand = *vector;
        return true;
    }
    if (op == Op::In) {
        if (operands.size() != 2) {
            return fail("IN needs a register and a port");
        }
        const std::optional<std::uint8_t> destination = registerNamed(operands[0]);
        const std::optional<std::uint32_t> port = valueOf(operands[1]);
        if (!destination || !port) {
            return fail("IN is 'IN Rd, port'");
        }
        out->a = *destination;
        out->mode = Mode::Abs;
        out->operand = *port;
        return true;
    }
    if (op == Op::Out) {
        if (operands.size() != 2) {
            return fail("OUT needs a port and a register");
        }
        const std::optional<std::uint32_t> port = valueOf(operands[0]);
        const std::optional<std::uint8_t> source = registerNamed(operands[1]);
        if (!port || !source) {
            return fail("OUT is 'OUT port, Rs'");
        }
        out->b = *source;
        out->mode = Mode::Abs;
        out->operand = *port;
        return true;
    }
    if (isJump(op)) {
        if (operands.size() != 1) {
            return fail(std::string{mnemonic(op)} + " takes one target");
        }
        const std::optional<std::uint32_t> target = valueOf(operands[0]);
        if (!target) {
            return fail("cannot evaluate '" + operands[0] + "'");
        }
        out->mode = Mode::Rel;
        out->operand = *target;
        return true;
    }
    if (isUnary(op)) {
        if (operands.size() != 1) {
            return fail(std::string{mnemonic(op)} + " takes one register");
        }
        const std::optional<std::uint8_t> reg = registerNamed(operands[0]);
        if (!reg) {
            return fail(std::string{mnemonic(op)} + " takes a register");
        }
        out->b = *reg;
        return true;
    }
    if (op == Op::Store || op == Op::StoreB) {
        if (operands.size() != 2) {
            return fail("STORE needs a destination and a register");
        }
        const std::optional<std::uint8_t> source = registerNamed(operands[1]);
        if (!source) {
            return fail("the second operand of STORE must be a register");
        }
        out->b = *source;
        return readBracket(operands[0], true);
    }
    if (op == Op::Lea) {
        if (operands.size() != 2) {
            return fail("LEA needs a register and an address");
        }
        const std::optional<std::uint8_t> destination = registerNamed(operands[0]);
        const std::optional<std::uint32_t> target = valueOf(operands[1]);
        if (!destination || !target) {
            return fail("LEA is 'LEA Rd, address'");
        }
        out->a = *destination;
        out->mode = Mode::Abs;
        out->operand = *target;
        return true;
    }

    // Two-operand moves and arithmetic: `OP Rd, source`.
    if (operands.size() != 2) {
        return fail(std::string{mnemonic(op)} + " needs two operands");
    }
    const std::optional<std::uint8_t> destination = registerNamed(operands[0]);
    if (!destination) {
        return fail("the destination must be a register");
    }
    out->a = *destination;
    const std::string& source = operands[1];
    if (const std::optional<std::uint8_t> reg = registerNamed(source)) {
        out->b = *reg;
        return true;
    }
    if (!source.empty() && source.front() == '#') {
        const std::optional<std::uint32_t> value = valueOf(source.substr(1));
        if (!value) {
            return fail("cannot evaluate '" + source + "'");
        }
        out->mode = Mode::Imm;
        out->operand = *value;
        return true;
    }
    if (!source.empty() && source.front() == '[') {
        return readBracket(source, false);
    }
    return fail("cannot read the source operand '" + source + "'");
}


}  // namespace

int instructionWords(Mode mode) {
    switch (mode) {
    case Mode::Reg:
    case Mode::RegInd:
        return 1;
    case Mode::Imm:
    case Mode::Indexed:
    case Mode::Rel:
        return 2;
    case Mode::Abs:
        return 3;
    }
    return 1;
}

int instructionBytes(Mode mode) { return instructionWords(mode) * 2; }

std::uint32_t cyclesFor(Op op, Mode mode) {
    const std::size_t index = static_cast<std::size_t>(op);
    std::uint32_t cycles = index < kCycles.size() ? kCycles[index] : 1;
    switch (mode) {
    case Mode::Imm:
    case Mode::Indexed:
    case Mode::RegInd:
        cycles += 1;
        break;
    case Mode::Abs:
        cycles += 2;
        break;
    case Mode::Reg:
    case Mode::Rel:
        break;
    }
    return cycles;
}

std::string_view mnemonic(Op op) {
    const std::size_t index = static_cast<std::size_t>(op);
    return index < kMnemonics.size() ? kMnemonics[index] : "NOP";
}

bool isJump(Op op) {
    switch (op) {
    case Op::Call:
    case Op::Jmp:
    case Op::Jz:
    case Op::Jnz:
    case Op::Jc:
    case Op::Jnc:
    case Op::Jn:
    case Op::Jns:
    case Op::Jlt:
    case Op::Jgt:
    case Op::Jle:
    case Op::Jge:
        return true;
    default:
        return false;
    }
}

bool isUnary(Op op) {
    switch (op) {
    case Op::Not:
    case Op::Inc:
    case Op::Dec:
    case Op::Neg:
    case Op::Push:
    case Op::Pop:
        return true;
    default:
        return false;
    }
}

void writeInstruction(const Instruction& instruction, std::uint32_t address,
                      std::vector<std::uint8_t>& out) {
    const auto word = [&out](std::uint32_t value) {
        out.push_back(static_cast<std::uint8_t>(value & 0xFF));
        out.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    };
    const std::uint16_t first = static_cast<std::uint16_t>(
        (static_cast<std::uint16_t>(instruction.op) << 10) |
        (static_cast<std::uint16_t>(instruction.a & 0x07) << 7) |
        (static_cast<std::uint16_t>(instruction.b & 0x07) << 4) |
        static_cast<std::uint16_t>(instruction.mode));
    word(first);
    switch (instruction.mode) {
    case Mode::Imm:
        word(instruction.operand & 0xFFFF);
        break;
    case Mode::Abs:
        word(instruction.operand & 0xFFFF);
        word((instruction.operand >> 16) & 0xFF);
        break;
    case Mode::Indexed:
        word(instruction.operand & 0xFF);
        break;
    case Mode::Rel: {
        const std::int32_t next = static_cast<std::int32_t>(address + static_cast<std::uint32_t>(instructionBytes(Mode::Rel)));
        const std::int32_t displacement = static_cast<std::int32_t>(instruction.operand) - next;
        word(static_cast<std::uint32_t>(displacement) & 0xFFFF);
        break;
    }
    case Mode::Reg:
    case Mode::RegInd:
        break;
    }
}

Instruction readInstruction(const Bus& bus, std::uint32_t address, std::uint32_t* next) {
    Instruction instruction;
    const std::uint16_t first = bus.read16(address);
    const auto opcode = static_cast<std::uint8_t>((first >> 10) & 0x3F);
    const auto mode = static_cast<Mode>(first & 0x0F);
    instruction.a = static_cast<std::uint8_t>((first >> 7) & 0x07);
    instruction.b = static_cast<std::uint8_t>((first >> 4) & 0x07);
    instruction.mode = mode;
    if (opcode >= static_cast<std::uint8_t>(Op::Count) || first == 0xFFFF ||
        static_cast<std::uint8_t>(mode) > static_cast<std::uint8_t>(Mode::Rel)) {
        instruction.valid = false;
    }
    instruction.op = instruction.valid ? static_cast<Op>(opcode) : Op::Nop;

    const std::uint32_t size = static_cast<std::uint32_t>(instructionBytes(mode));
    switch (mode) {
    case Mode::Imm:
        instruction.operand = bus.read16(address + 2);
        break;
    case Mode::Abs:
        instruction.operand = bus.read16(address + 2) |
                              (static_cast<std::uint32_t>(bus.read(address + 4)) << 16);
        break;
    case Mode::Indexed: {
        const auto raw = static_cast<std::int8_t>(bus.read(address + 2));
        instruction.operand = static_cast<std::uint32_t>(static_cast<std::int32_t>(raw));
        break;
    }
    case Mode::Rel: {
        const auto raw = static_cast<std::int16_t>(bus.read16(address + 2));
        instruction.operand = address + size + static_cast<std::uint32_t>(static_cast<std::int32_t>(raw));
        break;
    }
    case Mode::Reg:
    case Mode::RegInd:
        break;
    }
    if (next != nullptr) {
        *next = address + size;
    }
    return instruction;
}

std::string formatInstruction(const Instruction& instruction, std::uint32_t address) {
    if (!instruction.valid) {
        return "; invalid instruction at $" + hex(address, 6);
    }
    const std::string name{kMnemonics[static_cast<std::size_t>(instruction.op)]};
    if (instruction.op == Op::Nop || instruction.op == Op::Halt || instruction.op == Op::Ret) {
        return name;
    }
    if (instruction.op == Op::Int) {
        return name + " " + std::to_string(instruction.operand);
    }
    if (instruction.op == Op::In) {
        return name + " " + registerName(instruction.a) + ", $" + hex(instruction.operand, 6);
    }
    if (instruction.op == Op::Out) {
        return name + " $" + hex(instruction.operand, 6) + ", " + registerName(instruction.b);
    }
    if (isJump(instruction.op)) {
        return name + " $" + hex(instruction.operand, 6);
    }
    if (isUnary(instruction.op)) {
        return name + " " + registerName(instruction.b);
    }
    if (instruction.op == Op::Lea) {
        return name + " " + registerName(instruction.a) + ", $" + hex(instruction.operand, 6);
    }

    // A store names its pointer in `a` and its data in `b`; everything else names
    // its destination in `a` and its pointer -- when it has one -- in `b`.
    const bool is_store = instruction.op == Op::Store || instruction.op == Op::StoreB;
    const std::uint8_t pointer = is_store ? instruction.a : instruction.b;
    const std::string source = [&instruction, pointer]() -> std::string {
        switch (instruction.mode) {
        case Mode::Reg:
            return registerName(instruction.b);
        case Mode::Imm:
            return "#" + std::to_string(instruction.operand);
        case Mode::Abs:
            return "[$" + hex(instruction.operand, 6) + "]";
        case Mode::RegInd:
            return "[" + registerName(pointer) + "]";
        case Mode::Indexed: {
            const auto displacement = static_cast<std::int32_t>(instruction.operand);
            const std::string sign = displacement < 0 ? "-" : "+";
            return "[" + registerName(pointer) + sign +
                   std::to_string(displacement < 0 ? -displacement : displacement) + "]";
        }
        case Mode::Rel:
            return "$" + hex(instruction.operand, 6);
        }
        return {};
    }();
    if (is_store) {
        return name + " " + source + ", " + registerName(instruction.b);
    }
    return name + " " + registerName(instruction.a) + ", " + source;
}

// ---------------------------------------------------------------------------
// The assembler
// ---------------------------------------------------------------------------

Assembly assemble(std::string_view source, std::uint32_t defaultOrigin) {
    Assembly result;
    result.origin = defaultOrigin;

    std::map<std::string, std::uint32_t> symbols;
    // EQU symbols are resolved in a first sweep, because a listing may use one above
    // the line that defines it and a constant is not an address.
    std::vector<std::pair<std::string, std::string>> equates;
    std::istringstream stream{std::string{source}};
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(stream, line)) {
        lines.push_back(line);
        const ParsedLine parsed = parseLine(line);
        if (parsed.mnemonic == "EQU" && !parsed.operands.empty()) {
            // `EQU NAME, value` and `NAME EQU value` are the same request.
            equates.emplace_back(parsed.label, parsed.operands.back());
        } else if (parsed.mnemonic == "EQU" && parsed.operands.empty()) {
            result.errors.push_back("EQU needs a name and a value");
        }
    }
    for (const auto& [name, expression] : equates) {
        if (name.empty()) {
            continue;
        }
        Expr expr{expression, symbols, true};
        const std::optional<std::int64_t> value = expr.value();
        if (value) {
            symbols[name] = static_cast<std::uint32_t>(*value);
        }
    }

    // Pass one: addresses. Instruction sizes depend on the mode, and the mode on the
    // syntax, never on a value -- which is what lets a label be used before it is
    // defined.
    std::uint32_t address = defaultOrigin;
    for (std::size_t index = 0; index < lines.size(); ++index) {
        const ParsedLine parsed = parseLine(lines[index]);
        if (!parsed.label.empty() && parsed.mnemonic != "EQU") {
            if (symbols.count(parsed.label) != 0) {
                result.errors.push_back("duplicate label '" + parsed.label + "'");
            }
            symbols[parsed.label] = address;
        }
        if (parsed.mnemonic.empty()) {
            continue;
        }
        if (parsed.mnemonic == "ORG") {
            Expr expr{parsed.operands.empty() ? "" : parsed.operands[0], symbols, false};
            const std::optional<std::int64_t> value = expr.value();
            if (!value) {
                result.errors.push_back("ORG needs an address");
            } else {
                address = static_cast<std::uint32_t>(*value);
                if (result.bytes.empty()) {
                    result.origin = address;
                }
            }
            continue;
        }
        if (parsed.mnemonic == "DB" || parsed.mnemonic == "DW") {
            for (const std::string& operand : parsed.operands) {
                if (parsed.mnemonic == "DB" && operand.size() >= 2 && operand.front() == '"' &&
                    operand.back() == '"') {
                    address += static_cast<std::uint32_t>(operand.size() - 2);
                } else {
                    address += parsed.mnemonic == "DB" ? 1U : 2U;
                }
            }
            continue;
        }
        if (parsed.mnemonic == "DS") {
            Expr expr{parsed.operands.empty() ? "" : parsed.operands[0], symbols, false};
            const std::optional<std::int64_t> value = expr.value();
            address += static_cast<std::uint32_t>(value.value_or(0));
            continue;
        }
        if (parsed.mnemonic == "EQU") {
            continue;
        }
        // An instruction. Its size is a function of its operands' syntax.
        Instruction probe;
        std::string error;
        if (!encodeInstruction(parsed, symbols, false, &probe, &error)) {
            result.errors.push_back("line " + std::to_string(index + 1) + ": " + error);
            continue;
        }
        address += static_cast<std::uint32_t>(instructionBytes(probe.mode));
    }

    // Pass two: bytes.
    address = result.origin;
    std::vector<std::uint8_t> out;
    const auto padTo = [&out](std::uint32_t target) {
        while (out.size() < target) {
            out.push_back(0);
        }
    };
    for (std::size_t index = 0; index < lines.size(); ++index) {
        const ParsedLine parsed = parseLine(lines[index]);
        if (parsed.mnemonic.empty() || parsed.mnemonic == "EQU") {
            continue;
        }
        if (parsed.mnemonic == "ORG") {
            Expr expr{parsed.operands.empty() ? "" : parsed.operands[0], symbols, true};
            const std::optional<std::int64_t> value = expr.value();
            if (value) {
                address = static_cast<std::uint32_t>(*value);
                padTo(address - result.origin);
            }
            continue;
        }
        if (parsed.mnemonic == "DB" || parsed.mnemonic == "DW") {
            for (const std::string& operand : parsed.operands) {
                if (parsed.mnemonic == "DB" && operand.size() >= 2 && operand.front() == '"' &&
                    operand.back() == '"') {
                    // A string is the bytes it spells: `DB "HB-2", 0` is how a
                    // listing holds a message without a character a comma.
                    for (std::size_t at = 1; at + 1 < operand.size(); ++at) {
                        out.push_back(static_cast<std::uint8_t>(operand[at]));
                    }
                    continue;
                }
                Expr expr{operand, symbols, true};
                const std::optional<std::int64_t> value = expr.value();
                if (!value) {
                    result.errors.push_back("cannot evaluate '" + operand + "'");
                    continue;
                }
                if (parsed.mnemonic == "DB") {
                    out.push_back(static_cast<std::uint8_t>(*value & 0xFF));
                } else {
                    out.push_back(static_cast<std::uint8_t>(*value & 0xFF));
                    out.push_back(static_cast<std::uint8_t>((*value >> 8) & 0xFF));
                }
            }
            continue;
        }
        if (parsed.mnemonic == "DS") {
            Expr expr{parsed.operands.empty() ? "" : parsed.operands[0], symbols, true};
            const std::optional<std::int64_t> value = expr.value();
            for (std::int64_t pad = 0; pad < value.value_or(0); ++pad) {
                out.push_back(0);
            }
            continue;
        }
        Instruction instruction;
        std::string error;
        if (!encodeInstruction(parsed, symbols, true, &instruction, &error)) {
            result.errors.push_back("line " + std::to_string(index + 1) + ": " + error);
            continue;
        }
        writeInstruction(instruction, static_cast<std::uint32_t>(result.origin + out.size()), out);
    }

    result.bytes = std::move(out);
    result.ok = result.errors.empty();
    return result;
}

}  // namespace timewarp::hb2
