// SPDX-License-Identifier: MIT
//
// The machine around the program: ports, sound and keys, `WAIT`, `SPEED`, the timer,
// the features reached by name, and the reset that puts state back.

#include "basic_internal.hpp"
#include "basic_statements_internal.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstddef>
#include <filesystem>
#include <optional>
#include <regex>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/screen_mode.hpp"

namespace timewarp::lang::basic {

namespace {

/// The text of one statement argument: `"text"`, a string variable, or an
/// expression -- evaluated with the string evaluator, exactly as `PUT #` does.
[[nodiscard]] std::string argument_text(Interpreter& interpreter, const State& state,
                                        const std::string& field) {
    const std::string trimmed = trim(field);
    if (trimmed.size() >= 2 && trimmed.front() == '"' && trimmed.back() == '"') {
        return trimmed.substr(1, trimmed.size() - 2);
    }
    try {
        return Strings{interpreter, state}.evaluate(trimmed);
    } catch (const BasicError&) {
        return trimmed;
    }
}

/// The `#channel` of a port statement, or -1.
[[nodiscard]] int channel_of(const std::string& field) {
    std::string text = trim(field);
    if (!text.empty() && text.front() == '#') {
        text.erase(0, 1);
    }
    const std::optional<long> value = parse_integer(text);
    return value.has_value() ? static_cast<int>(*value) : -1;
}

}  // namespace

std::string Machine::serial(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.empty()) {
        return "❌ SERIAL needs OPEN, READ, WRITE or CLOSE\n";
    }
    const std::string first = trim(parts[0]);
    const std::string action = upper(first.substr(0, first.find(' ')));
    const std::string first_rest = first.size() > action.size() ? trim(first.substr(action.size()))
                                                                 : std::string{};

    if (action == "OPEN") {
        const int channel = channel_of(first_rest);
        if (channel < 0 || parts.size() < 3) {
            return "❌ SERIAL OPEN needs #channel, \"path\", baud\n";
        }
        const std::string path = argument_text(interpreter_, state_, parts[1]);
        const std::optional<long> baud = parse_integer(parts[2]);
        if (!baud.has_value()) {
            return "❌ SERIAL OPEN needs a baud rate\n";
        }
        std::string error;
        auto& port = state_.serial_ports[channel];
        if (!port) {
            port = std::make_shared<timewarp::io::SerialPort>();
        }
        if (!port->open(path, static_cast<int>(*baud), &error)) {
            state_.serial_ports.erase(channel);
            return "❌ " + error + "\n";
        }
        return "📡 SERIAL #" + std::to_string(channel) + " open: " + path + " at " +
               std::to_string(*baud) + " baud\n";
    }

    if (action == "CLOSE") {
        const int channel = channel_of(first_rest);
        const auto found = state_.serial_ports.find(channel);
        if (found == state_.serial_ports.end() || !found->second->is_open()) {
            return "❌ SERIAL #" + std::to_string(channel) + " is not open\n";
        }
        found->second->close();
        state_.serial_ports.erase(found);
        return "📡 SERIAL #" + std::to_string(channel) + " closed\n";
    }

    const int channel = action == "READ" || action == "WRITE" ? channel_of(first_rest) : -1;
    if (channel < 0) {
        return "❌ SERIAL " + action + " needs #channel\n";
    }
    const auto found = state_.serial_ports.find(channel);
    if (found == state_.serial_ports.end() || !found->second->is_open()) {
        return "❌ SERIAL #" + std::to_string(channel) + " is not open\n";
    }

    if (action == "WRITE") {
        std::string payload;
        for (std::size_t index = 1; index < parts.size(); ++index) {
            payload += argument_text(interpreter_, state_, parts[index]);
        }
        payload += "\n";
        std::string error;
        if (!found->second->write(payload, &error)) {
            return "❌ " + error + "\n";
        }
        return "📝 SERIAL #" + std::to_string(channel) + " wrote " +
               std::to_string(payload.size()) + " bytes\n";
    }

    // READ: a variable to put the line in, and an optional timeout in ms.
    if (parts.size() < 2) {
        return "❌ SERIAL READ needs #channel, variable$\n";
    }
    int timeout = 1000;
    if (parts.size() >= 3) {
        if (const std::optional<long> ms = parse_integer(parts[2]); ms.has_value()) {
            timeout = static_cast<int>(std::clamp<long>(*ms, 0, 60000));
        }
    }
    std::string error;
    const std::string line = found->second->read_line(timeout, &error);
    if (!error.empty()) {
        return "❌ " + error + "\n";
    }
    std::string target = trim(parts[1]);
    if (!target.empty() && target.back() != '$') {
        target += '$';
    }
    assign(target, line);
    return "📡 SERIAL #" + std::to_string(channel) + " read " +
           std::to_string(line.size()) + " chars\n";
}

std::string Machine::parallel(const std::string& args) {
    const std::vector<std::string> parts = split_trimmed(args, ',');
    if (parts.empty()) {
        return "❌ PARALLEL needs OPEN, WRITE, BYTE or CLOSE\n";
    }
    const std::string first = trim(parts[0]);
    const std::string action = upper(first.substr(0, first.find(' ')));
    const std::string first_rest =
        first.size() > action.size() ? trim(first.substr(action.size())) : std::string{};

    if (action == "OPEN") {
        const int channel = channel_of(first_rest);
        if (channel < 0 || parts.size() < 2) {
            return "❌ PARALLEL OPEN needs #channel, \"path\"\n";
        }
        const std::string path = argument_text(interpreter_, state_, parts[1]);
        std::string error;
        auto& port = state_.parallel_ports[channel];
        if (!port) {
            port = std::make_shared<timewarp::io::ParallelPort>();
        }
        if (!port->open(path, &error)) {
            state_.parallel_ports.erase(channel);
            return "❌ " + error + "\n";
        }
        return "🖨️ PARALLEL #" + std::to_string(channel) + " open: " + path + "\n";
    }

    if (action == "CLOSE") {
        const int channel = channel_of(first_rest);
        const auto found = state_.parallel_ports.find(channel);
        if (found == state_.parallel_ports.end() || !found->second->is_open()) {
            return "❌ PARALLEL #" + std::to_string(channel) + " is not open\n";
        }
        found->second->close();
        state_.parallel_ports.erase(found);
        return "🖨️ PARALLEL #" + std::to_string(channel) + " closed\n";
    }

    const int channel = channel_of(first_rest);
    const auto found = state_.parallel_ports.find(channel);
    if (found == state_.parallel_ports.end() || !found->second->is_open()) {
        return "❌ PARALLEL #" + std::to_string(channel) + " is not open\n";
    }

    if (action == "WRITE") {
        std::string payload;
        for (std::size_t index = 1; index < parts.size(); ++index) {
            payload += argument_text(interpreter_, state_, parts[index]);
        }
        std::string error;
        if (!found->second->write(payload, &error)) {
            return "❌ " + error + "\n";
        }
        return "🖨️ PARALLEL #" + std::to_string(channel) + " wrote " +
               std::to_string(payload.size()) + " bytes\n";
    }

    if (action == "BYTE") {
        if (parts.size() < 2) {
            return "❌ PARALLEL BYTE needs #channel, value\n";
        }
        try {
            const auto value = static_cast<long>(eval(parts[1])) & 0xFF;
            std::string error;
            if (!found->second->write_byte(static_cast<unsigned char>(value), &error)) {
                return "❌ " + error + "\n";
            }
            return "🖨️ PARALLEL #" + std::to_string(channel) + " <- " +
                   std::to_string(value) + "\n";
        } catch (const std::exception& error) {
            return "❌ PARALLEL BYTE: " + std::string{error.what()} + "\n";
        }
    }

    return "❌ PARALLEL " + action + " is not a command (OPEN, WRITE, BYTE, CLOSE)\n";
}


std::string Machine::speed(const std::string& args) {
    const std::optional<long> value = parse_integer(args);
    if (!value.has_value()) {
        return "❌ SPEED requires numeric value (0-10)\n";
    }
    const long clamped = std::max(0L, std::min(10L, *value));
    return "🐢 Turtle speed set to " + std::to_string(clamped) + "\n";
}


std::string Machine::on_timer(const std::string& args) {
    static const std::regex pattern{R"(\((\d+(?:\.\d+)?)\)\s*GOSUB\s*(\d+))",
                                    std::regex::ECMAScript | std::regex::icase};
    std::smatch match;
    const std::string text = trim(args);
    if (!std::regex_search(text, match, pattern)) {
        return "❌ ON TIMER requires format: ON TIMER(n) GOSUB line\n";
    }
    const double interval = std::stod(match[1].str());
    const long target = std::stol(match[2].str());
    if (interval <= 0.0) {
        return "❌ ON TIMER needs a positive interval, got " + printed_number(interval) +
               "\n";
    }
    state_.timer_defined = true;
    state_.timer_interval = interval;
    state_.timer_target = target;
    return "⏱️ Timer set: every " + printed_number(interval) + "s GOSUB " +
           std::to_string(target) + "\n";
}

double Machine::now_seconds() {
    using clock = std::chrono::steady_clock;
    static const clock::time_point origin = clock::now();
    return std::chrono::duration<double>(clock::now() - origin).count();
}

void Machine::service_timer() {
    if (!state_.timer_enabled || !state_.timer_defined) {
        return;
    }
    // A handler that took longer than the interval must not be entered again from
    // inside itself: the machine would be recursively interrupting itself, which no
    // BASIC ever did. The handler is over when its `GOSUB` frame has been returned.
    if (timer_running) {
        if (state_.gosubs.size() >= timer_depth) {
            return;
        }
        timer_running = false;
    }
    const double now = now_seconds();
    if (now < state_.timer_next_due) {
        return;
    }
    const auto found =
        interpreter_.line_number_map.find(static_cast<int>(state_.timer_target));
    if (found == interpreter_.line_number_map.end()) {
        // A target that is not in the program cannot be a handler; switching the timer
        // off says so once instead of failing every statement.
        state_.timer_enabled = false;
        return;
    }
    // The next event is counted from now, not from the missed one: a program that
    // paused in `INPUT` should not be hit by a burst of catch-up interrupts.
    state_.timer_next_due = now + state_.timer_interval;
    state_.gosubs.push_back(interpreter_.current_line);
    timer_depth = state_.gosubs.size();
    timer_running = true;
    interpreter_.jump_to(found->second);
}

std::string Machine::play(const std::string& args) { return play_music(*this, args); }


std::string Machine::say(const std::string& args) { return speak(*this, args); }

std::string Machine::shape(const std::string& args) { return draw_shape(*this, args); }

std::string Machine::particle(const std::string& args) {
    return create_particle(*this, args);
}

std::string Machine::fractal(const std::string& args) { return draw_fractal(*this, args); }

std::string Machine::poke(const std::string& args) {
    if (args.find(',') == std::string::npos) {
        return "❌ POKE requires: POKE address, value\n";
    }
    const std::size_t comma = args.find(',');
    const std::string address_text = trim(args.substr(0, comma));
    const std::string value_text = trim(args.substr(comma + 1));
    // The arguments are *expressions*, as they are on every machine that had a POKE:
    // `POKE 32768 + 4 * 80 + 5, 65` is how a program writes a cell it computed. They
    // were bare literals until 20.1, which made a computed address an error.
    try {
        const auto address = static_cast<long>(eval(address_text));
        const auto value = static_cast<long>(eval(value_text));
        if (address < 0 || address > 65535) {
            return "❌ POKE address must be 0-65535\n";
        }
        if (value < 0 || value > 255) {
            return "❌ POKE value must be 0-255\n";
        }
        if (interpreter_.machine_attached()) {
            // A machine has real memory, and a POKE into a cell draws. Refusing a
            // write into ROM is the one case a program has to be told about: the
            // byte cannot land and pretending it did would be the silent failure
            // this project treats as the bug.
            if (!interpreter_.machine_poke(static_cast<int>(address),
                                           static_cast<std::uint8_t>(value))) {
                return "❌ POKE " + std::to_string(address) + " is ROM: it cannot be written\n";
            }
            return {};
        }
        state_.memory[static_cast<int>(address)] = static_cast<int>(value);
    } catch (const std::exception& error) {
        return "❌ POKE error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::peek(const std::string& args) {
    // An expression, for the same reason `POKE`'s address is one: a program reads the
    // cell it computed.
    try {
        const auto address = static_cast<long>(eval(trim(args)));
        if (address < 0 || address > 65535) {
            return "❌ PEEK address must be 0-65535: got " + std::to_string(address) + "\n";
        }
        const int value = interpreter_.machine_attached()
                              ? static_cast<int>(
                                    interpreter_.machine_peek(static_cast<int>(address)))
                              : [this, address] {
                                    const auto found =
                                        state_.memory.find(static_cast<int>(address));
                                    return found == state_.memory.end() ? 0 : found->second;
                                }();
        return "ℹ️ PEEK(" + std::to_string(address) + ") = " + std::to_string(value) + "\n";
    } catch (const std::exception& error) {
        return "❌ PEEK error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::out(const std::string& args) {
    if (args.find(',') == std::string::npos) {
        return "❌ OUT requires: OUT port, value\n";
    }
    const std::size_t comma = args.find(',');
    const std::string port_text = trim(args.substr(0, comma));
    const std::string value_text = trim(args.substr(comma + 1));
    double port_number = 0.0;
    double value_number = 0.0;
    if (!parse_float(port_text, port_number)) {
        return "❌ Expected numeric value for port number, got '" + port_text + "'\n";
    }
    if (!parse_float(value_text, value_number)) {
        return "❌ Expected numeric value for port value, got '" + value_text + "'\n";
    }
    try {
        const auto port = static_cast<long>(eval(port_text));
        const auto value = static_cast<long>(eval(value_text));
        if (port < 0 || port > 65535) {
            return "❌ OUT port must be 0-65535\n";
        }
        if (value < 0 || value > 255) {
            return "❌ OUT value must be 0-255\n";
        }
        state_.ports[static_cast<int>(port)] = static_cast<int>(value);
    } catch (const std::exception& error) {
        return "❌ OUT error: " + std::string{error.what()} + "\n";
    }
    return {};
}

std::string Machine::in_port(const std::string& args) {
    double port_number = 0.0;
    if (!parse_float(trim(args), port_number)) {
        return "❌ Expected numeric value for port number, got '" + trim(args) + "'\n";
    }
    try {
        const auto port = static_cast<long>(eval(trim(args)));
        if (port < 0 || port > 65535) {
            return "❌ IN port must be 0-65535: got " + std::to_string(port) + "\n";
        }
        const auto found = state_.ports.find(static_cast<int>(port));
        const int value = found == state_.ports.end() ? 0 : found->second;
        return "ℹ️ IN(" + std::to_string(port) + ") = " + std::to_string(value) + "\n";
    } catch (const std::exception& error) {
        return "❌ IN error: " + std::string{error.what()} + "\n";
    }
}

std::string Machine::shell(const std::string& args) {
    std::string command = trim(args);
    if (command.empty()) {
        return "❌ SHELL requires command string\n";
    }
    if (command.size() >= 2 && command.front() == '"' && command.back() == '"') {
        command = command.substr(1, command.size() - 2);
    }
    if (command.empty()) {
        return "❌ SHELL requires a command string\n";
    }
    // The reference logs the attempt and refuses to run it; the port refuses
    // without a log sink, and says the same thing.
    return "ℹ️ SHELL commands are disabled in IDE for security. Command: " + command + "\n";
}

std::string Machine::make(const std::string& args) {
    const std::string text = trim(args);
    if (text.empty() || text.front() != '"') {
        return "❌ MAKE expects MAKE \"name value\n";
    }
    const std::size_t closing = text.find('"', 1);
    if (closing == std::string::npos) {
        return "❌ MAKE expects MAKE \"name value\n";
    }
    const std::string name = upper(text.substr(1, closing - 1));
    const std::string value_text = trim(text.substr(closing + 1));
    try {
        assign(name, eval(value_text));
    } catch (const std::exception&) {
        assign(name, strip_quotes(value_text));
    }
    return {};
}

std::string Machine::reset_program(std::string_view message) {
    interpreter_.variables().clear();
    interpreter_.arrays.clear();
    interpreter_.function_hook = nullptr;
    state_.defaults.clear();
    state_.fors.clear();
    state_.gosubs.clear();
    state_.ifs.clear();
    state_.whiles.clear();
    state_.dos.clear();
    state_.calls.clear();
    state_.string_arrays.clear();
    state_.types.clear();
    state_.records.clear();
    state_.subs.clear();
    state_.functions.clear();
    state_.user_fns.clear();
    state_.data.clear();
    state_.data_pointer = 0;
    state_.scanned = false;
    state_.select_expression.clear();
    state_.in_select = false;
    state_.memory.clear();
    state_.ports.clear();
    state_.palette.clear();
    state_.files.clear();
    state_.error_handler = 0;
    state_.error_index = -1;
    state_.timer_defined = false;
    state_.timer_enabled = false;
    state_.screen_mode = 0;
    state_.cols = 80;
    state_.rows = 25;
    state_.screen_width = 800;
    state_.screen_height = 600;
    // `RUN` is a new run, so the screen it could read back goes with the rest of
    // the state: a program that starts with `POINT` must not see the picture its
    // previous run left, and `STICK` must answer 0 until the new run's own
    // `JOYINIT` -- the two facts `SCREEN 0` and the missing `JOYINIT` mean.
    state_.raster = Raster{};
    state_.joystick_initialised = false;
    functions_installed_ = false;
    // The reference clears the program itself, so the run ends here; moving the
    // counter past the last line is the same observable end.
    interpreter_.jump_to(interpreter_.program_lines().size());
    return std::string{message};
}

}  // namespace timewarp::lang::basic

namespace timewarp::lang {

std::string execute_basic(Interpreter& interpreter, std::string_view command,
                          TurtleHost& turtle) {
    basic::State& state = interpreter.executor_state<basic::State>();
    basic::Machine machine{interpreter, turtle, state};
    try {
        return machine.execute(command);
    } catch (const basic::BasicError& error) {
        return "❌ " + std::string{error.what()} + "\n";
    }
}

}  // namespace timewarp::lang
