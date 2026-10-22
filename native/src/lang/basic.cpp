// SPDX-License-Identifier: MIT
//
// BASIC's core: the text helpers the dispatch shares, variables, the load-time
// scan for DATA and SUB/FUNCTION blocks, and the drawing helpers.
//
// `basic_statements.cpp` holds the dispatch itself, because the reference's
// statement list is long enough that one file for both would be a file nobody
// can search.

#include "basic_internal.hpp"

#include "timewarp/io/gamepad.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <set>
#include <span>
#include <string>
#include <utility>

namespace timewarp::lang::basic {
namespace {

/// The local wall clock, as `time.localtime()` gives it to the reference.
std::tm local_now() {
    const std::time_t now = std::time(nullptr);
    std::tm parts{};
#ifdef _WIN32
    localtime_s(&parts, &now);
#else
    localtime_r(&now, &parts);
#endif
    return parts;
}

/// -1 when the condition is not a string comparison, else its truth.
Rgb clamp_colour(double red, double green, double blue) {
    const auto clamp = [](double value) {
        if (value <= 0.0) {
            return 0;
        }
        if (value >= 255.0) {
            return 255;
        }
        return static_cast<int>(value);
    };
    return Rgb{clamp(red), clamp(green), clamp(blue)};
}

Rgb default_colour(int index) {
    switch (index) {
        case 0:
            return Rgb{0, 0, 0};
        case 1:
            return Rgb{0, 0, 170};
        case 2:
            return Rgb{0, 170, 0};
        case 3:
            return Rgb{0, 170, 170};
        case 4:
            return Rgb{170, 0, 0};
        case 5:
            return Rgb{170, 0, 170};
        case 6:
            return Rgb{170, 85, 0};
        case 7:
            return Rgb{170, 170, 170};
        case 8:
            return Rgb{85, 85, 85};
        case 9:
            return Rgb{85, 85, 255};
        case 10:
            return Rgb{85, 255, 85};
        case 11:
            return Rgb{85, 255, 255};
        case 12:
            return Rgb{255, 85, 85};
        case 13:
            return Rgb{255, 85, 255};
        case 14:
            return Rgb{255, 255, 85};
        case 15:
            return Rgb{255, 255, 255};
        default:
            return Rgb{-1, -1, -1};
    }
}

}  // namespace

std::string clock_time_text() {
    const std::tm now = local_now();
    char buffer[16];
    std::snprintf(buffer, sizeof buffer, "%02d:%02d:%02d", now.tm_hour, now.tm_min,
                  now.tm_sec);
    return buffer;
}

std::string clock_date_text() {
    const std::tm now = local_now();
    char buffer[32];
    std::snprintf(buffer, sizeof buffer, "%02d-%02d-%04d", now.tm_mon + 1, now.tm_mday,
                  now.tm_year + 1900);
    return buffer;
}

double clock_timer_seconds() {
    const std::tm now = local_now();
    const auto instant = std::chrono::system_clock::now();
    const double slice =
        std::chrono::duration<double>(instant.time_since_epoch()).count();
    const double fraction = slice - std::floor(slice);
    return static_cast<double>(now.tm_hour) * 3600.0 +
           static_cast<double>(now.tm_min) * 60.0 + static_cast<double>(now.tm_sec) +
           fraction;
}

std::vector<std::string> split_plain(std::string_view text, char separator) {
    std::vector<std::string> parts;
    std::string current;
    for (const char character : text) {
        if (character == separator) {
            parts.push_back(current);
            current.clear();
        } else {
            current.push_back(character);
        }
    }
    parts.push_back(current);
    return parts;
}

FileHandle::~FileHandle() {
    if (handle != nullptr) {
        std::fclose(handle);
    }
}

// -- construction ----------------------------------------------------------

Machine::Machine(Interpreter& interpreter, TurtleHost& turtle, State& state)
    : interpreter_{interpreter}, turtle_{turtle}, state_{state} {}

void Machine::install_functions() {
    if (functions_installed_) {
        return;
    }
    functions_installed_ = true;
    interpreter_.function_hook = [this](std::string_view name) { return user_function(name); };
}

std::optional<expr::UserFunction> Machine::user_function(std::string_view raw_name) {
    const std::string name = upper(raw_name);
    if (name == "PEEK") {
        return expr::UserFunction{1, [this](std::span<const double> arguments) {
                                      const int address = static_cast<int>(arguments[0]);
                                      const auto found = state_.memory.find(address);
                                      return found == state_.memory.end()
                                                 ? 0.0
                                                 : static_cast<double>(found->second);
                                  }};
    }
    if (name == "IN") {
        return expr::UserFunction{1, [this](std::span<const double> arguments) {
                                      const int port = static_cast<int>(arguments[0]);
                                      const auto found = state_.ports.find(port);
                                      return found == state_.ports.end()
                                                 ? 0.0
                                                 : static_cast<double>(found->second);
                                  }};
    }
    if (name == "STICK" || name == "STRIG") {
        // A gamepad answers only after `JOYINIT` found one. Before that -- and
        // on a machine with no joystick, or no backend at all -- these are 0,
        // which is the answer the reference gives and the answer a program
        // written for a machine without one already expects.
        const bool stick = name == "STICK";
        return expr::UserFunction{1, [this, stick](std::span<const double> arguments) {
                                      if (!state_.joystick_initialised) {
                                          return 0.0;
                                      }
                                      auto& pad = timewarp::io::Gamepad::instance();
                                      const int index = static_cast<int>(arguments[0]);
                                      return stick
                                                 ? static_cast<double>(pad.axis(index))
                                                 : (pad.button(index) ? 1.0 : 0.0);
                                  }};
    }
    if (name == "POINT") {
        // What is on the screen at a point, as `&H00RRGGBB`. -1 outside a
        // graphics screen, so `IF POINT(X,Y) = -1` can ask whether there is one.
        return expr::UserFunction{2, [this](std::span<const double> arguments) {
                                      if (!state_.raster.active()) {
                                          return -1.0;
                                      }
                                      const auto x = static_cast<int>(std::lround(arguments[0]));
                                      const auto y = static_cast<int>(std::lround(arguments[1]));
                                      return static_cast<double>(
                                          pack_colour(state_.raster.at(x, y)));
                                  }};
    }
    if (name == "EOF") {
        return expr::UserFunction{1, [this](std::span<const double> arguments) {
                                      const int channel = static_cast<int>(arguments[0]);
                                      const auto found = state_.files.find(channel);
                                      if (found == state_.files.end() ||
                                          found->second->handle == nullptr) {
                                          return -1.0;
                                      }
                                      const long position = std::ftell(found->second->handle);
                                      const int next = std::fgetc(found->second->handle);
                                      std::fseek(found->second->handle, position, SEEK_SET);
                                      return next == EOF ? -1.0 : 0.0;
                                  }};
    }
    const auto definition = state_.user_fns.find(name);
    if (definition == state_.user_fns.end()) {
        return std::nullopt;
    }
    const std::vector<std::string> parameters = definition->second.first;
    const std::string body = definition->second.second;
    expr::UserFunction function;
    function.arity = parameters.size();
    function.call = [this, parameters, body](std::span<const double> arguments) {
        std::vector<std::pair<std::string, std::optional<Value>>> saved;
        for (std::size_t index = 0; index < parameters.size(); ++index) {
            const std::string& parameter = parameters[index];
            const auto existing = interpreter_.variables().find(parameter);
            saved.emplace_back(parameter, existing == interpreter_.variables().end()
                                               ? std::nullopt
                                               : std::optional<Value>{existing->second});
            interpreter_.set_number(parameter,
                                    index < arguments.size() ? arguments[index] : 0.0);
        }
        const auto restore = [this, &saved]() {
            for (const auto& [parameter, value] : saved) {
                if (value.has_value()) {
                    interpreter_.variables()[parameter] = *value;
                } else {
                    interpreter_.remove_variable(parameter);
                }
            }
        };
        double result = 0.0;
        try {
            result = interpreter_.evaluate_expression(body);
        } catch (...) {
            restore();
            throw;
        }
        restore();
        return result;
    };
    return function;
}

// -- variables -------------------------------------------------------------

std::pair<std::string, std::string> Machine::resolve(std::string_view raw) const {
    const std::string name = upper(trim(raw));
    if (name.empty()) {
        return {"", "double"};
    }
    const char last = name.back();
    if (last == '$') {
        return {name.substr(0, name.size() - 1), "string"};
    }
    if (last == '%') {
        return {name.substr(0, name.size() - 1), "int"};
    }
    if (last == '&') {
        return {name.substr(0, name.size() - 1), "long"};
    }
    if (last == '!') {
        return {name.substr(0, name.size() - 1), "single"};
    }
    if (last == '#') {
        return {name.substr(0, name.size() - 1), "double"};
    }
    const auto found = state_.defaults.find(name.front());
    return {name, found == state_.defaults.end() ? "double" : found->second};
}

void Machine::validate_name(std::string_view raw) const {
    const std::string name = trim(raw);
    if (name.empty()) {
        throw BasicError("Variable name cannot be empty");
    }
    static const std::set<std::string> reserved{
        "FOR",   "NEXT",  "WHILE", "WEND", "IF",     "THEN",   "ELSE",
        "SELECT", "CASE", "END",   "SUB",  "FUNCTION", "RETURN", "GOSUB",
        "GOTO",  "PRINT", "INPUT", "READ", "DATA",   "DIM",    "LET",
        "REM",   "LOCATE", "CLS",  "STOP"};
    if (reserved.count(upper(name)) != 0) {
        throw BasicError("'" + name +
                         "' is a reserved keyword and cannot be used as variable name");
    }
    if (!(std::isalpha(static_cast<unsigned char>(name[0])) != 0 || name[0] == '_')) {
        throw BasicError("Variable name must start with letter or underscore, got '" + name +
                         "'");
    }
    static const std::string suffixes{"%$!#"};
    for (std::size_t index = 0; index < name.size(); ++index) {
        const char character = name[index];
        const bool allowed =
            std::isalnum(static_cast<unsigned char>(character)) != 0 || character == '_' ||
            (index == name.size() - 1 && suffixes.find(character) != std::string::npos);
        if (!allowed) {
            throw BasicError("Invalid character '" + std::string(1, character) +
                             "' in variable name '" + name + "'");
        }
    }
}

void Machine::assign(std::string_view raw, const Value& value) {
    const auto [base, type] = resolve(raw);
    if (base.empty()) {
        return;
    }
    if (type == "string") {
        if (value.is_text()) {
            interpreter_.set_text(base + "$", value.text());
        } else if (value.is_number()) {
            interpreter_.set_text(base + "$", python_number_text(value.number()));
        } else {
            interpreter_.set_text(base + "$", "");
        }
        return;
    }
    double number = 0.0;
    if (value.is_number()) {
        number = value.number();
    } else if (value.is_text()) {
        if (!parse_float(value.text(), number)) {
            number = 0.0;
        }
    }
    if (type == "int" || type == "long") {
        number = std::trunc(number);
    }
    interpreter_.set_number(base, number);
}

void Machine::assign(std::string_view raw, double value) { assign(raw, Value{value}); }

void Machine::assign(std::string_view raw, const std::string& value) {
    assign(raw, Value{value});
}

std::optional<double> Machine::number_of(const std::string& base) const {
    const auto found = interpreter_.variables().find(base);
    if (found == interpreter_.variables().end() || !found->second.is_number()) {
        return std::nullopt;
    }
    return found->second.number();
}

std::string Machine::text_of(std::string_view raw) const {
    const std::string key = upper(trim(raw));
    const auto found = interpreter_.variables().find(key);
    if (found != interpreter_.variables().end() && found->second.is_text()) {
        return found->second.text();
    }
    return {};
}

double Machine::eval(std::string_view expression) {
    return interpreter_.evaluate_expression(expand_records(*this, expression));
}

bool Machine::divert_on_error() {
    if (state_.error_handler == 0) {
        return false;
    }
    const auto handler = interpreter_.line_number_map.find(state_.error_handler);
    if (handler == interpreter_.line_number_map.end()) {
        return false;
    }
    state_.error_index = static_cast<long>(interpreter_.current_line);
    double erl = static_cast<double>(interpreter_.current_line + 1);
    const auto& lines = interpreter_.program_lines();
    if (interpreter_.current_line < lines.size() &&
        lines[interpreter_.current_line].number.has_value()) {
        erl = static_cast<double>(*lines[interpreter_.current_line].number);
    }
    interpreter_.set_number("ERR", 1.0);
    interpreter_.set_number("ERL", erl);
    interpreter_.jump_to(handler->second);
    return true;
}

// -- program scan ----------------------------------------------------------

void Machine::scan_program() {
    if (state_.scanned) {
        return;
    }
    state_.scanned = true;
    const auto& lines = interpreter_.program_lines();
    for (std::size_t index = 0; index < lines.size(); ++index) {
        const std::string text = trim(lines[index].text);
        const std::string code = upper(text);
        if (code.rfind("DATA ", 0) == 0) {
            for (const std::string& field : split_plain(text.substr(5), ',')) {
                std::string value = trim(field);
                if (value.size() >= 2 && value.front() == '"' && value.back() == '"') {
                    value = value.substr(1, value.size() - 2);
                }
                state_.data.push_back(value);
            }
            continue;
        }
        const bool is_sub = code.rfind("SUB ", 0) == 0;
        const bool is_function = code.rfind("FUNCTION ", 0) == 0;
        if (!is_sub && !is_function) {
            continue;
        }
        const std::string header = trim(text.substr(is_sub ? 4 : 9));
        Definition definition;
        const std::size_t open = header.find('(');
        std::string name;
        if (open != std::string::npos) {
            name = upper(trim(header.substr(0, open)));
            std::string parameters = header.substr(open + 1);
            if (!parameters.empty() && parameters.back() == ')') {
                parameters.pop_back();
            }
            for (const std::string& parameter : split_plain(parameters, ',')) {
                if (!trim(parameter).empty()) {
                    definition.parameters.push_back(upper(trim(parameter)));
                }
            }
        } else {
            name = upper(header);
        }
        const std::string marker = is_sub ? "END SUB" : "END FUNCTION";
        for (std::size_t scan = index + 1; scan < lines.size(); ++scan) {
            if (upper(trim(lines[scan].text)) == marker) {
                definition.start = index + 1;
                definition.end = scan;
                if (is_sub) {
                    state_.subs[name] = definition;
                } else {
                    state_.functions[name] = definition;
                }
                break;
            }
        }
    }
}

// -- drawing ---------------------------------------------------------------

Rgb Machine::palette_colour(int index) const {
    const auto custom = state_.palette.find(index);
    if (custom != state_.palette.end()) {
        return custom->second;
    }
    const Rgb standard = default_colour(index);
    if (standard.red >= 0) {
        return standard;
    }
    if (index >= 0 && index <= 255) {
        return Rgb{index, index, index};
    }
    return turtle_.pen_colour();
}

Rgb Machine::resolve_colour(std::string_view raw, Rgb fallback) {
    const std::string value = trim(raw);
    if (value.empty()) {
        return fallback;
    }
    if (value.size() == 7 && value.front() == '#') {
        int components[3] = {0, 0, 0};
        bool valid = true;
        for (int index = 0; index < 3; ++index) {
            const std::string part = value.substr(static_cast<std::size_t>(1 + index * 2), 2);
            if (!std::isxdigit(static_cast<unsigned char>(part[0])) ||
                !std::isxdigit(static_cast<unsigned char>(part[1]))) {
                valid = false;
                break;
            }
            components[index] = static_cast<int>(std::strtol(part.c_str(), nullptr, 16));
        }
        return valid ? Rgb{components[0], components[1], components[2]} : fallback;
    }
    const auto named = colour_names().find(upper(value));
    if (named != colour_names().end()) {
        return named->second;
    }
    const std::vector<std::string> parts = split_top(value, " ");
    if (parts.size() == 3) {
        double values[3] = {0.0, 0.0, 0.0};
        for (std::size_t index = 0; index < 3; ++index) {
            if (!parse_float(parts[index], values[index])) {
                return fallback;
            }
        }
        return clamp_colour(values[0], values[1], values[2]);
    }
    double index = 0.0;
    if (parse_float(value, index)) {
        return palette_colour(static_cast<int>(index));
    }
    return fallback;
}

void Machine::move_to(double x, double y) {
    if (filling_ && pen_down_) {
        if (fill_points_.empty()) {
            fill_points_.push_back(Point{pose_.x(), pose_.y()});
        }
        fill_points_.push_back(Point{x, y});
    }
    pose_.go_to(x, y);
    turtle_.goto_xy(x, y);
}

void Machine::step_forward(double distance) {
    if (filling_ && pen_down_ && fill_points_.empty()) {
        fill_points_.push_back(Point{pose_.x(), pose_.y()});
    }
    pose_.move(distance);
    if (filling_ && pen_down_) {
        fill_points_.push_back(Point{pose_.x(), pose_.y()});
    }
    turtle_.forward(distance);
}

void Machine::turn_right(double degrees) {
    pose_.turn(degrees);
    turtle_.right(degrees);
}

void Machine::turn_left(double degrees) {
    pose_.turn(-degrees);
    turtle_.left(degrees);
}

void Machine::set_heading_degrees(double degrees) {
    pose_.set_heading(degrees);
    turtle_.set_heading(degrees);
}

void Machine::pen_up() {
    pen_down_ = false;
    turtle_.pen_up();
}

void Machine::pen_down() {
    pen_down_ = true;
    turtle_.pen_down();
}

void Machine::begin_fill() {
    filling_ = true;
    fill_points_.clear();
}

void Machine::end_fill() {
    filling_ = false;
    if (fill_points_.size() >= 3) {
        const Rgb colour = turtle_.pen_colour();
        turtle_.draw_polygon(fill_points_, colour, colour, pen_width_, true);
        ++fillable_shapes_;
    }
    fill_points_.clear();
}

const std::map<std::string, Rgb>& colour_names() {
    static const std::map<std::string, Rgb> names{
        {"BLACK", {0, 0, 0}},      {"WHITE", {255, 255, 255}},
        {"RED", {255, 0, 0}},      {"GREEN", {0, 255, 0}},
        {"BLUE", {0, 0, 255}},     {"YELLOW", {255, 255, 0}},
        {"CYAN", {0, 255, 255}},   {"MAGENTA", {255, 0, 255}},
        {"PINK", {255, 192, 203}}, {"ORANGE", {255, 165, 0}},
        {"PURPLE", {128, 0, 128}}, {"BROWN", {139, 69, 19}},
        {"LIME", {0, 255, 0}},     {"NAVY", {0, 0, 128}},
        {"TEAL", {0, 128, 128}},   {"MAROON", {128, 0, 0}},
        {"OLIVE", {128, 128, 0}},  {"SILVER", {192, 192, 192}},
        {"GOLD", {255, 215, 0}},   {"GRAY", {128, 128, 128}},
        {"GREY", {128, 128, 128}}};
    return names;
}

std::string expand_records(Machine& machine, std::string_view expression) {
    if (expression.find('.') == std::string_view::npos) {
        return std::string{expression};
    }
    const std::string text{expression};
    std::string result;
    std::size_t index = 0;
    while (index < text.size()) {
        const std::size_t start = index;
        if (std::isalpha(static_cast<unsigned char>(text[index])) == 0 && text[index] != '_') {
            result.push_back(text[index]);
            ++index;
            continue;
        }
        while (index < text.size() &&
               (std::isalnum(static_cast<unsigned char>(text[index])) != 0 ||
                text[index] == '_')) {
            ++index;
        }
        const std::string object = upper(text.substr(start, index - start));
        std::size_t look = index;
        while (look < text.size() && is_space(text[look])) {
            ++look;
        }
        if (look >= text.size() || text[look] != '.') {
            result.append(text, start, index - start);
            continue;
        }
        ++look;
        while (look < text.size() && is_space(text[look])) {
            ++look;
        }
        const std::size_t field_start = look;
        if (look >= text.size() ||
            (std::isalpha(static_cast<unsigned char>(text[look])) == 0 && text[look] != '_')) {
            result.append(text, start, look - start);
            index = look;
            continue;
        }
        while (look < text.size() &&
               (std::isalnum(static_cast<unsigned char>(text[look])) != 0 ||
                text[look] == '_')) {
            ++look;
        }
        const std::string field = upper(text.substr(field_start, look - field_start));
        const auto record = machine.state().records.find(object);
        if (record == machine.state().records.end()) {
            result.append(text, start, look - start);
        } else {
            const auto value = record->second.find(field);
            if (value != record->second.end() && value->second.is_number()) {
                result += format_numeric(value->second.number(), "");
            } else {
                result += "0";
            }
        }
        index = look;
    }
    return result;
}

}  // namespace timewarp::lang::basic
