// SPDX-License-Identifier: MIT
//
// The command table and everything it does to the turtle.
#include "logo_internal.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <chrono>
#include <random>
#include <set>
#include <thread>
#include <utility>

namespace timewarp::lang {
namespace logo_detail {

// -- dispatch --------------------------------------------------------------

std::string LogoMachine::run_command(std::string_view command_text) {
    std::string command{command_text};
    const std::size_t comment = command.find(';');
    if (comment != std::string::npos) {
        command.resize(comment);
    }
    const std::string trimmed = strip(command);
    if (trimmed.empty()) {
        return {};
    }
    const std::string shouted = upper(trimmed);
    const std::vector<std::string> words = split_ws(shouted);
    if (words.empty()) {
        return {};
    }
    const std::string& name = words[0];
    std::vector<std::string> args(words.begin() + 1, words.end());

    std::string raw_args;
    {
        const std::vector<std::string> parts = split_ws_limit(trimmed, 1);
        if (parts.size() > 1) {
            raw_args = strip(parts[1]);
        }
    }

    // A procedure shadows a primitive, as it does in the reference.
    const auto procedure = procedures_.find(name);
    if (procedure != procedures_.end()) {
        return call_procedure(name, args);
    }

    try {
        if (name == "FORWARD" || name == "FD") {
            return do_forward(args);
        }
        if (name == "BACK" || name == "BK" || name == "BACKWARD") {
            return do_back(args);
        }
        if (name == "LEFT" || name == "LT") {
            return do_left(args);
        }
        if (name == "RIGHT" || name == "RT") {
            return do_right(args);
        }
        if (name == "PENUP" || name == "PU") {
            pen_down_ = false;
            turtle_.pen_up();
            return {};
        }
        if (name == "PENDOWN" || name == "PD") {
            pen_down_ = true;
            turtle_.pen_down();
            return {};
        }
        if (name == "HOME") {
            home();
            return {};
        }
        if (name == "CLEARSCREEN" || name == "CS") {
            clear_screen();
            return {};
        }
        if (name == "CLEAN" || name == "CLEAR") {
            turtle_.clear();
            return {};
        }
        if (name == "HIDETURTLE" || name == "HT") {
            turtle_.hide_turtle();
            return {};
        }
        if (name == "SHOWTURTLE" || name == "ST") {
            turtle_.show_turtle();
            return {};
        }
        if (name == "SETXY") {
            return do_setxy(args);
        }
        if (name == "SETPOSITION" || name == "SETPOS") {
            return do_setposition(args);
        }
        if (name == "SETX") {
            return do_setx(args);
        }
        if (name == "SETY") {
            return do_sety(args);
        }
        if (name == "SETHEADING" || name == "SETH") {
            return do_setheading(args);
        }
        if (name == "SETPENCOLOR" || name == "SETPC" || name == "SETCOLOR") {
            return do_setcolor(args);
        }
        if (name == "SETBGCOLOR" || name == "SETBG") {
            return do_setbgcolor(args);
        }
        if (name == "SETPENWIDTH" || name == "SETPW" || name == "PENWIDTH" ||
            name == "PENSIZE" || name == "SETPENSIZE") {
            return do_setpenwidth(args);
        }
        if (name == "REPEAT") {
            return do_repeat(trimmed);
        }
        if (name == "IF") {
            return do_if(trimmed);
        }
        if (name == "IFELSE") {
            return do_ifelse(trimmed);
        }
        if (name == "STOP") {
            return "STOP_PROCEDURE";
        }
        if (name == "OUTPUT") {
            return do_output(args);
        }
        if (name == "TO") {
            return do_to(trimmed);
        }
        if (name == "END") {
            return {};
        }
        if (name == "MAKE") {
            return do_make(args, raw_args);
        }
        if (name == "THING") {
            return do_thing(args);
        }
        if (name == "PRINT") {
            return do_print(args, raw_args);
        }
        if (name == "SQL") {
            // The rest of the line is the statement: SQL is not a Logo reporter,
            // so it must not be split into Logo words or evaluated.
            return interpreter_.run_sql(raw_args);
        }
        if (name == "SHOW") {
            return do_show(args);
        }
        if (name == "TYPE") {
            return do_type(args);
        }
        if (name == "WORD") {
            return do_word(args);
        }
        if (name == "LIST") {
            return do_list(args);
        }
        if (name == "SENTENCE") {
            return do_sentence(args);
        }
        if (name == "FIRST") {
            return do_first(args, false);
        }
        if (name == "LAST") {
            return do_first(args, true);
        }
        if (name == "BUTFIRST") {
            return do_butfirst(args, false);
        }
        if (name == "BUTLAST") {
            return do_butfirst(args, true);
        }
        if (name == "ITEM") {
            return do_item(args);
        }
        if (name == "COUNT") {
            return do_count(args);
        }
        if (name == "SUM" || name == "+") {
            return do_arithmetic(args, '+');
        }
        if (name == "DIFFERENCE" || name == "-") {
            return do_arithmetic(args, '-');
        }
        if (name == "PRODUCT" || name == "*") {
            return do_arithmetic(args, '*');
        }
        if (name == "QUOTIENT" || name == "/") {
            return do_arithmetic(args, '/');
        }
        if (name == "RANDOM") {
            return do_random(args);
        }
        if (name == "FOREVER") {
            return do_forever(trimmed);
        }
        if (name == "REPCOUNT") {
            return repr_integer(interpreter_.number("REPCOUNT"));
        }
        if (name == "ARC") {
            return do_arc(args);
        }
        if (name == "BEZIER") {
            return do_bezier(args);
        }
        if (name == "FILLED") {
            return do_filled(trimmed);
        }
        if (name == "PENSTYLE") {
            // PENSTYLE n [, length] -- 0 solid, 1 dashed, 2 dotted, 3 dash-dot,
            // 4 invisible. A native extension rather than a reference command: the
            // canvas learned to draw a dashed pen in 19.0 and a pen style nothing
            // can ask for is a feature nobody has.
            // `args` is already split by the dispatcher.
            const std::vector<std::string>& fields = args;
            if (fields.empty()) {
                return "❌ PENSTYLE needs a style number (0 solid, 1 dashed, "
                       "2 dotted, 3 dash-dot, 4 invisible)\n";
            }
            const std::optional<double> style = eval_number(fields[0]);
            if (!style.has_value()) {
                return "❌ PENSTYLE needs a number, got '" + fields[0] + "'\n";
            }
            const int which = static_cast<int>(*style);
            if (which < 0 || which > 4) {
                return "❌ Value " + std::to_string(which) +
                       " for PENSTYLE is outside 0-4\n";
            }
            double length = 1.0;
            if (fields.size() >= 2) {
                if (const std::optional<double> given = eval_number(fields[1])) {
                    length = *given;
                }
            }
            turtle_.set_pen_style(which, length);
            return {};
        }
        if (name == "LABEL") {
            return do_label(trimmed);
        }
        if (name == "WAIT") {
            if (!args.empty()) {
                const std::optional<double> ticks = eval_number(args[0]);
                if (!ticks.has_value()) {
                    return "❌ WAIT requires a numeric argument\n";
                }
                const double seconds = std::max(0.0, *ticks / 60.0);
                if (seconds > 0.0) {
                    std::this_thread::sleep_for(std::chrono::duration<double>(seconds));
                }
            }
            return {};
        }
        if (name == "BYE") {
            interpreter_.running = false;
            return "✅ Goodbye!\n";
        }
        if (name == "OP" || name == "OUTPUT") {
            if (!args.empty()) {
                const std::optional<double> value = eval_number(args[0]);
                if (value.has_value()) {
                    return repr_value(*value);
                }
                std::string joined;
                for (const std::string& piece : args) {
                    if (!joined.empty()) {
                        joined.push_back(' ');
                    }
                    joined += piece;
                }
                return joined;
            }
            return {};
        }
        if (name == "REM") {
            return {};
        }
        if (name == "FOREACH") {
            return do_foreach(trimmed);
        }
        if (name == "MAP") {
            return do_higher_order(trimmed, 'm');
        }
        if (name == "FILTER") {
            return do_higher_order(trimmed, 'f');
        }
        if (name == "REDUCE") {
            return do_higher_order(trimmed, 'r');
        }
        if (name == "WHILE") {
            return do_while(trimmed, false);
        }
        if (name == "UNTIL") {
            return do_while(trimmed, true);
        }
        if (name == "LOCAL") {
            return do_local(args);
        }
        if (name == "WINDOW" || name == "WRAP" || name == "FENCE") {
            return "ℹ️ Screen mode: " + name + "\n";
        }
        if (name == "STAMP" || name == "STAMPCLEAN") {
            // The reference stamps for both spellings; STAMPCLEAN is otherwise
            // the same call.
            return do_stamp();
        }
        if (name == "TEXTSIZE" || name == "SETLABELHEIGHT") {
            return do_textsize(args);
        }
        if (name == "DOT") {
            return do_dot(args);
        }
        if (name == "PENPAINT" || name == "PPT") {
            pen_down_ = true;
            turtle_.pen_down();
            return {};
        }
        if (name == "PENERASE" || name == "PE") {
            pen_mode_ = "erase";
            return {};
        }
        if (name == "PENREVERSE" || name == "PX") {
            pen_mode_ = "reverse";
            return {};
        }
        if (name == "PENMODE") {
            return pen_mode_ + "\n";
        }
        if (name == "ENABLE3D" || name == "3DON") {
            pose_.enable_3d();
            turtle_.enable_3d();
            return "🐢 3D turtle mode enabled\n";
        }
        if (name == "DISABLE3D" || name == "3DOFF" || name == "2DON") {
            pose_.disable_3d();
            turtle_.disable_3d();
            return "🐢 2D turtle mode restored\n";
        }
        if (name == "PITCH") {
            if (args.empty()) {
                return "❌ PITCH requires an angle\n";
            }
            const std::optional<double> angle = eval_number(args[0]);
            if (!angle.has_value()) {
                return "❌ Expected a number, got: " + args[0] + "\n";
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.pitch(*angle);
            turtle_.pitch(*angle);
            return {};
        }
        if (name == "ROLL" || name == "TILTROLL") {
            if (args.empty()) {
                return "❌ ROLL requires an angle\n";
            }
            const std::optional<double> angle = eval_number(args[0]);
            if (!angle.has_value()) {
                return "❌ Expected a number, got: " + args[0] + "\n";
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.roll(*angle);
            turtle_.roll(*angle);
            return {};
        }
        if (name == "YAW") {
            if (args.empty()) {
                return "❌ YAW requires an angle\n";
            }
            const std::optional<double> angle = eval_number(args[0]);
            if (!angle.has_value()) {
                return "❌ Expected a number, got: " + args[0] + "\n";
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            right(*angle);
            return {};
        }
        if (name == "TILTUP" || name == "UPWARD" || name == "UP") {
            double angle = 45.0;
            if (!args.empty()) {
                const std::optional<double> value = eval_number(args[0]);
                if (value.has_value()) {
                    angle = *value;
                }
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.pitch(angle);
            turtle_.pitch(angle);
            return {};
        }
        if (name == "TILTDOWN" || name == "DOWNWARD" || name == "DOWN") {
            double angle = 45.0;
            if (!args.empty()) {
                const std::optional<double> value = eval_number(args[0]);
                if (value.has_value()) {
                    angle = *value;
                }
            }
            if (!pose_.is_3d()) {
                pose_.enable_3d();
                turtle_.enable_3d();
            }
            pose_.pitch(-angle);
            turtle_.pitch(-angle);
            return {};
        }
    } catch (const BudgetStop&) {
        throw;
    } catch (const std::exception& error) {
        return std::string{"❌ "} + error.what() + "\n";
    }

    // A statement that is just an expression, which is how a template such as
    // `[:? * 2]` produces a value.
    if (!name.empty() && name.front() == ':') {
        std::string expression;
        for (const std::string& piece : words) {
            if (!expression.empty()) {
                expression.push_back(' ');
            }
            expression += piece;
        }
        try {
            return repr_value(eval(expression));
        } catch (const std::exception&) {
            // Falls through to the unknown-command message.
        }
    }
    return "❌ Unknown Logo command: " + name + "\n";
}

// -- motion ----------------------------------------------------------------

std::string LogoMachine::do_forward(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ FORWARD expects 1 argument(s), got " +
               std::to_string(consumed.size()) + "\n";
    }
    forward(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_back(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ BACK expects 1 argument(s), got " + std::to_string(consumed.size()) +
               "\n";
    }
    back(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_left(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ LEFT expects 1 argument(s), got " + std::to_string(consumed.size()) +
               "\n";
    }
    left(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_right(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 1);
    if (consumed.size() != 1) {
        return "❌ RIGHT expects 1 argument(s), got " +
               std::to_string(consumed.size()) + "\n";
    }
    right(eval_arg(consumed[0]));
    return {};
}

std::string LogoMachine::do_setxy(const std::vector<std::string>& args) {
    const std::vector<std::string> consumed = consume_args(args, 2);
    if (consumed.size() != 2) {
        return "❌ SETXY expects 2 argument(s), got " + std::to_string(consumed.size()) +
               "\n";
    }
    const double x = eval_arg(consumed[0]);
    const double y = eval_arg(consumed[1]);
    go_to(x, y);
    return {};
}

std::string LogoMachine::do_setposition(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETPOSITION requires a list argument [x y]\n";
    }
    std::string joined;
    for (const std::string& piece : args) {
        if (!joined.empty()) {
            joined.push_back(' ');
        }
        joined += piece;
    }
    if (joined.front() == '[' && joined.back() == ']') {
        const std::vector<std::string> parts =
            split_ws(std::string_view{joined}.substr(1, joined.size() - 2));
        if (parts.size() != 2) {
            return "❌ SETPOSITION list must contain exactly 2 numbers\n";
        }
        const double x = eval_arg(parts[0]);
        const double y = eval_arg(parts[1]);
        go_to(x, y);
        return {};
    }
    if (args.size() == 2) {
        return do_setxy(args);
    }
    return "❌ SETPOSITION requires a list argument [x y]\n";
}

std::string LogoMachine::do_setx(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETX requires x coordinate\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    go_to(eval(expression), pose_.y());
    return {};
}

std::string LogoMachine::do_sety(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETY requires y coordinate\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    go_to(pose_.x(), eval(expression));
    return {};
}

std::string LogoMachine::do_setheading(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETHEADING requires angle\n";
    }
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    set_heading(eval(expression));
    return {};
}

std::string LogoMachine::do_setpenwidth(const std::vector<std::string>& args) {
    std::string expression;
    for (const std::string& piece : args) {
        if (!expression.empty()) {
            expression.push_back(' ');
        }
        expression += piece;
    }
    if (expression.empty()) {
        return "❌ SETPENWIDTH requires a width argument\n";
    }
    const double value = eval(expression);
    const double clamped = std::min(100.0, std::max(1.0, std::trunc(value)));
    pen_width_ = clamped;
    turtle_.set_pen_width(clamped);
    return {};
}

// -- colour ----------------------------------------------------------------

std::string LogoMachine::do_setcolor(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETCOLOR requires color\n";
    }
    if (in_set(colour_reporters(), upper(args[0])) || args[0].front() == ':') {
        const std::string colour = resolve_colour_argument(args);
        if (colour.rfind("❌", 0) == 0) {
            return colour;
        }
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_color(*named);
            return {};
        }
        // A hex colour or three numbers, spelled the way `pencolor` accepts.
        const std::string hex = strip_char(colour, '"');
        if (hex.size() == 7 && hex.front() == '#') {
            const auto channel = [&hex](std::size_t start) {
                return static_cast<int>(std::strtol(hex.substr(start, 2).c_str(), nullptr, 16));
            };
            turtle_.set_color(Rgb{channel(1), channel(3), channel(5)});
        }
        return {};
    }

    std::vector<std::string> working = args;
    std::string merged;
    for (const std::string& piece : working) {
        if (!merged.empty()) {
            merged.push_back(' ');
        }
        merged += piece;
    }
    if (merged.front() == '[' && merged.back() == ']') {
        std::string inner = merged.substr(1, merged.size() - 2);
        std::replace(inner.begin(), inner.end(), ',', ' ');
        working = split_ws(inner);
        if (working.size() != 3) {
            return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n";
        }
    }

    if (working.size() == 1) {
        const std::string colour = strip_char(working[0], '"');
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_color(*named);
        } else if (colour.size() == 7 && colour.front() == '#') {
            const auto channel = [&colour](std::size_t start) {
                return static_cast<int>(
                    std::strtol(colour.substr(start, 2).c_str(), nullptr, 16));
            };
            turtle_.set_color(Rgb{channel(1), channel(3), channel(5)});
        } else if (const std::optional<double> slot = eval_number(colour);
                   slot.has_value()) {
            // A bare number is a palette slot, which is what `SETPC 1` means in
            // the reference's own examples. Without this branch the statement was
            // accepted, printed nothing, and changed no colour -- a statement that
            // acknowledged work it had not done.
            turtle_.set_color(turtle_.resolve_color(static_cast<int>(*slot)));
        }
        return {};
    }
    if (working[0].front() == '"') {
        // A quoted colour name is complete in itself; trailing tokens belong to
        // whatever follows on the line.
        const std::string colour = strip_char(working[0], '"');
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_color(*named);
        }
        return {};
    }
    if (working.size() >= 3) {
        const std::vector<std::string> rgb =
            working.size() == 3 ? working : group_rgb_arguments(working);
        if (rgb.size() != 3) {
            return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n";
        }
        try {
            const int red = clamp_channel(eval_arg(rgb[0]));
            const int green = clamp_channel(eval_arg(rgb[1]));
            const int blue = clamp_channel(eval_arg(rgb[2]));
            turtle_.set_color(Rgb{red, green, blue});
        } catch (const std::exception&) {
            return "❌ SETCOLOR RGB values must be integers\n";
        }
        return {};
    }
    return "❌ SETCOLOR requires 1 color name/hex or 3 RGB values\n";
}

std::string LogoMachine::do_setbgcolor(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SETBGCOLOR requires color\n";
    }
    if (in_set(colour_reporters(), upper(args[0])) || args[0].front() == ':') {
        const std::string colour = resolve_colour_argument(args);
        if (colour.rfind("❌", 0) == 0) {
            return colour;
        }
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_background(*named);
            return {};
        }
        return "❌ SETBGCOLOR only supports named colors for now\n";
    }

    std::vector<std::string> working = args;
    std::string merged;
    for (const std::string& piece : working) {
        if (!merged.empty()) {
            merged.push_back(' ');
        }
        merged += piece;
    }
    if (merged.front() == '[' && merged.back() == ']') {
        std::string inner = merged.substr(1, merged.size() - 2);
        std::replace(inner.begin(), inner.end(), ',', ' ');
        working = split_ws(inner);
        if (working.size() != 3) {
            return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n";
        }
    }
    if (working.size() == 1) {
        const std::string colour = strip_char(working[0], '"');
        if (const std::optional<Rgb> named = named_colour(colour); named.has_value()) {
            turtle_.set_background(*named);
        } else {
            return "❌ SETBGCOLOR only supports named colors for now\n";
        }
        return {};
    }
    if (working.size() >= 3) {
        const std::vector<std::string> rgb =
            working.size() == 3 ? working : group_rgb_arguments(working);
        if (rgb.size() != 3) {
            return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n";
        }
        try {
            const int red = clamp_channel(eval_arg(rgb[0]));
            const int green = clamp_channel(eval_arg(rgb[1]));
            const int blue = clamp_channel(eval_arg(rgb[2]));
            turtle_.set_background(Rgb{red, green, blue});
        } catch (const std::exception&) {
            return "❌ SETBGCOLOR RGB values must be integers\n";
        }
        return {};
    }
    return "❌ SETBGCOLOR requires 1 color name or 3 RGB values\n";
}

}  // namespace logo_detail
}  // namespace timewarp::lang
