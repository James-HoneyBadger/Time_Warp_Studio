// SPDX-License-Identifier: MIT
//
// Variables, lists, words, the drawing extras and the higher-order primitives.
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

// -- variables and data ----------------------------------------------------

std::string LogoMachine::do_make(const std::vector<std::string>& args,
                                 std::string_view raw) {
    if (args.size() < 2) {
        return "❌ MAKE requires variable name and value\n";
    }
    const std::string name = upper(strip_char(args[0], '"'));
    std::string value_expr;
    {
        const std::vector<std::string> parts = split_ws_limit(raw, 1);
        if (parts.size() > 1) {
            value_expr = strip(parts[1]);
        }
    }
    if (value_expr.empty()) {
        value_expr.clear();
        for (std::size_t index = 1; index < args.size(); ++index) {
            if (!value_expr.empty()) {
                value_expr.push_back(' ');
            }
            value_expr += args[index];
        }
    }

    try {
        set_number(name, eval(value_expr));
        return {};
    } catch (const std::exception&) {
        // Not a number: a word or a list, stored the way the reference does.
    }
    if (!value_expr.empty() && value_expr.front() == '"' &&
        value_expr.find(' ') == std::string::npos) {
        set_word(name, rstrip(std::string_view{value_expr}.substr(1)));
    } else if (value_expr.size() >= 2 && value_expr.front() == '"' &&
               value_expr.back() == '"') {
        set_word(name, value_expr.substr(1, value_expr.size() - 2));
    } else if (value_expr.size() >= 2 && value_expr.front() == '[' &&
               value_expr.back() == ']') {
        lists_[name] = split_ws(
            std::string_view{value_expr}.substr(1, value_expr.size() - 2));
    } else {
        set_word(name, value_expr);
    }
    return {};
}

std::string LogoMachine::do_thing(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ THING requires variable name\n";
    }
    const std::string name = upper(strip_char(args[0], '"'));
    const auto number = interpreter_.variables().find(name);
    if (number != interpreter_.variables().end()) {
        return repr_value(number->second.number());
    }
    const auto word = words_.find(name);
    if (word != words_.end()) {
        return word->second;
    }
    const auto list = lists_.find(name);
    if (list != lists_.end()) {
        return repr_list(list->second);
    }
    return "❌ Variable " + name + " not found\n";
}

std::string LogoMachine::do_print(const std::vector<std::string>& args,
                                  std::string_view raw) {
    if (args.empty()) {
        return "\n";
    }
    std::string text;
    if (!raw.empty()) {
        text = std::string{raw};
    } else {
        for (const std::string& piece : args) {
            if (!text.empty()) {
                text.push_back(' ');
            }
            text += piece;
        }
    }

    if (text.size() >= 2 && text.front() == '[' && text.back() == ']') {
        const std::string inner = text.substr(1, text.size() - 2);
        // The reference collapses whitespace after tightening the brackets, so
        // `[a [ b ] c]` keeps the inner pair tight and loses the rest.
        std::string tightened;
        for (std::size_t index = 0; index < inner.size();) {
            if (inner[index] == '[') {
                tightened.push_back('[');
                ++index;
                while (index < inner.size() && is_space(inner[index])) {
                    ++index;
                }
                continue;
            }
            if (is_space(inner[index])) {
                std::size_t end = index;
                while (end < inner.size() && is_space(inner[end])) {
                    ++end;
                }
                if (end < inner.size() && inner[end] == ']') {
                    index = end;
                    continue;
                }
                tightened.push_back(' ');
                index = end;
                continue;
            }
            tightened.push_back(inner[index]);
            ++index;
        }
        return strip(tightened) + "\n";
    }
    if (text.front() == '"' && text.find(' ') == std::string::npos) {
        return text.substr(1) + "\n";
    }

    if (args.size() == 1 && args[0].front() == ':') {
        const std::string name = upper(std::string_view{args[0]}.substr(1));
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            return format_value(number->second.render(), number->second.is_text()) + "\n";
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            return word->second + "\n";
        }
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            return repr_list(list->second) + "\n";
        }
    }

    // A user procedure that OUTPUTs a value is a reporter: `PRINT DOUBLE 21`.
    // The built-in reporters are all the expression evaluator knows, so without
    // this the call was printed back as if it were the answer.
    {
        const std::vector<std::string> tokens = split_ws(text);
        if (!tokens.empty()) {
            const std::string name = upper(tokens[0]);
            const auto params = procedure_params_.find(name);
            if (params != procedure_params_.end()) {
                const std::vector<std::string> call_args(tokens.begin() + 1,
                                                         tokens.end());
                const std::vector<std::string> used =
                    consume_args(call_args, params->second.size());
                std::size_t consumed = 0;
                for (const std::string& piece : used) {
                    consumed += split_ws(piece).size();
                }
                // Tokens left over are not this procedure's to read, so they
                // fall through to the ordinary expression path.
                if (consumed == call_args.size()) {
                    last_output_.reset();
                    const std::string result = call_procedure(name, call_args);
                    if (result.rfind("❌", 0) == 0) {
                        return result;
                    }
                    if (!last_output_.has_value()) {
                        return "❌ Procedure " + name +
                               " did not OUTPUT a value\n";
                    }
                    const Value value{*last_output_};
                    return format_value(value.render(), value.is_text()) + "\n";
                }
            }
        }
    }

    if (in_set(colour_reporters(), upper(args[0]))) {
        std::string joined;
        for (const std::string& piece : args) {
            if (!joined.empty()) {
                joined.push_back(' ');
            }
            joined += piece;
        }
        std::string result = run_text(joined);
        if (!result.empty() && result.back() == '\n') {
            result.pop_back();
        }
        double value = 0.0;
        if (looks_like_number(result, value)) {
            result = format_value(repr_real(value), false);
        }
        return result + "\n";
    }

    if (args.size() == 1) {
        const std::string reporter = upper(args[0]);
        if (reporter == "POS" || reporter == "XCOR" || reporter == "YCOR" ||
            reporter == "HEADING") {
            if (reporter == "POS") {
                return "[" + repr_real(pose_.x()) + " " +
                       repr_real(pose_.y()) + "]\n";
            }
            if (reporter == "XCOR") {
                return repr_real(pose_.x()) + "\n";
            }
            if (reporter == "YCOR") {
                return repr_real(pose_.y()) + "\n";
            }
            return repr_real(pose_.heading()) + "\n";
        }
    }

    try {
        const double value = eval(text);
        return format_value(repr_real(value), false) + "\n";
    } catch (const std::exception&) {
        return text + "\n";
    }
}

std::string LogoMachine::format_value(std::string_view text, bool as_text) const {
    if (as_text) {
        return std::string{text};
    }
    double value = 0.0;
    if (looks_like_number(text, value)) {
        return std::trunc(value) == value ? repr_integer(value) : general_text(value);
    }
    return std::string{text};
}

std::string LogoMachine::do_show(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ SHOW requires value\n";
    }
    std::string raw;
    for (const std::string& piece : args) {
        if (!raw.empty()) {
            raw.push_back(' ');
        }
        raw += piece;
    }
    std::string shown = do_print(args, raw);
    if (shown.empty() || shown.back() != '\n') {
        shown.push_back('\n');
    }
    return shown;
}

std::string LogoMachine::do_type(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ TYPE requires value\n";
    }
    std::string raw;
    for (const std::string& piece : args) {
        if (!raw.empty()) {
            raw.push_back(' ');
        }
        raw += piece;
    }
    return rstrip(do_print(args, raw));
}

std::string LogoMachine::do_word(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        return "❌ WORD requires at least two arguments\n";
    }
    std::string result;
    for (const std::string& argument : args) {
        try {
            const double value = interpreter_.evaluate_expression(argument);
            result += std::trunc(value) == value ? repr_integer(value) : repr_real(value);
        } catch (const std::exception&) {
            result += strip_char(argument, '"');
        }
    }
    return result;
}

std::string LogoMachine::do_list(const std::vector<std::string>& args) {
    // The reference mixes both kinds in one Python list, so the repr has to be
    // rebuilt with floats keeping their `.0` and words quoted.
    std::vector<std::pair<bool, std::string>> items;
    items.reserve(args.size());
    for (const std::string& argument : args) {
        try {
            const double value = interpreter_.evaluate_expression(argument);
            items.emplace_back(true, repr_real(value));
        } catch (const std::exception&) {
            items.emplace_back(false, strip_char(argument, '"'));
        }
    }
    std::string repr = "[";
    for (std::size_t index = 0; index < items.size(); ++index) {
        if (index > 0) {
            repr += ", ";
        }
        repr += items[index].first ? items[index].second
                                   : "'" + items[index].second + "'";
    }
    repr += "]";
    lists_["_TEMP_LIST"] = {};
    for (const auto& item : items) {
        lists_["_TEMP_LIST"].push_back(item.second);
    }
    return repr;
}

std::string LogoMachine::do_sentence(const std::vector<std::string>& args) {
    std::vector<std::string> items;
    for (const std::string& argument : args) {
        if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
            for (const std::string& piece :
                 split_ws(std::string_view{argument}.substr(1, argument.size() - 2))) {
                items.push_back(piece);
            }
            continue;
        }
        try {
            const double value = interpreter_.evaluate_expression(argument);
            items.push_back(repr_value(value));
        } catch (const std::exception&) {
            items.push_back(strip_char(argument, '"'));
        }
    }
    return repr_list(items);
}

std::string LogoMachine::do_first(const std::vector<std::string>& args, bool last) {
    const std::string label = last ? "LAST" : "FIRST";
    if (args.empty()) {
        return "❌ " + label + " requires argument\n";
    }
    const std::vector<std::string> consumed = consume_args(args, 1);
    const std::string& argument = consumed[0];

    if (argument.front() == ':') {
        const std::string name = upper(std::string_view{argument}.substr(1));
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            if (list->second.empty()) {
                return {};
            }
            return last ? list->second.back() : list->second.front();
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            if (word->second.empty()) {
                return {};
            }
            return last ? std::string{word->second.back()}
                        : std::string{word->second.front()};
        }
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            const std::string text = number->second.render();
            if (text.empty()) {
                return {};
            }
            return last ? std::string{text.back()} : std::string{text.front()};
        }
    }

    if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
        const std::vector<std::string> items =
            split_ws(std::string_view{argument}.substr(1, argument.size() - 2));
        if (items.empty()) {
            return {};
        }
        return last ? items.back() : items.front();
    }
    try {
        return repr_value(interpreter_.evaluate_expression(argument));
    } catch (const std::exception&) {
        const std::string word = strip_char(argument, '"');
        if (word.empty()) {
            return {};
        }
        return last ? std::string{word.back()} : std::string{word.front()};
    }
}

std::string LogoMachine::do_butfirst(const std::vector<std::string>& args, bool last) {
    const std::string label = last ? "BUTLAST" : "BUTFIRST";
    if (args.empty()) {
        return "❌ " + label + " requires argument\n";
    }
    const std::vector<std::string> consumed = consume_args(args, 1);
    const std::string& argument = consumed[0];

    const auto shorten = [last](const std::vector<std::string>& items) {
        if (items.size() <= 1) {
            return std::string{"[]"};
        }
        std::vector<std::string> kept(items.begin() + (last ? 0 : 1),
                                      items.end() - (last ? 1 : 0));
        return repr_list(kept);
    };
    const auto shorten_word = [last](const std::string& text) {
        if (text.size() <= 1) {
            return std::string{};
        }
        return last ? text.substr(0, text.size() - 1) : text.substr(1);
    };

    if (argument.front() == ':') {
        const std::string name = upper(std::string_view{argument}.substr(1));
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            return shorten(list->second);
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            return shorten_word(word->second);
        }
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            return shorten_word(number->second.render());
        }
    }
    if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
        return shorten(split_ws(std::string_view{argument}.substr(1, argument.size() - 2)));
    }
    try {
        return shorten_word(repr_real(interpreter_.evaluate_expression(argument)));
    } catch (const std::exception&) {
        return shorten_word(strip_char(argument, '"'));
    }
}

std::string LogoMachine::do_item(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        return "❌ ITEM requires index and thing\n";
    }
    const std::vector<std::string> consumed = consume_args(args, 2);
    if (consumed.size() < 2) {
        return "❌ ITEM requires index and thing\n";
    }
    double position = 0.0;
    try {
        position = std::trunc(eval(consumed[0])) - 1.0;
    } catch (const std::exception&) {
        return "❌ Invalid index\n";
    }
    const long long index = static_cast<long long>(position);
    const std::string& thing = consumed[1];

    if (thing.front() == ':') {
        const std::string name = upper(std::string_view{thing}.substr(1));
        const auto list = lists_.find(name);
        if (list != lists_.end()) {
            if (index >= 0 && index < static_cast<long long>(list->second.size())) {
                return list->second[static_cast<std::size_t>(index)];
            }
            return {};
        }
        const auto word = words_.find(name);
        if (word != words_.end()) {
            if (index >= 0 && index < static_cast<long long>(word->second.size())) {
                return std::string{word->second[static_cast<std::size_t>(index)]};
            }
            return {};
        }
        const auto number = interpreter_.variables().find(name);
        if (number != interpreter_.variables().end()) {
            const std::string text = number->second.render();
            if (index >= 0 && index < static_cast<long long>(text.size())) {
                return std::string{text[static_cast<std::size_t>(index)]};
            }
            return {};
        }
    }
    if (thing.size() >= 2 && thing.front() == '[' && thing.back() == ']') {
        const std::vector<std::string> items =
            split_ws(std::string_view{thing}.substr(1, thing.size() - 2));
        if (index >= 0 && index < static_cast<long long>(items.size())) {
            return items[static_cast<std::size_t>(index)];
        }
        return {};
    }
    try {
        const std::string text = repr_value(interpreter_.evaluate_expression(thing));
        if (index >= 0 && index < static_cast<long long>(text.size())) {
            return std::string{text[static_cast<std::size_t>(index)]};
        }
    } catch (const std::exception&) {
        const std::string word = strip_char(thing, '"');
        if (index >= 0 && index < static_cast<long long>(word.size())) {
            return std::string{word[static_cast<std::size_t>(index)]};
        }
    }
    return {};
}

std::string LogoMachine::do_count(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ COUNT requires argument\n";
    }
    const std::string& argument = args[0];
    if (argument.size() >= 2 && argument.front() == '[' && argument.back() == ']') {
        return std::to_string(
            split_ws(std::string_view{argument}.substr(1, argument.size() - 2)).size());
    }
    try {
        return std::to_string(repr_value(interpreter_.evaluate_expression(argument)).size());
    } catch (const std::exception&) {
        return std::to_string(strip_char(argument, '"').size());
    }
}

std::string LogoMachine::do_arithmetic(const std::vector<std::string>& args,
                                       char operation) {
    const char* const label = operation == '+'   ? "SUM"
                              : operation == '-' ? "DIFFERENCE"
                              : operation == '*' ? "PRODUCT"
                                                 : "QUOTIENT";
    if (args.size() < 2) {
        return std::string{"❌ "} + label + " requires two numbers\n";
    }
    double first = 0.0;
    double second = 0.0;
    try {
        first = interpreter_.evaluate_expression(args[0]);
        second = interpreter_.evaluate_expression(args[1]);
    } catch (const std::exception&) {
        return std::string{"❌ Invalid numbers for "} + label + "\n";
    }
    switch (operation) {
        case '+':
            return repr_value(first + second);
        case '-':
            return repr_value(first - second);
        case '*':
            return repr_value(first * second);
        default:
            if (second == 0.0) {
                return "❌ Division by zero\n";
            }
            return repr_value(first / second);
    }
}

std::string LogoMachine::do_random(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ RANDOM requires limit\n";
    }
    double limit = 0.0;
    try {
        limit = std::trunc(interpreter_.evaluate_expression(args[0]));
    } catch (const std::exception&) {
        return "❌ Invalid limit for RANDOM\n";
    }
    if (limit <= 0.0) {
        return "0";
    }
    return std::to_string(random_.randint(0, static_cast<int>(limit) - 1));
}

// -- graphics extras -------------------------------------------------------

std::string LogoMachine::do_arc(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        return "❌ ARC requires angle and radius\n";
    }
    double angle = 0.0;
    double radius = 0.0;
    try {
        angle = interpreter_.evaluate_expression(args[0]);
        radius = interpreter_.evaluate_expression(args[1]);
    } catch (const std::exception&) {
        return "❌ Invalid parameters for ARC\n";
    }
    turtle_.circle(radius, angle);
    return {};
}

std::string LogoMachine::do_bezier(const std::vector<std::string>& args) {
    // BEZIER cx1 cy1 cx2 cy2 x y -- a cubic from where the turtle stands to
    // (x, y), bending through the two control points.
    //
    // A native extension, like PENSTYLE: the canvas has drawn a bezier since
    // 19.0 and no language could ask for one. The turtle ends at the curve's end
    // point facing along its final tangent, which is what makes chaining two of
    // them produce a smooth curve rather than a corner.
    if (args.size() < 6) {
        return "❌ BEZIER requires cx1 cy1 cx2 cy2 x y\n";
    }
    double values[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    for (std::size_t index = 0; index < 6; ++index) {
        try {
            values[index] = interpreter_.evaluate_expression(args[index]);
        } catch (const std::exception&) {
            return "❌ Invalid parameters for BEZIER\n";
        }
    }
    const Point start{pose_.x(), pose_.y()};
    const Point first_control{values[0], values[1]};
    const Point second_control{values[2], values[3]};
    const Point finish{values[4], values[5]};
    if (pen_down_) {
        turtle_.bezier({start, first_control, second_control, finish},
                       turtle_.pen_colour(), pen_width_);
    }
    // The end tangent of a cubic is the direction from the last control point to
    // the end, so the heading follows the curve.
    const double dx = finish.x - second_control.x;
    const double dy = finish.y - second_control.y;
    pose_.go_to(finish.x, finish.y);
    if (dx != 0.0 || dy != 0.0) {
        constexpr double kDegrees = 57.29577951308232;
        pose_.set_heading(std::atan2(dx, dy) * kDegrees);
    }
    // The curve is already drawn; the host's `goto_xy` would draw a straight line
    // from where the turtle stood to where it now is, and heading it would leave
    // a second mark. The pen is lifted for the reposition only, which is also how
    // the reference moved a turtle without drawing.
    if (pen_down_) {
        turtle_.pen_up();
    }
    turtle_.goto_xy(finish.x, finish.y);
    // Heading 0 is north and grows clockwise, so a direction is atan2(dx, dy) in
    // degrees -- the same conversion every other motion word uses.
    turtle_.set_heading(pose_.heading());
    if (pen_down_) {
        turtle_.pen_down();
    }
    if (on_change_) {
        on_change_();
    }
    return {};
}

std::string LogoMachine::do_filled(std::string_view command) {
    const std::size_t open = command.find('[');
    if (open == std::string_view::npos) {
        return "❌ FILLED requires color and [commands]\n";
    }
    const std::vector<std::string> header = split_ws(strip(command.substr(0, open)));
    Rgb fill{255, 255, 255};
    std::size_t search_from = open;

    if (header.size() >= 2) {
        const std::vector<std::string> colour_tokens(header.begin() + 1, header.end());
        if (colour_tokens.size() == 1) {
            const std::string name = upper(strip_char(colour_tokens[0], '"'));
            if (const std::optional<Rgb> named = named_colour(name); named.has_value()) {
                fill = *named;
            } else {
                double grey = 0.0;
                fill = looks_like_number(name, grey)
                           ? Rgb{clamp_channel(grey), clamp_channel(grey), clamp_channel(grey)}
                           : Rgb{255, 255, 255};
            }
        } else if (colour_tokens.size() >= 3) {
            double red = 0.0;
            double green = 0.0;
            double blue = 0.0;
            if (looks_like_number(colour_tokens[0], red) &&
                looks_like_number(colour_tokens[1], green) &&
                looks_like_number(colour_tokens[2], blue)) {
                fill = Rgb{clamp_channel(red), clamp_channel(green), clamp_channel(blue)};
            } else {
                fill = Rgb{255, 255, 255};
            }
        }
    } else {
        // The colour is a bracketed triple, and the body follows it.
        const std::size_t close = command.find(']', open);
        if (close == std::string_view::npos) {
            return "❌ FILLED requires a color argument\n";
        }
        const std::vector<std::string> parts =
            split_ws(strip(command.substr(open + 1, close - open - 1)));
        if (parts.size() == 3) {
            double red = 0.0;
            double green = 0.0;
            double blue = 0.0;
            if (looks_like_number(parts[0], red) && looks_like_number(parts[1], green) &&
                looks_like_number(parts[2], blue)) {
                fill = Rgb{clamp_channel(red), clamp_channel(green), clamp_channel(blue)};
            } else {
                fill = Rgb{255, 255, 255};
            }
        } else if (parts.size() == 1) {
            double grey = 0.0;
            fill = looks_like_number(parts[0], grey)
                       ? Rgb{clamp_channel(grey), clamp_channel(grey), clamp_channel(grey)}
                       : Rgb{255, 255, 255};
        } else {
            fill = Rgb{255, 255, 255};
        }
        search_from = close + 1;
    }

    const std::string_view body_text = command.substr(search_from);
    const std::size_t body_open = body_text.find('[');
    const std::size_t body_close = body_text.rfind(']');
    if (body_open == std::string_view::npos || body_open + 1 >= body_close) {
        return "❌ FILLED commands malformed\n";
    }
    const std::string body = strip(body_text.substr(body_open + 1, body_close - body_open - 1));

    std::vector<Point> path{Point{pose_.x(), pose_.y()}};
    const std::function<void()> previous = on_change_;
    on_change_ = [this, &path]() {
        const Point current{pose_.x(), pose_.y()};
        if (path.empty() || path.back().x != current.x || path.back().y != current.y) {
            path.push_back(current);
        }
    };
    std::string result;
    try {
        result = run_text(body);
    } catch (...) {
        on_change_ = previous;
        throw;
    }
    on_change_ = previous;
    const Point final_point{pose_.x(), pose_.y()};
    if (path.empty() || path.back().x != final_point.x || path.back().y != final_point.y) {
        path.push_back(final_point);
    }
    if (path.size() >= 3) {
        turtle_.draw_polygon(path, turtle_.pen_colour(), fill, pen_width_, true);
    }
    return result;
}

std::string LogoMachine::do_label(std::string_view command) {
    std::string rest = strip(command);
    if (starts_with_upper(rest, "LABEL")) {
        rest = strip(std::string_view{rest}.substr(5));
    }
    if (rest.empty()) {
        return "❌ LABEL requires text\n";
    }
    const std::string text = strip_char(rest, '"');
    turtle_.draw_text(pose_.x(), pose_.y(), text.c_str(), font_size_);
    return {};
}

std::string LogoMachine::do_stamp() {
    constexpr double kSize = 10.0;
    const double radians = pose_.heading() * kPi / 180.0;
    const double left_angle = radians + 140.0 * kPi / 180.0;
    const double right_angle = radians - 140.0 * kPi / 180.0;
    std::vector<Point> triangle{
        Point{pose_.x() + kSize * std::sin(radians), pose_.y() - kSize * std::cos(radians)},
        Point{pose_.x() + kSize * 0.5 * std::sin(left_angle),
              pose_.y() - kSize * 0.5 * std::cos(left_angle)},
        Point{pose_.x() + kSize * 0.5 * std::sin(right_angle),
              pose_.y() - kSize * 0.5 * std::cos(right_angle)},
    };
    const Rgb colour = turtle_.pen_colour();
    // The reference stores a shape whose stroke width is TurtleShape's default,
    // not the pen width, so the stamp does not grow with SETPENWIDTH.
    turtle_.draw_polygon(triangle, colour, colour, 1.0, true);
    char buffer[64];
    std::snprintf(buffer, sizeof buffer, "🐢 Stamped at (%.1f, %.1f)\n", pose_.x(),
                  pose_.y());
    return buffer;
}

std::string LogoMachine::do_textsize(const std::vector<std::string>& args) {
    if (args.empty()) {
        return "❌ TEXTSIZE requires a size argument\n";
    }
    const std::optional<double> size = eval_number(args[0]);
    if (!size.has_value()) {
        return "❌ TEXTSIZE: invalid size '" + args[0] + "'\n";
    }
    const int rounded = static_cast<int>(*size);
    if (rounded < 1 || rounded > 200) {
        return "❌ TEXTSIZE must be between 1 and 200\n";
    }
    font_size_ = rounded;
    return {};
}

std::string LogoMachine::do_dot(const std::vector<std::string>& args) {
    double size = pen_width_ > 1.0 ? pen_width_ * 2.0 : 4.0;
    if (!args.empty()) {
        const std::optional<double> given = eval_number(args[0]);
        if (given.has_value()) {
            size = *given;
        }
    }
    const double radius = size / 2.0;
    constexpr int kSegments = 16;
    std::vector<Point> points;
    points.reserve(kSegments);
    for (int segment = 0; segment < kSegments; ++segment) {
        const double angle = static_cast<double>(segment) * 360.0 / kSegments * kPi / 180.0;
        points.push_back(Point{pose_.x() + radius * std::cos(angle),
                               pose_.y() + radius * std::sin(angle)});
    }
    const Rgb colour = turtle_.pen_colour();
    turtle_.draw_polygon(points, colour, colour, 1.0, true);
    return {};
}

// -- higher-order list primitives ------------------------------------------

std::string LogoMachine::run_template(std::string_view template_text,
                                      const std::map<std::string, Value>& bindings) {
    std::map<std::string, std::optional<Value>> saved;
    for (const auto& [name, value] : bindings) {
        const auto existing = interpreter_.variables().find(name);
        saved[name] = existing == interpreter_.variables().end()
                          ? std::optional<Value>{}
                          : std::optional<Value>{existing->second};
    }
    for (const auto& [name, value] : bindings) {
        interpreter_.variables()[name] = value;
    }
    std::string output;
    try {
        output = run_text(template_text);
    } catch (...) {
        for (const auto& [name, value] : saved) {
            if (value.has_value()) {
                interpreter_.variables()[name] = *value;
            } else {
                interpreter_.variables().erase(name);
            }
        }
        throw;
    }
    for (const auto& [name, value] : saved) {
        if (value.has_value()) {
            interpreter_.variables()[name] = *value;
        } else {
            interpreter_.variables().erase(name);
        }
    }
    return output;
}

Value template_value(std::string_view item) {
    double number = 0.0;
    if (looks_like_number(item, number)) {
        return Value{number};
    }
    return Value{std::string{item}};
}

std::string LogoMachine::do_foreach(std::string_view command) {
    const std::size_t found = upper(command).find("FOREACH");
    const std::string after = strip(command.substr(found + 7));
    std::vector<std::string> blocks;
    std::size_t index = 0;
    while (index < after.size() && blocks.size() < 2) {
        if (after[index] != '[') {
            ++index;
            continue;
        }
        int depth = 0;
        std::size_t cursor = index;
        while (cursor < after.size()) {
            if (after[cursor] == '[') {
                ++depth;
            } else if (after[cursor] == ']') {
                --depth;
                if (depth == 0) {
                    blocks.emplace_back(strip(after.substr(index + 1, cursor - index - 1)));
                    index = cursor;
                    break;
                }
            }
            ++cursor;
        }
        ++index;
    }
    if (blocks.size() < 2) {
        return "❌ FOREACH requires [list] [template]\n";
    }
    std::string output;
    for (const std::string& item : split_ws(blocks[0])) {
        tick();
        output += run_template(blocks[1], {{"?", template_value(item)}});
    }
    return output;
}

std::string LogoMachine::do_higher_order(std::string_view command, char which) {
    const std::string name = which == 'm' ? "MAP" : which == 'f' ? "FILTER" : "REDUCE";
    const std::size_t found = upper(command).find(name);
    const std::string after = strip(command.substr(found + name.size()));

    const std::size_t open = after.find('[');
    if (open == std::string::npos) {
        return "❌ " + name + (which == 'm'   ? " requires [template] [list]\n"
                                : which == 'f' ? " requires [predicate] [list]\n"
                                               : " requires [template] [list]\n");
    }
    const std::size_t close = matching_bracket(after, open);
    if (close == std::string::npos) {
        return "❌ " + name + " requires [template] [list]\n";
    }
    const std::string template_text = strip(after.substr(open + 1, close - open - 1));
    const std::string rest = strip(after.substr(close + 1));

    std::vector<std::string> items;
    if (!rest.empty() && rest.front() == '[') {
        if (rest.size() >= 2 && rest.back() == ']') {
            items = split_ws(std::string_view{rest}.substr(1, rest.size() - 2));
        } else {
            items = split_ws(rest);
        }
    } else {
        items = split_ws(rest);
    }

    if (which == 'r') {
        if (items.size() < 1) {
            return "❌ REDUCE requires a non-empty list\n";
        }
        Value accumulator = template_value(items[0]);
        for (std::size_t index = 1; index < items.size(); ++index) {
            tick();
            const std::string result =
                run_template(template_text, {{"?1", accumulator},
                                             {"?2", template_value(items[index])}});
            const std::string trimmed = strip(result);
            accumulator = template_value(trimmed);
        }
        return accumulator.render() + "\n";
    }

    std::vector<std::string> results;
    for (const std::string& item : items) {
        tick();
        const std::string result =
            run_template(template_text, {{"?", template_value(item)}});
        if (which == 'm') {
            results.push_back(strip(result));
            continue;
        }
        const std::string trimmed = strip(result);
        double value = 0.0;
        bool truthy = false;
        if (looks_like_number(trimmed, value)) {
            truthy = value != 0.0;
        } else {
            truthy = !trimmed.empty() && upper(trimmed) != "FALSE" && trimmed != "0";
        }
        if (truthy) {
            results.push_back(item);
        }
    }
    std::string joined;
    for (const std::string& piece : results) {
        if (!joined.empty()) {
            joined.push_back(' ');
        }
        joined += piece;
    }
    return "[" + joined + "]\n";
}

std::string LogoMachine::do_local(const std::vector<std::string>& args) {
    for (const std::string& argument : args) {
        std::size_t start = 0;
        while (start < argument.size() && argument[start] == ':') {
            ++start;
        }
        // The reference strips only `:` here, so `LOCAL "X` makes a variable
        // whose name really is `"X`. Reproduced rather than tidied up: the
        // parity harness compares variable names.
        const std::string name = upper(std::string_view{argument}.substr(start));
        if (interpreter_.variables().find(name) == interpreter_.variables().end() &&
            words_.find(name) == words_.end()) {
            interpreter_.set_number(name, 0.0);
        }
    }
    return {};
}

}  // namespace logo_detail
}  // namespace timewarp::lang
