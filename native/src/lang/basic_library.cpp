// SPDX-License-Identifier: MIT
//
// What a BASIC statement reaches that is not BASIC: the shape library
// (`features/shapes.py`), the L-System renderer (`features/fractals.py`), the
// MML player's text (`features/music.py`), particle effects
// (`features/particles.py`) and speech (`features/speech.py`).
//
// The geometry is ported as arithmetic rather than as data because the drawing
// is the observable output: a star that is 0.1 off is a different picture, and
// the parity gate compares the moves rather than the colours.

#include "basic_internal.hpp"

#include <cmath>
#include <string>
#include <utility>
#include <vector>

namespace timewarp::lang::basic {
namespace {

constexpr double kPi = 3.14159265358979323846;

struct Preset {
    const char* name;
    const char* axiom;
    double angle;
    /// Each rule is a predecessor followed by its successor, in the reference's
    /// order; the last byte is unused.
    std::pair<char, const char*> rules[5];
};

const std::vector<Preset>& fractal_presets() {
    static const std::vector<Preset> presets{
        {"KOCH", "F--F--F", 60.0, {{'F', "F+F--F+F"}}},
        {"SIERPINSKI", "F-G-G", 120.0, {{'F', "F-G+F+G-F"}, {'G', "GG"}}},
        {"DRAGON", "FX", 90.0, {{'X', "X+YF+"}, {'Y', "-FX-Y"}}},
        {"PLANT", "X", 25.0, {{'X', "F+[[X]-X]-F[-FX]+X"}, {'F', "FF"}}},
        {"TREE", "0", 45.0, {{'1', "11"}, {'0', "1[0]0"}}},
        {"HILBERT", "A", 90.0, {{'A', "-BF+AFA+FB-"}, {'B', "+AF-BFB-FA+"}}},
        {"PEANO", "X", 90.0,
         {{'X', "XFYFX+F+YFXFY-F-XFYFX"}, {'Y', "YFXFY-F-XFYFX+F+YFXFY"}}},
        {"GOSPER", "A", 60.0, {{'A', "A-B--B+A++AA+B-"}, {'B', "+A-BB--B-A++A+B"}}},
        {"LEVY", "F", 45.0, {{'F', "+F--F+"}}},
        {"SQUARE", "F+F+F+F", 90.0, {{'F', "F+F-F-FF+F+F-F"}}},
        {"CRYSTAL", "F+F+F+F", 90.0, {{'F', "FF+F++F+F"}}},
        {"RINGS", "F+F+F+F", 90.0, {{'F', "FF+F+F+F+F+F-F"}}},
        {"BUSH", "Y", 25.7, {{'X', "X[-FFF][+FFF]FX"}, {'Y', "YFX[+Y][-Y]"}}},
        {"SEAWEED", "F", 22.5, {{'F', "FF-[-F+F+F]+[+F-F-F]"}}},
        {"PENROSE",
         "[7]++[7]++[7]++[7]++[7]",
         36.0,
         {{'6', "81++91----71[-81----61]++"},
          {'7', "+81--91[---61--71]+"},
          {'8', "-61++71[+++81++91]-"},
          {'9', "--81++++61[+91++++71]--71"},
          {'1', ""}}}};
    return presets;
}

std::string preset_names() {
    std::string names;
    for (const Preset& preset : fractal_presets()) {
        if (!names.empty()) {
            names += ", ";
        }
        names += preset.name;
    }
    return names;
}

const Preset* find_preset(std::string_view name) {
    const std::string wanted = upper(name);
    for (const Preset& preset : fractal_presets()) {
        if (wanted == preset.name) {
            return &preset;
        }
    }
    return nullptr;
}

std::string expand(const Preset& preset, int iterations) {
    std::string current = preset.axiom;
    for (int round = 0; round < iterations; ++round) {
        std::string next;
        for (const char character : current) {
            const char* replacement = nullptr;
            for (const auto& [predecessor, successor] : preset.rules) {
                if (predecessor == character && successor != nullptr) {
                    replacement = successor;
                    break;
                }
            }
            if (replacement == nullptr) {
                next.push_back(character);
            } else {
                next += replacement;
            }
        }
        current = std::move(next);
    }
    return current;
}

std::vector<Point> polygon_points(int sides, double size, double cx, double cy) {
    std::vector<Point> points;
    const double step = 2.0 * kPi / sides;
    const double start = -kPi / 2.0;
    for (int index = 0; index < sides; ++index) {
        const double angle = start + index * step;
        points.push_back(Point{cx + size * std::cos(angle), cy + size * std::sin(angle)});
    }
    return points;
}

std::vector<Point> star_points(int arms, double outer, double inner, double cx, double cy) {
    std::vector<Point> points;
    const double step = kPi / arms;
    const double start = -kPi / 2.0;
    for (int index = 0; index < arms * 2; ++index) {
        const double angle = start + index * step;
        const double radius = index % 2 == 0 ? outer : inner;
        points.push_back(Point{cx + radius * std::cos(angle), cy + radius * std::sin(angle)});
    }
    return points;
}

std::vector<Point> heart_points(double size, double cx, double cy) {
    std::vector<Point> points;
    for (int index = 0; index < 100; ++index) {
        const double t = index * 2.0 * kPi / 100.0;
        const double sine = std::sin(t);
        const double x = 16.0 * sine * sine * sine;
        const double y = 13.0 * std::cos(t) - 5.0 * std::cos(2.0 * t) -
                         2.0 * std::cos(3.0 * t) - std::cos(4.0 * t);
        points.push_back(Point{cx + x * size / 20.0, cy - y * size / 20.0});
    }
    return points;
}

std::vector<Point> arrow_points(double length, double width, double cx, double cy,
                                double direction) {
    const double half_width = width / 2.0;
    const double shaft = width / 4.0;
    const double head = length / 3.0;
    const std::vector<Point> raw{{0.0, shaft},  {length - head, shaft},
                                 {length - head, half_width}, {length, 0.0},
                                 {length - head, -half_width},
                                 {length - head, -shaft}, {0.0, -shaft}};
    const double radians = direction * kPi / 180.0;
    const double cosine = std::cos(radians);
    const double sine = std::sin(radians);
    std::vector<Point> points;
    for (const Point& point : raw) {
        points.push_back(Point{point.x * cosine - point.y * sine + cx,
                               point.x * sine + point.y * cosine + cy});
    }
    return points;
}

std::vector<Point> spiral_points(double turns, double start_radius, double end_radius,
                                 double cx, double cy) {
    std::vector<Point> points;
    const auto steps = static_cast<int>(turns * 36.0);
    for (int index = 0; index <= steps; ++index) {
        const double t = static_cast<double>(index) / static_cast<double>(steps);
        const double angle = t * turns * 2.0 * kPi;
        const double radius = start_radius + t * (end_radius - start_radius);
        points.push_back(Point{cx + radius * std::cos(angle), cy + radius * std::sin(angle)});
    }
    return points;
}

std::vector<Point> gear_points(int teeth, double outer, double inner, double cx, double cy) {
    std::vector<Point> points;
    const double step = 2.0 * kPi / teeth;
    for (int index = 0; index < teeth; ++index) {
        const double base = index * step;
        const std::pair<double, double> profile[4] = {{0.0, inner},
                                                      {step * 0.2, outer},
                                                      {step * 0.5, outer},
                                                      {step * 0.7, inner}};
        for (const auto& [offset, radius] : profile) {
            const double angle = base + offset;
            points.push_back(
                Point{cx + radius * std::cos(angle), cy + radius * std::sin(angle)});
        }
    }
    return points;
}

std::vector<Point> cross_points(double size, double thickness, double cx, double cy) {
    const double half_size = size / 2.0;
    const double half_thickness = thickness / 2.0;
    return {Point{cx - half_thickness, cy - half_size},
            Point{cx + half_thickness, cy - half_size},
            Point{cx + half_thickness, cy - half_thickness},
            Point{cx + half_size, cy - half_thickness},
            Point{cx + half_size, cy + half_thickness},
            Point{cx + half_thickness, cy + half_thickness},
            Point{cx + half_thickness, cy + half_size},
            Point{cx - half_thickness, cy + half_size},
            Point{cx - half_thickness, cy + half_thickness},
            Point{cx - half_size, cy + half_thickness},
            Point{cx - half_size, cy - half_thickness},
            Point{cx - half_thickness, cy - half_thickness}};
}

std::vector<Point> diamond_points(double width, double height, double cx, double cy) {
    return {Point{cx, cy - height / 2.0}, Point{cx + width / 2.0, cy},
            Point{cx, cy + height / 2.0}, Point{cx - width / 2.0, cy}};
}

/// The walk every filled shape in the library shares: lift, move to the first
/// vertex, draw around it, close, and return to where the turtle started.
void trace_polygon(Machine& machine, const std::vector<Point>& points, bool fill) {
    const double origin_x = machine.pose().x();
    const double origin_y = machine.pose().y();
    if (fill) {
        machine.begin_fill();
    }
    machine.pen_up();
    machine.move_to(points.front().x, points.front().y);
    machine.pen_down();
    for (std::size_t index = 1; index < points.size(); ++index) {
        machine.move_to(points[index].x, points[index].y);
    }
    machine.move_to(points.front().x, points.front().y);
    if (fill) {
        machine.end_fill();
    }
    machine.pen_up();
    machine.move_to(origin_x, origin_y);
    machine.pen_down();
}

}  // namespace

std::string draw_shape(Machine& machine, std::string_view raw_args) {
    const std::vector<std::string> fields = split_plain(raw_args, ',');
    if (fields.size() < 2) {
        return "❌ SHAPE requires name and size\n";
    }
    std::string name = upper(trim(fields[0]));
    int parameter = 0;
    bool has_parameter = false;
    const std::size_t space = name.find(' ');
    if (space != std::string::npos) {
        const std::optional<long> parsed = [&]() -> std::optional<long> {
            double value = 0.0;
            if (!parse_float(name.substr(space + 1), value)) {
                return std::nullopt;
            }
            return static_cast<long>(value);
        }();
        name = name.substr(0, space);
        if (parsed.has_value()) {
            parameter = static_cast<int>(*parsed);
            has_parameter = true;
        }
    }
    double size = 0.0;
    try {
        size = machine.eval(trim(fields[1]));
    } catch (const std::exception& error) {
        return "❌ SHAPE error: " + std::string{error.what()} + "\n";
    }
    bool fill = false;
    if (fields.size() > 2) {
        const std::string flag = upper(trim(fields[2]));
        fill = !flag.empty() && flag != "0" && flag != "FALSE";
    }
    const double centre_x = machine.pose().x();
    const double centre_y = machine.pose().y();


    if (name == "POLYGON") {
        const int sides = has_parameter ? parameter : 6;
        if (sides < 3) {
            return "❌ Polygon needs at least 3 sides\n";
        }
        trace_polygon(machine, polygon_points(sides, size, centre_x, centre_y), fill);
        return "🔷 Drew " + std::to_string(sides) + "-sided polygon\n";
    }
    if (name == "STAR") {
        const int arms = has_parameter ? parameter : 5;
        if (arms < 3) {
            return "❌ Star needs at least 3 points\n";
        }
        trace_polygon(machine,
                      star_points(arms, size, size * 0.4, centre_x, centre_y), fill);
        return "⭐ Drew " + std::to_string(arms) + "-pointed star\n";
    }
    if (name == "HEART") {
        trace_polygon(machine, heart_points(size, centre_x, centre_y), fill);
        return "❤️ Drew heart\n";
    }
    if (name == "ARROW") {
        trace_polygon(machine,
                      arrow_points(size, size / 3.0, centre_x, centre_y,
                                   machine.pose().heading()),
                      fill);
        return "➡️ Drew arrow\n";
    }
    if (name == "SPIRAL") {
        const int turns = has_parameter ? parameter : 3;
        const std::vector<Point> points =
            spiral_points(static_cast<double>(turns), 5.0, size, centre_x, centre_y);
        machine.pen_up();
        machine.move_to(points.front().x, points.front().y);
        machine.pen_down();
        for (std::size_t index = 1; index < points.size(); ++index) {
            machine.move_to(points[index].x, points[index].y);
        }
        return "🌀 Drew spiral (" + std::to_string(turns) + " turns)\n";
    }
    if (name == "GEAR") {
        const int teeth = has_parameter ? parameter : 12;
        trace_polygon(machine, gear_points(teeth, size, size * 0.7, centre_x, centre_y), fill);
        return "⚙️ Drew gear (" + std::to_string(teeth) + " teeth)\n";
    }
    if (name == "CROSS") {
        trace_polygon(machine, cross_points(size, size / 3.0, centre_x, centre_y), fill);
        return "✚ Drew cross\n";
    }
    if (name == "DIAMOND") {
        trace_polygon(machine, diamond_points(size, size * 1.5, centre_x, centre_y), fill);
        return "💎 Drew diamond\n";
    }
    return "❌ Unknown shape: " + name +
           ". Available: polygon, star, heart, arrow, spiral, gear, cross, diamond\n";
}

std::string draw_fractal(Machine& machine, std::string_view raw_args) {
    const std::vector<std::string> fields = split_plain(raw_args, ',');
    if (fields.empty() || trim(fields[0]).empty()) {
        return "ℹ️ Available fractals: " + preset_names() + "\n";
    }
    const std::string name = upper(trim(fields[0]));
    int iterations = 4;
    double step = 10.0;
    try {
        if (fields.size() > 1) {
            iterations = static_cast<int>(machine.eval(trim(fields[1])));
        }
        if (fields.size() > 2) {
            step = machine.eval(trim(fields[2]));
        }
    } catch (const std::exception& error) {
        return "❌ FRACTAL error: " + std::string{error.what()} + "\n";
    }
    iterations = std::max(1, std::min(iterations, 12));

    const Preset* preset = find_preset(name);
    if (preset == nullptr) {
        return "❌ Unknown fractal: " + name + ". Available: " + preset_names() + "\n";
    }
    const std::string program = expand(*preset, iterations);

    struct SavedPose {
        double x = 0.0;
        double y = 0.0;
        double heading = 0.0;
    };
    std::vector<SavedPose> stack;
    long moves = 0;
    for (const char character : program) {
        const bool draws = character == 'F' || character == 'G' || character == '0' ||
                           character == '1' || (character >= '6' && character <= '9');
        if (draws) {
            machine.step_forward(step);
            ++moves;
        } else if (character == 'f' || character == 'g') {
            machine.pen_up();
            machine.step_forward(step);
            machine.pen_down();
            ++moves;
        } else if (character == '+') {
            machine.turn_right(preset->angle);
        } else if (character == '-') {
            machine.turn_left(preset->angle);
        } else if (character == '|') {
            machine.turn_right(180.0);
        } else if (character == '[') {
            stack.push_back(SavedPose{machine.pose().x(), machine.pose().y(),
                                      machine.pose().heading()});
        } else if (character == ']') {
            if (!stack.empty()) {
                const SavedPose saved = stack.back();
                stack.pop_back();
                machine.pen_up();
                machine.move_to(saved.x, saved.y);
                machine.set_heading_degrees(saved.heading);
                machine.pen_down();
            }
        }
    }
    return "🌿 L-System rendered (" + std::to_string(moves) + " moves)\n";
}

std::string create_particle(Machine& machine, std::string_view raw_args) {
    const std::vector<std::string> fields = split_plain(raw_args, ',');
    if (fields.size() < 3) {
        return "❌ PARTICLE requires effect, x, y\n";
    }
    const std::string effect = upper(trim(fields[0]));
    try {
        const double x = machine.eval(trim(fields[1]));
        const double y = machine.eval(trim(fields[2]));
        if (fields.size() > 3) {
            (void)machine.eval(trim(fields[3]));  // intensity, unused without a canvas
        }
        return "✨ Created " + effect + " effect at (" + python_number_text(x) + ", " +
               python_number_text(y) + ")\n";
    } catch (const std::exception& error) {
        return "❌ PARTICLE error: " + std::string{error.what()} + "\n";
    }
}

std::string play_music(Machine& machine, std::string_view raw_args) {
    (void)machine;
    std::string music = trim(raw_args);
    if (music.size() >= 2 && music.front() == '"' && music.back() == '"') {
        music = music.substr(1, music.size() - 2);
    }
    if (music.empty()) {
        return "❌ PLAY requires MML string\n";
    }
    // The notes go to the host's synth; the acknowledgement below is the
    // reference's text and stays exactly as it was, so the parity gate still
    // compares equal. Until this line was added, PLAY printed a tune and played
    // silence -- the reference does the same, because its speech and audio were
    // optional Python backends that the retired build never shipped.
    machine.turtle().play_music(music.c_str());
    std::string shown = music.size() > 30 ? music.substr(0, 30) : music;
    if (music.size() > 30) {
        shown += "...";
    }
    return "🎵 Playing: " + shown + "\n";
}

std::string speak(Machine& machine, std::string_view raw_args) {
    std::string text = trim(raw_args);
    if (text.size() >= 2 && text.front() == '"' && text.back() == '"') {
        text = text.substr(1, text.size() - 2);
    } else {
        const auto found = machine.interpreter().variables().find(upper(text));
        if (found != machine.interpreter().variables().end() && found->second.is_text()) {
            text = found->second.text();
        }
    }
    if (text.empty()) {
        return "❌ SAY requires text\n";
    }
    // The reference refuses when its optional `pyttsx3` backend is absent, and
    // this build has no speech synthesiser at all. Saying "🗣️ HELLO" would be an
    // acknowledgement of something that did not happen -- the exact lie the
    // sound commands used to tell -- so the refusal is the same sentence the
    // reference gives, and the interpreter adds the `[line N]` annotation.
    return "❌ Speech synthesis not available\n";
}

}  // namespace timewarp::lang::basic
