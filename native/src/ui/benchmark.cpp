// SPDX-License-Identifier: MIT

#include "timewarp/ui/benchmark.hpp"

#include <QStringList>

#include <optional>
#include <string>
#include <vector>

#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/ui/native_runner.hpp"

namespace timewarp::ui {

namespace {

/// Counts what a canvas would have been asked to draw, and nothing else.
///
/// A pen-down `forward` is one ink command, a shape is one, and a pen-up move is
/// none -- so two programs that draw the same picture count the same however many
/// times they reposition the turtle.
class CountingHost final : public timewarp::lang::TurtleHost {
public:
    int ink = 0;

    void forward(double) override { count(); }
    void back(double) override { count(); }
    void right(double /*degrees*/) override {}
    void left(double /*degrees*/) override {}
    void goto_xy(double, double) override { count(); }
    void set_heading(double /*degrees*/) override {}
    void home() override {}
    void clear() override { ++ink; }
    void reset() override { ++ink; }

    void enable_3d() override {}
    void disable_3d() override {}
    void pitch(double /*degrees*/) override {}
    void roll(double /*degrees*/) override {}

    void pen_up() override { down_ = false; }
    void pen_down() override { down_ = true; }
    void set_pen_width(double /*width*/) override {}
    void set_color(timewarp::lang::Rgb colour) override { pen_ = colour; }
    void set_background(timewarp::lang::Rgb /*colour*/) override {}

    void circle(double /*radius*/, double /*extent*/) override { ++ink; }
    void draw_rect(double, double, double, double, timewarp::lang::Rgb) override { ++ink; }
    void draw_text(double, double, const char*, int /*font_size*/) override { ++ink; }
    void draw_point(double, double, timewarp::lang::Rgb) override { ++ink; }
    void fill_last_shape(timewarp::lang::Rgb) override {}
    void draw_polygon(const std::vector<timewarp::lang::Point>&,
                      timewarp::lang::Rgb, timewarp::lang::Rgb, double, bool) override {
        ++ink;
    }

    [[nodiscard]] timewarp::lang::Rgb pen_colour() const override { return pen_; }

    void show_turtle() override {}
    void hide_turtle() override {}
    void set_svga_mode(int /*width*/, int /*height*/) override {}
    void set_palette_color(int /*index*/, timewarp::lang::Rgb /*colour*/) override {}
    [[nodiscard]] timewarp::lang::Rgb resolve_color(int /*index*/) const override {
        return {255, 255, 255};
    }

private:
    void count() {
        if (down_) {
            ++ink;
        }
    }

    bool down_ = true;
    timewarp::lang::Rgb pen_{255, 255, 255};
};

}  // namespace

BenchResult runHeadless(Language language, const QString& name, const QString& source) {
    BenchResult result;
    result.name = name;
    result.language = displayName(language);

    const std::optional<timewarp::lang::Language> engine = nativeLanguage(language);
    if (!engine.has_value()) {
        result.error = QStringLiteral("%1 has no native engine").arg(displayName(language));
        return result;
    }

    CountingHost host;
    timewarp::lang::Interpreter interpreter{*engine, host};
    std::vector<std::string> lines;
    interpreter.set_output_callback([&lines](const std::string& line) { lines.push_back(line); });
    // A program that asks a question gets the empty answer, which is what a
    // benchmark can honestly give it: there is nobody there to type.
    interpreter.set_input_callback([](const std::string&) { return std::string{}; });
    interpreter.load_program(source.toStdString());
    interpreter.run();

    // The engine's own measurement, so the table's "time" column is the same
    // number the Telemetry HUD shows for a window run.
    result.elapsed_ms = interpreter.elapsed_ms();
    result.ink_commands = host.ink;
    // An error is not always a thrown one: a bad statement is reported with the
    // engine's ❌ line and the loop carries on. A program that printed one did not
    // finish, whatever `error()` says.
    bool reported_error = false;
    for (const std::string& line : interpreter.output()) {
        if (line.find("\u274C") != std::string::npos) {
            reported_error = true;
            if (result.error.isEmpty()) {
                result.error = QString::fromStdString(line);
            }
        }
    }
    result.completed = interpreter.error().empty() && !interpreter.timed_out() &&
                       !reported_error;
    if (!interpreter.error().empty()) {
        result.error = QString::fromStdString(interpreter.error());
    }

    QStringList output;
    output.reserve(static_cast<int>(lines.size()));
    for (const std::string& line : lines) {
        output.append(QString::fromStdString(line));
    }
    result.output = output.join(QLatin1Char('\n'));
    return result;
}

QString comparisonTable(const QList<BenchResult>& results) {
    // Fixed-width columns and a header, because the point of the table is that two
    // rows can be compared by eye without adding the numbers up in your head.
    QString text = QStringLiteral("\u2500\u2500 Both panes \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n");
    text += QStringLiteral("%1  %2  %3  %4  %5\n")
                .arg(QStringLiteral("program"), -18)
                .arg(QStringLiteral("language"), -11)
                .arg(QStringLiteral("outcome"), -9)
                .arg(QStringLiteral("time"), 9)
                .arg(QStringLiteral("ink"), 5);
    for (const BenchResult& result : results) {
        const QString outcome = result.completed ? QStringLiteral("finished")
                                                 : QStringLiteral("stopped");
        text += QStringLiteral("%1  %2  %3  %4  %5\n")
                    .arg(result.name, -18)
                    .arg(result.language, -11)
                    .arg(outcome, -9)
                    .arg(QStringLiteral("%1 ms").arg(result.elapsed_ms, 0, 'f', 1), 9)
                    .arg(result.ink_commands, 5);
        if (!result.error.isEmpty()) {
            text += QStringLiteral("  %1: %2\n").arg(result.name, result.error);
        }
    }
    return text;
}

}  // namespace timewarp::ui
