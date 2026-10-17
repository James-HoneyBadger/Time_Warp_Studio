// SPDX-License-Identifier: MIT
//
// Runs a ported language on a background thread and feeds the canvas.
//
// The counterpart of `PythonWorker` for languages that no longer need Python.
// It keeps the same shape as the bridge -- streamed output, a prompt the GUI
// answers, one terminal signal -- so the window's execution path does not have
// to care which engine ran the program.
//
// `timewarp_lang` itself stays Qt-free; this adapter is where the engine meets
// QThread and the canvas queue.

#pragma once

#include "timewarp/canvas/command_queue.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/language.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/ui/language.hpp"

#include <QPointer>
#include <QString>
#include <QThread>

#include <atomic>
#include <condition_variable>
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace timewarp::ui {

/// The native engine for a UI language, or empty when it still runs through
/// the bridge. This is the one place the two enums are paired.
[[nodiscard]] std::optional<timewarp::lang::Language> nativeLanguage(Language language);

/// A `lang::TurtleHost` that queues canvas commands.
class NativeCanvasHost final : public timewarp::lang::TurtleHost {
public:
    using Wakeup = std::function<void()>;

    explicit NativeCanvasHost(timewarp::canvas::CanvasCommandQueue& queue);

    void set_wakeup(Wakeup wakeup) { wakeup_ = std::move(wakeup); }
    void flush() { queue_.flush(); }

    /// The surface a run starts on: the reference's dark background and white
    /// pen. Without this a program that draws before naming a colour would put
    /// white ink on the IDE's white canvas -- a run that reports four strokes
    /// and shows nothing.
    void begin_run();

    [[nodiscard]] std::uint64_t submitted() const noexcept { return submitted_; }

    void forward(double distance) override;
    void back(double distance) override;
    void right(double degrees) override;
    void left(double degrees) override;
    void goto_xy(double x, double y) override;
    void set_heading(double degrees) override;
    void home() override;
    void clear() override;
    void reset() override;
    void enable_3d() override;
    void disable_3d() override;
    void pitch(double degrees) override;
    void roll(double degrees) override;
    void pen_up() override;
    void pen_down() override;
    void set_pen_width(double width) override;
    void set_color(timewarp::lang::Rgb colour) override;
    void set_background(timewarp::lang::Rgb colour) override;
    void circle(double radius, double extent = 360.0) override;
    void draw_rect(double x0, double y0, double x1, double y1,
                   timewarp::lang::Rgb fill) override;
    void draw_text(double x, double y, const char* text, int font_size = 12) override;
    void draw_point(double x, double y, timewarp::lang::Rgb colour) override;
    void fill_last_shape(timewarp::lang::Rgb fill) override;
    void draw_polygon(const std::vector<timewarp::lang::Point>& points,
                      timewarp::lang::Rgb stroke, timewarp::lang::Rgb fill,
                      double width, bool filled) override;
    [[nodiscard]] timewarp::lang::Rgb pen_colour() const override;
    void show_turtle() override;
    void hide_turtle() override;
    void set_svga_mode(int width, int height) override;
    void set_palette_color(int index, timewarp::lang::Rgb colour) override;
    [[nodiscard]] timewarp::lang::Rgb resolve_color(int index) const override;

private:
    void submit(const timewarp::canvas::CanvasCommand& command);
    void submit_text(timewarp::canvas::CanvasCommand command, std::string_view text);
    void submit_points(timewarp::canvas::CanvasCommand command,
                       const std::vector<double>& points);
    /// Puts the canvas's own turtle back where the 3-D pose projects to, after
    /// a 3-D excursion has left it somewhere the 2-D commands cannot describe.
    void sync_canvas_pose();

    timewarp::canvas::CanvasCommandQueue& queue_;
    Wakeup wakeup_;
    std::uint64_t submitted_ = 0;
    std::map<int, timewarp::lang::Rgb> palette_;
    timewarp::lang::Rgb pen_{255, 255, 255};
    /// The reference's canvas colour, remembered so a `CS` -- which resets the
    /// canvas, background included -- can put it back.
    timewarp::lang::Rgb background_{10, 10, 20};
    /// Where the turtle is, for the 3-D projection. In 2-D the queue's own
    /// motion commands carry the arithmetic; in 3-D the projection has to be
    /// computed here, because the canvas is a plane.
    timewarp::lang::TurtlePose pose_;
    bool pen_down_ = true;
    bool canvas_pose_dirty_ = false;
    double pen_width_ = 2.0;
};

class NativeRunner final : public QThread {
    Q_OBJECT

public:
    /// One binding the run left behind, for the Variables panel.
    struct VariableRow {
        QString name;
        QString type;
        QString value;
    };

    NativeRunner(timewarp::lang::Language language,
                 timewarp::canvas::CanvasCommandQueue& queue, QObject* parent = nullptr);
    ~NativeRunner() override;

    void load_program(const QString& source);
    void request_stop();
    void answerInput(const QString& text);

    /// Called from the runner thread whenever a drawing command reaches the
    /// canvas queue, so the window drains it. Without this a run that produces
    /// more commands than the queue holds stalls until something else wakes the
    /// GUI -- the queue has no consumer on its own.
    void set_wakeup(std::function<void()> wakeup);

    [[nodiscard]] std::vector<VariableRow> variables() const;
    [[nodiscard]] std::uint64_t steps() const noexcept;
    [[nodiscard]] double elapsedMs() const noexcept;
    [[nodiscard]] std::uint64_t drawingCommands() const noexcept;

signals:
    /// One line of program output, without a newline: the console adds it.
    void outputAvailable(const QString& text);
    /// The program is waiting for a line of input.
    void inputRequested(const QString& prompt);
    /// The run ended. `completed` is false when it was stopped.
    void run_finished(bool completed, const QString& error);

protected:
    void run() override;

private:
    std::string read_input(const std::string& prompt);

    timewarp::lang::Language language_;
    std::unique_ptr<NativeCanvasHost> host_;
    QString source_;

    mutable std::mutex interpreter_mutex_;
    /// Owned by `run()`; this pointer only exists so `request_stop` can reach
    /// the engine from the GUI thread. Null outside a run, under the mutex.
    timewarp::lang::Interpreter* interpreter_ = nullptr;

    mutable std::mutex snapshot_mutex_;
    std::vector<VariableRow> variables_;
    std::uint64_t steps_ = 0;
    double elapsed_ms_ = 0.0;

    std::mutex input_mutex_;
    std::condition_variable input_ready_;
    bool waiting_input_ = false;
    QString answer_;

    std::atomic<bool> stop_requested_{false};
};

}  // namespace timewarp::ui
