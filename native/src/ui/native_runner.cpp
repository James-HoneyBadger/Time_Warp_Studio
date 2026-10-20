// SPDX-License-Identifier: MIT

#include "timewarp/ui/native_runner.hpp"

#include "timewarp/audio/music.hpp"
#include "timewarp/lang/debug.hpp"
#include "timewarp/lang/executor.hpp"
#include "timewarp/ui/sound_player.hpp"

#include <chrono>
#include <utility>

namespace timewarp::ui {

using timewarp::canvas::CanvasCommand;
using timewarp::canvas::CanvasCommandKind;
using timewarp::canvas::make_circle;
using timewarp::canvas::make_color;
using timewarp::canvas::make_command;
using timewarp::canvas::make_draw_rect;
using timewarp::canvas::make_motion;
using timewarp::canvas::make_palette_color;
using timewarp::canvas::make_position;
using timewarp::canvas::make_rgb;
using timewarp::canvas::make_svga_mode;
using timewarp::lang::Point;
using timewarp::lang::Rgb;

namespace {

/// A colour as the packed integer a canvas command carries.
[[nodiscard]] constexpr std::uint32_t colour_bits(Rgb colour) {
    return timewarp::canvas::make_rgb(colour.red, colour.green, colour.blue);
}

}  // namespace

std::optional<timewarp::lang::Language> nativeLanguage(Language language) {
    switch (language) {
        case Language::Basic:
            return timewarp::lang::Language::Basic;
        case Language::Pilot:
            return timewarp::lang::Language::Pilot;
        case Language::Logo:
            return timewarp::lang::Language::Logo;
        case Language::Comal:
            return timewarp::lang::Language::Comal;
        case Language::Rexx:
            return timewarp::lang::Language::Rexx;
        case Language::Scheme:
            return timewarp::lang::Language::Scheme;
        case Language::Focal:
            return timewarp::lang::Language::Focal;
        case Language::Chip8:
            return timewarp::lang::Language::Chip8;
        case Language::Apl:
            return timewarp::lang::Language::Apl;
        case Language::Smalltalk:
            return timewarp::lang::Language::Smalltalk;
        case Language::Unknown:
            return std::nullopt;
    }
    return std::nullopt;
}

NativeCanvasHost::NativeCanvasHost(timewarp::canvas::CanvasCommandQueue& queue)
    : queue_{queue} {
    // The reference's surface colours. CHIP-8 sets slots 1 and 2 before it asks
    // for either, but a program that remembers a slot without setting it must
    // still get the colour the reference would have given it.
    palette_[0] = Rgb{255, 255, 255};
    palette_[1] = Rgb{16, 16, 20};
    palette_[2] = Rgb{140, 255, 140};
}

void NativeCanvasHost::submit(const CanvasCommand& command) {
    ++submitted_;
    if (!timewarp::canvas::accepted(queue_.push(command))) {
        return;
    }
    if (wakeup_) {
        wakeup_();
    }
}

void NativeCanvasHost::submit_text(CanvasCommand command, std::string_view text) {
    ++submitted_;
    if (!timewarp::canvas::accepted(queue_.push(command, text))) {
        return;
    }
    if (wakeup_) {
        wakeup_();
    }
}

void NativeCanvasHost::submit_points(CanvasCommand command,
                                     const std::vector<double>& points) {
    ++submitted_;
    if (!timewarp::canvas::accepted(queue_.push(command, {}, points))) {
        return;
    }
    if (wakeup_) {
        wakeup_();
    }
}

void NativeCanvasHost::sync_canvas_pose() {
    if (!canvas_pose_dirty_) {
        return;
    }
    canvas_pose_dirty_ = false;
    // The canvas keeps its own turtle, and a 3-D excursion leaves it somewhere
    // the 2-D commands cannot describe. Putting it back costs three commands
    // and only happens after such an excursion.
    submit(make_command(CanvasCommandKind::PenUp));
    const Point projected = pose_.project();
    submit(make_position(projected.x, projected.y));
    submit(make_motion(CanvasCommandKind::SetHeading, pose_.heading()));
    if (pen_down_) {
        submit(make_command(CanvasCommandKind::PenDown));
    }
}

void NativeCanvasHost::forward(double distance) {
    if (pose_.is_3d()) {
        const Point from = pose_.project();
        pose_.move(distance);
        const Point to = pose_.project();
        canvas_pose_dirty_ = true;
        if (!pen_down_ || distance == 0.0) {
            return;
        }
        submit_points(timewarp::canvas::make_polyline(
                          CanvasCommandKind::DrawPolyline, colour_bits(pen_), pen_width_),
                      {from.x, from.y, to.x, to.y});
        return;
    }
    sync_canvas_pose();
    pose_.move(distance);
    submit(make_motion(CanvasCommandKind::Forward, distance));
}

void NativeCanvasHost::back(double distance) {
    if (pose_.is_3d()) {
        forward(-distance);
        return;
    }
    sync_canvas_pose();
    pose_.move(-distance);
    submit(make_motion(CanvasCommandKind::Back, distance));
}

void NativeCanvasHost::right(double degrees) {
    pose_.turn(degrees);
    submit(make_motion(CanvasCommandKind::Right, degrees));
}

void NativeCanvasHost::left(double degrees) {
    pose_.turn(-degrees);
    submit(make_motion(CanvasCommandKind::Left, degrees));
}

void NativeCanvasHost::goto_xy(double x, double y) {
    if (pose_.is_3d()) {
        // The reference's `goto` appends a line between the *unprojected*
        // coordinates, so a `SETPOS` while the turtle is pitched draws where
        // the numbers say rather than where the projection would put it.
        if (pen_down_) {
            submit_points(timewarp::canvas::make_polyline(CanvasCommandKind::DrawPolyline,
                                                         colour_bits(pen_), pen_width_),
                          {pose_.x(), pose_.y(), x, y});
        }
        pose_.go_to(x, y);
        canvas_pose_dirty_ = true;
        return;
    }
    sync_canvas_pose();
    pose_.go_to(x, y);
    submit(make_position(x, y));
}

void NativeCanvasHost::set_heading(double degrees) {
    pose_.set_heading(degrees);
    canvas_pose_dirty_ = false;
    submit(make_motion(CanvasCommandKind::SetHeading, degrees));
}

void NativeCanvasHost::home() {
    pose_.go_to(0.0, 0.0);
    pose_.set_heading(0.0);
    canvas_pose_dirty_ = false;
    submit(make_command(CanvasCommandKind::Home));
}

void NativeCanvasHost::clear() { submit(make_command(CanvasCommandKind::Clear)); }

void NativeCanvasHost::reset() {
    pose_.home();
    pen_ = Rgb{255, 255, 255};
    pen_width_ = 2.0;
    pen_down_ = true;
    canvas_pose_dirty_ = false;
    submit(make_command(CanvasCommandKind::Reset));
    // `Reset` takes the canvas's own background with it, and the reference's
    // `CS` leaves the background alone; putting it back is the difference.
    submit(make_color(CanvasCommandKind::SetBackgroundColor,
                      make_rgb(background_.red, background_.green, background_.blue)));
}

void NativeCanvasHost::enable_3d() {
    pose_.enable_3d();
    canvas_pose_dirty_ = true;
}

void NativeCanvasHost::disable_3d() {
    pose_.disable_3d();
    canvas_pose_dirty_ = true;
}

void NativeCanvasHost::pitch(double degrees) {
    pose_.pitch(degrees);
    canvas_pose_dirty_ = true;
}

void NativeCanvasHost::roll(double degrees) {
    pose_.roll(degrees);
    canvas_pose_dirty_ = true;
}

void NativeCanvasHost::pen_up() {
    pen_down_ = false;
    submit(make_command(CanvasCommandKind::PenUp));
}

void NativeCanvasHost::pen_down() {
    pen_down_ = true;
    submit(make_command(CanvasCommandKind::PenDown));
}

void NativeCanvasHost::set_pen_width(double width) {
    pen_width_ = width;
    submit(make_motion(CanvasCommandKind::SetPenWidth, width));
}

void NativeCanvasHost::set_color(Rgb colour) {
    pen_ = colour;
    submit(make_color(CanvasCommandKind::SetPenColor,
                      make_rgb(colour.red, colour.green, colour.blue)));
}

void NativeCanvasHost::set_background(Rgb colour) {
    background_ = colour;
    submit(make_color(CanvasCommandKind::SetBackgroundColor,
                      make_rgb(colour.red, colour.green, colour.blue)));
}

void NativeCanvasHost::begin_run() {
    pen_ = Rgb{255, 255, 255};
    pen_width_ = 2.0;
    pen_down_ = true;
    // The theme's surface, unless the run before this one had the program name a
    // background: `background_` is mutated by `set_background`, so a fresh run
    // must reset it from the default rather than inherit the last program's.
    background_ = default_background_;
    pose_.home();
    canvas_pose_dirty_ = false;
    submit(make_color(CanvasCommandKind::SetPenColor, make_rgb(255, 255, 255)));
    submit(make_motion(CanvasCommandKind::SetPenWidth, 2.0));
    submit(make_color(CanvasCommandKind::SetBackgroundColor,
                      make_rgb(background_.red, background_.green, background_.blue)));
}

void NativeCanvasHost::circle(double radius, double extent) {
    submit(make_circle(radius, extent));
}

void NativeCanvasHost::fill_last_shape(Rgb fill) {
    submit(make_color(CanvasCommandKind::FillLastShape,
                      make_rgb(fill.red, fill.green, fill.blue)));
}

Rgb NativeCanvasHost::pen_colour() const { return pen_; }

void NativeCanvasHost::draw_rect(double x0, double y0, double x1, double y1, Rgb fill) {
    CanvasCommand command = make_draw_rect(x0, y0, x1 - x0, y1 - y0);
    command.flags |= timewarp::canvas::kCanvasFlagFilled;
    command.color = make_rgb(fill.red, fill.green, fill.blue);
    submit(command);
}

void NativeCanvasHost::draw_text(double x, double y, const char* text, int font_size) {
    CanvasCommand command = make_command(CanvasCommandKind::DrawText);
    command.x = x;
    command.y = y;
    command.w = font_size;
    submit_text(command, text);
}

void NativeCanvasHost::draw_point(double x, double y, Rgb colour) {
    submit(timewarp::canvas::make_draw_point(x, y,
                                             make_rgb(colour.red, colour.green, colour.blue)));
}

void NativeCanvasHost::draw_polygon(const std::vector<Point>& points, Rgb stroke, Rgb fill,
                                    double width, bool filled) {
    if (points.size() < 2) {
        return;
    }
    std::vector<double> flat;
    flat.reserve(points.size() * 2);
    for (const Point& point : points) {
        flat.push_back(point.x);
        flat.push_back(point.y);
    }
    CanvasCommand command = timewarp::canvas::make_polyline(
        CanvasCommandKind::DrawPolyline, colour_bits(stroke), width, filled, filled);
    if (filled) {
        // Same convention as the bridge's `poly` command: the fill colour rides
        // in `b`, next to the flag that says there is one.
        command.b = static_cast<double>(colour_bits(fill));
    }
    submit_points(command, flat);
}

void NativeCanvasHost::show_turtle() { submit(make_command(CanvasCommandKind::ShowTurtle)); }

void NativeCanvasHost::hide_turtle() { submit(make_command(CanvasCommandKind::HideTurtle)); }

void NativeCanvasHost::set_svga_mode(int width, int height) {
    submit(make_svga_mode(width, height));
}

void NativeCanvasHost::set_palette_color(int index, Rgb colour) {
    palette_[index] = colour;
    submit(make_palette_color(index, make_rgb(colour.red, colour.green, colour.blue)));
}

// -- sound and keyboard -----------------------------------------------------

void NativeCanvasHost::beep(int milliseconds) {
    if (sound_ != nullptr) {
        // The PC speaker's own bell is a square wave around 880Hz, which is also
        // what `BEEP` means on the machines these dialects come from.
        sound_->note(880.0, milliseconds > 0 ? milliseconds : 120);
    }
}

void NativeCanvasHost::tone(double frequency, int milliseconds, const char* waveform) {
    if (sound_ == nullptr) {
        return;
    }
    timewarp::audio::Waveform shape = timewarp::audio::Waveform::Square;
    const std::string name = waveform != nullptr ? waveform : "";
    if (name == "sine") {
        shape = timewarp::audio::Waveform::Sine;
    } else if (name == "triangle") {
        shape = timewarp::audio::Waveform::Triangle;
    } else if (name == "noise") {
        shape = timewarp::audio::Waveform::Noise;
    }
    sound_->note(frequency, milliseconds, shape);
}

void NativeCanvasHost::play_music(const char* notation) {
    if (sound_ == nullptr || notation == nullptr) {
        return;
    }
    timewarp::audio::ParseResult result;
    const std::vector<timewarp::audio::Tone> tones =
        timewarp::audio::parse_play(notation, &result);
    // A partly-malformed tune still plays what parsed: the acknowledgement the
    // language printed is the reference's, so the sound is a best effort.
    sound_->play(tones);
}

void NativeCanvasHost::stop_sound() {
    if (sound_ != nullptr) {
        sound_->stop();
    }
}

bool NativeCanvasHost::audio_available() const {
    return sound_ != nullptr && sound_->available();
}

void NativeCanvasHost::push_keystroke(char key) {
    const std::lock_guard<std::mutex> lock{keys_mutex_};
    keys_.push_back(key);
}

std::string NativeCanvasHost::take_keystrokes() {
    const std::lock_guard<std::mutex> lock{keys_mutex_};
    std::string keys;
    keys.swap(keys_);
    return keys;
}

Rgb NativeCanvasHost::resolve_color(int index) const {
    const auto found = palette_.find(index);
    if (found != palette_.end()) {
        return found->second;
    }
    if (index >= 0 && index <= 255) {
        // The reference treats an unset slot as the grey of its own number.
        const int grey = index;
        return Rgb{grey, grey, grey};
    }
    return Rgb{255, 255, 255};
}

NativeRunner::NativeRunner(timewarp::lang::Language language,
                           timewarp::canvas::CanvasCommandQueue& queue, QObject* parent)
    : QThread{parent}, language_{language}, host_{std::make_unique<NativeCanvasHost>(queue)} {}

NativeRunner::~NativeRunner() {
    request_stop();
    if (isRunning()) {
        wait();
    }
}

void NativeRunner::load_program(const QString& source) {
    source_ = source;
}

void NativeRunner::set_wakeup(std::function<void()> wakeup) {
    host_->set_wakeup(std::move(wakeup));
}

void NativeRunner::set_surface_color(timewarp::lang::Rgb colour) {
    host_->set_default_background(colour);
}

void NativeRunner::set_sound_player(SoundPlayer* player) {
    host_->set_sound_player(player);
}

void NativeRunner::push_key(char key) { host_->push_keystroke(key); }

void NativeRunner::request_stop() {
    stop_requested_.store(true, std::memory_order_release);
    {
        const std::lock_guard<std::mutex> lock{interpreter_mutex_};
        if (interpreter_ != nullptr) {
            interpreter_->request_stop();
        }
        // A paused run is blocked inside the session, so stopping it means telling
        // the session too -- otherwise Stop would leave a thread asleep holding the
        // interpreter and the window would never hear the run finish.
        if (debug_ != nullptr) {
            debug_->request_stop();
        }
    }
    {
        const std::lock_guard<std::mutex> lock{input_mutex_};
        waiting_input_ = false;
    }
    input_ready_.notify_all();
}

void NativeRunner::set_breakpoints(const std::set<std::size_t>& lines) {
    breakpoints_ = lines;
}

void NativeRunner::request_continue() {
    const std::lock_guard<std::mutex> lock{interpreter_mutex_};
    if (debug_ != nullptr) {
        debug_->request_continue();
        emit resumed();
    }
}

void NativeRunner::request_step() {
    const std::lock_guard<std::mutex> lock{interpreter_mutex_};
    if (debug_ != nullptr) {
        debug_->request_step();
        emit resumed();
    }
}

bool NativeRunner::isPaused() const {
    const std::lock_guard<std::mutex> lock{interpreter_mutex_};
    return debug_ != nullptr && debug_->paused();
}

void NativeRunner::answerInput(const QString& text) {
    {
        const std::lock_guard<std::mutex> lock{input_mutex_};
        answer_ = text;
        waiting_input_ = false;
    }
    input_ready_.notify_all();
}

void NativeRunner::publishVariables(const timewarp::lang::Interpreter& engine) {
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    variables_.clear();
    variables_.reserve(engine.variables().size());
    for (const auto& [name, value] : engine.variables()) {
        variables_.push_back(VariableRow{QString::fromStdString(name),
                                         QString::fromLatin1(value.type_name()),
                                         QString::fromStdString(value.render())});
    }
}

std::vector<NativeRunner::VariableRow> NativeRunner::variables() const {
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    return variables_;
}

std::vector<timewarp::lang::LineProfile> NativeRunner::profile() const {
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    return profile_;
}

std::uint64_t NativeRunner::steps() const noexcept {
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    return steps_;
}

double NativeRunner::elapsedMs() const noexcept {
    const std::lock_guard<std::mutex> lock{snapshot_mutex_};
    return elapsed_ms_;
}

std::uint64_t NativeRunner::drawingCommands() const noexcept {
    return host_ != nullptr ? host_->submitted() : 0;
}

std::string NativeRunner::read_input(const std::string& prompt) {
    std::unique_lock<std::mutex> lock{input_mutex_};
    if (stop_requested_.load(std::memory_order_acquire)) {
        return {};
    }
    waiting_input_ = true;
    answer_.clear();
    const QString question = QString::fromStdString(prompt);
    lock.unlock();
    emit inputRequested(question);
    lock.lock();
    while (waiting_input_ && !stop_requested_.load(std::memory_order_acquire)) {
        // A bounded wait, because C++20's condition_variable has no way to
        // observe an atomic without a notification. The stop path notifies, so
        // this is a safety net rather than the mechanism.
        input_ready_.wait_for(lock, std::chrono::milliseconds{50});
    }
    waiting_input_ = false;
    if (stop_requested_.load(std::memory_order_acquire)) {
        return {};
    }
    return answer_.toStdString();
}

void NativeRunner::run() {
    auto engine = std::make_unique<timewarp::lang::Interpreter>(language_, *host_);
    // A fresh session per run: a stop requested for the previous run must not make
    // this one exit at its first statement.
    auto session = std::make_unique<timewarp::lang::DebugSession>();
    session->set_breakpoints(breakpoints_);
    timewarp::lang::Interpreter* engine_ptr = engine.get();
    session->on_paused = [this, engine_ptr](int line) {
        // While the run is stopped, the Variables panel has to show the state it
        // is stopped in rather than the state it will end in.
        publishVariables(*engine_ptr);
        emit paused(line);
    };
    {
        const std::lock_guard<std::mutex> lock{interpreter_mutex_};
        interpreter_ = engine.get();
        debug_ = std::move(session);
        engine->set_debug_session(debug_.get());
    }
    engine->set_output_callback([this](const std::string& line) {
        emit outputAvailable(QString::fromStdString(line));
    });
    engine->set_input_callback(
        [this](const std::string& prompt) { return read_input(prompt); });
    engine->set_profiling(profiling_);

    // A binary ROM was decoded through the codec that preserves every byte
    // (including 0x0D, which a QTextDocument would normalise); a listing is
    // UTF-8 like every other document.
    const bool binary_rom = language_ == timewarp::lang::Language::Chip8 &&
                            source_.contains(QChar{0});
    engine->load_program(binary_rom ? romTextToBytes(source_).toStdString()
                                    : source_.toStdString());
    host_->begin_run();
    engine->run();
    host_->flush();

    publishVariables(*engine);
    {
        const std::lock_guard<std::mutex> lock{snapshot_mutex_};
        steps_ = engine->steps_used();
        elapsed_ms_ = engine->elapsed_ms();
        // Copied, not referenced: the interpreter dies with this run, and the
        // report outlives it.
        profile_ = engine->line_profiles();
    }

    const bool completed = !stop_requested_.load(std::memory_order_acquire) &&
                           !engine->timed_out();
    const QString error = QString::fromStdString(engine->error());

    {
        const std::lock_guard<std::mutex> lock{interpreter_mutex_};
        interpreter_ = nullptr;
        debug_.reset();
    }
    emit run_finished(completed, error);
}

}  // namespace timewarp::ui
