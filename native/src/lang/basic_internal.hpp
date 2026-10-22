// SPDX-License-Identifier: MIT
//
// The internals of the BASIC executor: its state, the text helpers the
// reference leans on, and the two classes the statements are split across.
//
// The split mirrors the reference's own: `basic_text.cpp` is the string
// expression evaluator (`utils/string_evaluator.py`), `basic.cpp` is the
// statement dispatch (`languages/basic.py`) and `basic_features.cpp` is
// everything a statement reaches that is not the language itself -- shapes,
// fractals, media and the confined file service.

#pragma once

#include "timewarp/io/ports.hpp"
#include "timewarp/lang/basic.hpp"
#include "timewarp/lang/interpreter.hpp"
#include "timewarp/lang/raster.hpp"
#include "timewarp/lang/turtle_host.hpp"
#include "timewarp/lang/value.hpp"

#include <cstdio>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace timewarp::lang::basic {

/// A BASIC failure carried as an exception inside the executor.
///
/// The reference returns the error text from each handler; the port throws and
/// renders once, which is the same output with one place that spells the
/// prefix.
class BasicError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// -- text -----------------------------------------------------------------

[[nodiscard]] bool is_space(char character);
[[nodiscard]] std::string trim(std::string_view text);
[[nodiscard]] std::string rstrip(std::string_view text);
[[nodiscard]] std::string lstrip(std::string_view text);
[[nodiscard]] std::string upper(std::string_view text);
[[nodiscard]] std::string lower(std::string_view text);

/// `cmd == keyword` or `cmd` begins with `keyword` followed by a space.
///
/// `DOLLARS = 1` is an assignment, not a `DO` loop: a keyword has to end where
/// its name ends, or a learner who names a variable `RUNNER` loses it.
[[nodiscard]] bool is_keyword(std::string_view command, std::string_view keyword);

/// Text up to `: REM` or `:REM`, outside string literals.
[[nodiscard]] std::string strip_comment(std::string_view args);

/// Splits on `:` outside string literals.
[[nodiscard]] std::vector<std::string> split_statements(std::string_view command);

/// Splits on any of *separators*, outside quotes and parentheses.
[[nodiscard]] std::vector<std::string> split_top(std::string_view text,
                                                 std::string_view separators);

/// Python's `float(text)`: whole-string, surrounding space allowed.
[[nodiscard]] bool parse_float(std::string_view text, double& value);

/// `f"{value:g}"` -- six significant digits, the reference's PRINT format.
[[nodiscard]] std::string format_general(double value);
/// A number as PRINT shows it, honouring an `int`/`long` variable's type.
[[nodiscard]] std::string format_numeric(double value, std::string_view preferred);
/// Python's `str(number)`: `3.0` keeps its `.0` where C++ would print `3`.
[[nodiscard]] std::string printed_number(double value);

// -- the clock ------------------------------------------------------------

/// `TIME$` as `HH:MM:SS`, from the machine's local time.
///
/// The reference reads these from `features/game_support.py`, which is why a
/// program can print them at all; they are not constants, so the parity gate
/// cannot compare them directly -- a row asserts their shape (`LEN(TIME$)`) 
/// instead.
[[nodiscard]] std::string clock_time_text();
/// `DATE$` as `MM-DD-YYYY`, from the machine's local time.
[[nodiscard]] std::string clock_date_text();
/// `TIMER`: seconds since local midnight, fraction included.
[[nodiscard]] double clock_timer_seconds();

// -- colour packing --------------------------------------------------------

/// A pixel as one number, `&H00RRGGBB` -- what `POINT` answers and what a
/// captured sprite holds in its array.
[[nodiscard]] int pack_colour(Rgb colour);
/// The colour a packed number names. Channels outside 0-255 are clamped rather
/// than wrapped, so a hand-written array value cannot turn red into blue.
[[nodiscard]] Rgb unpack_colour(long value);

// -- state ----------------------------------------------------------------

struct ForFrame {
    std::string variable;
    double limit = 0.0;
    double step = 1.0;
    /// Index of the `FOR` line, which `NEXT` returns to.
    std::size_t body = 0;
};

/// One open block `IF`, and whether its body is being skipped.
struct IfFrame {
    bool skip = false;
    int depth = 0;
};

struct CallFrame {
    std::size_t return_line = 0;
    /// Empty for a `SUB`; the function's name for a `FUNCTION`.
    std::string function_name;
    /// Values the parameters overwrote, restored when the block returns.
    std::vector<std::pair<std::string, std::optional<Value>>> saved;
};

struct Definition {
    std::vector<std::string> parameters;
    std::size_t start = 0;
    std::size_t end = 0;
};

/// The BASIC file channel table, closed when the run ends.
struct FileHandle {
    FileHandle() = default;
    FileHandle(const FileHandle&) = delete;
    FileHandle& operator=(const FileHandle&) = delete;
    ~FileHandle();
    std::FILE* handle = nullptr;
};

/// A sprite a program captured with `SPRITE name, x, y, w, h`: the pixels, and
/// the hotspot the canvas positions when it is placed. Named sprites live here
/// rather than in the canvas because `SPRITE name, OFF` has to forget one, and
/// the part that forgets is the part that knows the program.
struct CapturedSprite {
    int width = 0;
    int height = 0;
    /// Where it was last placed, so `SPRITEMOVE` can move it by a delta and still
    /// keep the engine's raster in step with the picture.
    double x = 0.0;
    double y = 0.0;
    bool placed = false;
    /// Row-major from the top row, one colour per pixel.
    std::vector<Rgb> pixels;
};

struct State {
    /// DEFINT/DEFLNG/DEFSNG/DEFDBL/DEFSTR: initial letter to type name.
    std::map<char, std::string> defaults;
    std::vector<ForFrame> fors;
    std::vector<std::size_t> gosubs;
    std::vector<IfFrame> ifs;
    std::vector<std::size_t> whiles;
    std::vector<std::size_t> dos;
    std::vector<CallFrame> calls;
    std::map<std::string, std::vector<std::string>> string_arrays;
    /// `TYPE POINT / X AS INTEGER / END TYPE`: the name and its field defaults.
    std::map<std::string, std::map<std::string, Value>> types;
    /// `DIM P AS POINT`: the instances, by variable name.
    std::map<std::string, std::map<std::string, Value>> records;
    std::map<std::string, Definition> subs;
    std::map<std::string, Definition> functions;
    /// `DEF FNA(X) = ...`: the parameters and the body expression.
    std::map<std::string, std::pair<std::vector<std::string>, std::string>> user_fns;
    std::vector<std::string> data;
    std::size_t data_pointer = 0;
    bool scanned = false;
    std::string select_expression;
    bool in_select = false;
    std::map<int, int> memory;
    std::map<int, int> ports;
    std::map<int, Rgb> palette;
    std::map<int, std::shared_ptr<FileHandle>> files;
    /// Open serial and parallel ports, by channel number. A channel is `AS #n`, the
    /// same spelling a file uses, because a learner already knows that one. Shared
    /// pointers for the same reason `files` uses them: the map is copied with the
    /// state and a device handle must not be.
    std::map<int, std::shared_ptr<timewarp::io::SerialPort>> serial_ports;
    std::map<int, std::shared_ptr<timewarp::io::ParallelPort>> parallel_ports;
    /// `ON ERROR GOTO n`; 0 means no handler.
    int error_handler = 0;
    /// The program-line index of the statement that failed, for RESUME.
    long error_index = -1;
    bool timer_defined = false;
    bool timer_enabled = false;
    /// What `ON TIMER(n) GOSUB line` said, and when the next event is due. The two
    /// flags above used to be all there was: the statement acknowledged the setting
    /// and nothing ever read it, so the timer never fired.
    double timer_interval = 0.0;
    long timer_target = 0;
    double timer_next_due = 0.0;
    /// SCREEN's configuration, with the reference's defaults.
    int screen_mode = 0;
    int cols = 80;
    int rows = 25;
    int screen_width = 800;
    int screen_height = 600;
    /// The pixels a program can read back, once `SCREEN 1` has given it a
    /// screen. Empty in text mode, which is why `GET`, `PUT` and `POINT` have
    /// an error to report rather than a silent zero.
    Raster raster;
    /// The sprites `SPRITE` has captured, by upper-cased name.
    std::map<std::string, CapturedSprite> sprites;
    /// `JOYINIT` found a gamepad. `STICK`/`STRIG` answer 0 until it does, which
    /// is what the documented behaviour says and what keeps a machine with a
    /// joystick plugged in from changing a program that never asked for one.
    bool joystick_initialised = false;
};

// -- the string expression evaluator ---------------------------------------

/// `utils/string_evaluator.py`, ported.
///
/// It is a separate evaluator rather than an extension of the arithmetic one
/// because the reference's behaviour *is* this shape: it returns `str(expr)`
/// for anything it cannot read, which is why an unknown name can reach a
/// learner as source text rather than as an error.
class Strings {
public:
    Strings(Interpreter& interpreter, const State& state);

    /// Evaluates *expression* to text. Throws `BasicError` on a malformed call.
    [[nodiscard]] std::string evaluate(std::string_view expression);

private:
    using Argument = std::variant<std::string, double>;

    [[nodiscard]] std::string evaluate_expr(std::string_view expression);
    [[nodiscard]] Argument evaluate_arg(std::string_view argument);
    [[nodiscard]] std::string evaluate_function(std::string_view expression);
    [[nodiscard]] std::vector<std::string> parse_arguments(std::string_view text) const;
    [[nodiscard]] std::string concatenate(std::string_view expression, char op);

    [[nodiscard]] static bool looks_numeric(std::string_view text);
    [[nodiscard]] static std::string as_text(const Argument& argument);
    [[nodiscard]] std::string string_variable(std::string_view name) const;
    [[nodiscard]] std::string array_element(std::string_view name, std::string_view index) const;

    Interpreter& interpreter_;
    const State& state_;
};

// -- the statement machine -------------------------------------------------

class Machine {
public:
    Machine(Interpreter& interpreter, TurtleHost& turtle, State& state);

    std::string execute(std::string_view command);

    [[nodiscard]] Interpreter& interpreter() noexcept { return interpreter_; }
    [[nodiscard]] TurtleHost& turtle() noexcept { return turtle_; }
    [[nodiscard]] State& state() noexcept { return state_; }

    /// Arithmetic with `VAR.FIELD` expanded, as the reference rewrites the
    /// expression before handing it to its evaluator.
    [[nodiscard]] double eval(std::string_view expression);

    // -- drawing, used by the statement handlers and by the shape library ----
    [[nodiscard]] const TurtlePose& pose() const noexcept { return pose_; }
    [[nodiscard]] Rgb resolve_colour(std::string_view value, Rgb fallback);
    [[nodiscard]] Rgb palette_colour(int index) const;
    void move_to(double x, double y);
    void step_forward(double distance);
    void turn_right(double degrees);
    void turn_left(double degrees);
    void set_heading_degrees(double degrees);
    void pen_up();
    void pen_down();
    void begin_fill();
    void end_fill();

private:
    // -- variables ---------------------------------------------------------
    [[nodiscard]] std::pair<std::string, std::string> resolve(std::string_view name) const;
    void assign(std::string_view name, const Value& value);
    void assign(std::string_view name, double value);
    void assign(std::string_view name, const std::string& value);
    [[nodiscard]] std::optional<double> number_of(const std::string& base) const;
    [[nodiscard]] std::string text_of(std::string_view name) const;
    void validate_name(std::string_view name) const;
    void scan_program();

    // -- statements --------------------------------------------------------
    std::string print(const std::string& args);
    std::string print_using(const std::string& args);
    std::string print_to_file(const std::string& args);
    std::string line_input(const std::string& args);
    std::string let(const std::string& args);
    std::string input(const std::string& args);
    std::string conditional(const std::string& args);
    std::string goto_line(const std::string& args);
    std::string gosub(const std::string& args);
    std::string return_from_sub();
    std::string for_loop(const std::string& args);
    std::string next(const std::string& args);
    std::string while_loop(const std::string& args);
    std::string wend();
    std::string do_loop(const std::string& args);
    std::string loop(const std::string& args);
    std::string select_case(const std::string& args);
    std::string case_of(const std::string& args);
    std::string sub_definition(const std::string& args);
    std::string end_sub();
    std::string function_definition(const std::string& args);
    std::string end_function();
    std::string call(const std::string& args);
    std::string type_definition(const std::string& args);
    std::string dim(const std::string& args);
    std::string read_data(const std::string& args);
    std::string restore();
    std::string color(const std::string& args);
    std::string palette(const std::string& args);
    std::string width(const std::string& args);
    std::string open_file(const std::string& args);
    std::string close_files(const std::string& args);
    std::string input_file(const std::string& args);
    std::string get_record(const std::string& args);
    std::string put_record(const std::string& args);
    std::string screen(const std::string& args);
    std::string locate(const std::string& args);
    std::string draw_line(const std::string& args);
    std::string draw_circle(const std::string& args);
    std::string draw_point(const std::string& args, bool foreground);
    std::string paint(const std::string& args);
    std::string sprite_get(const std::string& args);
    std::string sprite_put(const std::string& args);
    std::string sprite_visible(const std::string& args, bool visible);
    std::string sprite_move(const std::string& args);
    std::string gradient(const std::string& args);
    std::string ellipse_gradient(const std::string& args);
    std::string swap(const std::string& args);
    std::string erase(const std::string& args);
    std::string constant(const std::string& args);
    std::string define_fn(const std::string& args);
    std::string define_types(const std::string& type, const std::string& args);
    std::string on_error_goto(const std::string& args);
    std::string resume(const std::string& args);
    std::string resume_next();
    std::string randomize(const std::string& args);
    std::string sleep(const std::string& args);
    std::string set_xy(const std::string& args);
    std::string sound(const std::string& args);
    std::string speed(const std::string& args);
    std::string sprite(const std::string& args);
    /// Typed characters the host has not handed to `INKEY$` yet.
    std::string key_buffer_;
    /// `SERIAL OPEN #1, "/dev/ttyUSB0", 9600` and its READ/WRITE/CLOSE.
    std::string serial(const std::string& args);
    /// `PARALLEL OPEN #1, "/dev/lp0"`, `PARALLEL BYTE #1, 255`.
    std::string parallel(const std::string& args);
    /// `DRAW "U20 R20 D20 L20"`: the turtle path string.
    std::string draw_path(const std::string& args);
    /// `WAIT ms`: pause the program.
    std::string wait(const std::string& args);
    /// The characters typed at the canvas since the last `INKEY$`, or empty.
    [[nodiscard]] std::string key_text();
    std::string on_timer(const std::string& args);
    /// True while a timer handler is on the stack, and the `GOSUB` depth it started
    /// at: a handler must not be interrupted by the timer that called it.
    bool timer_running = false;
    std::size_t timer_depth = 0;
    /// Fires `ON TIMER` when it is due. Called before each statement; a no-op unless
    /// `TIMER ON` has been said, and never while the handler is already running.
    void service_timer();
    /// The machine's clock, in seconds. Monotonic, so a timer cannot be made to fire
    /// early by a system clock going backwards.
    [[nodiscard]] static double now_seconds();
    std::string play(const std::string& args);
    std::string say(const std::string& args);
    std::string shape(const std::string& args);
    std::string particle(const std::string& args);
    std::string fractal(const std::string& args);
    std::string poke(const std::string& args);
    std::string peek(const std::string& args);
    std::string out(const std::string& args);
    std::string in_port(const std::string& args);
    std::string shell(const std::string& args);
    std::string make(const std::string& args);
    std::string reset_program(std::string_view message);

    // -- expression functions ---------------------------------------------
    void install_functions();
    [[nodiscard]] std::optional<expr::UserFunction> user_function(std::string_view name);
    /// `ON ERROR GOTO`: records the failure and jumps, returning true.
    bool divert_on_error();

    Interpreter& interpreter_;
    TurtleHost& turtle_;
    State& state_;
    bool functions_installed_ = false;
    /// The turtle's position, mirrored here because the host does not report
    /// it and SHAPE/FRACTAL need it to place their geometry.
    TurtlePose pose_;
    bool pen_down_ = true;
    double pen_width_ = 2.0;
    /// Set between `begin_fill` and `end_fill`, while moves are recorded.
    bool filling_ = false;
    /// Circles and shapes drawn since `begin_fill`, as the reference records
    /// them in `turtle.lines`.
    std::vector<Point> fill_points_;
    /// How many closed shapes a later PAINT could fill.
    int fillable_shapes_ = 0;
};

// -- the pieces a statement reaches but the language does not own ----------

/// `SHAPE name, size [, fill]`, from `features/shapes.py`.
[[nodiscard]] std::string draw_shape(Machine& machine, std::string_view args);
/// `PARTICLE effect, x, y [, intensity]`, from `features/particles.py`.
[[nodiscard]] std::string create_particle(Machine& machine, std::string_view args);
/// `FRACTAL name [, iterations [, step]]`, from `features/fractals.py`.
[[nodiscard]] std::string draw_fractal(Machine& machine, std::string_view args);
/// `PLAY`'s MML text, rendered by `features/music.py`.
[[nodiscard]] std::string play_music(Machine& machine, std::string_view args);
/// `SAY`/`SPEAK`, from `features/speech.py`.
[[nodiscard]] std::string speak(Machine& machine, std::string_view args);

/// A path confined to `base` (the process's working directory when it is empty),
/// as `utils/validators.py` confines it.
[[nodiscard]] std::string resolve_confined_path(std::string_view filename,
                                                std::string_view base);

/// The named colours `TurtleState.resolve_color` knows.
[[nodiscard]] const std::map<std::string, Rgb>& colour_names();

/// `VAR.FIELD` rewritten as its numeric value, before evaluation.
[[nodiscard]] std::string expand_records(Machine& machine, std::string_view expression);

/// Python's `str.split(separator)`, which keeps empty fields.
[[nodiscard]] std::vector<std::string> split_plain(std::string_view text, char separator);

/// `text.strip('"')`.
[[nodiscard]] std::string strip_quotes(std::string_view text);

/// Whether `expression` joins something textual with `+`.
[[nodiscard]] bool has_string_concat(std::string_view expression);

/// Whether `upper_expression` begins with a string function the reference sends
/// to the string evaluator.
[[nodiscard]] bool mentions_string_function(std::string_view upper_expression);

/// -1 when *condition* is not a string comparison, else 1 true / 0 false.
[[nodiscard]] int try_string_comparison(Machine& machine, std::string_view condition);

/// Replaces a whole-word *name* with `0`.
[[nodiscard]] std::string substitute_undefined(std::string_view expression,
                                              std::string_view name);

/// Whether an evaluator message is a division-by-zero failure.
[[nodiscard]] bool division_by_zero(std::string_view message);

/// `isdigit` for a whole string.
[[nodiscard]] bool is_all_digits(std::string_view text);

}  // namespace timewarp::lang::basic
