// SPDX-License-Identifier: MIT
//
// HB-OS: eight processes on a 60 Hz clock, and the desktop that shows them.
//
// ---------------------------------------------------------------------------
// Why the machine has an operating system at all
// ---------------------------------------------------------------------------
//
// In 1982 you chose a language and cold-booted into it; that exclusivity *was* the
// machine, and it is what a home computer could afford. By 1992 the thing the whole
// industry spent its money abolishing was exactly that: several programs resident at
// once, each with a window, taking turns by the clock.
//
// So this is the 1992 answer to the eleven-language socket. A task is an HB-16
// program with its own registers, its own console and its own window; the scheduler
// is round-robin on the frame tick; the desktop composes the windows into the
// framebuffer, topmost first, and a task that faults is killed and named while the
// desktop survives it.
//
// The clock is the machine's, never the host's. A replay of the same number of ticks
// therefore produces the same screen -- which is the rule every test in this machine
// depends on, and the reason a scheduler here is a value type rather than a thread.

#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "timewarp/hb2/bus.hpp"
#include "timewarp/hb2/cpu.hpp"
#include "timewarp/hb2/video.hpp"

namespace timewarp::hb2 {

struct Rect {
    int x = 0;
    int y = 0;
    int w = 0;
    int h = 0;

    [[nodiscard]] bool contains(int px, int py) const noexcept {
        return px >= x && py >= y && px < x + w && py < y + h;
    }
};

/// One resident program: its registers, its window and what it has printed.
struct Task {
    int id = 0;
    std::string name;
    Cpu cpu;
    Rect window;
    /// Where the program was loaded, so a second one cannot be loaded over it.
    std::uint32_t load_at = 0;
    std::uint32_t load_bytes = 0;
    bool ready = true;
    /// What `INT 1` and `INT 8` have written, which is what the window shows.
    std::string console;
    /// The exit code an `INT 0` gave, and whether it has stopped.
    int exit_code = 0;
    bool finished = false;
    std::string failure;

    Task(int identifier, std::string task_name, Bus& bus, std::uint32_t entry, Rect frame)
        : id(identifier), name(std::move(task_name)), cpu(bus), window(frame) {
        cpu.reset(entry);
    }
};

/// Round-robin over the tasks, one quantum each per tick.
class Scheduler {
public:
    static constexpr int kMaxTasks = 8;

    explicit Scheduler(Bus& bus);

    /// Loads a listing at the next free address and starts it. Returns the task id,
    /// or 0 with a sentence in lastError() when it will not assemble or fit.
    int spawn(const std::string& name, const std::string& source, std::uint32_t entry);
    /// Stops every task and forgets them, including their addresses.
    void clear();

    /// Gives each ready task `quantum` instructions, in turn. Returns how many tasks
    /// actually ran, which is what "the desktop is alive" means.
    int tick(std::uint32_t quantum = 2000);

    [[nodiscard]] const std::vector<Task>& tasks() const noexcept { return tasks_; }
    [[nodiscard]] std::size_t size() const noexcept { return tasks_.size(); }
    [[nodiscard]] bool empty() const noexcept { return tasks_.empty(); }
    [[nodiscard]] Task* byId(int id);
    [[nodiscard]] const Task* byId(int id) const;
    /// The task the processor was last given, or 0 for the console's own program.
    [[nodiscard]] int current() const noexcept { return current_; }
    /// One line a task: its name, whether it is still going, and its window.
    [[nodiscard]] std::vector<std::string> status() const;
    [[nodiscard]] const std::string& lastError() const noexcept { return error_; }

    /// Where a task's program is loaded, and the size each is allowed. A task that
    /// does not fit is refused rather than overlapping its neighbour.
    static constexpr std::uint32_t kFirstAddress = 0x001000;
    static constexpr std::uint32_t kTaskBytes = 0x004000;

private:
    Bus& bus_;
    std::vector<Task> tasks_;
    int next_id_ = 1;
    std::size_t next_ = 0;
    int current_ = 0;
    std::uint32_t next_address_ = kFirstAddress;
    std::string error_;
};

/// The windows, the wallpaper, the task bar and the pointer.
class Desktop {
public:
    Desktop();

    /// Gives a task a window, on top, and focuses it -- which is what starting a
    /// program does.
    void addWindow(int task, const std::string& title, Rect frame);
    void removeWindow(int task);
    void clear();

    /// Composes the desktop into the framebuffer. `MODE 0` has no framebuffer to
    /// draw in, so a desktop in text mode is left alone.
    void draw(Video& video, const Scheduler& scheduler);

    /// The task whose window is topmost at a point, or 0 for the desktop itself.
    [[nodiscard]] int topAt(int x, int y) const;
    /// Raises and focuses a window, which is what clicking it does.
    void focus(int task);
    [[nodiscard]] int focused() const noexcept { return focused_; }
    void move(int task, int x, int y);
    void resize(int task, int w, int h);
    [[nodiscard]] Rect frameOf(int task) const;
    [[nodiscard]] std::size_t count() const noexcept { return windows_.size(); }

    /// Puts the pointer at a pixel, which the desktop draws and a program can read.
    void movePointer(int x, int y);
    [[nodiscard]] int pointerX() const noexcept { return pointer_x_; }
    [[nodiscard]] int pointerY() const noexcept { return pointer_y_; }

private:
    struct Window {
        int task = 0;
        std::string title;
        Rect frame;
        int z = 0;
    };

    [[nodiscard]] Window* findByTask(int task);
    [[nodiscard]] const Window* findByTask(int task) const;
    void drawWindow(Video& video, const Window& window, const Task& task, bool focused) const;

    std::vector<Window> windows_;
    int next_z_ = 1;
    int focused_ = 0;
    int pointer_x_ = 160;
    int pointer_y_ = 100;
};

}  // namespace timewarp::hb2
