// SPDX-License-Identifier: MIT
//
// Ports: talking to something that is not this program.
//
// The point is a Raspberry Pi, an Arduino, a Micro:bit, a LEGO interface or the
// parallel-port projects these machines were built around: a program that reads a
// sensor and draws the answer is a far better lesson than one that prints a
// constant. Both ports are plain POSIX devices on Linux -- `/dev/ttyUSB0`,
// `/dev/ttyACM0`, `/dev/serial/by-id/...`, `/dev/lp0`, `/dev/parport0` -- so no
// library is needed and nothing about the build changes.
//
// #### Why there is a policy
//
// A learner's program must not be able to open `/dev/mem` or a disk. `PortPolicy`
// is an allow-list of device families plus whatever a teacher adds through
// `TIME_WARP_PORTS`; anything else is refused **by the engine**, with a sentence
// the program prints. This mirrors how `SHELL` is blocked: the language is for
// learning, and a program that can talk to any device node can also delete the
// user's files.
//
// #### What is deliberately synchronous
//
// Reads have a timeout in milliseconds and return what arrived. There is no
// background reader thread: a program that wants a stream asks in a loop, which
// is what a BASIC program with a `GOTO` does anyway, and it keeps the engine
// single-threaded per run.

#pragma once

#include <cstddef>
#include <string>
#include <vector>

namespace timewarp::io {

/// Which device paths a program may open.
///
/// The defaults are the serial and parallel device families. `extra` holds exact
/// paths a teacher allowed, which is how a board on an unusual name is reachable
/// without widening the rule for everybody.
struct PortPolicy {
    std::vector<std::string> prefixes;
    std::vector<std::string> extra;

    /// The default policy, plus `TIME_WARP_PORTS` (`:`-separated exact paths).
    /// The environment is read once so a program cannot change the rules it is
    /// running under.
    [[nodiscard]] static const PortPolicy& current();

    [[nodiscard]] bool allows(const std::string& path) const;
};

/// A serial device, opened at a baud rate and read with timeouts.
///
/// Not copyable: it owns a file descriptor, and two objects closing one fd is a
/// bug that reads as "the port randomly stops working".
class SerialPort {
public:
    SerialPort() = default;
    ~SerialPort();
    SerialPort(const SerialPort&) = delete;
    SerialPort& operator=(const SerialPort&) = delete;
    SerialPort(SerialPort&& other) noexcept;
    SerialPort& operator=(SerialPort&& other) noexcept;

    /// Opens `path` at `baud` (300…115200). Returns false with a reason in
    /// `error` when the device is missing, not allowed, or refuses the settings.
    bool open(const std::string& path, int baud, std::string* error);
    void close();
    [[nodiscard]] bool is_open() const noexcept { return handle_ >= 0; }
    [[nodiscard]] const std::string& path() const noexcept { return path_; }

    /// Writes every byte. Returns false with a reason when the device has gone
    /// away -- unplugging a USB adapter mid-write is a normal thing to happen, not
    /// an exception.
    bool write(const std::string& bytes, std::string* error);

    /// Reads whatever has arrived, up to `max_bytes`, waiting up to `timeout_ms`.
    /// Returns an empty string on timeout, which is not an error.
    std::string read(std::size_t max_bytes, int timeout_ms, std::string* error);

    /// Reads one line without its terminator. Empty on timeout.
    std::string read_line(int timeout_ms, std::string* error);

private:
    int handle_ = -1;
    std::string path_;
};

/// A parallel port, which is write-only from a program's point of view.
class ParallelPort {
public:
    ParallelPort() = default;
    ~ParallelPort();
    ParallelPort(const ParallelPort&) = delete;
    ParallelPort& operator=(const ParallelPort&) = delete;
    ParallelPort(ParallelPort&& other) noexcept;
    ParallelPort& operator=(ParallelPort&& other) noexcept;

    bool open(const std::string& path, std::string* error);
    void close();
    [[nodiscard]] bool is_open() const noexcept { return handle_ >= 0; }
    [[nodiscard]] const std::string& path() const noexcept { return path_; }

    bool write(const std::string& bytes, std::string* error);
    /// Puts one byte on the data lines. The lesson-shaped operation: eight LEDs.
    bool write_byte(unsigned char value, std::string* error);

private:
    int handle_ = -1;
    std::string path_;
};

/// The baud rates a program may ask for, largest first, for the error message.
[[nodiscard]] std::vector<int> supported_bauds();

}  // namespace timewarp::io
