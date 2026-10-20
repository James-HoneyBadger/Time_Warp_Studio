// SPDX-License-Identifier: MIT

#include "timewarp/io/ports.hpp"

#include <algorithm>
#include <cstdlib>
#include <utility>

#if defined(__unix__) || defined(__APPLE__)
#include <cerrno>
#include <cstring>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>
#define TIME_WARP_HAS_POSIX_PORTS 1
#else
#define TIME_WARP_HAS_POSIX_PORTS 0
#endif

namespace timewarp::io {

namespace {

#if TIME_WARP_HAS_POSIX_PORTS

/// The `speed_t` for a baud rate, or 0 when this platform has no constant for it.
/// Every rate a school is likely to meet is here; the ones that are not get a
/// message rather than a silently different speed.
[[nodiscard]] speed_t speed_for(int baud) {
    switch (baud) {
        case 300:
            return B300;
        case 600:
            return B600;
        case 1200:
            return B1200;
        case 2400:
            return B2400;
        case 4800:
            return B4800;
        case 9600:
            return B9600;
        case 19200:
            return B19200;
        case 38400:
            return B38400;
#ifdef B57600
        case 57600:
            return B57600;
#endif
#ifdef B115200
        case 115200:
            return B115200;
#endif
        default:
            return 0;
    }
}

[[nodiscard]] std::string describe(int error_number) {
    return std::strerror(error_number);
}

#endif  // TIME_WARP_HAS_POSIX_PORTS

void set_error(std::string* error, const std::string& message) {
    if (error != nullptr) {
        *error = message;
    }
}

}  // namespace

bool ports_supported() {
#if TIME_WARP_HAS_POSIX_PORTS
    return true;
#else
    return false;
#endif
}

std::vector<int> supported_bauds() {
    return {115200, 57600, 38400, 19200, 9600, 4800, 2400, 1200, 600, 300};
}

const PortPolicy& PortPolicy::current() {
    static const PortPolicy policy = [] {
        PortPolicy built;
        // The device families a school actually plugs in. `by-id` is a directory
        // of stable symlinks, which is why it is a prefix of its own.
        //
        // `/dev/ttys` is a BSD pseudo-terminal, which is the only kind macOS has:
        // Linux's `/dev/pts/` does not exist there, so a test -- or a program --
        // opening a pty was refused by this list rather than by the line
        // discipline. `/dev/tty.` and `/dev/cu.` are how a USB serial adapter
        // appears on a Mac (`cu.usbserial-1420`), one for call-out and one for
        // call-in, and neither matches the bare `/dev/tty` this list must not
        // allow.
        built.prefixes = {"/dev/ttyS",   "/dev/ttyUSB", "/dev/ttyACM", "/dev/ttyAMA",
                          "/dev/ttySAC", "/dev/serial/", "/dev/pts/",  "/dev/lp",
                          "/dev/parport", "/dev/usb/lp", "/dev/ttys",  "/dev/tty.",
                          "/dev/cu."};
        if (const char* extra = std::getenv("TIME_WARP_PORTS"); extra != nullptr) {
            std::string remaining{extra};
            while (!remaining.empty()) {
                const std::size_t colon = remaining.find(':');
                const std::string entry = remaining.substr(0, colon);
                if (!entry.empty()) {
                    built.extra.push_back(entry);
                }
                if (colon == std::string::npos) {
                    break;
                }
                remaining.erase(0, colon + 1);
            }
        }
        return built;
    }();
    return policy;
}

bool PortPolicy::allows(const std::string& path) const {
    if (path.empty()) {
        return false;
    }
    // A relative path is refused outright: `../dev/ttyS0` from a document's own
    // directory must not become a device.
    if (path.front() != '/') {
        return false;
    }
    // No traversal segments, for the same reason.
    if (path.find("/../") != std::string::npos || path.find("/..") == path.size() - 3) {
        return false;
    }
    if (std::find(extra.begin(), extra.end(), path) != extra.end()) {
        return true;
    }
    for (const std::string& prefix : prefixes) {
        if (path.rfind(prefix, 0) == 0) {
            return true;
        }
    }
    return false;
}

// ---------------------------------------------------------------------------
// SerialPort
// ---------------------------------------------------------------------------

SerialPort::~SerialPort() { close(); }

SerialPort::SerialPort(SerialPort&& other) noexcept
    : handle_{other.handle_}, path_{std::move(other.path_)} {
    other.handle_ = -1;
}

SerialPort& SerialPort::operator=(SerialPort&& other) noexcept {
    if (this != &other) {
        close();
        handle_ = other.handle_;
        path_ = std::move(other.path_);
        other.handle_ = -1;
    }
    return *this;
}

bool SerialPort::open(const std::string& path, int baud, std::string* error) {
    close();
#if !TIME_WARP_HAS_POSIX_PORTS
    (void)baud;
    set_error(error, "this build has no serial support (no POSIX device nodes)");
    return false;
#else
    if (!PortPolicy::current().allows(path)) {
        set_error(error, "`" + path + "` is not a port this program may open. Ports "
                                 "are limited to serial and parallel devices; a teacher "
                                 "can allow another path with TIME_WARP_PORTS.");
        return false;
    }
    const speed_t speed = speed_for(baud);
    if (speed == 0) {
        set_error(error, "unsupported baud rate " + std::to_string(baud));
        return false;
    }

    // O_NOCTTY: the port must not become the program's controlling terminal.
    // O_NONBLOCK: a read on a quiet line must return, not wait forever, and it is
    // cleared below once the settings are applied.
    const int handle = ::open(path.c_str(), O_RDWR | O_NOCTTY | O_NONBLOCK);
    if (handle < 0) {
        set_error(error, "cannot open " + path + ": " + describe(errno));
        return false;
    }

    termios settings{};
    if (::tcgetattr(handle, &settings) != 0) {
        set_error(error, "cannot read the settings of " + path + ": " + describe(errno));
        ::close(handle);
        return false;
    }
    // Raw: no echo, no line translation, no signal characters. A program talking
    // to a board wants the bytes it sent, not a terminal's opinion of them.
    ::cfmakeraw(&settings);
    settings.c_cflag |= static_cast<tcflag_t>(CLOCAL | CREAD);
    ::cfsetispeed(&settings, speed);
    ::cfsetospeed(&settings, speed);
    // Return as soon as one byte is available, so a timeout is the caller's
    // choice and not the line discipline's.
    settings.c_cc[VMIN] = 0;
    settings.c_cc[VTIME] = 0;
    if (::tcsetattr(handle, TCSANOW, &settings) != 0) {
        set_error(error, "cannot configure " + path + ": " + describe(errno));
        ::close(handle);
        return false;
    }

    handle_ = handle;
    path_ = path;
    return true;
#endif
}

void SerialPort::close() {
#if TIME_WARP_HAS_POSIX_PORTS
    if (handle_ >= 0) {
        ::close(handle_);
    }
#endif
    handle_ = -1;
    path_.clear();
}

bool SerialPort::write(const std::string& bytes, std::string* error) {
    if (!is_open()) {
        set_error(error, "the serial port is not open");
        return false;
    }
#if !TIME_WARP_HAS_POSIX_PORTS
    set_error(error, "this build has no serial support");
    return false;
#else
    std::size_t written = 0;
    while (written < bytes.size()) {
        const ssize_t count = ::write(handle_, bytes.data() + written, bytes.size() - written);
        if (count < 0) {
            if (errno == EINTR) {
                continue;
            }
            // `EAGAIN` and `EWOULDBLOCK` are the same value on Linux, which is
            // what this build targets; naming both trips -Wlogical-op.
            if (errno == EAGAIN) {
                // The kernel buffer is full: a board reading slower than the
                // program writes is a real thing, so wait rather than drop bytes.
                ::usleep(2000);
                continue;
            }            set_error(error, std::string{"write failed: "} + describe(errno));
            return false;
        }
        written += static_cast<std::size_t>(count);
    }
    return true;
#endif
}

std::string SerialPort::read(std::size_t max_bytes, int timeout_ms, std::string* error) {
    if (!is_open()) {
        set_error(error, "the serial port is not open");
        return {};
    }
#if !TIME_WARP_HAS_POSIX_PORTS
    set_error(error, "this build has no serial support");
    return {};
#else
    if (max_bytes == 0) {
        return {};
    }
    // Polled rather than `VMIN`/`VTIME`, because the timeout has to apply to the
    // whole read: a `VTIME` of 1 second returns per byte, so a 200ms timeout for
    // four bytes would take 800ms.
    const int slice_ms = 5;
    int waited = 0;
    std::string collected;
    while (true) {
        char buffer[256];
        const std::size_t want = std::min(sizeof(buffer), max_bytes - collected.size());
        const ssize_t count = ::read(handle_, buffer, want);
        if (count > 0) {
            collected.append(buffer, static_cast<std::size_t>(count));
            if (collected.size() >= max_bytes) {
                break;
            }
        } else if (count < 0 && errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
            set_error(error, std::string{"read failed: "} + describe(errno));
            break;
        }
        if (waited >= timeout_ms) {
            break;
        }
        ::usleep(slice_ms * 1000);
        waited += slice_ms;
    }
    return collected;
#endif
}

std::string SerialPort::read_line(int timeout_ms, std::string* error) {
    std::string line;
    const int slice_ms = 10;
    for (int waited = 0; waited <= timeout_ms; waited += slice_ms) {
        const std::string chunk = read(1, slice_ms, error);
        if (!chunk.empty()) {
            if (chunk.front() == '\n') {
                return line;
            }
            if (chunk.front() != '\r') {
                line.push_back(chunk.front());
            }
        }
    }
    // A timeout with a partial line is still what the device said.
    return line;
}

// ---------------------------------------------------------------------------
// ParallelPort
// ---------------------------------------------------------------------------

ParallelPort::~ParallelPort() { close(); }

ParallelPort::ParallelPort(ParallelPort&& other) noexcept
    : handle_{other.handle_}, path_{std::move(other.path_)} {
    other.handle_ = -1;
}

ParallelPort& ParallelPort::operator=(ParallelPort&& other) noexcept {
    if (this != &other) {
        close();
        handle_ = other.handle_;
        path_ = std::move(other.path_);
        other.handle_ = -1;
    }
    return *this;
}

bool ParallelPort::open(const std::string& path, std::string* error) {
    close();
#if !TIME_WARP_HAS_POSIX_PORTS
    set_error(error, "this build has no parallel support (no POSIX device nodes)");
    return false;
#else
    if (!PortPolicy::current().allows(path)) {
        set_error(error, "`" + path + "` is not a port this program may open. Ports "
                                 "are limited to serial and parallel devices; a teacher "
                                 "can allow another path with TIME_WARP_PORTS.");
        return false;
    }
    const int handle = ::open(path.c_str(), O_WRONLY | O_NOCTTY);
    if (handle < 0) {
        set_error(error, "cannot open " + path + ": " + describe(errno));
        return false;
    }
    handle_ = handle;
    path_ = path;
    return true;
#endif
}

void ParallelPort::close() {
#if TIME_WARP_HAS_POSIX_PORTS
    if (handle_ >= 0) {
        ::close(handle_);
    }
#endif
    handle_ = -1;
    path_.clear();
}

bool ParallelPort::write(const std::string& bytes, std::string* error) {
    if (!is_open()) {
        set_error(error, "the parallel port is not open");
        return false;
    }
#if !TIME_WARP_HAS_POSIX_PORTS
    set_error(error, "this build has no parallel support");
    return false;
#else
    std::size_t written = 0;
    while (written < bytes.size()) {
        const ssize_t count = ::write(handle_, bytes.data() + written, bytes.size() - written);
        if (count < 0) {
            if (errno == EINTR) {
                continue;
            }
            set_error(error, std::string{"write failed: "} + describe(errno));
            return false;
        }
        written += static_cast<std::size_t>(count);
    }
    return true;
#endif
}

bool ParallelPort::write_byte(unsigned char value, std::string* error) {
    return write(std::string(1, static_cast<char>(value)), error);
}

}  // namespace timewarp::io
