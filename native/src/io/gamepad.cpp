// SPDX-License-Identifier: MIT
//
// See `include/timewarp/io/gamepad.hpp` for why this exists and what it does.
//
// Only the Linux joystick interface is implemented. That is not a shortcut: it
// is the only one of the three platforms whose joystick API is a file that can
// be read without linking a framework, and the other two answer `available()`
// false, which is the behaviour every machine without a gamepad already had.
// A Windows or macOS backend belongs beside this one, not instead of it.

#include "timewarp/io/gamepad.hpp"

#include <cstdlib>

#ifndef _WIN32
#include <fcntl.h>
#include <unistd.h>
#endif

#ifdef __linux__
#include <linux/joystick.h>
#endif

namespace timewarp::io {
namespace {

#ifdef __linux__
/// The devices tried in order when `TIME_WARP_GAMEPAD` names none, so a test or
/// a machine with several joysticks can name one instead of taking the first.
const char* const kCandidates[] = {"/dev/input/js0", "/dev/input/js1", "/dev/input/js2",
                                   "/dev/input/js3"};
#endif

}  // namespace

Gamepad& Gamepad::instance() {
    static Gamepad pad;
    return pad;
}

Gamepad::~Gamepad() {
#ifndef _WIN32
    if (descriptor_ >= 0) {
        ::close(descriptor_);
    }
#endif
}

bool Gamepad::available() {
    if (!tried_) {
        open_device();
    }
    return descriptor_ >= 0;
}

const char* Gamepad::device() const noexcept {
    return descriptor_ >= 0 ? device_path_.c_str() : "";
}

void Gamepad::open_device() {
    tried_ = true;
#ifndef _WIN32
    const char* requested = std::getenv("TIME_WARP_GAMEPAD");
    if (requested != nullptr && requested[0] != '\0') {
        descriptor_ = ::open(requested, O_RDONLY | O_NONBLOCK);
        if (descriptor_ >= 0) {
            device_path_ = requested;
        }
        return;
    }
#ifdef __linux__
    for (const char* candidate : kCandidates) {
        descriptor_ = ::open(candidate, O_RDONLY | O_NONBLOCK);
        if (descriptor_ >= 0) {
            device_path_ = candidate;
            return;
        }
    }
#endif
#endif
}

void Gamepad::poll() {
#ifdef __linux__
    if (!available()) {
        return;
    }
    js_event event{};
    while (::read(descriptor_, &event, sizeof event) ==
           static_cast<ssize_t>(sizeof event)) {
        const unsigned char type = event.type & ~JS_EVENT_INIT;
        if (type == JS_EVENT_AXIS && event.number < axes_.size()) {
            axes_[event.number] = scale_axis(static_cast<long>(event.value));
        } else if (type == JS_EVENT_BUTTON && event.number < buttons_.size()) {
            buttons_[event.number] = event.value != 0;
        }
    }
#endif
}

int Gamepad::axis(int index) {
    poll();
    if (index < 0 || static_cast<std::size_t>(index) >= axes_.size()) {
        return 128;
    }
    return axes_[static_cast<std::size_t>(index)];
}

bool Gamepad::button(int index) {
    poll();
    if (index < 0 || static_cast<std::size_t>(index) >= buttons_.size()) {
        return false;
    }
    return buttons_[static_cast<std::size_t>(index)];
}

}  // namespace timewarp::io
