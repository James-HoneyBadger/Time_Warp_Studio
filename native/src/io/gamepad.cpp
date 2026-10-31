// SPDX-License-Identifier: MIT
//
// See `include/timewarp/io/gamepad.hpp` for why this exists and what it does.
//
// The Linux joystick interface, which is a character device that can be read
// without linking a framework. A machine with no gamepad answers `available()`
// false, which is the behaviour every machine without one already had.

#include "timewarp/io/gamepad.hpp"

#include <cstdlib>
#include <fcntl.h>
#include <linux/joystick.h>
#include <unistd.h>

namespace timewarp::io {
namespace {

/// The devices tried in order when `TIME_WARP_GAMEPAD` names none, so a test or
/// a machine with several joysticks can name one instead of taking the first.
const char* const kCandidates[] = {"/dev/input/js0", "/dev/input/js1", "/dev/input/js2",
                                   "/dev/input/js3"};

}  // namespace

Gamepad& Gamepad::instance() {
    static Gamepad pad;
    return pad;
}

Gamepad::~Gamepad() {
    if (descriptor_ >= 0) {
        ::close(descriptor_);
    }
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
    const char* requested = std::getenv("TIME_WARP_GAMEPAD");
    if (requested != nullptr && requested[0] != '\0') {
        descriptor_ = ::open(requested, O_RDONLY | O_NONBLOCK);
        if (descriptor_ >= 0) {
            device_path_ = requested;
        }
        return;
    }
    for (const char* candidate : kCandidates) {
        descriptor_ = ::open(candidate, O_RDONLY | O_NONBLOCK);
        if (descriptor_ >= 0) {
            device_path_ = candidate;
            return;
        }
    }
}

void Gamepad::poll() {
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
