// SPDX-License-Identifier: MIT

#include "timewarp/ui/chip8_emulator.hpp"

#include <QFile>
#include <QFileInfo>

#include <algorithm>
#include <string_view>
#include <utility>

namespace timewarp::ui {

namespace {

/// 16 ms is 62.5 frames a second: close enough to the 60 Hz the timers assume,
/// and a whole number of milliseconds, which `QTimer` needs.
constexpr int kFrameMilliseconds = 1000 / kChip8FramesPerSecond;

}  // namespace

Chip8Emulator::Chip8Emulator(QObject* parent) : QObject(parent), clock_(this) {
    clock_.setTimerType(Qt::PreciseTimer);
    clock_.setInterval(kFrameMilliseconds);
    connect(&clock_, &QTimer::timeout, this, &Chip8Emulator::tick);
}

Chip8Emulator::~Chip8Emulator() = default;

bool Chip8Emulator::loadFile(const QString& path, QString* error) {
    QFile file{path};
    if (!file.open(QIODevice::ReadOnly)) {
        const QString message =
            tr("Could not read %1: %2")
                .arg(QFileInfo{path}.fileName(), file.errorString());
        error_ = message;
        if (error != nullptr) {
            *error = message;
        }
        pause();
        machine_.reset();
        program_ = {};
        name_.clear();
        path_.clear();
        emit errorOccurred(message);
        emit stateChanged();
        emit frameReady();
        return false;
    }
    const QByteArray bytes = file.readAll();
    file.close();
    if (!loadBytes(bytes, QFileInfo{path}.fileName(), error)) {
        return false;
    }
    path_ = QFileInfo{path}.absoluteFilePath();
    return true;
}

bool Chip8Emulator::loadBytes(const QByteArray& bytes, const QString& name, QString* error) {
    timewarp::lang::Chip8Program program;
    try {
        // A `QByteArray` is not a string: passing its size keeps a NUL inside a
        // ROM rather than stopping at it the way `fromLatin1` would.
        program = timewarp::lang::assemble_chip8(
            std::string_view{bytes.constData(), static_cast<std::size_t>(bytes.size())});
    } catch (const timewarp::lang::Chip8Error& failure) {
        const QString message = QString::fromStdString(failure.what());
        error_ = message;
        if (error != nullptr) {
            *error = message;
        }
        pause();
        machine_.reset();
        program_ = {};
        name_.clear();
        path_.clear();
        emit errorOccurred(message);
        emit stateChanged();
        emit frameReady();
        return false;
    }

    pause();
    clock_.stop();
    program_ = std::move(program);
    name_ = name;
    path_.clear();
    error_.clear();
    makeMachine();
    emit stateChanged();
    emit frameReady();
    return true;
}

bool Chip8Emulator::loadSource(const QString& source, QString* error) {
    return loadBytes(source.toUtf8(), tr("document"), error);
}

void Chip8Emulator::makeMachine() {
    machine_ = std::make_unique<timewarp::lang::Chip8Machine>(program_.memory, program_.entry);
    // The whole-program executor reads the key mask once because it cannot
    // receive a later press. This machine can, so Fx0A waits.
    machine_->set_interactive(true);
}

void Chip8Emulator::start() {
    if (machine_ == nullptr || running_ || machine_->halted()) {
        return;
    }
    error_.clear();
    running_ = true;
    clock_.start();
    emit stateChanged();
}

void Chip8Emulator::pause() {
    if (!running_) {
        clock_.stop();
        return;
    }
    running_ = false;
    clock_.stop();
    emit stateChanged();
}

void Chip8Emulator::step() {
    if (machine_ == nullptr || running_ || machine_->halted()) {
        return;
    }
    try {
        machine_->step();
    } catch (const timewarp::lang::Chip8Error& failure) {
        fail(QString::fromStdString(failure.what()));
        emit frameReady();
        return;
    }
    emit frameReady();
    emit stateChanged();
}

void Chip8Emulator::reset() {
    const bool was_running = running_;
    running_ = false;
    clock_.stop();
    if (machine_ == nullptr) {
        if (was_running) {
            emit stateChanged();
        }
        return;
    }
    makeMachine();
    error_.clear();
    emit frameReady();
    emit stateChanged();
}

void Chip8Emulator::setSpeed(int cycles_per_frame) {
    const int clamped = std::clamp(cycles_per_frame, 1, kChip8MaximumSpeed);
    if (clamped == speed_) {
        return;
    }
    speed_ = clamped;
    emit stateChanged();
}

void Chip8Emulator::setKey(int key, bool down) {
    if (machine_ != nullptr) {
        machine_->set_key(key, down);
    }
}

void Chip8Emulator::releaseAllKeys() {
    if (machine_ != nullptr) {
        machine_->set_keys(0);
    }
}

unsigned Chip8Emulator::keys() const noexcept {
    return machine_ != nullptr ? machine_->keys() : 0;
}

Chip8Snapshot Chip8Emulator::snapshot() const {
    Chip8Snapshot snapshot;
    snapshot.speed = speed_;
    if (machine_ == nullptr) {
        return snapshot;
    }
    snapshot.v = machine_->v;
    snapshot.i = machine_->i;
    snapshot.pc = machine_->pc;
    snapshot.stack = machine_->stack;
    snapshot.delay_timer = machine_->delay_timer;
    snapshot.sound_timer = machine_->sound_timer;
    snapshot.cycles = machine_->cycles();
    snapshot.loaded = true;
    snapshot.running = running_;
    snapshot.halted = machine_->halted();
    snapshot.waiting_for_key = machine_->waiting_for_key();
    snapshot.framebuffer = machine_->framebuffer();
    snapshot.memory = machine_->memory;
    return snapshot;
}

void Chip8Emulator::tick() {
    if (running_) {
        advanceFrame();
    }
}

void Chip8Emulator::advanceFrame() {
    if (machine_ == nullptr) {
        return;
    }
    try {
        for (int cycle = 0; cycle < speed_ && !machine_->halted(); ++cycle) {
            machine_->step();
        }
        // The clock is the frame, so the timers follow it whether the program
        // executed ten opcodes or none while waiting for a key.
        machine_->tick_timers();
    } catch (const timewarp::lang::Chip8Error& failure) {
        fail(QString::fromStdString(failure.what()));
        emit frameReady();
        return;
    }
    emit frameReady();
    if (machine_->halted()) {
        running_ = false;
        clock_.stop();
        emit stateChanged();
    }
}

void Chip8Emulator::fail(const QString& message) {
    error_ = message;
    running_ = false;
    clock_.stop();
    emit errorOccurred(message);
    emit stateChanged();
}

}  // namespace timewarp::ui
