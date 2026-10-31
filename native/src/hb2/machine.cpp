// SPDX-License-Identifier: MIT

#include "timewarp/hb2/machine.hpp"

#include <algorithm>
#include <array>
#include <cstring>
#include <sstream>

namespace timewarp::hb2 {

namespace {

constexpr std::uint64_t kFnvOffset = 0xCBF29CE484222325ULL;
constexpr std::uint64_t kFnvPrime = 0x100000001B3ULL;

std::uint64_t mix(std::uint64_t hash, std::uint8_t byte) { return (hash ^ byte) * kFnvPrime; }

void appendWord(std::vector<std::uint8_t>& out, std::uint32_t value) {
    out.push_back(static_cast<std::uint8_t>(value & 0xFF));
    out.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    out.push_back(static_cast<std::uint8_t>((value >> 16) & 0xFF));
    out.push_back(static_cast<std::uint8_t>((value >> 24) & 0xFF));
}

std::uint32_t readWord(const std::vector<std::uint8_t>& bytes, std::size_t at) {
    if (at + 4 > bytes.size()) {
        return 0;
    }
    return static_cast<std::uint32_t>(bytes[at]) |
           (static_cast<std::uint32_t>(bytes[at + 1]) << 8) |
           (static_cast<std::uint32_t>(bytes[at + 2]) << 16) |
           (static_cast<std::uint32_t>(bytes[at + 3]) << 24);
}

std::string hexRegion(std::uint32_t base, std::uint32_t size) {
    static constexpr char kHex[] = "0123456789ABCDEF";
    const auto digits = [](std::uint32_t value, int count) {
        std::string text(static_cast<std::size_t>(count), '0');
        for (int index = count - 1; index >= 0; --index) {
            text[static_cast<std::size_t>(index)] = kHex[value & 0xF];
            value >>= 4;
        }
        return text;
    };
    return "$" + digits(base, 6) + "..$" + digits(base + size - 1, 6);
}

}  // namespace

Machine::Machine() : cpu_(bus_) {
    bus_.attach(RegionKind::Video, &video_);
    bus_.attach(RegionKind::Audio, &audio_);
    bus_.attach(RegionKind::Input, &input_);
    bus_.attach(RegionKind::Rtc, &rtc_);
    bus_.attach(RegionKind::Uart, &uart_);
    bus_.attach(RegionKind::Rom, &rom_);
    cpu_.setInterruptHandler([this](Cpu& cpu, std::uint16_t vector) {
        return service(cpu, vector);
    });
    reset();
}

void Machine::reset(std::uint32_t entry) {
    bus_.reset();
    console_.clear();
    exit_code_ = 0;
    cpu_.reset(entry);
}

void Machine::loadProgram(const std::vector<std::uint8_t>& bytes, std::uint32_t address) {
    for (std::size_t index = 0; index < bytes.size(); ++index) {
        const std::uint32_t at = address + static_cast<std::uint32_t>(index);
        if (at >= Bus::kRamSize) {
            // A module that lands past the end of RAM is a malformed module, and the
            // machine refuses it rather than writing over the top of itself.
            break;
        }
        bus_.ram()[at] = bytes[index];
    }
    cpu_.reset(address);
}

Assembly Machine::load(const std::string& source, std::uint32_t origin) {
    Assembly assembly = assemble(source, origin);
    if (!assembly.ok) {
        return assembly;
    }
    loadProgram(assembly.bytes, assembly.origin);
    return assembly;
}

std::uint32_t Machine::run(std::uint32_t budget) { return cpu_.run(budget); }

std::uint64_t Machine::runFrames(int frames, std::uint32_t stepsPerFrame) {
    std::uint64_t ran = 0;
    for (int frame = 0; frame < frames; ++frame) {
        if (cpu_.status() == CpuStatus::Running) {
            cpu_.run(stepsPerFrame);
        }
        tickFrame();
        ++ran;
        if (cpu_.status() == CpuStatus::Faulted) {
            // A faulted processor is reported, not hidden: the frame still ticked,
            // because the machine around a broken program is still a machine.
            break;
        }
    }
    return ran;
}

void Machine::tickFrame() {
    bus_.tick();
    // The 1982 latch, drained into channel zero: a program that POKEs a note gets a
    // sound from the same place it always did.
    if (const std::uint8_t note = video_.takeSoundLatch(); note != 0) {
        audio_.trigger(0, 0, std::max(1, 256 / std::max<int>(1, note)), 0xC0);
    }
}

CpuStatus Machine::status() const { return cpu_.status(); }

std::string Machine::statusText() const {
    switch (cpu_.status()) {
    case CpuStatus::Running:
        return "running";
    case CpuStatus::Halted:
        return "halted";
    case CpuStatus::Faulted:
        break;
    }
    return "faulted: " + cpu_.fault();
}

// ---------------------------------------------------------------------------
// The operating system's table
// ---------------------------------------------------------------------------

bool Machine::service(Cpu& cpu, std::uint16_t vector) {
    CpuRegisters& r = cpu.registers();
    switch (vector) {
    case kServiceExit:
        exit_code_ = r.r[0];
        return false;
    case kServicePrint: {
        const auto character = static_cast<char>(r.r[0] & 0xFF);
        console_.push_back(character);
        video_.putChar(static_cast<std::uint8_t>(r.r[0] & 0xFF));
        return true;
    }
    case kServiceFrame:
        r.r[0] = static_cast<std::uint16_t>(video_.frames() & 0xFFFF);
        return true;
    case kServiceFill:
        video_.fill(r.r[0], r.r[1], r.r[2], r.r[3], static_cast<std::uint8_t>(r.r[4] & 0xFF));
        return true;
    case kServiceLine:
        video_.line(r.r[0], r.r[1], r.r[2], r.r[3], static_cast<std::uint8_t>(r.r[4] & 0xFF));
        return true;
    case kServiceMode:
        if (r.r[0] <= static_cast<std::uint16_t>(Video::Mode::High)) {
            video_.setMode(static_cast<Video::Mode>(r.r[0]));
            bus_.setCompat(r.r[0] == 0);
        }
        return true;
    case kServicePalette:
        video_.setDac(r.r[0], Rgb{static_cast<std::uint8_t>(r.r[1]),
                                  static_cast<std::uint8_t>(r.r[2]),
                                  static_cast<std::uint8_t>(r.r[3])});
        return true;
    case kServiceClear:
        if (video_.mode() == Video::Mode::Text) {
            video_.clearText();
        } else {
            video_.fill(0, 0, video_.width(), video_.height(), static_cast<std::uint8_t>(r.r[0] & 0xFF));
        }
        return true;
    case kServicePrintString: {
        std::uint32_t at = r.r[0];
        for (int guard = 0; guard < 65536; ++guard) {
            const std::uint8_t character = bus_.read(at);
            if (character == 0) {
                break;
            }
            console_.push_back(static_cast<char>(character));
            video_.putChar(character);
            ++at;
        }
        return true;
    }
    case kServiceSprite:
        video_.setSprite(r.r[0], true, r.r[1], r.r[2], r.r[3], static_cast<std::uint8_t>(r.r[4] & 0xFF));
        return true;
    case kServiceClearText:
        video_.clearText();
        return true;
    case kServiceClock:
        r.r[0] = rtc_.read(Rtc::kYear);
        r.r[1] = rtc_.read(Rtc::kMonth);
        r.r[2] = rtc_.read(Rtc::kDay);
        r.r[3] = rtc_.read(Rtc::kHours);
        r.r[4] = rtc_.read(Rtc::kMinutes);
        r.r[5] = rtc_.read(Rtc::kSeconds);
        return true;
    case kServiceSerial:
        uart_.write(Uart::kData, static_cast<std::uint8_t>(r.r[0] & 0xFF));
        return true;
    case kServicePattern: {
        // A sprite's mask is thirty-two bytes in RAM; this is the one call that
        // copies it into the chip, so a listing can say what a sprite looks like
        // with `DB` rows instead of sixty stores.
        const int pattern = r.r[0] % Video::kPatternCount;
        const std::uint32_t source = r.r[1];
        for (int index = 0; index < Video::kSpriteBytes; ++index) {
            video_.write(Video::kRegisterOffset + Video::kPatternBase +
                             static_cast<std::uint32_t>(pattern * Video::kSpriteBytes + index),
                         bus_.read(source + static_cast<std::uint32_t>(index)));
        }
        return true;
    }
    case kServiceNote:
        audio_.trigger(r.r[0], r.r[1], r.r[2], static_cast<std::uint8_t>(r.r[3] & 0xFF));
        return true;
    case kServiceWave:
        // One sample of one slot: `R0` slot, `R1` index, `R2` the sample. The bank is
        // memory-mapped too; this is the call that spares a program the 24-bit
        // arithmetic, which is what a BIOS is for.
        audio_.write(Audio::kWaveformBase + static_cast<std::uint32_t>(r.r[0] % Audio::kWaveformCount) *
                                                Audio::kWaveformLength +
                         static_cast<std::uint32_t>(r.r[1] % Audio::kWaveformLength),
                     static_cast<std::uint8_t>(r.r[2] & 0xFF));
        return true;
    default:
        cpu.fail("unknown service INT " + std::to_string(vector));
        return false;
    }
}

// ---------------------------------------------------------------------------
// Snapshot
// ---------------------------------------------------------------------------

std::uint64_t Machine::hash() const {
    std::uint64_t value = kFnvOffset;
    for (const std::uint8_t byte : bus_.ram()) {
        value = mix(value, byte);
    }
    value ^= video_.hash();
    value = mix(value, static_cast<std::uint8_t>(cpu_.status()));
    for (const std::uint16_t reg : cpu_.registers().r) {
        value = mix(value, static_cast<std::uint8_t>(reg & 0xFF));
        value = mix(value, static_cast<std::uint8_t>(reg >> 8));
    }
    return value;
}

std::vector<std::uint8_t> Machine::save() {
    std::vector<std::uint8_t> image;
    image.reserve(2 * 1024 * 1024);
    image.push_back('H');
    image.push_back('B');
    image.push_back('2');
    image.push_back('M');
    image.push_back(static_cast<std::uint8_t>(kSnapshotVersion));

    appendWord(image, static_cast<std::uint32_t>(bus_.ram().size()));
    image.insert(image.end(), bus_.ram().begin(), bus_.ram().end());

    const std::vector<std::uint8_t> video_state = video_.state();
    appendWord(image, static_cast<std::uint32_t>(video_state.size()));
    image.insert(image.end(), video_state.begin(), video_state.end());

    const std::vector<std::uint8_t> audio_state = audio_.state();
    appendWord(image, static_cast<std::uint32_t>(audio_state.size()));
    image.insert(image.end(), audio_state.begin(), audio_state.end());

    appendWord(image, cpu_.registers().pc);
    appendWord(image, cpu_.registers().sp);
    appendWord(image, cpu_.registers().flags);
    for (const std::uint16_t reg : cpu_.registers().r) {
        appendWord(image, reg);
    }
    appendWord(image, static_cast<std::uint32_t>(console_.size()));
    image.insert(image.end(), console_.begin(), console_.end());
    return image;
}

bool Machine::restore(const std::vector<std::uint8_t>& image) {
    if (image.size() < 5 || image[0] != 'H' || image[1] != 'B' || image[2] != '2' ||
        image[3] != 'M' || image[4] != kSnapshotVersion) {
        return false;
    }
    std::size_t at = 5;
    const auto take = [&image, &at](std::size_t count) -> const std::uint8_t* {
        if (at + count > image.size()) {
            return nullptr;
        }
        const std::uint8_t* pointer = image.data() + at;
        at += count;
        return pointer;
    };

    const std::uint32_t ram_size = readWord(image, at);
    at += 4;
    if (ram_size != bus_.ram().size()) {
        return false;
    }
    const std::uint8_t* ram = take(ram_size);
    if (ram == nullptr) {
        return false;
    }
    std::copy(ram, ram + ram_size, bus_.ram().begin());

    const std::uint32_t video_size = readWord(image, at);
    at += 4;
    const std::uint8_t* video = take(video_size);
    if (video == nullptr) {
        return false;
    }
    video_.setState(std::vector<std::uint8_t>(video, video + video_size));
    // The 1982 map follows the video mode, as it does when a program asks for MODE 0:
    // a restored machine has to route the same way the machine that saved it did.
    bus_.setCompat(video_.mode() == Video::Mode::Text);

    const std::uint32_t audio_size = readWord(image, at);
    at += 4;
    const std::uint8_t* audio = take(audio_size);
    if (audio == nullptr) {
        return false;
    }
    audio_.setState(std::vector<std::uint8_t>(audio, audio + audio_size));

    if (at + 7 * 4 > image.size()) {
        return false;
    }
    CpuRegisters registers;
    registers.pc = readWord(image, at);
    at += 4;
    registers.sp = static_cast<std::uint16_t>(readWord(image, at));
    at += 4;
    registers.flags = static_cast<std::uint16_t>(readWord(image, at));
    at += 4;
    for (std::uint16_t& reg : registers.r) {
        reg = static_cast<std::uint16_t>(readWord(image, at));
        at += 4;
    }
    registers.cycles = cpu_.registers().cycles;
    registers.instructions = cpu_.registers().instructions;
    cpu_.registers() = registers;

    const std::uint32_t console_size = readWord(image, at);
    at += 4;
    const std::uint8_t* console = take(console_size);
    if (console == nullptr) {
        return false;
    }
    console_.assign(reinterpret_cast<const char*>(console), console_size);
    return true;
}

std::vector<std::string> mapListing() {
    std::vector<std::string> rows;
    for (const Region& region : kRegions) {
        std::string row = std::string{region.name};
        while (row.size() < 12) {
            row.push_back(' ');
        }
        row += hexRegion(region.base, region.size);
        row += region.compat_only ? "  (MODE 0)" : "";
        rows.push_back(row);
    }
    return rows;
}

}  // namespace timewarp::hb2
