// SPDX-License-Identifier: MIT
//
// The sound and port layers, tested without a speaker or a board.
//
// Both are pure enough to check exactly: `PLAY`'s notation becomes a list of
// tones with arithmetic that either matches equal temperament or does not, the
// mixer produces bytes a test can count, and a serial port can be exercised
// against a pseudo-terminal -- a real device node with a real file descriptor,
// whose other end the test owns. That is how GPIO and Arduino work with no
// hardware attached; the alternative is a mock that proves the mock.

#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

#include <cstdlib>
#include <fcntl.h>
#include <unistd.h>

#include "check.hpp"
#include "timewarp/audio/music.hpp"
#include "timewarp/io/ports.hpp"

namespace {

void announce(const char* name) {
    std::printf("-- %s\n", name);
    std::fflush(stdout);
}

using timewarp::audio::ParseResult;
using timewarp::audio::Tone;
using timewarp::audio::Waveform;

/// Equal temperament, so the expected frequencies are arithmetic rather than
/// magic numbers copied from the implementation.
double expected(double semitones_from_a4) {
    return 440.0 * std::pow(2.0, semitones_from_a4 / 12.0);
}

void test_play_parses_notes() {
    announce("PLAY notation");
    ParseResult error;

    // The default octave is 4, so `C` is middle C: nine semitones below A4.
    const std::vector<Tone> scale =
        timewarp::audio::parse_play("CDEFGAB", &error);
    TW_CHECK(error.ok);
    TW_CHECK_EQ(scale.size(), std::size_t{7});
    TW_CHECK_NEAR(scale.at(0).frequency, expected(-9.0), 0.01);   // C4
    TW_CHECK_NEAR(scale.at(5).frequency, expected(0.0), 0.01);    // A4 = 440
    TW_CHECK_NEAR(scale.at(6).frequency, expected(2.0), 0.01);    // B4

    // Accidentals and octaves. A flat and a sharp of the same key are one pitch,
    // `O` sets the octave and `>`/`<` step it: five notes, seven tokens.
    const std::vector<Tone> accidentals =
        timewarp::audio::parse_play("C#5 Db5 O3 C > C < C", &error);
    TW_CHECK(error.ok);
    TW_CHECK_EQ(accidentals.size(), std::size_t{5});
    TW_CHECK_NEAR(accidentals.at(0).frequency, accidentals.at(1).frequency, 0.01);
    TW_CHECK_NEAR(accidentals.at(0).frequency, expected(4.0), 0.01);    // C#5: four above A4
    TW_CHECK_NEAR(accidentals.at(2).frequency, expected(-21.0), 0.01);  // C3
    TW_CHECK_NEAR(accidentals.at(3).frequency, expected(-9.0), 0.01);   // C4
    TW_CHECK_NEAR(accidentals.at(4).frequency, expected(-21.0), 0.01);  // C3 again

    // A rest is a tone with no pitch, not a gap in the list: it still takes time.
    const std::vector<Tone> with_rest = timewarp::audio::parse_play("C P C", &error);
    TW_CHECK_EQ(with_rest.size(), std::size_t{3});
    TW_CHECK_NEAR(with_rest.at(1).frequency, 0.0, 0.0);

    // `N` counts semitones from C0, where `N1` is that C: so `N60` is B4, two
    // semitones above A4, and `N0` is a rest.
    const std::vector<Tone> numbered = timewarp::audio::parse_play("N0 N60", &error);
    TW_CHECK(error.ok);
    TW_CHECK_NEAR(numbered.at(0).frequency, 0.0, 0.0);
    TW_CHECK_NEAR(numbered.at(1).frequency, expected(2.0), 0.01);

    // Nothing at all is a mistake rather than silence.
    const std::vector<Tone> empty = timewarp::audio::parse_play("", &error);
    TW_CHECK(!error.ok);
    TW_CHECK(empty.empty());
    const std::vector<Tone> garbage = timewarp::audio::parse_play("H", &error);
    TW_CHECK(!error.ok);
    TW_CHECK(garbage.empty());
}

void test_play_lengths_and_tempo() {
    announce("PLAY lengths");
    ParseResult error;

    // T120: a quarter note is 60000/120 = 500ms. L8 halves it, `.` adds half
    // again, and a note's own digits override the default.
    const std::vector<Tone> quarters = timewarp::audio::parse_play("T120 L4 C", &error);
    TW_CHECK(error.ok);
    TW_CHECK_EQ(quarters.at(0).milliseconds, 500);

    const std::vector<Tone> eighths = timewarp::audio::parse_play("T120 L8 C", &error);
    TW_CHECK_EQ(eighths.at(0).milliseconds, 250);

    const std::vector<Tone> dotted = timewarp::audio::parse_play("T120 C4.", &error);
    TW_CHECK_EQ(dotted.at(0).milliseconds, 750);

    // A note's own octave digit wins over `O`, so a length is set with `L`.
    const std::vector<Tone> own_length = timewarp::audio::parse_play("T60 L2 C", &error);
    TW_CHECK_EQ(own_length.at(0).milliseconds, 2000);

    // A tempo of 60 halves every note's speed relative to 120.
    const std::vector<Tone> slow = timewarp::audio::parse_play("T60 L4 C", &error);
    TW_CHECK_EQ(slow.at(0).milliseconds, 1000);
}

void test_render_and_wav() {
    announce("rendering and WAV");
    Tone tone;
    tone.frequency = 440.0;
    tone.milliseconds = 100;
    tone.volume = 0.5;

    const std::vector<std::int16_t> pcm = timewarp::audio::render({tone}, 22050);
    // 100ms at 22.05kHz is 2205 samples, plus the release tail the mixer adds.
    TW_CHECK(pcm.size() >= std::size_t{2205});
    TW_CHECK(pcm.size() < std::size_t{2205 + 2205});

    // It is a square wave at volume 0.5, so the loudest sample is about half of
    // full scale -- not silence, and not clipping.
    std::int16_t loudest = 0;
    for (const std::int16_t sample : pcm) {
        loudest = std::max(loudest, static_cast<std::int16_t>(std::abs(sample)));
    }
    TW_CHECK(loudest > 10000);
    TW_CHECK(loudest < 20000);

    // A rest renders silence, which is what makes `PLAY "P"` quiet rather than
    // broken.
    Tone rest = tone;
    rest.frequency = 0.0;
    const std::vector<std::int16_t> silence = timewarp::audio::render({rest}, 22050);
    bool any_sound = false;
    for (const std::int16_t sample : silence) {
        any_sound = any_sound || sample != 0;
    }
    TW_CHECK(!any_sound);

    // The file is what a player expects: RIFF, WAVE, and the data size in the
    // header matching the samples.
    const std::string file = timewarp::audio::wav(pcm, 22050);
    TW_CHECK_EQ(file.size(), std::size_t{44} + pcm.size() * 2);
    TW_CHECK_EQ(file.compare(0, 4, "RIFF"), 0);
    TW_CHECK_EQ(file.compare(8, 4, "WAVE"), 0);
    TW_CHECK_EQ(file.compare(12, 4, "fmt "), 0);
    TW_CHECK_EQ(file.compare(36, 4, "data"), 0);
    const auto data_size =
        static_cast<std::uint32_t>(static_cast<unsigned char>(file[40])) |
        (static_cast<std::uint32_t>(static_cast<unsigned char>(file[41])) << 8);
    TW_CHECK_EQ(data_size, static_cast<std::uint32_t>(pcm.size() * 2));

    // Deterministic: the same tune is the same bytes, which is what makes a
    // rendered file comparable at all.
    TW_CHECK(timewarp::audio::wav(timewarp::audio::render({tone}, 22050), 22050) == file);
}

void test_sound_ticks() {
    announce("SOUND ticks");
    // One tick is 1/18.2s, so `SOUND 440, 18` is about a second. This is the
    // number the acknowledgement prints as well.
    const Tone tone = timewarp::audio::tone_from_sound(440, 989);
    TW_CHECK_EQ(tone.milliseconds, 989);
    TW_CHECK_NEAR(tone.frequency, 440.0, 0.0);
    TW_CHECK(tone.waveform == Waveform::Square);

    // A zero-frequency `SOUND` is a rest, and a zero duration is one tick rather
    // than a zero-length buffer the mixer would divide by.
    const Tone quiet = timewarp::audio::tone_from_sound(0, 0);
    TW_CHECK_NEAR(quiet.frequency, 0.0, 0.0);
    TW_CHECK_EQ(quiet.milliseconds, 1);
}

void test_port_policy() {
    announce("port policy");
    const timewarp::io::PortPolicy& policy = timewarp::io::PortPolicy::current();

    // The device families a school plugs in.
    TW_CHECK(policy.allows("/dev/ttyUSB0"));
    TW_CHECK(policy.allows("/dev/ttyACM0"));
    TW_CHECK(policy.allows("/dev/ttyAMA0"));
    TW_CHECK(policy.allows("/dev/serial/by-id/usb-Arduino-if00"));
    TW_CHECK(policy.allows("/dev/lp0"));
    TW_CHECK(policy.allows("/dev/parport0"));
    // The pseudo-terminals the tests use, and which a program may open because
    // they belong to the user running it.
    TW_CHECK(policy.allows("/dev/pts/3"));

    // Everything else is refused, including the ways round the rule.
    TW_CHECK(!policy.allows("/dev/mem"));
    TW_CHECK(!policy.allows("/dev/sda"));
    TW_CHECK(!policy.allows("/etc/passwd"));
    TW_CHECK(!policy.allows(""));
    TW_CHECK(!policy.allows("dev/ttyUSB0"));
    TW_CHECK(!policy.allows("/dev/../mem"));
    TW_CHECK(!policy.allows("/tmp/../etc/shadow"));
    // The prefix list must not be read as allowing the controlling terminal, or
    // the disk the system runs from.
    TW_CHECK(!policy.allows("/dev/tty"));
}

/// A pseudo-terminal pair, which is a real serial device with the other end in
/// the test's hand.
class PseudoTerminal {
public:
    PseudoTerminal() {
        master_ = ::posix_openpt(O_RDWR | O_NOCTTY);
        if (master_ < 0 || ::grantpt(master_) != 0 || ::unlockpt(master_) != 0) {
            if (master_ >= 0) {
                ::close(master_);
            }
            master_ = -1;
            return;
        }
        const char* name = ::ptsname(master_);
        if (name != nullptr) {
            slave_ = name;
        }
    }
    ~PseudoTerminal() {
        if (master_ >= 0) {
            ::close(master_);
        }
    }
    PseudoTerminal(const PseudoTerminal&) = delete;
    PseudoTerminal& operator=(const PseudoTerminal&) = delete;

    [[nodiscard]] bool valid() const { return master_ >= 0 && !slave_.empty(); }
    [[nodiscard]] int master() const { return master_; }
    [[nodiscard]] const std::string& slave() const { return slave_; }

    /// What the device sent, read from the test's end.
    [[nodiscard]] std::string readFromDevice(std::size_t bytes) {
        std::string collected;
        for (int attempt = 0; attempt < 100 && collected.size() < bytes; ++attempt) {
            char buffer[64];
            const ssize_t count = ::read(master_, buffer, sizeof buffer);
            if (count > 0) {
                collected.append(buffer, static_cast<std::size_t>(count));
            } else {
                ::usleep(2000);
            }
        }
        return collected;
    }

    void sendToDevice(const std::string& text) {
        [[maybe_unused]] const ssize_t written =
            ::write(master_, text.data(), text.size());
    }

private:
    int master_ = -1;
    std::string slave_;
};

void test_serial_over_a_pseudo_terminal() {
    announce("serial port");
    PseudoTerminal pty;
    TW_CHECK(pty.valid());
    if (!pty.valid()) {
        return;
    }

    timewarp::io::SerialPort port;
    std::string error;
    const bool opened = port.open(pty.slave(), 9600, &error);
    // The message first: `TW_CHECK` on the boolean alone says only that it
    // failed, and the reason is the part worth reading.
    TW_CHECK_EQ(error, std::string{});
    TW_CHECK(opened);
    TW_CHECK(port.is_open());

    // A program writing to the port is a board receiving bytes.
    TW_CHECK(port.write("PING\n", &error));
    TW_CHECK_EQ(pty.readFromDevice(5), std::string{"PING\n"});

    // A board sending a line is a program reading one.
    pty.sendToDevice("TEMP 21.5\n");
    TW_CHECK_EQ(port.read_line(2000, &error), std::string{"TEMP 21.5"});

    // A timeout with nothing sent is empty, not an error: a program polling a
    // quiet sensor must not see a failure.
    error.clear();
    TW_CHECK_EQ(port.read_line(50, &error), std::string{});
    TW_CHECK(error.empty());

    port.close();
    TW_CHECK(!port.is_open());
    // Writing after close is a refusal with a reason, not a crash.
    TW_CHECK(!port.write("x", &error));
    TW_CHECK(!error.empty());
}

void test_ports_refuse_unsafe_paths() {
    announce("ports refuse unsafe paths");
    timewarp::io::SerialPort port;
    std::string error;
    TW_CHECK(!port.open("/dev/mem", 9600, &error));
    TW_CHECK(error.find("not a port") != std::string::npos);

    timewarp::io::ParallelPort parallel;
    TW_CHECK(!parallel.open("/etc/passwd", &error));
    TW_CHECK(!parallel.is_open());
}

}  // namespace

int main() {
    test_play_parses_notes();
    test_play_lengths_and_tempo();
    test_render_and_wav();
    test_sound_ticks();
    test_port_policy();
    test_serial_over_a_pseudo_terminal();
    test_ports_refuse_unsafe_paths();
    return tw::test::summary("media");
}
