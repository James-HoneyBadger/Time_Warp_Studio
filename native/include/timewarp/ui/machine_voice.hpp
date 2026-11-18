// SPDX-License-Identifier: MIT
//
// The machine's own voice: the sounds an HB-1 makes that are not a program's.
//
// ---------------------------------------------------------------------------
// Why a machine makes a noise
// ---------------------------------------------------------------------------
//
// Until 20.2 the HB-1 was silent unless a *program* asked for a sound: the
// `SoundPlayer` existed only to be handed to a runner. A computer of the period was
// not like that. It chirped when the power came on, clicked when a key went down,
// rattled when the disk head stepped, and buzzed when a program did something wrong
// -- and a reader who has used one remembers those four sounds better than the
// screen.
//
// These are not samples. They are short tone sequences built from the same
// `audio::Mixer` a program's `SOUND` uses, because a machine with one sound chip
// should have one voice and not two.
//
// #### Counting, so a test can hear
//
// Every request is counted, sounded or not: `count(Event::Boot)` after one cold boot
// is one, and it is still one with the volume at zero. `sounded()` is what actually
// reached the player, so "muted means the beeper said nothing" is an assertion and
// not a hope.

#pragma once

#include <array>
#include <cstddef>
#include <vector>

#include <QObject>

#include "timewarp/audio/music.hpp"

namespace timewarp::ui {

class SoundPlayer;

class MachineVoice final : public QObject {
    Q_OBJECT

public:
    /// The five sounds the machine makes by itself.
    enum class Event {
        /// Power on, and a cold boot.
        Boot,
        /// A key went down at the prompt.
        Key,
        /// The disk head moved: `SAVE`, `LOAD`, `CATALOG`, a commit.
        Disk,
        /// A program reported an error.
        Error,
        /// The tape motor ran: `TSAVE`/`TLOAD`.
        Tape,
    };
    static constexpr std::size_t kEventCount = 5;

    explicit MachineVoice(QObject* parent = nullptr);

    /// The player to speak through. Null is normal in a headless run: events are
    /// still counted, and nothing can hear them.
    void setPlayer(SoundPlayer* player) { player_ = player; }

    /// Silences the machine without changing what it *did*: the counter still counts.
    void setMuted(bool muted) { muted_ = muted; }
    [[nodiscard]] bool muted() const noexcept { return muted_; }

    /// 0..100, the same scale CONFIG shows. Scales every tone the machine makes; a
    /// program's own `SOUND` volume is the program's business.
    void setVolume(int percent);
    [[nodiscard]] int volume() const noexcept { return volume_; }

    /// Makes one sound, if it is not muted.
    void play(Event event);
    /// A tone from the sound-latch register, which is a program's `POKE`.
    void tone(int code);

    [[nodiscard]] int count(Event event) const;
    /// How many sounds reached the player since the machine was made.
    [[nodiscard]] int sounded() const noexcept { return sounded_; }
    /// Everything asked for, muted or not.
    [[nodiscard]] int requested() const noexcept { return requested_; }
    void resetCounts();

    /// The tones `event` is made of, so a test can check the sound without a speaker
    /// and the manual can describe it.
    [[nodiscard]] static std::vector<timewarp::audio::Tone> tonesFor(Event event);
    /// The tone a sound-latch code means: 100 Hz plus 8 Hz per step, 80 ms long.
    [[nodiscard]] static timewarp::audio::Tone toneForCode(int code);

private:
    void emit_tones(const std::vector<timewarp::audio::Tone>& tones);

    SoundPlayer* player_ = nullptr;
    bool muted_ = false;
    int volume_ = 60;
    int sounded_ = 0;
    int requested_ = 0;
    std::array<int, kEventCount> counts_{};
};

}  // namespace timewarp::ui
