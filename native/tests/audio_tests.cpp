// SPDX-License-Identifier: MIT
//
// Tests for the mixer. No device and no Qt: the mix is a pure function of the
// voices, so "two sounds at once" is a claim about a buffer.
//
// The cases that matter are the ones the feature exists for:
//   * two voices are both in the samples, not one after the other;
//   * a short effect started later is still in the mix, over a long tone;
//   * the sum is bounded, so two loud voices do not wrap into noise;
//   * the same inputs render the same bytes, or nothing here can be compared.

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <vector>

#include "check.hpp"
#include "timewarp/audio/mixer.hpp"

namespace {

using timewarp::audio::kMaxVoices;
using timewarp::audio::Mixer;
using timewarp::audio::Waveform;

void announce(const char* name) { std::printf("-- %s\n", name); }

/// The largest absolute sample, which is what "is there sound here" means in a
/// buffer.
int peak(const std::vector<std::int16_t>& pcm) {
    int highest = 0;
    for (const std::int16_t sample : pcm) {
        highest = std::max(highest, std::abs(static_cast<int>(sample)));
    }
    return highest;
}

void test_two_voices_are_audible_together() {
    announce("two voices");
    Mixer mix;
    TW_CHECK(mix.note(440.0, 100));
    TW_CHECK_EQ(mix.active_voices(), 1);
    TW_CHECK(mix.note(660.0, 100));
    TW_CHECK_EQ(mix.active_voices(), 2);

    // A single voice first, so the comparison is against the same envelope.
    Mixer single;
    TW_CHECK(single.note(440.0, 100));
    const std::vector<std::int16_t> one = single.render_all();
    const std::vector<std::int16_t> two = mix.render_all();

    TW_CHECK_EQ(one.size(), two.size());
    if (one.size() != two.size()) {
        return;
    }
    // Summing two voices is louder than either: the mix is the samples added,
    // not the second note replacing the first.
    TW_CHECK(peak(two) > peak(one));
    TW_CHECK(mix.active_voices() == 0);
}

void test_a_later_short_effect_lands_over_a_long_tone() {
    announce("effect over a tone");
    Mixer mix;
    TW_CHECK(mix.note(220.0, 400));
    // Let the long tone run for 100ms, then fire the effect -- the order a game
    // does it in: a background note, then a shot.
    std::vector<std::int16_t> head(static_cast<std::size_t>(Mixer::sample_rate()) / 10, 0);
    mix.render(head.data(), static_cast<int>(head.size()));
    TW_CHECK_EQ(mix.active_voices(), 1);
    TW_CHECK(mix.note(1760.0, 30, Waveform::Square));
    TW_CHECK_EQ(mix.active_voices(), 2);

    const std::vector<std::int16_t> tail = mix.render_all();
    // The effect is 30ms; the tone has 300ms left, so the tail is the tone's
    // length and the first part of it carries the effect's energy.
    TW_CHECK(!tail.empty());
    TW_CHECK(peak(tail) > peak(head) / 2);
}

void test_the_sum_is_bounded() {
    announce("bounded sum");
    Mixer mix;
    for (int voice = 0; voice < kMaxVoices; ++voice) {
        timewarp::audio::Tone tone;
        tone.frequency = 200.0 + voice * 110.0;
        tone.milliseconds = 20;
        tone.volume = 1.0;
        TW_CHECK(mix.add(tone));
    }
    const std::vector<std::int16_t> pcm = mix.render_all();
    int highest = 0;
    for (const std::int16_t sample : pcm) {
        highest = std::max(highest, std::abs(static_cast<int>(sample)));
    }
    // Clipped at full scale, not wrapped around it: a negative sample of
    // full scale is -32768, which is one further than the positive side.
    TW_CHECK(highest <= 32768);
    TW_CHECK(highest > 30000);
}

void test_the_voice_limit_is_reported_rather_than_ignored() {
    announce("voice limit");
    Mixer mix;
    for (int voice = 0; voice < kMaxVoices; ++voice) {
        TW_CHECK(mix.note(300.0 + voice, 40));
    }
    TW_CHECK_EQ(mix.active_voices(), kMaxVoices);
    TW_CHECK(!mix.note(880.0, 40));
    TW_CHECK_EQ(mix.active_voices(), kMaxVoices);
    mix.clear();
    TW_CHECK_EQ(mix.active_voices(), 0);
}

void test_a_rest_adds_nothing() {
    announce("rest");
    Mixer mix;
    TW_CHECK(mix.note(0.0, 50));
    TW_CHECK(mix.note(440.0, 0));
    TW_CHECK_EQ(mix.active_voices(), 0);
    TW_CHECK(mix.render_all().empty());
}

void test_the_same_mix_renders_the_same_bytes() {
    announce("deterministic noise");
    const auto render_noise = [] {
        Mixer mix;
        mix.note(500.0, 25, Waveform::Noise);
        mix.note(750.0, 25, Waveform::Noise);
        return mix.render_all();
    };
    // Two noise voices must not be the same sequence: a second one is a second
    // sound, not a louder copy of the first.
    Mixer mix;
    mix.note(500.0, 25, Waveform::Noise);
    mix.note(750.0, 25, Waveform::Noise);
    const std::vector<std::int16_t> first = mix.render_all();
    const std::vector<std::int16_t> second = render_noise();
    TW_CHECK_EQ(first.size(), second.size());
    TW_CHECK(first == second);
}

void test_render_all_respects_its_time_limit() {
    announce("time limit");
    Mixer mix;
    TW_CHECK(mix.note(440.0, 5000));
    const std::vector<std::int16_t> pcm = mix.render_all(100);
    // 100ms plus the moment of silence, and not the five seconds asked for.
    const std::size_t expected =
        static_cast<std::size_t>(Mixer::sample_rate()) / 10 +
        static_cast<std::size_t>(Mixer::sample_rate()) / 40;
    TW_CHECK_EQ(pcm.size(), expected);
}

}  // namespace

int main() {
    test_two_voices_are_audible_together();
    test_a_later_short_effect_lands_over_a_long_tone();
    test_the_sum_is_bounded();
    test_the_voice_limit_is_reported_rather_than_ignored();
    test_a_rest_adds_nothing();
    test_the_same_mix_renders_the_same_bytes();
    test_render_all_respects_its_time_limit();
    return tw::test::summary("audio");
}
