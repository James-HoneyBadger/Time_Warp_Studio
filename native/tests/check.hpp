// SPDX-License-Identifier: MIT
//
// Minimal dependency-free test harness.
//
// The port is intentionally standalone (no gtest/Catch2), so the whole
// library plus its tests build with nothing but a C++20 compiler and CMake.

#pragma once

#include <cstdio>
#include <string>
#include <string_view>
#include <type_traits>

namespace tw::test {

inline int g_checks = 0;
inline int g_failures = 0;

/// Renders a value for failure messages.
template <class T>
std::string show(const T& value) {
    if constexpr (std::is_same_v<std::remove_cvref_t<T>, bool>) {
        return value ? "true" : "false";
    } else if constexpr (std::is_arithmetic_v<std::remove_cvref_t<T>>) {
        return std::to_string(value);
    } else if constexpr (std::is_convertible_v<T, std::string_view>) {
        return "\"" + std::string{std::string_view{value}} + "\"";
    } else if constexpr (requires { value.toStdString(); }) {
        // QString, QByteArray and anything else that renders itself. A requires-
        // expression rather than a Qt include: this header is shared with the
        // suites that link no Qt at all.
        return "\"" + value.toStdString() + "\"";
    } else {
        return "<value>";
    }
}

inline void fail(const char* file, int line, const std::string& message) {
    ++g_failures;
    std::printf("FAIL %s:%d\n     %s\n", file, line, message.c_str());
    // Flushed immediately, and not for tidiness: a test that dereferences an empty
    // container crashes in release builds, and with block-buffered stdout the
    // assertion that failed *just before* the crash was still in the buffer and
    // went to the floor with the process. The one message that explains the crash
    // was the one message nobody saw.
    std::fflush(stdout);
}

/// Prints the tally and returns a process exit code.
[[nodiscard]] inline int summary(const char* suite) {
    std::printf("%-24s %3d checks, %d failure(s)\n", suite, g_checks,
                g_failures);
    std::fflush(stdout);
    return g_failures == 0 ? 0 : 1;
}

}  // namespace tw::test

#define TW_CHECK(cond)                                                       \
    do {                                                                     \
        ++::tw::test::g_checks;                                              \
        if (!(cond)) {                                                       \
            ::tw::test::fail(__FILE__, __LINE__, "expected: " #cond);        \
        }                                                                    \
    } while (false)

#define TW_CHECK_EQ(actual, expected)                                        \
    do {                                                                     \
        ++::tw::test::g_checks;                                              \
        const auto& tw_actual = (actual);                                    \
        const auto& tw_expected = (expected);                                \
        if (!(tw_actual == tw_expected)) {                                   \
            ::tw::test::fail(__FILE__, __LINE__,                             \
                             std::string{"expected "} +                      \
                                 ::tw::test::show(tw_expected) +             \
                                 " but got " + ::tw::test::show(tw_actual)); \
        }                                                                    \
    } while (false)

#define TW_CHECK_NEAR(actual, expected, tolerance)                           \
    do {                                                                     \
        ++::tw::test::g_checks;                                              \
        const double tw_actual = (actual);                                   \
        const double tw_expected = (expected);                               \
        const double tw_tolerance = (tolerance);                             \
        const double tw_delta = tw_actual > tw_expected                      \
                                    ? tw_actual - tw_expected                 \
                                    : tw_expected - tw_actual;                \
        if (!(tw_delta <= tw_tolerance)) {                                   \
            ::tw::test::fail(__FILE__, __LINE__,                             \
                             std::string{"expected "} +                      \
                                 ::tw::test::show(tw_expected) +             \
                                 " (+-" + ::tw::test::show(tw_tolerance) +   \
                                 ") but got " + ::tw::test::show(tw_actual));\
        }                                                                    \
    } while (false)

#define TW_CHECK_THROWS(expr, ExceptionType)                                 \
    do {                                                                     \
        ++::tw::test::g_checks;                                              \
        bool tw_threw = false;                                               \
        try {                                                                \
            (void)(expr);                                                    \
        } catch (const ExceptionType&) {                                     \
            tw_threw = true;                                                 \
        } catch (...) {                                                      \
            ::tw::test::fail(__FILE__, __LINE__,                             \
                             "wrong exception type from: " #expr);           \
            tw_threw = true;                                                 \
        }                                                                    \
        if (!tw_threw) {                                                     \
            ::tw::test::fail(__FILE__, __LINE__,                             \
                             "expected " #ExceptionType " from: " #expr);    \
        }                                                                    \
    } while (false)
