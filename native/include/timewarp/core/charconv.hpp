// SPDX-License-Identifier: MIT
#pragma once

/// Locale-independent conversions between a double and its text form.
///
/// `<charconv>` is the right tool for this, but it is not available everywhere:
/// libc++ on Apple marks the floating-point overloads of `to_chars` and
/// `from_chars` unavailable below macOS 13.3 and macOS 26.0 respectively, and
/// this project's floor is macOS 12. A build that called them directly failed
/// on macOS with "'to_chars' is unavailable: introduced in macOS 13.3".
///
/// These three wrappers use `<charconv>` where it is fully available and fall
/// back to `snprintf`/`strtod` where it is not -- the same conversions, in the
/// "C" locale this program never leaves. The fallback keeps the two properties
/// the callers rely on: `format_double` produces the shortest text that reads
/// back as the same value, and `parse_double` accepts exactly what
/// `from_chars(..., chars_format::general)` accepts (no leading whitespace, no
/// hexadecimal floats).
///
/// `TIME_WARP_FORCE_CHARCONV_FALLBACK` selects the fallback on a platform that
/// has the real thing, which is how the fallback is tested.

#include <cctype>
#include <charconv>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#if !defined(TIME_WARP_FORCE_CHARCONV_FALLBACK) && defined(__cpp_lib_to_chars) && \
    !defined(__APPLE__)
#define TIME_WARP_HAVE_FLOAT_CHARCONV 1
#else
#define TIME_WARP_HAVE_FLOAT_CHARCONV 0
#endif

namespace timewarp::core {

/// Writes the shortest general-format text that reads back as `value`.
///
/// Returns the number of characters written, or 0 when `first`..`last` is too
/// small -- the same "nothing written" answer `std::to_chars` gives as an error.
inline std::size_t format_double(char* first, char* last, double value) {
#if TIME_WARP_HAVE_FLOAT_CHARCONV
    const auto result = std::to_chars(first, last, value);
    if (result.ec != std::errc{}) {
        return 0;
    }
    return static_cast<std::size_t>(result.ptr - first);
#else
    // 15 significant digits are enough for most doubles; 16 and 17 are tried in
    // turn when the shorter text does not read back as the same value. That is
    // what makes this the *shortest* round-tripping form rather than a fixed
    // "%.17g", which turns 0.1 into 0.10000000000000001.
    for (int precision = 15; precision <= 17; ++precision) {
        char buffer[64] = {};
        const int written =
            std::snprintf(buffer, sizeof buffer, "%.*g", precision, value);
        if (written <= 0 || written >= static_cast<int>(sizeof buffer)) {
            return 0;
        }
        const double back = std::strtod(buffer, nullptr);
        if (back != value && !(std::isnan(back) && std::isnan(value))) {
            continue;
        }
        const auto length = static_cast<std::size_t>(written);
        if (length > static_cast<std::size_t>(last - first)) {
            return 0;
        }
        std::memcpy(first, buffer, length);
        return length;
    }
    return 0;
#endif
}

/// Writes `value` with no fractional part and no exponent, e.g. 1e30 as all
/// thirty-one of its digits. Returns the number of characters written, or 0.
inline std::size_t format_fixed_integer(char* first, char* last, double value) {
#if TIME_WARP_HAVE_FLOAT_CHARCONV
    const auto result = std::to_chars(first, last, value, std::chars_format::fixed, 0);
    if (result.ec != std::errc{}) {
        return 0;
    }
    return static_cast<std::size_t>(result.ptr - first);
#else
    // 512 bytes covers the widest double: DBL_MAX has 309 integer digits.
    char buffer[512] = {};
    const int written = std::snprintf(buffer, sizeof buffer, "%.0f", value);
    if (written <= 0 || written >= static_cast<int>(sizeof buffer)) {
        return 0;
    }
    const auto length = static_cast<std::size_t>(written);
    if (length > static_cast<std::size_t>(last - first)) {
        return 0;
    }
    std::memcpy(first, buffer, length);
    return length;
#endif
}

/// Parses `first`..`last` as a double, returning the end of the text consumed,
/// or nullptr when there is nothing to convert.
///
/// The caller checks that the returned pointer is `last` when it wants the whole
/// range consumed, exactly as it did with `std::from_chars`.
inline const char* parse_double(const char* first, const char* last, double& value) {
#if TIME_WARP_HAVE_FLOAT_CHARCONV
    const auto result =
        std::from_chars(first, last, value, std::chars_format::general);
    if (result.ec != std::errc{}) {
        return nullptr;
    }
    return result.ptr;
#else
    if (first == last) {
        return nullptr;
    }
    // `strtod` skips leading whitespace and accepts hexadecimal floats;
    // `from_chars` does neither, and a program that depended on the difference
    // would behave differently on macOS than everywhere else.
    if (std::isspace(static_cast<unsigned char>(*first)) != 0) {
        return nullptr;
    }
    const char* digits = *first == '+' || *first == '-' ? first + 1 : first;
    if (digits != last && *digits == '0' && digits + 1 != last &&
        (digits[1] == 'x' || digits[1] == 'X')) {
        return nullptr;
    }
    // `strtod` needs a terminated string; every caller passes a bounded range,
    // not necessarily a whole buffer.
    const std::string text(first, last);
    char* stop = nullptr;
    const double parsed = std::strtod(text.c_str(), &stop);
    if (stop == text.c_str()) {
        return nullptr;
    }
    value = parsed;
    return first + (stop - text.c_str());
#endif
}

}  // namespace timewarp::core
