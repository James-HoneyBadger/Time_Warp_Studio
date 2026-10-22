// SPDX-License-Identifier: MIT
//
// Draws the application icon and writes it in the three formats the packages use:
//
//   packaging/linux/time-warp-studio.png     the desktop entry's icon
//   packaging/windows/time-warp-studio.ico   16/24/32/48/64/128/256
//   packaging/macos/time-warp-studio.icns    16..512
//
// This is the C++ successor to `Scripts/generate_icon.py`, and it draws the same
// design from the same geometry: a dark field, ten concentric rings (one per
// language) and a single amber square in the middle, every size rendered from the
// shapes rather than downscaled from one bitmap, which is what keeps the
// sixteen-pixel icon legible instead of a smudge.
//
// It is C++ because the repository no longer carries a Python interpreter, and
// because the icons are a release artefact of the same tree as everything else, so
// what produces them belongs in the build. It links zlib and nothing else: the PNG
// chunks need a CRC-32 and a deflate stream, and those are zlib's to compute. It
// deliberately does not link Qt -- this is a command-line generator, not a part of
// the IDE.
//
// The three files it writes are committed, so it is held to byte equality with
// them: `--check` regenerates in memory, compares against what is on disk and exits
// 1 if a committed icon is stale. That is what keeps a release diff readable
// rather than carrying silent icon churn.
//
//   time_warp_icon                  write the three icons under the checkout
//   time_warp_icon --root DIR       write them under DIR instead
//   time_warp_icon --check          compare, write nothing, exit 1 if stale
//
// Exit status: 0 success (or up to date), 1 a stale or missing icon in `--check`,
// 2 the arguments did not make a run, 3 a write failed.

#include <zlib.h>

#include "timewarp/tools/cli.hpp"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

namespace {

namespace fs = std::filesystem;

using Bytes = std::vector<std::uint8_t>;
using Rgb = std::array<std::uint8_t, 3>;

constexpr Rgb kBackground{20, 20, 40};
constexpr Rgb kSquareColour{255, 200, 50};
constexpr int kRingCount = 10;
constexpr double kRingInner = 20.0;
constexpr double kRingWidth = 2.0;
constexpr std::array<double, 4> kSquareBox{100.0, 100.0, 156.0, 156.0};
constexpr double kDesignSize = 256.0;

/// The size the desktop entry's icon is drawn at.
constexpr int kLinuxSize = 256;

struct Ring {
    double radius;
    double thickness;
    Rgb colour;
};

/// Supersampling per axis. Enough that the two-pixel ring survives at 16 px; the
/// large sizes need less of it and cost quadratically more.
[[nodiscard]] int supersample(int size) {
    return size <= 128 ? 4 : 2;
}

[[nodiscard]] Rgb ringColour(int index) {
    return {static_cast<std::uint8_t>(100 + index * 15), 200, 255};
}

[[nodiscard]] Rgb sampleAt(double x,
                           double y,
                           double size,
                           const std::vector<Ring>& rings,
                           const std::array<double, 4>& square) {
    if (square[0] <= x && x < square[2] && square[1] <= y && y < square[3]) {
        return kSquareColour;
    }
    const double centre = size / 2.0;
    const double dx = x - centre;
    const double dy = y - centre;
    const double distance = std::sqrt(dx * dx + dy * dy);
    for (const Ring& ring : rings) {
        if (ring.radius - ring.thickness <= distance && distance <= ring.radius) {
            return ring.colour;
        }
    }
    return kBackground;
}

/// `size` x `size` RGBA pixels, anti-aliased by supersampling.
[[nodiscard]] Bytes render(int size) {
    const double exact = static_cast<double>(size);
    const double scale = exact / kDesignSize;
    const int samples = supersample(size);

    std::vector<Ring> rings;
    rings.reserve(kRingCount);
    for (int index = 0; index < kRingCount; ++index) {
        const double inset = kRingInner + static_cast<double>(index) * 10.0;
        const double radius = (kDesignSize / 2.0 - inset) * scale;
        double thickness = kRingWidth * scale * std::sqrt(exact / kDesignSize);
        // A ring thinner than a pixel would disappear at small sizes; widen it
        // just enough to keep a visible line rather than a faint one.
        rings.push_back({radius, std::max(thickness, 0.9), ringColour(index)});
    }

    std::array<double, 4> square{};
    for (std::size_t index = 0; index < square.size(); ++index) {
        square[index] = kSquareBox[index] * scale;
    }

    const std::size_t stride = static_cast<std::size_t>(size) * 4U;
    Bytes pixels(stride * static_cast<std::size_t>(size));
    const double inverse = 1.0 / (samples * samples);
    for (int y = 0; y < size; ++y) {
        for (int x = 0; x < size; ++x) {
            int red = 0;
            int green = 0;
            int blue = 0;
            for (int subY = 0; subY < samples; ++subY) {
                for (int subX = 0; subX < samples; ++subX) {
                    const double sx =
                        static_cast<double>(x) + (static_cast<double>(subX) + 0.5) / samples;
                    const double sy =
                        static_cast<double>(y) + (static_cast<double>(subY) + 0.5) / samples;
                    const Rgb colour = sampleAt(sx, sy, exact, rings, square);
                    red += colour[0];
                    green += colour[1];
                    blue += colour[2];
                }
            }
            const std::size_t offset =
                static_cast<std::size_t>(y) * stride + static_cast<std::size_t>(x) * 4U;
            pixels[offset] = static_cast<std::uint8_t>(static_cast<int>(red * inverse + 0.5));
            pixels[offset + 1] = static_cast<std::uint8_t>(static_cast<int>(green * inverse + 0.5));
            pixels[offset + 2] = static_cast<std::uint8_t>(static_cast<int>(blue * inverse + 0.5));
            pixels[offset + 3] = 255;
        }
    }
    return pixels;
}

void appendByte(Bytes& out, std::uint8_t value) {
    out.push_back(value);
}

void appendBig32(Bytes& out, std::uint32_t value) {
    appendByte(out, static_cast<std::uint8_t>((value >> 24) & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>((value >> 16) & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>((value >> 8) & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>(value & 0xFFU));
}

void appendLittle16(Bytes& out, std::uint16_t value) {
    appendByte(out, static_cast<std::uint8_t>(value & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>((value >> 8) & 0xFFU));
}

void appendLittle32(Bytes& out, std::uint32_t value) {
    appendByte(out, static_cast<std::uint8_t>(value & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>((value >> 8) & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>((value >> 16) & 0xFFU));
    appendByte(out, static_cast<std::uint8_t>((value >> 24) & 0xFFU));
}

/// Deflate at level 9, the level the Python this replaced asked `zlib.compress`
/// for. `compress2` is the same call underneath, with the same window, memory
/// level and strategy, so the stream -- and the file -- is the same stream.
[[nodiscard]] std::optional<Bytes> deflate9(const Bytes& raw) {
    uLongf bound = compressBound(static_cast<uLong>(raw.size()));
    Bytes compressed(bound);
    const int status =
        compress2(compressed.data(), &bound, raw.data(), static_cast<uLong>(raw.size()), 9);
    if (status != Z_OK) {
        return std::nullopt;
    }
    compressed.resize(bound);
    return compressed;
}

void appendChunk(Bytes& out, std::string_view tag, const Bytes& payload) {
    appendBig32(out, static_cast<std::uint32_t>(payload.size()));
    out.insert(out.end(), tag.begin(), tag.end());
    out.insert(out.end(), payload.begin(), payload.end());
    uLong crc = crc32(0L, Z_NULL, 0);
    crc = crc32(crc, reinterpret_cast<const Bytef*>(tag.data()), static_cast<uInt>(tag.size()));
    // zlib reads a null buffer as "start over", so an empty chunk -- IEND -- must
    // not be handed its null `data()`.
    if (!payload.empty()) {
        crc = crc32(crc, payload.data(), static_cast<uInt>(payload.size()));
    }
    appendBig32(out, static_cast<std::uint32_t>(crc));
}

/// RGBA pixels as a PNG: colour type 6, eight bits a channel, no interlace, and
/// filter type 0 on every row.
[[nodiscard]] std::optional<Bytes> encodePng(const Bytes& pixels, int size) {
    const std::size_t stride = static_cast<std::size_t>(size) * 4U;
    Bytes raw;
    raw.reserve((stride + 1U) * static_cast<std::size_t>(size));
    for (int y = 0; y < size; ++y) {
        const std::size_t offset = static_cast<std::size_t>(y) * stride;
        raw.push_back(0);
        raw.insert(raw.end(), pixels.begin() + static_cast<std::ptrdiff_t>(offset),
                   pixels.begin() + static_cast<std::ptrdiff_t>(offset + stride));
    }

    const std::optional<Bytes> compressed = deflate9(raw);
    if (!compressed) {
        return std::nullopt;
    }

    Bytes header;
    appendBig32(header, static_cast<std::uint32_t>(size));
    appendBig32(header, static_cast<std::uint32_t>(size));
    appendByte(header, 8);
    appendByte(header, 6);
    appendByte(header, 0);
    appendByte(header, 0);
    appendByte(header, 0);

    Bytes out{0x89U, 'P', 'N', 'G', '\r', '\n', 0x1AU, '\n'};
    appendChunk(out, "IHDR", header);
    appendChunk(out, "IDAT", *compressed);
    appendChunk(out, "IEND", {});
    return out;
}

/// One 32-bit DIB icon image, the form every Windows icon loader reads. The small
/// sizes are carried this way rather than as PNGs because the icon also goes to
/// NSIS, which reads the installer's icon itself and does not decode
/// PNG-compressed entries.
[[nodiscard]] Bytes encodeDib(const Bytes& pixels, int size) {
    const std::uint32_t stride = static_cast<std::uint32_t>(size) * 4U;
    Bytes header;
    appendLittle32(header, 40);
    appendLittle32(header, static_cast<std::uint32_t>(size));
    // The DIB's height covers the colour image *and* the AND mask.
    appendLittle32(header, static_cast<std::uint32_t>(size * 2));
    appendLittle16(header, 1);
    appendLittle16(header, 32);
    appendLittle32(header, 0);
    appendLittle32(header, stride * static_cast<std::uint32_t>(size));
    appendLittle32(header, 0);
    appendLittle32(header, 0);
    appendLittle32(header, 0);
    appendLittle32(header, 0);

    Bytes body;
    body.reserve(static_cast<std::size_t>(stride) * static_cast<std::size_t>(size));
    for (int y = size - 1; y >= 0; --y) {  // bottom-up
        const std::size_t row = static_cast<std::size_t>(y) * stride;
        for (int x = 0; x < size; ++x) {
            const std::size_t offset = row + static_cast<std::size_t>(x) * 4U;
            body.push_back(pixels[offset + 2]);
            body.push_back(pixels[offset + 1]);
            body.push_back(pixels[offset]);
            body.push_back(pixels[offset + 3]);
        }
    }

    // The AND mask: one bit a pixel, all zero because the alpha channel is what
    // decides transparency. Rows are padded to four bytes.
    const std::size_t maskStride = ((static_cast<std::size_t>(size) + 31U) / 32U) * 4U;
    body.insert(body.end(), maskStride * static_cast<std::size_t>(size), 0);

    header.insert(header.end(), body.begin(), body.end());
    return header;
}

struct Rendered {
    int size = 0;
    Bytes pixels;
    Bytes png;
};

struct IconFile {
    fs::path relative;
    Bytes contents;
};

constexpr std::array<int, 8> kRenderedSizes{16, 24, 32, 48, 64, 128, 256, 512};
// The .ico keeps 24 for the Windows small-icon list and the rest for Explorer.
// 512 is not an ICO size -- the directory entry cannot name it -- so it goes to
// the .icns only.
constexpr std::array<int, 7> kIcoSizes{16, 24, 32, 48, 64, 128, 256};
// The .icns wants 512 as the Retina 256; 24/48/64 are ICO-only sizes.
constexpr std::array<int, 6> kIcnsSizes{16, 32, 64, 128, 256, 512};

[[nodiscard]] const Rendered& renderedImage(const std::vector<Rendered>& images, int size) {
    const auto found = std::find_if(images.begin(), images.end(),
                                    [size](const Rendered& image) { return image.size == size; });
    return *found;
}

/// One entry per size; every one is a PNG, which macOS has accepted since 10.7.
[[nodiscard]] std::string_view icnsType(int size) {
    const auto found = std::find(kIcnsSizes.begin(), kIcnsSizes.end(), size);
    const std::size_t index = static_cast<std::size_t>(found - kIcnsSizes.begin());
    static constexpr std::array<std::string_view, 6> kTags{"icp4", "icp5", "icp6",
                                                           "ic07", "ic08", "ic09"};
    return kTags[index];
}

[[nodiscard]] Bytes encodeIco(const std::vector<Rendered>& images) {
    std::vector<Bytes> payloads;
    payloads.reserve(kIcoSizes.size());
    for (const int size : kIcoSizes) {
        const Rendered& image = renderedImage(images, size);
        payloads.push_back(size >= 128 ? image.png : encodeDib(image.pixels, size));
    }

    Bytes out;
    appendLittle16(out, 0);
    appendLittle16(out, 1);
    appendLittle16(out, static_cast<std::uint16_t>(kIcoSizes.size()));
    std::uint32_t offset = static_cast<std::uint32_t>(6U + 16U * kIcoSizes.size());
    for (std::size_t index = 0; index < kIcoSizes.size(); ++index) {
        const int size = kIcoSizes[index];
        const Bytes& payload = payloads[index];
        // 256 is written as 0: a directory entry's width and height are bytes.
        appendByte(out, static_cast<std::uint8_t>(size < 256 ? size : 0));
        appendByte(out, static_cast<std::uint8_t>(size < 256 ? size : 0));
        appendByte(out, 0);
        appendByte(out, 0);
        appendLittle16(out, 1);
        appendLittle16(out, 32);
        appendLittle32(out, static_cast<std::uint32_t>(payload.size()));
        appendLittle32(out, offset);
        offset += static_cast<std::uint32_t>(payload.size());
    }
    for (const Bytes& payload : payloads) {
        out.insert(out.end(), payload.begin(), payload.end());
    }
    return out;
}

[[nodiscard]] Bytes encodeIcns(const std::vector<Rendered>& images) {
    Bytes body;
    for (const int size : kIcnsSizes) {
        const std::string_view tag = icnsType(size);
        const Bytes& data = renderedImage(images, size).png;
        body.insert(body.end(), tag.begin(), tag.end());
        appendBig32(body, static_cast<std::uint32_t>(data.size() + 8U));
        body.insert(body.end(), data.begin(), data.end());
    }

    Bytes out{'i', 'c', 'n', 's'};
    appendBig32(out, static_cast<std::uint32_t>(body.size() + 8U));
    out.insert(out.end(), body.begin(), body.end());
    return out;
}

[[nodiscard]] std::optional<std::vector<IconFile>> buildIcons(std::string& error) {
    std::vector<Rendered> images;
    images.reserve(kRenderedSizes.size());
    for (const int size : kRenderedSizes) {
        Rendered image;
        image.size = size;
        image.pixels = render(size);
        const std::optional<Bytes> png = encodePng(image.pixels, size);
        if (!png) {
            error = "zlib could not compress the " + std::to_string(size) + "-pixel image";
            return std::nullopt;
        }
        image.png = *png;
        images.push_back(std::move(image));
    }

    std::vector<IconFile> files;
    files.push_back({fs::path{"packaging"} / "linux" / "time-warp-studio.png",
                     renderedImage(images, kLinuxSize).png});
    files.push_back({fs::path{"packaging"} / "windows" / "time-warp-studio.ico", encodeIco(images)});
    files.push_back({fs::path{"packaging"} / "macos" / "time-warp-studio.icns", encodeIcns(images)});
    return files;
}

[[nodiscard]] std::optional<Bytes> readFile(const fs::path& path) {
    std::error_code status;
    if (!fs::is_regular_file(path, status)) {
        return std::nullopt;
    }
    std::ifstream input(path, std::ios::binary);
    if (!input) {
        return std::nullopt;
    }
    input.seekg(0, std::ios::end);
    const std::streamoff length = input.tellg();
    input.seekg(0, std::ios::beg);
    Bytes contents;
    if (length > 0) {
        contents.resize(static_cast<std::size_t>(length));
        input.read(reinterpret_cast<char*>(contents.data()), static_cast<std::streamsize>(length));
        if (!input) {
            return std::nullopt;
        }
    }
    return contents;
}

[[nodiscard]] bool writeFile(const fs::path& path, const Bytes& contents, std::string& error) {
    std::error_code status;
    const fs::path parent = path.parent_path();
    if (!parent.empty()) {
        fs::create_directories(parent, status);
        if (status) {
            error = "cannot create " + parent.generic_string() + ": " + status.message();
            return false;
        }
    }
    std::ofstream output(path, std::ios::binary | std::ios::trunc);
    if (!output) {
        error = "cannot open " + path.generic_string() + " for writing";
        return false;
    }
    output.write(reinterpret_cast<const char*>(contents.data()),
                 static_cast<std::streamsize>(contents.size()));
    if (!output) {
        error = "cannot write " + path.generic_string();
        return false;
    }
    return true;
}

/// The checkout this tool writes into. The path configured at build time is
/// preferred, so a binary in a build tree outside the repository still updates the
/// icons it was built from; failing that, the directories above the executable are
/// walked looking for the `VERSION` marker rather than counting levels.
[[nodiscard]] std::optional<fs::path> locateRoot(const char* program) {
    std::error_code status;
    const fs::path configured{TIME_WARP_SOURCE_DIR};
    if (fs::exists(configured / "VERSION", status)) {
        return configured;
    }
    const fs::path executable = fs::absolute(program, status);
    if (status) {
        return std::nullopt;
    }
    fs::path directory = executable.parent_path();
    for (int depth = 0; depth < 8 && !directory.empty(); ++depth) {
        if (fs::exists(directory / "VERSION", status)) {
            return directory;
        }
        const fs::path parent = directory.parent_path();
        if (parent == directory) {
            break;
        }
        directory = parent;
    }
    return std::nullopt;
}

constexpr std::string_view kUsage{
    "usage: time_warp_icon [--check]\n"
    "\n"
    "  --check      compare with the files on disk, write nothing, and exit 1 if\n"
    "               any is stale or missing\n"
    "\n"
    "With no --root the checkout is found from the executable's own location, so a\n"
    "build tree anywhere still writes the icons the packages carry.\n"};

}  // namespace

int main(int argc, char** argv) {
    const timewarp::cli::Invocation invocation =
        timewarp::cli::parse("time_warp_icon", argc, argv, /*expects_command=*/false);
    if (invocation.help) {
        std::cout << kUsage;
        std::printf("\n  --root DIR    the checkout to write under\n"
                    "  --json PATH   write the machine report to PATH (\"-\" for stdout)\n"
                    "  --quiet       only what failed\n"
                    "  --help        this message\n");
        return 0;
    }
    if (!invocation.ok()) {
        std::fprintf(stderr, "time_warp_icon: %s\n", invocation.error.c_str());
        std::cout << kUsage;
        return 2;
    }

    const bool check = invocation.has("--check");
    for (const std::string& argument : invocation.positionals()) {
        std::fprintf(stderr, "time_warp_icon: unknown argument '%s'\n", argument.c_str());
        std::cout << kUsage;
        return 2;
    }

    // The default root, in order: what the caller said, then the checkout the
    // executable sits in, then where the command was run.
    fs::path root = invocation.root;
    if (!invocation.root_given) {
        const std::optional<fs::path> located = locateRoot(argv[0]);
        if (located) {
            root = *located;
        }
    }

    std::string error;
    const std::optional<std::vector<IconFile>> icons = buildIcons(error);
    if (!icons) {
        std::fprintf(stderr, "❌ %s\n", error.c_str());
        return 3;
    }

    timewarp::cli::Report report{"time_warp_icon", invocation};
    int exitCode = 0;
    for (const IconFile& icon : *icons) {
        const fs::path target = root / icon.relative;
        const std::string name = icon.relative.generic_string();
        if (check) {
            const std::optional<Bytes> committed = readFile(target);
            if (!committed) {
                report.check(name, false, "missing");
                continue;
            }
            if (*committed != icon.contents) {
                report.check(name, false, "stale");
                continue;
            }
            report.check(name, true);
            continue;
        }
        if (!writeFile(target, icon.contents, error)) {
            std::fprintf(stderr, "❌ %s\n", error.c_str());
            return 3;
        }
        report.check(name, true);
    }
    exitCode = report.finish();
    return exitCode == 2 ? 3 : exitCode;
}
