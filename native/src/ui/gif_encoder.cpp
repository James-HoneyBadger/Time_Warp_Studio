// SPDX-License-Identifier: MIT

#include "timewarp/ui/gif_encoder.hpp"

#include <QHash>
#include <QSaveFile>

#include <cstdint>

namespace timewarp::ui::gif {

namespace {

/// Six levels per channel: 0, 51, 102, 153, 204, 255. The cube is 216 colours and
/// the table is padded to 256, which is what a GIF's colour table has to be.
constexpr int kLevels = 6;
constexpr int kTableEntries = 256;
constexpr int kLzwMinimumCodeSize = 8;

void appendU16(QByteArray& out, int value) {
    out.append(static_cast<char>(value & 0xFF));
    out.append(static_cast<char>((value >> 8) & 0xFF));
}

int levelOf(int component) {
    // Nearest level, so a flat 0x33 grey lands on 51 rather than falling to 0.
    return qBound(0, (component * (kLevels - 1) + 127) / 255, kLevels - 1);
}

std::uint8_t paletteIndex(QRgb colour) {
    const int red = levelOf(qRed(colour));
    const int green = levelOf(qGreen(colour));
    const int blue = levelOf(qBlue(colour));
    return static_cast<std::uint8_t>(red * kLevels * kLevels + green * kLevels + blue);
}

QByteArray colourTable() {
    QByteArray table;
    table.reserve(kTableEntries * 3);
    for (int red = 0; red < kLevels; ++red) {
        for (int green = 0; green < kLevels; ++green) {
            for (int blue = 0; blue < kLevels; ++blue) {
                table.append(static_cast<char>(red * 255 / (kLevels - 1)));
                table.append(static_cast<char>(green * 255 / (kLevels - 1)));
                table.append(static_cast<char>(blue * 255 / (kLevels - 1)));
            }
        }
    }
    table.append(QByteArray(kTableEntries * 3 - table.size(), '\0'));
    return table;
}

/// GIF's variable-width codes are packed least-significant bit first, which is
/// the one detail of the format that cannot be guessed from the spec's examples.
class BitWriter {
public:
    explicit BitWriter(QByteArray& out) : out_(out) {}

    void write(int code, int bits) {
        buffer_ |= static_cast<std::uint32_t>(code) << count_;
        count_ += bits;
        while (count_ >= 8) {
            out_.append(static_cast<char>(buffer_ & 0xFF));
            buffer_ >>= 8;
            count_ -= 8;
        }
    }

    void finish() {
        if (count_ > 0) {
            out_.append(static_cast<char>(buffer_ & 0xFF));
            buffer_ = 0;
            count_ = 0;
        }
    }

private:
    QByteArray& out_;
    std::uint32_t buffer_ = 0;
    int count_ = 0;
};

/// GIF LZW: clear, then the longest dictionary match, growing the code width as
/// the dictionary fills. The dictionary is keyed by `(prefix code << 8) | next`,
/// which is unique because a pixel index is a byte.
QByteArray lzwCompress(const QByteArray& indices) {
    QByteArray packed;
    BitWriter bits{packed};

    const int clear = 1 << kLzwMinimumCodeSize;
    const int end = clear + 1;
    int code_size = kLzwMinimumCodeSize + 1;
    int next_code = end + 1;

    QHash<int, int> dictionary;

    bits.write(clear, code_size);
    if (indices.isEmpty()) {
        bits.write(end, code_size);
        bits.finish();
        return packed;
    }

    int prefix = static_cast<std::uint8_t>(indices.at(0));
    for (int position = 1; position < indices.size(); ++position) {
        const int next = static_cast<std::uint8_t>(indices.at(position));
        const int key = (prefix << 8) | next;
        const auto found = dictionary.constFind(key);
        if (found != dictionary.constEnd()) {
            prefix = found.value();
            continue;
        }

        bits.write(prefix, code_size);
        if (next_code < 4096) {
            dictionary.insert(key, next_code);
            ++next_code;
            // The code the decoder reads next has to carry this many bits, so the
            // width grows as soon as the table has outgrown the old one.
            if (next_code > (1 << code_size) && code_size < 12) {
                ++code_size;
            }
        } else {
            // A full dictionary is cleared rather than grown: 12 bits is the
            // format's ceiling, and a second clear code is how a long frame
            // starts over.
            bits.write(clear, code_size);
            dictionary.clear();
            next_code = end + 1;
            code_size = kLzwMinimumCodeSize + 1;
        }
        prefix = next;
    }
    bits.write(prefix, code_size);
    bits.write(end, code_size);
    bits.finish();
    return packed;
}

/// GIF data is carried in sub-blocks of at most 255 bytes, each with its length
/// byte, and the run ends with a zero-length block.
void appendSubBlocks(QByteArray& out, const QByteArray& data) {
    for (int offset = 0; offset < data.size(); offset += 255) {
        const int chunk = qMin(255, data.size() - offset);
        out.append(static_cast<char>(chunk));
        out.append(data.constData() + offset, chunk);
    }
    out.append('\0');
}

void fail(QString* error, const QString& message) {
    if (error != nullptr) {
        *error = message;
    }
}

}  // namespace

void appendFrame(QVector<Frame>& frames, const QImage& image, int delayMs) {
    if (!frames.isEmpty() && frames.last().image == image) {
        frames.last().delayMs += delayMs;
        return;
    }
    Frame frame;
    frame.image = image;
    frame.delayMs = delayMs;
    frames.append(frame);
}

bool writeGif(const QString& path, const QVector<Frame>& frames, QString* error) {
    if (frames.isEmpty()) {
        fail(error, QStringLiteral("there are no frames to write"));
        return false;
    }
    const QSize size = frames.constFirst().image.size();
    if (size.width() <= 0 || size.height() <= 0) {
        fail(error, QStringLiteral("the first frame has no size"));
        return false;
    }
    for (const Frame& frame : frames) {
        if (frame.image.size() != size) {
            fail(error, QStringLiteral("every frame must be the same size"));
            return false;
        }
    }

    QByteArray out;
    out.append(QByteArrayLiteral("GIF89a"));
    appendU16(out, size.width());
    appendU16(out, size.height());
    // Global colour table, 256 entries (the low three bits are log2(256) - 1).
    out.append(static_cast<char>(0xF7));
    out.append('\0');  // background colour index
    out.append('\0');  // pixel aspect ratio: square
    out.append(colourTable());

    // NETSCAPE2.0 loop extension with a repeat count of zero, which means forever.
    out.append(QByteArrayLiteral("\x21\xFF\x0B"
                                 "NETSCAPE2.0"
                                 "\x03\x01\x00\x00\x00"));

    for (const Frame& frame : frames) {
        const QImage image = frame.image.convertToFormat(QImage::Format_RGB32);
        QByteArray indices;
        indices.reserve(size.width() * size.height());
        for (int y = 0; y < size.height(); ++y) {
            const auto* line = reinterpret_cast<const QRgb*>(image.constScanLine(y));
            for (int x = 0; x < size.width(); ++x) {
                indices.append(static_cast<char>(paletteIndex(line[x])));
            }
        }

        // Graphic control extension: dispose "leave in place", delay in
        // hundredths of a second, no transparency.
        out.append('\x21');
        out.append('\xF9');
        out.append('\x04');
        out.append(static_cast<char>(0x04));
        // A zero delay is read as "as fast as possible" by some viewers, so the
        // smallest honest delay is one hundredth of a second.
        appendU16(out, qBound(1, (frame.delayMs + 5) / 10, 0xFFFF));
        out.append('\0');
        out.append('\0');

        out.append('\x2C');  // image descriptor
        appendU16(out, 0);
        appendU16(out, 0);
        appendU16(out, size.width());
        appendU16(out, size.height());
        out.append('\0');  // no local table, not interlaced
        out.append(static_cast<char>(kLzwMinimumCodeSize));
        appendSubBlocks(out, lzwCompress(indices));
    }
    out.append('\x3B');  // trailer

    QSaveFile file{path};
    if (!file.open(QIODevice::WriteOnly)) {
        fail(error, file.errorString());
        return false;
    }
    if (file.write(out) != out.size() || !file.commit()) {
        fail(error, file.errorString());
        return false;
    }
    return true;
}

}  // namespace timewarp::ui::gif
