// SPDX-License-Identifier: MIT

#include "timewarp/ui/diff.hpp"

#include <QStringList>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace timewarp::ui {

namespace {

/// The largest LCS table worth building: a million cells is four megabytes and a
/// few milliseconds, and everything past it is two files that have little to say to
/// each other.
constexpr std::size_t kMaxCells = 1'000'000;

}  // namespace

QStringList diffLines(const QString& text) {
    QStringList lines = text.split(QLatin1Char('\n'));
    if (!lines.isEmpty() && lines.last().isEmpty()) {
        lines.removeLast();
    }
    return lines;
}

QList<DiffLine> compareDocuments(const QString& before, const QString& after) {
    const QStringList left = diffLines(before);
    const QStringList right = diffLines(after);
    const auto line = [](const QStringList& list, std::size_t index) {
        return list.at(static_cast<int>(index));
    };
    const std::size_t left_size = static_cast<std::size_t>(left.size());
    const std::size_t right_size = static_cast<std::size_t>(right.size());

    // The common head and tail, which is most of any two versions of one document.
    std::size_t head = 0;
    while (head < left_size && head < right_size && line(left, head) == line(right, head)) {
        ++head;
    }
    std::size_t tail = 0;
    while (tail < left_size - head && tail < right_size - head &&
           line(left, left_size - 1 - tail) == line(right, right_size - 1 - tail)) {
        ++tail;
    }

    QList<DiffLine> result;
    result.reserve(static_cast<int>(left_size + right_size));
    for (std::size_t index = 0; index < head; ++index) {
        result.append({DiffKind::Context, line(left, index)});
    }

    const std::size_t na = left_size - head - tail;
    const std::size_t nb = right_size - head - tail;
    if (na == 0 || nb == 0 || na * nb > kMaxCells) {
        // Nothing in common worth finding: every line of the middle went, and every
        // line of the other middle arrived.
        for (std::size_t index = 0; index < na; ++index) {
            result.append({DiffKind::Removed, line(left, head + index)});
        }
        for (std::size_t index = 0; index < nb; ++index) {
            result.append({DiffKind::Added, line(right, head + index)});
        }
    } else {
        const std::size_t columns = nb + 1;
        std::vector<std::uint32_t> table((na + 1) * columns, 0);
        for (std::size_t i = 1; i <= na; ++i) {
            for (std::size_t j = 1; j <= nb; ++j) {
                table[i * columns + j] =
                    line(left, head + i - 1) == line(right, head + j - 1)
                        ? table[(i - 1) * columns + (j - 1)] + 1
                        : std::max(table[(i - 1) * columns + j],
                                   table[i * columns + (j - 1)]);
            }
        }

        // Walk back from the corner, collecting in reverse, then reverse the run.
        QList<DiffLine> middle;
        std::size_t i = na;
        std::size_t j = nb;
        while (i > 0 || j > 0) {
            if (i > 0 && j > 0 && line(left, head + i - 1) == line(right, head + j - 1)) {
                middle.append({DiffKind::Context, line(left, head + i - 1)});
                --i;
                --j;
                // A strict `>`: on a tie the walk takes the addition, so that the
                // reversal below puts the removal first. `-old` above `+new` is the
                // order every diff presents a replacement in, and the other way
                // round reads as "new, then what it replaced".
            } else if (j == 0 || (i > 0 && table[(i - 1) * columns + j] >
                                              table[i * columns + (j - 1)])) {
                middle.append({DiffKind::Removed, line(left, head + i - 1)});
                --i;
            } else {
                middle.append({DiffKind::Added, line(right, head + j - 1)});
                --j;
            }
        }
        std::reverse(middle.begin(), middle.end());
        result += middle;
    }

    for (std::size_t index = 0; index < tail; ++index) {
        result.append({DiffKind::Context, line(left, left_size - tail + index)});
    }
    return result;
}

DiffCounts countChanges(const QList<DiffLine>& lines) {
    DiffCounts counts;
    for (const DiffLine& line : lines) {
        if (line.kind == DiffKind::Added) {
            ++counts.added;
        } else if (line.kind == DiffKind::Removed) {
            ++counts.removed;
        }
    }
    return counts;
}

QString unifiedDiff(const QString& before, const QString& after, int context) {
    const QList<DiffLine> lines = compareDocuments(before, after);
    const DiffCounts counts = countChanges(lines);
    if (counts.empty()) {
        return {};
    }

    const int around = std::max(0, context);
    // Which lines of the result are within `around` of a change: those are the ones
    // a hunk shows, and the rest are the gaps a `@@` header stands for.
    const int total = lines.size();
    // std::vector, not QList: Qt 5.15 has no QList(size, value) constructor.
    std::vector<char> shown(static_cast<std::size_t>(total), 0);
    for (int index = 0; index < total; ++index) {
        if (lines.at(index).kind == DiffKind::Context) {
            continue;
        }
        for (int other = std::max(0, index - around);
             other <= std::min(total - 1, index + around); ++other) {
            shown[static_cast<std::size_t>(other)] = 1;
        }
    }

    // The line numbers each side has reached, for the hunk headers.
    int left_line = 1;
    int right_line = 1;
    QString out;
    int index = 0;
    while (index < total) {
        if (!shown.at(index)) {
            if (lines.at(index).kind != DiffKind::Added) {
                ++left_line;
            }
            if (lines.at(index).kind != DiffKind::Removed) {
                ++right_line;
            }
            ++index;
            continue;
        }
        const int hunk_start_left = left_line;
        const int hunk_start_right = right_line;
        QString body;
        int left_count = 0;
        int right_count = 0;
        while (index < total && shown.at(index)) {
            const DiffLine& line = lines.at(index);
            switch (line.kind) {
                case DiffKind::Context:
                    body += QLatin1Char(' ') + line.text + QLatin1Char('\n');
                    ++left_line;
                    ++right_line;
                    ++left_count;
                    ++right_count;
                    break;
                case DiffKind::Removed:
                    body += QLatin1Char('-') + line.text + QLatin1Char('\n');
                    ++left_line;
                    ++left_count;
                    break;
                case DiffKind::Added:
                    body += QLatin1Char('+') + line.text + QLatin1Char('\n');
                    ++right_line;
                    ++right_count;
                    break;
            }
            ++index;
        }
        out += QStringLiteral("@@ -%1,%2 +%3,%4 @@\n")
                   .arg(hunk_start_left)
                   .arg(left_count)
                   .arg(hunk_start_right)
                   .arg(right_count);
        out += body;
    }
    return out;
}

QString diffSummary(const QList<DiffLine>& lines) {
    const DiffCounts counts = countChanges(lines);
    if (counts.empty()) {
        return QStringLiteral("no changes");
    }
    return QStringLiteral("+%1 \u2212%2").arg(counts.added).arg(counts.removed);
}

}  // namespace timewarp::ui
