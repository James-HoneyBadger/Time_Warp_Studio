// SPDX-License-Identifier: MIT
//
// A temporary directory that a test cannot forget to clean up, and that reports
// the path it used when creating it fails.
//
// Every phase of `docs/PORTING_PLAN.md` that touches disk -- autosave, version
// history, run history, find-in-files, classroom bundles -- needs one of these,
// and the failure that matters is a test writing into a learner's real
// `~/.time_warp`. That is what `TIME_WARP_DATA_DIR` exists for; this header is
// how a test uses it.

#pragma once

#include <QDir>
#include <QFile>
#include <QString>
#include <QTemporaryDir>

#include "check.hpp"

namespace tw::test {

/// A scratch directory for one test. Destroyed when it goes out of scope.
class TempDir {
public:
    /// Creates the directory and checks that it worked. A constructor cannot
    /// return an error, so `valid()` is what a caller checks before using it.
    TempDir() : dir_() {
        if (!dir_.isValid()) {
            TW_CHECK(false);
        }
    }

    TempDir(const TempDir&) = delete;
    TempDir& operator=(const TempDir&) = delete;
    TempDir(TempDir&&) = delete;
    TempDir& operator=(TempDir&&) = delete;
    ~TempDir() = default;

    [[nodiscard]] bool valid() const { return dir_.isValid(); }

    /// The directory's own path, no trailing separator.
    [[nodiscard]] QString path() const { return dir_.path(); }

    /// `path()` joined with `relative`, with the parent directories created.
    /// Empty when the parent could not be created.
    [[nodiscard]] QString path(const QString& relative) const {
        const QString full = dir_.filePath(relative);
        const QDir parent = QFileInfo{full}.dir();
        if (!parent.exists() && !parent.mkpath(QStringLiteral("."))) {
            return {};
        }
        return full;
    }

    /// Writes `text` to `relative`, creating parents. Returns the path, or an
    /// empty string on failure.
    ///
    /// Not `[[nodiscard]]`: the common use is a fixture that writes four files for
    /// its side effects, and forcing a cast at each call site would make the test
    /// harder to read than the failure it is guarding against. A test that cares
    /// whether the write worked checks the path in the case where that is the
    /// thing under test.
    QString write(const QString& relative, const QByteArray& text) const {
        const QString full = path(relative);
        if (full.isEmpty()) {
            return {};
        }
        QFile file{full};
        if (!file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
            return {};
        }
        if (file.write(text) != text.size()) {
            return {};
        }
        file.close();
        return full;
    }

    /// Points `TIME_WARP_DATA_DIR` at this directory, so the code under test
    /// writes here rather than in a home directory. The previous value is
    /// restored in `restoreDataDir()`.
    void useAsDataDir() const {
        previous_data_dir_ = qgetenv("TIME_WARP_DATA_DIR");
        had_previous_ = !previous_data_dir_.isEmpty();
        qputenv("TIME_WARP_DATA_DIR", dir_.path().toUtf8());
    }

    /// Puts `TIME_WARP_DATA_DIR` back the way it was.
    void restoreDataDir() const {
        if (had_previous_) {
            qputenv("TIME_WARP_DATA_DIR", previous_data_dir_);
        } else {
            qunsetenv("TIME_WARP_DATA_DIR");
        }
    }

private:
    QTemporaryDir dir_;
    mutable QByteArray previous_data_dir_;
    mutable bool had_previous_ = false;
};

}  // namespace tw::test
