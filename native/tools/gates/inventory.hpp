// SPDX-License-Identifier: MIT
//
// The command inventory: which literals each executor dispatches on.
//
// Extracted from the C++ sources rather than kept as a list, because a list is a
// second copy of the dispatchers and the copy that is wrong is always the one the
// report trusts. It is a *candidate* list: the comparison harvests punctuation and
// error messages along with the commands, and `isCommandLiteral` is what keeps the
// figure about commands rather than about every string in the file.
//
// The inventory is what makes the command table's completeness measurable: every
// candidate is either exercised by a row or waived with a reason, and a gap is a
// prompt to look rather than proof of a defect.

#pragma once

#include <QMap>
#include <QSet>
#include <QString>

namespace timewarp::gates::inventory {

/// One language's candidates.
struct Entry {
    QSet<QString> dispatchLiterals;
    QSet<QString> tableCommands;
    QSet<QString> notCommands;

    [[nodiscard]] QSet<QString> implemented() const {
        return dispatchLiterals | tableCommands;
    }
};

/// Every language's candidates, by the module name the sources are keyed by.
[[nodiscard]] QMap<QString, Entry> collect();

}  // namespace timewarp::gates::inventory
