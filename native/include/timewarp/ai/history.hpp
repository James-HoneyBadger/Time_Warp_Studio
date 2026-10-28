// SPDX-License-Identifier: MIT
//
// The conversation the panel can keep between sessions.
//
// A conversation is normally a session: closing the application forgets it, which
// is the right answer for a machine in a classroom and the reason nothing here
// writes anything unless the user asks for it. Asking for it is one tick in AI
// Settings, and this is the file that tick writes.
//
// #### Why the directory is a parameter
//
// `timewarp::ai` knows nothing about the application's data directory -- it is the
// library the command-line tool and the window both use -- so the caller says where
// the file goes. The panel passes the data directory's `assistant` leaf; a test
// passes a temporary directory and never touches a learner's file.

#pragma once

#include <QList>
#include <QString>

#include "timewarp/ai/prompt.hpp"

namespace timewarp::ai {

/// How many turns are written. The panel keeps the same number in memory: a longer
/// file would hold program text that nothing would ever send.
inline constexpr int kStoredTurns = 12;

/// One file of the last turns, in the directory the caller names.
class ConversationStore {
public:
    explicit ConversationStore(QString directory);

    /// The file this store reads and writes: `<directory>/conversation.json`.
    [[nodiscard]] QString path() const;

    /// The stored turns, oldest first, capped at `kStoredTurns`. Empty when there is
    /// no file, when it cannot be read, or when it is not the JSON this writes --
    /// a damaged file is forgetfulness, not an error a learner has to act on.
    [[nodiscard]] QList<Message> load() const;

    /// Writes `turns`, keeping the newest `kStoredTurns`, through `QSaveFile`, and
    /// restricts the result to its owner. False when the directory or the file
    /// cannot be written; the caller keeps working without persistence.
    bool save(const QList<Message>& turns) const;

    /// Forgets the stored conversation. Not an error when there is nothing to
    /// forget.
    void clear() const;

private:
    QString directory_;
};

}  // namespace timewarp::ai
