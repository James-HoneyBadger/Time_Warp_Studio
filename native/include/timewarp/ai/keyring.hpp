// SPDX-License-Identifier: MIT
//
// Where a stored key actually lives.
//
// A key in a settings file is a key in a text file. This is the seam that lets the
// assistant put it somewhere the operating system already protects -- the Windows
// Credential Manager, the macOS Keychain, or the freedesktop secret service on
// Linux -- and it is a *seam* rather than a dependency because each of those is
// reached differently, and a build on a machine with none of them has to keep
// working: it keeps the key in the settings file, restricted to its owner, exactly
// as before.
//
// #### The override
//
// `TIME_WARP_AI_KEYRING=file` refuses the system store, for a user who would rather
// have one place their key lives, and for a test that must not write to a developer's
// keychain. `system` asks for it. Unset, the build's own answer applies.

#pragma once

#include <QString>

namespace timewarp::ai {

/// Where a key is kept.
enum class KeyRing {
    /// The operating system's own store.
    System,
    /// The settings file, restricted to its owner. The only option on a machine
    /// where nothing else exists.
    File,
};

/// The account name a stored key is filed under, in the system store.
inline constexpr const char* kKeyRingAccount = "assistant";

/// Which store this build, on this machine, should use. `TIME_WARP_AI_KEYRING`
/// overrides it in both directions.
[[nodiscard]] KeyRing available_keyring();

/// The `time_warp_ai keyring` / settings dialog sentence for the answer above.
[[nodiscard]] QString keyring_name(KeyRing ring);

/// The stored key, or empty when there is none or the store cannot be read. Empty is
/// also what a `KeyRing::File` build answers: the caller reads its own file.
[[nodiscard]] QString keyring_read(const QString& account);

/// Files `secret` under `account`. False when the store refused it -- a machine with
/// no unlocked keyring, a session with no secret service -- and the caller must fall
/// back rather than believe the secret was kept.
bool keyring_write(const QString& account, const QString& secret);

/// Removes the stored key. Not an error when there was nothing to remove.
void keyring_erase(const QString& account);

}  // namespace timewarp::ai
