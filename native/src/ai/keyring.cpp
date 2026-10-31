// SPDX-License-Identifier: MIT

#include "timewarp/ai/keyring.hpp"

#include <QByteArray>

#if defined(TIME_WARP_HAVE_LIBSECRET)
// glib's D-Bus headers name a member `signals`, which is a macro in Qt, so the
// keyword macros are put back afterwards rather than before: this include cannot move
// above the Qt ones without moving every other include with it.
#  pragma push_macro("signals")
#  pragma push_macro("emit")
#  pragma push_macro("slots")
#  undef signals
#  undef emit
#  undef slots
#  include <libsecret/secret.h>
#  pragma pop_macro("slots")
#  pragma pop_macro("emit")
#  pragma pop_macro("signals")
#endif

namespace timewarp::ai {
namespace {

/// Whether this build was compiled with the secret service at all.
constexpr bool kCompiledWithKeyRing =
#if defined(TIME_WARP_HAVE_LIBSECRET)
    true;
#else
    false;
#endif

#if defined(TIME_WARP_HAVE_LIBSECRET)
const SecretSchema& assistant_schema() {
    // Every member is named: the schema's reserved tail is part of the ABI, and a
    // build with -Werror will not accept an aggregate that leaves it out.
    static const SecretSchema schema = {
        "org.timewarpstudio.Assistant",
        SECRET_SCHEMA_NONE,
        {{"account", SECRET_SCHEMA_ATTRIBUTE_STRING},
         {"application", SECRET_SCHEMA_ATTRIBUTE_STRING}},
        0,
        nullptr,
        nullptr,
        nullptr,
        nullptr,
        nullptr,
        nullptr,
        nullptr,
    };
    return schema;
}
#endif

}  // namespace

KeyRing available_keyring() {
    // The variable is read on every call rather than cached: a test sets it after the
    // application object exists, and a user can export it for one session.
    const QString choice =
        qEnvironmentVariable("TIME_WARP_AI_KEYRING").trimmed().toLower();
    if (choice == QLatin1String("file")) {
        return KeyRing::File;
    }
    if (choice == QLatin1String("system")) {
        return kCompiledWithKeyRing ? KeyRing::System : KeyRing::File;
    }
    return kCompiledWithKeyRing ? KeyRing::System : KeyRing::File;
}

QString keyring_name(KeyRing ring) {
    if (ring == KeyRing::File) {
        return QStringLiteral("the settings file, restricted to its owner");
    }
#if defined(TIME_WARP_HAVE_LIBSECRET)
    return QStringLiteral("the system keyring");
#else
    return QStringLiteral("the settings file, restricted to its owner");
#endif
}

QString keyring_read(const QString& account) {
#if defined(TIME_WARP_HAVE_LIBSECRET)
    GError* error = nullptr;
    gchar* secret = secret_password_lookup_sync(
        &assistant_schema(), nullptr, &error, "account", account.toUtf8().constData(),
        "application", "time_warp_studio", nullptr);
    if (error != nullptr) {
        g_error_free(error);
        return {};
    }
    if (secret == nullptr) {
        return {};
    }
    const QString value = QString::fromUtf8(secret);
    secret_password_free(secret);
    return value;
#else
    Q_UNUSED(account);
    return {};
#endif
}

bool keyring_write(const QString& account, const QString& secret) {
#if defined(TIME_WARP_HAVE_LIBSECRET)
    GError* error = nullptr;
    const gboolean stored = secret_password_store_sync(
        &assistant_schema(), SECRET_COLLECTION_DEFAULT, "Time Warp Studio assistant key",
        secret.toUtf8().constData(), nullptr, &error, "account",
        account.toUtf8().constData(), "application", "time_warp_studio", nullptr);
    if (error != nullptr) {
        g_error_free(error);
        return false;
    }
    return stored == TRUE;
#else
    Q_UNUSED(account);
    Q_UNUSED(secret);
    return false;
#endif
}

void keyring_erase(const QString& account) {
#if defined(TIME_WARP_HAVE_LIBSECRET)
    GError* error = nullptr;
    static_cast<void>(secret_password_clear_sync(
        &assistant_schema(), nullptr, &error, "account", account.toUtf8().constData(),
        "application", "time_warp_studio", nullptr));
    if (error != nullptr) {
        g_error_free(error);
    }
#else
    Q_UNUSED(account);
#endif
}

}  // namespace timewarp::ai
