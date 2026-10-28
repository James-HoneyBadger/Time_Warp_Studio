// SPDX-License-Identifier: MIT

#include "timewarp/ai/keyring.hpp"

#include <QByteArray>

#include "timewarp/core/qt_compat.hpp"

#if defined(Q_OS_WIN)
#  ifndef NOMINMAX
#    define NOMINMAX
#  endif
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>

#  include <wincred.h>

#elif defined(Q_OS_MACOS)
#  include <CoreFoundation/CoreFoundation.h>
#  include <Security/Security.h>

#elif defined(TIME_WARP_HAVE_LIBSECRET)
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

/// Whether this build was compiled with a platform store at all.
constexpr bool kCompiledWithKeyRing =
#if defined(Q_OS_WIN) || defined(Q_OS_MACOS) || defined(TIME_WARP_HAVE_LIBSECRET)
    true;
#else
    false;
#endif

#if defined(Q_OS_WIN)
// One target per application rather than one per account: the store is per user
// already, and a name a reader can find in the Credential Manager is worth more
// than a hierarchy nothing displays.
constexpr wchar_t kWinTarget[] = L"TimeWarpStudio/assistant";
#endif

#if defined(Q_OS_MACOS)
constexpr char kMacService[] = "Time Warp Studio";

CFStringRef mac_service() {
    return CFStringCreateWithCString(nullptr, kMacService, kCFStringEncodingUTF8);
}

/// The query that names one stored item: a generic password for this service and
/// account.
CFMutableDictionaryRef mac_query(const QByteArray& account) {
    CFMutableDictionaryRef query = CFDictionaryCreateMutable(
        nullptr, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    CFDictionarySetValue(query, kSecClass, kSecClassGenericPassword);
    CFStringRef service = mac_service();
    CFDictionarySetValue(query, kSecAttrService, service);
    CFRelease(service);
    CFStringRef name = CFStringCreateWithCString(nullptr, account.constData(),
                                                 kCFStringEncodingUTF8);
    CFDictionarySetValue(query, kSecAttrAccount, name);
    CFRelease(name);
    return query;
}
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
#if defined(Q_OS_WIN)
    return QStringLiteral("the Windows Credential Manager");
#elif defined(Q_OS_MACOS)
    return QStringLiteral("the macOS Keychain");
#elif defined(TIME_WARP_HAVE_LIBSECRET)
    return QStringLiteral("the system keyring");
#else
    return QStringLiteral("the settings file, restricted to its owner");
#endif
}

QString keyring_read(const QString& account) {
#if defined(Q_OS_WIN)
    Q_UNUSED(account);
    PCREDENTIALW credential = nullptr;
    if (CredReadW(kWinTarget, CRED_TYPE_GENERIC, 0, &credential) == FALSE) {
        return {};
    }
    QString secret;
    if (credential != nullptr) {
        secret = timewarp::core::fromUtf16(
            credential->CredentialBlob,
            static_cast<int>(credential->CredentialBlobSize / sizeof(char16_t)));
        CredFree(credential);
    }
    return secret;
#elif defined(Q_OS_MACOS)
    CFMutableDictionaryRef query = mac_query(account.toUtf8());
    CFDictionarySetValue(query, kSecReturnData, kCFBooleanTrue);
    CFDictionarySetValue(query, kSecMatchLimit, kSecMatchLimitOne);
    CFTypeRef data = nullptr;
    const OSStatus status = SecItemCopyMatching(query, &data);
    CFRelease(query);
    if (status != errSecSuccess || data == nullptr) {
        return {};
    }
    QString secret;
    if (CFGetTypeID(data) == CFDataGetTypeID()) {
        const auto* bytes = CFDataGetBytePtr(static_cast<CFDataRef>(data));
        const CFIndex length = CFDataGetLength(static_cast<CFDataRef>(data));
        secret = QString::fromUtf8(reinterpret_cast<const char*>(bytes),
                                   static_cast<int>(length));
    }
    CFRelease(data);
    return secret;
#elif defined(TIME_WARP_HAVE_LIBSECRET)
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
#if defined(Q_OS_WIN)
    Q_UNUSED(account);
    // The blob is a UTF-16 copy of the key: the Credential Manager stores bytes, and
    // a key is text.
    const QByteArray utf16{
        reinterpret_cast<const char*>(secret.utf16()),
        secret.size() * static_cast<int>(sizeof(char16_t))};
    CREDENTIALW credential{};
    credential.Type = CRED_TYPE_GENERIC;
    credential.TargetName = const_cast<LPWSTR>(kWinTarget);
    credential.Comment = const_cast<LPWSTR>(L"Time Warp Studio AI assistant key");
    credential.CredentialBlobSize = static_cast<DWORD>(utf16.size());
    credential.CredentialBlob =
        reinterpret_cast<LPBYTE>(const_cast<char*>(utf16.constData()));
    credential.Persist = CRED_PERSIST_LOCAL_MACHINE;
    return CredWriteW(&credential, 0) != FALSE;
#elif defined(Q_OS_MACOS)
    const QByteArray bytes = secret.toUtf8();
    CFDataRef data = CFDataCreate(nullptr, reinterpret_cast<const UInt8*>(bytes.constData()),
                                  bytes.size());
    CFMutableDictionaryRef query = mac_query(account.toUtf8());
    CFDictionarySetValue(query, kSecValueData, data);
    const OSStatus added = SecItemAdd(query, nullptr);
    CFRelease(query);
    if (added == errSecSuccess) {
        CFRelease(data);
        return true;
    }
    if (added != errSecDuplicateItem) {
        CFRelease(data);
        return false;
    }
    // Already there: replace the secret rather than fail, so saving twice works.
    CFMutableDictionaryRef match = mac_query(account.toUtf8());
    CFMutableDictionaryRef attributes = CFDictionaryCreateMutable(
        nullptr, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    CFDictionarySetValue(attributes, kSecValueData, data);
    const OSStatus updated = SecItemUpdate(match, attributes);
    CFRelease(match);
    CFRelease(attributes);
    CFRelease(data);
    return updated == errSecSuccess;
#elif defined(TIME_WARP_HAVE_LIBSECRET)
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
#if defined(Q_OS_WIN)
    Q_UNUSED(account);
    static_cast<void>(CredDeleteW(kWinTarget, CRED_TYPE_GENERIC, 0));
#elif defined(Q_OS_MACOS)
    CFMutableDictionaryRef query = mac_query(account.toUtf8());
    static_cast<void>(SecItemDelete(query));
    CFRelease(query);
#elif defined(TIME_WARP_HAVE_LIBSECRET)
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
