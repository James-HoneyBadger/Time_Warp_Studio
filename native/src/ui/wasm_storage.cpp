// SPDX-License-Identifier: AGPL-3.0-only
//
// See `include/timewarp/ui/wasm_storage.hpp` for why this exists.
//
// The JavaScript snippets below are embedded rather than kept beside the page because
// they need `FS`, which exists only inside the Emscripten runtime the page has loaded.

#include "timewarp/ui/wasm_storage.hpp"

#include <QCoreApplication>
#include <QTimer>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>

// At file scope rather than in an anonymous namespace: `EM_JS` emits the function as an
// import and a string in a dedicated section, and the linker that reads that section is
// happiest with the documented shape. The names are prefixed so nothing can collide.

/// Keeps storage failures visible in the page's status strip, including failures before
/// the main window has been constructed.
EM_JS(void, tw_report_storage_status, (int success, int populating), {
    if (typeof window.timeWarpStorageStatus !== 'function') {
        return;
    }
    if (success) {
        window.timeWarpStorageStatus(
            populating ? 'Browser storage is ready.' : 'Saved to browser storage.', false);
    } else {
        window.timeWarpStorageStatus(
            populating
                ? 'Browser storage could not be loaded; changes may not survive a reload.'
                : 'Could not save to browser storage; the document remains unsaved.',
            true);
    }
});

/// Creates the home directory, then mounts IndexedDB over it. Returns 0 when the mount
/// failed, which the caller reports once rather than on every flush.
///
/// The mount point is the home directory rather than the projects folder alone: the
/// IDE's own data -- `~/.time_warp` for lesson progress, the classroom roster and the
/// autosave snapshots -- lives beside the documents, and a build that persisted only
/// one of the two would lose a learner's progress on reload while keeping the file they
/// typed.
EM_JS(int, tw_mount_projects, (), {
    try {
        if (!FS.analyzePath('/home/web_user').exists) {
            FS.mkdir('/home/web_user');
        }
        FS.mount(IDBFS, {}, '/home/web_user');
    } catch (error) {
        console.error('Time Warp Studio: could not mount the project volume', error);
        return 0;
    }
    return 1;
});

/// Writes the volume out, or reads it in when `populate` is non-zero.
///
/// `FS.syncfs` is asynchronous. Asyncify lets population finish before the window is
/// built, and lets a flush yield to browser events while IndexedDB completes the write.
EM_ASYNC_JS(int, tw_sync_projects, (int populate), {
    if (window.timeWarpStorageSyncing) {
        console.error('Time Warp Studio: another storage sync is still in progress');
        return 0;
    }
    window.timeWarpStorageSyncing = true;
    try {
        const error = await new Promise(function (resolve) {
            FS.syncfs(populate !== 0, resolve);
        });
        if (error) {
            console.error('Time Warp Studio: project storage sync failed', error);
            return 0;
        }
        if (populate !== 0 && !FS.analyzePath('/home/web_user/projects').exists) {
            FS.mkdir('/home/web_user/projects');
        }
        return 1;
    } catch (error) {
        console.error('Time Warp Studio: project storage is not mounted', error);
        return 0;
    } finally {
        window.timeWarpStorageSyncing = false;
    }
});

/// Starts a background flush without suspending the user interface.
EM_JS(void, tw_sync_projects_async, (), {
    if (window.timeWarpStorageSyncing) {
        return;
    }
    window.timeWarpStorageSyncing = true;
    try {
        FS.syncfs(false, function (error) {
            window.timeWarpStorageSyncing = false;
            if (error) {
                console.error('Time Warp Studio: automatic storage sync failed', error);
                if (typeof window.timeWarpStorageStatus === 'function') {
                    window.timeWarpStorageStatus(
                        'Automatic browser storage save failed; changes may not survive a reload.',
                        true);
                }
            } else if (typeof window.timeWarpStorageStatus === 'function') {
                window.timeWarpStorageStatus('Saved to browser storage.', false);
            }
        });
    } catch (error) {
        window.timeWarpStorageSyncing = false;
        console.error('Time Warp Studio: could not start automatic storage sync', error);
        if (typeof window.timeWarpStorageStatus === 'function') {
            window.timeWarpStorageStatus(
                'Could not start automatic browser storage save; changes may not survive a reload.',
                true);
        }
    }
});

/// The unload flush. It is registered here rather than in the page so that it is
/// installed by the same code that mounted the volume: a page that forgot the listener
/// and a page whose mount failed are then the same bug.
///
/// The callback is left empty on purpose. A reload is already under way and there is
/// nothing left to report to, but the write itself has been queued by the call.
EM_JS(void, tw_watch_unload, (), {
    if (window.timeWarpStorageHooked) {
        return;
    }
    window.timeWarpStorageHooked = true;
    window.addEventListener('beforeunload', function () {
        if (window.timeWarpStorageSyncing) {
            return;
        }
        window.timeWarpStorageSyncing = true;
        try {
            FS.syncfs(false, function (error) {
                window.timeWarpStorageSyncing = false;
                if (error) {
                    console.error('Time Warp Studio: could not flush on unload', error);
                }
            });
        } catch (error) {
            window.timeWarpStorageSyncing = false;
            console.error('Time Warp Studio: could not flush on unload', error);
        }
    });
});
#endif

namespace timewarp::ui::wasm {

namespace {
/// The interval the automatic flush runs at. Thirty seconds is a compromise: shorter
/// makes every open tab write to the database constantly, longer than this and a crash
/// costs the user a paragraph rather than a clause.
constexpr int kAutoSaveMilliseconds = 30000;
// The browser build is the only one with a volume to keep in step, so the flags
// that describe its state exist only there; a desktop build would warn on them.
#ifdef __EMSCRIPTEN__
bool g_storage_ready = false;
bool g_storage_sync_in_progress = false;
#endif
}  // namespace

QString projectRoot() {
    return QStringLiteral("/home/web_user/projects");
}

bool available() {
#ifdef __EMSCRIPTEN__
    return true;
#else
    return false;
#endif
}

void mountProjectStorage() {
#ifdef __EMSCRIPTEN__
    g_storage_ready = false;
    if (tw_mount_projects() == 0) {
        tw_report_storage_status(0, 1);
        return;
    }
    // Asyncify lets this synchronous call yield to IndexedDB while keeping window
    // construction and session restoration after the population has completed.
    g_storage_sync_in_progress = true;
    const bool populated = tw_sync_projects(1) != 0;
    g_storage_sync_in_progress = false;
    tw_report_storage_status(populated ? 1 : 0, 1);
    if (!populated) {
        return;
    }
    g_storage_ready = true;
    tw_watch_unload();
#else
    // Nothing to mount. The desktop writes the user's real files, and the save path's
    // call to `saveNow()` below is compiled to nothing.
#endif
}

void startAutoSave() {
#ifdef __EMSCRIPTEN__
    if (!g_storage_ready) {
        return;
    }
    QCoreApplication* application = QCoreApplication::instance();
    if (application == nullptr) {
        return;
    }
    auto* timer = new QTimer{application};
    timer->setInterval(kAutoSaveMilliseconds);
    QObject::connect(timer, &QTimer::timeout, application, [] {
        if (!g_storage_sync_in_progress) {
            tw_sync_projects_async();
        }
    });
    timer->start();
#else
    Q_UNUSED(kAutoSaveMilliseconds);
#endif
}

bool saveNow() {
#ifdef __EMSCRIPTEN__
    if (!g_storage_ready || g_storage_sync_in_progress) {
        tw_report_storage_status(0, 0);
        return false;
    }
    g_storage_sync_in_progress = true;
    const bool saved = tw_sync_projects(0) != 0;
    g_storage_sync_in_progress = false;
    tw_report_storage_status(saved ? 1 : 0, 0);
    return saved;
#else
    return true;
#endif
}

}  // namespace timewarp::ui::wasm
