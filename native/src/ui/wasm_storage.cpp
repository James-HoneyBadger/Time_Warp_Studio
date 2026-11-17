// SPDX-License-Identifier: MIT
//
// See `include/timewarp/ui/wasm_storage.hpp` for why this exists.
//
// The three JavaScript snippets below are `EM_JS` rather than a `.js` file beside the
// page because they need `FS`, which exists only inside the Emscripten runtime the page
// has loaded. A library file would have to find that runtime through a global, and a
// page that loads the module under a different name would then break silently; `EM_JS`
// is compiled into the module's own scope, so it cannot.

#include "timewarp/ui/wasm_storage.hpp"

#include <QCoreApplication>
#include <QTimer>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>

// At file scope rather than in an anonymous namespace: `EM_JS` emits the function as an
// import and a string in a dedicated section, and the linker that reads that section is
// happiest with the documented shape. The names are prefixed so nothing can collide.

/// Creates the directory the application expects to find, then mounts IndexedDB over
/// the home directory. Returns 0 when the mount failed, which the caller reports once
/// rather than on every flush.
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
        if (!FS.analyzePath('/home/web_user/projects').exists) {
            FS.mkdir('/home/web_user/projects');
        }
    } catch (error) {
        console.error('Time Warp Studio: could not mount the project volume', error);
        return 0;
    }
    return 1;
});

/// Writes the volume out, or reads it in when `populate` is non-zero.
///
/// `FS.syncfs` is asynchronous, and that is the point of the two directions: populating
/// has to finish before anything is shown, flushing does not have to finish before the
/// next keystroke.
EM_JS(void, tw_sync_projects, (int populate), {
    try {
        FS.syncfs(populate !== 0, function (error) {
            if (error) {
                console.error('Time Warp Studio: project storage sync failed', error);
            }
        });
    } catch (error) {
        console.error('Time Warp Studio: project storage is not mounted', error);
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
        try {
            FS.syncfs(false, function () {});
        } catch (error) {
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
    if (tw_mount_projects() == 0) {
        return;
    }
    // `populate` in this direction: the page starts with an empty in-memory tree, and
    // the files that were saved last time appear as the databases are read.
    tw_sync_projects(1);
    tw_watch_unload();
#else
    // Nothing to mount. The desktop writes the user's real files, and the save path's
    // call to `saveNow()` below is compiled to nothing.
#endif
}

void startAutoSave() {
#ifdef __EMSCRIPTEN__
    QCoreApplication* application = QCoreApplication::instance();
    if (application == nullptr) {
        return;
    }
    auto* timer = new QTimer{application};
    timer->setInterval(kAutoSaveMilliseconds);
    QObject::connect(timer, &QTimer::timeout, application, [] { saveNow(); });
    timer->start();
#else
    Q_UNUSED(kAutoSaveMilliseconds);
#endif
}

void saveNow() {
#ifdef __EMSCRIPTEN__
    tw_sync_projects(0);
#endif
}

}  // namespace timewarp::ui::wasm
