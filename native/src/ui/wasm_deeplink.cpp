// SPDX-License-Identifier: AGPL-3.0-only
//
// The browser half of deep links. `deeplink.cpp` parses a query string into a
// `DeepLink` without knowing where the string came from; this module reads it
// from the page, fetches a remote notebook, and turns the result into open
// documents.
//
// A desktop build compiles every function here to a no-op or to its pure-Qt
// core: `browserQueryString` and `fetchText` answer empty, so a deep link on a
// desktop does nothing, exactly as a command line with no file argument does.

#include "timewarp/ui/wasm_deeplink.hpp"

#include "timewarp/ui/deeplink.hpp"
#include "timewarp/ui/language.hpp"
#include "timewarp/ui/main_window.hpp"
#include "timewarp/ui/notebook.hpp"
#include "timewarp/ui/notebook_window.hpp"

#include <QBuffer>
#include <QCoreApplication>

#include <cstdlib>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>

// At file scope, the same way `wasm_storage.cpp` keeps them: `EM_JS` emits the
// function as an import plus a string in its own section, and the linker reads
// that section best in the documented shape. The names are prefixed so nothing
// can collide with the storage hooks beside them.

/// `location.search`, the raw query string including the leading `?`. Returned
/// as a malloc'd UTF-8 buffer the caller frees; an empty string when there is no
/// location (a worker, or a test harness without a page).
EM_JS(char*, tw_browser_query_string, (), {
    var search = (typeof location !== 'undefined') ? location.search : '';
    var bytes = lengthBytesUTF8(search) + 1;
    var buffer = _malloc(bytes);
    stringToUTF8(search, buffer, bytes);
    return buffer;
});

/// Fetches `url` and returns its text, or null when the fetch failed. The call
/// yields to the browser while the request is in flight, which is the same
/// Asyncify suspension the storage population uses, so a notebook link opens
/// before the window is shown rather than after a blank second.
///
/// Cross-origin requests obey the browser's CORS rules: a notebook host that
/// wants its notebooks embeddable must allow the page's origin, or the fetch
/// fails with the reason in the console.
EM_ASYNC_JS(char*, tw_fetch_text, (const char* urlUtf8), {
    var url = UTF8ToString(urlUtf8);
    try {
        var response = await fetch(url);
        if (!response.ok) {
            return 0;
        }
        var text = await response.text();
        var bytes = lengthBytesUTF8(text) + 1;
        var buffer = _malloc(bytes);
        stringToUTF8(text, buffer, bytes);
        return buffer;
    } catch (error) {
        console.error('Time Warp Studio: could not fetch notebook', error);
        return 0;
    }
});
#endif

namespace timewarp::ui::wasm {

QString browserQueryString() {
#ifdef __EMSCRIPTEN__
    char* raw = tw_browser_query_string();
    const QString query = QString::fromUtf8(raw);
    std::free(raw);
    return query;
#else
    return {};
#endif
}

QString fetchText(const QString& url) {
#ifdef __EMSCRIPTEN__
    const QByteArray bytes = url.toUtf8();
    char* raw = tw_fetch_text(bytes.constData());
    if (raw == nullptr) {
        return {};
    }
    const QString text = QString::fromUtf8(raw);
    std::free(raw);
    return text;
#else
    Q_UNUSED(url);
    return {};
#endif
}

namespace {

/// A title for a shared program that names the language rather than a file:
/// the link carried no file name, and "untitled" would hide what the document
/// actually is.
QString sharedProgramTitle(const QString& languageKey) {
    const Language language = languageFromKey(languageKey);
    if (language == Language::Unknown) {
        return QStringLiteral("Shared program");
    }
    return QCoreApplication::translate("wasm_deeplink", "Shared %1 program")
        .arg(displayName(language));
}

}  // namespace

bool applyDeepLink(MainWindow* window, const DeepLink& link, QString* error) {
    if (window == nullptr) {
        return false;
    }
    // Whether a *runnable* document was opened: a notebook is a document, but it
    // is run one cell at a time and has no single current program for `?run=1`
    // to mean.
    bool applied = false;

    if (!link.example.isEmpty()) {
        QString path = link.example;
        if (!path.startsWith(QStringLiteral(":/"))) {
            path = QStringLiteral(":/examples/") + path;
        }
        if (window->openFile(path, MainWindow::OpenFailure::Silent)) {
            applied = true;
        } else if (error != nullptr) {
            *error = QCoreApplication::translate(
                "wasm_deeplink", "the link named an example that is not in the "
                                 "gallery: %1").arg(link.example);
        }
    }

    if (!link.language.isEmpty() && !link.source.isEmpty()) {
        if (languageFromKey(link.language) == Language::Unknown) {
            if (error != nullptr) {
                *error = QCoreApplication::translate(
                    "wasm_deeplink", "the link names an unknown language: %1")
                             .arg(link.language);
            }
        } else if (window->openSourceDocument(link.language, link.source,
                                              sharedProgramTitle(link.language)) !=
                   nullptr) {
            applied = true;
        }
    }

    if (!link.notebook.isEmpty()) {
        const QString text = fetchText(link.notebook);
        if (text.isEmpty()) {
            if (error != nullptr) {
                *error = QCoreApplication::translate(
                    "wasm_deeplink", "the notebook could not be fetched from %1")
                             .arg(link.notebook);
            }
        } else {
            QString parseError;
            QBuffer buffer;
            buffer.setData(text.toUtf8());
            if (!buffer.open(QIODevice::ReadOnly)) {
                parseError = buffer.errorString();
            } else {
                const Notebook notebook = readNotebook(buffer, &parseError);
                if (!parseError.isEmpty()) {
                    auto* window_ = new NotebookWindow{};
                    window_->setAttribute(Qt::WA_DeleteOnClose);
                    window_->setNotebook(notebook);
                    window_->show();
                    window_->raise();
                }
            }
            if (!parseError.isEmpty() && error != nullptr) {
                *error = parseError;
            }
        }
    }

    return applied;
}

}  // namespace timewarp::ui::wasm
