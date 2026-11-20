// SPDX-License-Identifier: AGPL-3.0-only

#include "timewarp/ui/browser_bridge.hpp"

#ifdef __EMSCRIPTEN__
#include <emscripten.h>

// See the header for why this exists. The bytes are already in memory, so the
// blob is built from the module's heap rather than copied through a second
// encoding; `slice` takes the copy the blob needs while the pointer is still
// valid.
EM_JS(int, tw_browser_download,
      (const char* data, int size, const char* fileNameUtf8, const char* mimeUtf8), {
          try {
              var bytes = HEAPU8.slice(data, data + size);
              var blob = new Blob([bytes], {type: UTF8ToString(mimeUtf8)});
              var url = URL.createObjectURL(blob);
              var anchor = document.createElement('a');
              anchor.href = url;
              anchor.download = UTF8ToString(fileNameUtf8);
              document.body.appendChild(anchor);
              anchor.click();
              document.body.removeChild(anchor);
              // The revoke is deferred: revoking before the click has finished
              // being handled can cancel the download in some browsers.
              setTimeout(function () { URL.revokeObjectURL(url); }, 0);
              return 1;
          } catch (error) {
              console.error('Time Warp Studio: download failed', error);
              return 0;
          }
      });
#endif

namespace timewarp::ui {

bool browserDownloadAvailable() {
#ifdef __EMSCRIPTEN__
    return true;
#else
    return false;
#endif
}

bool browserDownload(const QByteArray& data, const QString& fileName,
                     const QString& mimeType) {
#ifdef __EMSCRIPTEN__
    if (data.isEmpty()) {
        return false;
    }
    const QByteArray name = fileName.toUtf8();
    const QByteArray mime = mimeType.toUtf8();
    return tw_browser_download(data.constData(), data.size(), name.constData(),
                               mime.constData()) != 0;
#else
    Q_UNUSED(data);
    Q_UNUSED(fileName);
    Q_UNUSED(mimeType);
    return false;
#endif
}

}  // namespace timewarp::ui
