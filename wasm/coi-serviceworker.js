// SPDX-License-Identifier: AGPL-3.0-only
//
// Cross-origin isolation for a host that cannot send headers.
//
// The module is built from the threaded Qt for WebAssembly, because every runner in
// the IDE is a `QThread` and the single-threaded Qt compiles `QThread::start()` to an
// empty function. Threads need `SharedArrayBuffer`, and a browser exposes that only to
// a page that is cross-origin isolated: the document must arrive with
// `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy:
// require-corp`. GitHub Pages cannot set either.
//
// A service worker can. It is registered by `index.html`, and from then on every
// response the page receives is re-sent with the two headers added. The page reloads
// once after registration, because the document that registered the worker was already
// delivered without them. `require-corp` also means every subresource must be
// same-origin or carry `Cross-Origin-Resource-Policy`; everything beside this worker on
// the deployed site is same-origin.

self.addEventListener("install", () => {
  // A new worker takes over immediately rather than waiting for every tab to close:
  // the page that registered it is about to reload.
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("fetch", (event) => {
  const request = event.request;

  // A `only-if-cached` request from another origin may not be honoured, and asking
  // would throw. Leaving it alone lets the browser answer it as it would have.
  if (request.cache === "only-if-cached" && request.mode !== "same-origin") {
    return;
  }

  event.respondWith(
    fetch(request).then((response) => {
      // An opaque response has no readable headers and cannot be rebuilt; it also
      // carries nothing this application loads.
      if (response.status === 0 || response.type === "opaque") {
        return response;
      }
      const headers = new Headers(response.headers);
      headers.set("Cross-Origin-Embedder-Policy", "require-corp");
      headers.set("Cross-Origin-Opener-Policy", "same-origin");
      return new Response(response.body, {
        status: response.status,
        statusText: response.statusText,
        headers: headers
      });
    })
  );
});
