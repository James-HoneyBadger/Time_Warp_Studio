# Time Warp (Web)

A browser-based version of Time Warp powered by WebAssembly. It reuses the Rust interpreter and renders turtle graphics on an HTML5 canvas.

## Features

- TempleCode interpreter compiled to WebAssembly
- Code editor, Run (Ctrl+Enter), Reset Canvas, Clear Console
- Canvas rendering of turtle lines with colors
- Save PNG (downloads the canvas as a PNG)
- Examples dropdown with sample programs

## Structure

- `web-wasm/` — Rust `cdylib` crate that exposes a `run_code(code: &str)` function via `wasm-bindgen`, depending on the existing Rust engine.
- `site/` — Static web UI (HTML/JS) that loads the generated WASM and renders output.

## Build

Prereqs:

- Rust toolchain
- `wasm-pack` (recommended) or `wasm-bindgen-cli`

Build with wasm-pack (recommended):

```bash
cd Browser/web-wasm
wasm-pack build --target web --out-dir ../site/pkg
```

This produces `Browser/site/pkg/time.warp_web.js` and `time.warp_web_bg.wasm` that `site/main.js` imports.

## Run locally

Serve the static site from `Browser/site` with any simple HTTP server, e.g.:

```bash
cd Browser/site
python3 -m http.server 5173
```

Then open <http://localhost:5173> in a browser.

## Notes

- We gated the desktop-only `rfd` dependency to native targets so the `time.warp_rust` crate can be consumed from WebAssembly.
- PNG saving uses the browser canvas (`toDataURL`) for download, independent of the engine's `EXPORTPNG`.
- If you extend the interpreter in Rust, those features will be available on the web after rebuilding the wasm package.
