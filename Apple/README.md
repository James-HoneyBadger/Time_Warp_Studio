
# Time Warp IDE for Apple macOS

This is a fully native macOS application, packaged as a `.app` bundle for standalone use. It launches as a GUI from Finder and includes all required macOS metadata and icon.

## Features

- Native macOS GUI (eframe/egui)
- Turtle graphics canvas (500x400)
- Live console output
- File operations (Open, Save, Quick Save PNG)
- Built-in examples
- Keyboard shortcuts (Cmd+R, Cmd+S, Cmd+Q)
- macOS `.app` bundle with icon and Info.plist

## Prerequisites

- macOS 10.13+
- [Rust toolchain](https://rustup.rs) installed

## Build and Bundle

```bash
cd Apple
./bundle.sh
```

This creates `Time Warp.app` in the Apple folder. You can launch it from Finder or with:

```bash
open Temple\ Land.app
```

## Packaging and Distribution

- To notarize and distribute, see [Apple's official documentation](https://developer.apple.com/documentation/xcode/notarizing_macos_software_before_distribution).
- Replace `TimeWarp.icns` with your own app icon for branding.

## Project Structure

- `src/main.rs` — Main application source
- `Cargo.toml` — Build configuration
- `Info.plist` — macOS app metadata
- `TimeWarp.icns` — App icon
- `bundle.sh` — Bundling script
- `examples/` — Example Time Warp scripts

## License

MIT
