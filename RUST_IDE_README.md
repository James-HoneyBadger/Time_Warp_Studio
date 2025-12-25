# Time Warp IDE (Rust Version)

This is the new, high-performance Rust implementation of Time Warp IDE.
It is designed to be the centerpiece of Time Warp Studio.

## Features

*   **Native Performance**: Built with Rust and `egui` for blazing fast performance.
*   **Modular Architecture**:
    *   `src/core`: Core interpreter traits and state management.
    *   `src/languages`: Pluggable language modules (BASIC, PILOT, Logo, etc.).
    *   `src/ui`: Immediate mode GUI components.
*   **Integrated Interpreters**:
    *   **BASIC**: Implemented (PRINT, Variables).
    *   **PILOT**: Implemented (Type, Ask, Match, Conditionals).
    *   **Logo**: Implemented (Turtle Graphics, Forward, Back, Right, Left, Clear).
    *   **Others**: Stubs ready for implementation.
*   **Graphics**:
    *   Unified Canvas for Turtle Graphics.
    *   Real-time rendering using `egui::Painter`.

## Building

To build the project and install the executable to the root folder:

```bash
./Scripts/build_rust.sh
```

## Running

Run the executable from the project root:

```bash
./tw
```

## Development

The source code is located in `Platforms/Rust`.
To add a new language:
1.  Create `src/languages/my_lang.rs`.
2.  Implement the `Interpreter` trait.
3.  Register it in `src/app.rs`.
