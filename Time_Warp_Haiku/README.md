# Time Warp IDE - Haiku OS Edition

Educational programming environment for Haiku OS featuring TempleCode (BASIC, PILOT, Logo) with native BeOS/Haiku API integration.

## Features

- **Native Haiku GUI**: Uses BApplication, BWindow, and BView classes
- **Multi-language support**: BASIC, PILOT, Logo interpreters
- **Turtle graphics**: Integrated canvas using BBitmap and BView drawing
- **BeOS-style interface**: Menu bar, split view, status bar
- **Haiku-optimized**: Follows Haiku Human Interface Guidelines

## Building

### Prerequisites

- Haiku OS (R1/beta4 or later)
- GCC 11+ (included with Haiku)
- Jam build system

### Build Instructions

```bash
cd Time_Warp_Haiku
jam
```

### Run

```bash
./Time_Warp_Haiku
```

## Project Structure

```
Time_Warp_Haiku/
├── src/
│   ├── TimeWarpApp.cpp         # BApplication main class
│   ├── TimeWarpWindow.cpp      # Main BWindow
│   ├── EditorView.cpp          # BTextView for code editing
│   ├── OutputView.cpp          # Output display
│   ├── TurtleView.cpp          # BBitmap-based turtle graphics
│   ├── interpreters/
│   │   ├── BasicInterpreter.cpp
│   │   ├── PilotInterpreter.cpp
│   │   └── LogoInterpreter.cpp
│   └── main.cpp
├── headers/
│   ├── TimeWarpApp.h
│   ├── TimeWarpWindow.h
│   ├── EditorView.h
│   ├── OutputView.h
│   ├── TurtleView.h
│   └── interpreters/
│       ├── BasicInterpreter.h
│       ├── PilotInterpreter.h
│       └── LogoInterpreter.h
├── Jamfile                     # Build configuration
├── Time_Warp_Haiku.rdef        # Resource definition
└── README.md
```

## Architecture

### Haiku-Specific Features

- **BMessage-based communication**: Async message passing between components
- **BHandler threading**: Proper message loop integration
- **Scripting support**: BeOS scripting suite for automation
- **Replicants**: Turtle canvas as replicant for desktop embedding
- **Attributes**: File metadata for saving program type

### Language Executors

Reuses core interpreter logic from Time_Warp_Go with C++ wrappers:

- Stateless command processors
- Emoji-prefixed output (UTF-8 native in Haiku)
- Return text results for UI display

## License

Same as main Time Warp project - Educational use

## Credits

Built for Haiku OS - the spiritual successor to BeOS
Maintains compatibility with classic BeOS R5 API design patterns
