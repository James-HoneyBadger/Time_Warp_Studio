# Time Warp OS/2 (Warp 4) – Presentation Manager Edition

This is the OS/2 Warp 4 native port of Time Warp, implemented in C using the Presentation Manager (PM) API.

Note on folder name: OS/2 uses a slash in the product name. On most filesystems a slash is a path separator, so this directory is named `Time_Warp_OS2`.

## Features (initial)

- PM frame window with client area
- Menu and accelerators
- Console output via MLE control
- Turtle graphics canvas using GPI (basic line drawing)
- Minimal interpreters:
  - BASIC: PRINT, CLS
  - LOGO: FORWARD, RIGHT, CS
  - PILOT: T:, END (skeleton)

## Build on OS/2

Supported toolchains:

- OpenWatcom (recommended)
- EMX/GCC (OMF) on OS/2

### OpenWatcom

- Install OpenWatcom on OS/2 (wmake, wcl386, wrc available in PATH)
- From this folder:

```
wmake watcom
```

Artifacts:

- `bin/timewarp_os2.exe`

### EMX/GCC

- Ensure gcc (EMX) and `rc.exe` are available
- From this folder:

```
make emx
```

Artifacts:

- `bin/timewarp_os2.exe`

## Run

Launch `bin/timewarp_os2.exe` in Warp 4 (or ArcaOS). Use File → Open to load a program, Run → Run to execute.

## Layout

- `src/` – PM application and modules
- `resources/` – RC script and icons
- `docs/` – OS/2 specific notes
- `installer/` – Placeholder for future packaging
- `Makefile` – Portable make with `emx` and `watcom` targets

## Status

This is an initial functional port focused on core windowing, console, and simple turtle graphics. Expand interpreters and debugger similarly to other platform ports.
