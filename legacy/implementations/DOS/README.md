# Time Warp (DOS)

A minimal DOS version of Time Warp that runs inside MS-DOS/FreeDOS (e.g., via DOSBox) using DJGPP (32-bit GCC for DOS). It interprets simple TempleCode scripts and renders turtle graphics directly to VGA Mode 13h (320x200, 256-color).

## Features

- Text-mode launcher + graphics mode renderer (VGA Mode 13h)
- Supported commands (case-insensitive):
  - PRINT "text"
  - CLS
  - FD n, LT n, RT n
  - PU (pen up), PD (pen down)
  - SETXY x y
  - COLOR r g b  (0-255 each; approximated to Mode 13h palette)
  - REPEAT n ... ENDREPEAT (one level; no nesting for now)
- Starts turtle at canvas center, heading 0°, pen down, white color
- Exits graphics on any key; optional /noshow to just render and save BMP
- Optional SAVE BMP via command: SAVE "filename.bmp"

Note: This is intentionally minimal for DOS compatibility. It does not include the full expression evaluator of other targets.

## Build (DOSBox + DJGPP)

1) Install DOSBox and obtain a DJGPP environment (e.g., from <https://delorie.com/djgpp/> or a prebuilt image).

2) Inside DOSBox, mount this project folder and build:

```bat
# inside DOSBox
Z:\> mount c /path/to/Temple_Code
Z:\> c:
C:\> cd \Temple_Code\DOS\src
C:\TEMPLE_CODE\DOS\SRC\> set PATH=C:\DJGPP\BIN;%PATH%
C:\TEMPLE_CODE\DOS\SRC\> set DJGPP=C:\DJGPP\DJGPP.ENV
C:\TEMPLE_CODE\DOS\SRC\> gcc -O2 -ffast-math -s -o TLAND.EXE tland_fixed.c -lm
```

1) Run with an example:

```bat
C:\TEMPLE_CODE\DOS\SRC\> ..\examples\SQUARE.TC
# or
C:\TEMPLE_CODE\DOS\SRC\> TLAND.EXE ..\examples\SQUARE.TC
```

The program switches to graphics, draws, and waits for a key. Press any key to return to text mode.

## Save to BMP

- In a script: `SAVE "output.bmp"` writes a 24-bit BMP file to disk.
- Or add `--save out.bmp` to the command line (renders headless and saves without showing window).

## Known limitations

- No nested REPEAT yet.
- PRINT only supports quoted strings.
- COLOR is approximated to the default VGA palette; actual colors may vary.
- Coordinates: origin at top-left, increasing x→right, y→down. Turtle starts at center.

## Examples

See `DOS/examples/` for a couple of sample programs you can run.
