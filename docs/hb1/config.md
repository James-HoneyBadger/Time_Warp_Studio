# CONFIG — the machine's settings

```text
CONFIG
┌─ Machine ─┬─ Screen ─┬─ Glass ─┐
│ Boot ROM      [Last used ROM ▾]│
│ Boot screen   [Last used screen ▾]
│ Start-up      [Full ▾]         │
│ Prompt hist.  [100 lines ▾]    │
│ ☑ Remember each ROM's program  │
│ ☑ Switch to SPLIT when drawing │
└────────────────────────────────┘
            [Restore Defaults] [OK] [Cancel]
```

Three ways in, all the same screen:

- the **CONFIG** button in the machine's header;
- `Ctrl+,` while the machine has focus — the same chord the IDE uses for
  Preferences, which in this window opens the machine's settings instead;
- `CONFIG` or `SETUP` at the prompt.

The screen is the machine's, not the Studio's. A 1982 computer had a monitor, a
boot ROM and a keyboard; none of those are editor preferences, and mixing them
into the IDE's settings would lose that distinction.

**Everything previews live.** Move a slider and the whole machine changes behind
the dialog. **OK** keeps it, **Cancel** puts the machine back exactly as it was,
and **Restore Defaults** returns a stock machine without closing the screen.

---

## The pages

### Machine

| Setting | Values | Default | Effect |
| --- | --- | --- | --- |
| Boot ROM | `Last used ROM`, or any of the eleven | Last used | The ROM to cold-boot into at the next power-on |
| Boot screen | `Last used screen`, `TEXT`, `SPLIT`, `GRAPHICS` | Last used | The screen mode at the next power-on |
| Start-up message | `Full`, `Short`, `No start-up message` | Full | How much of the banner is printed |
| Prompt history | `Off`, 25, 50, 100, 250, 500 lines | 100 | How many prompt lines Up and Down recall |
| Remember each ROM's program | on / off | on | Keep all eleven program buffers between sessions |
| Switch to SPLIT when a program draws | on / off | on | Show ink the moment a program makes it |

**The boot settings are the exception to "live".** Boot ROM and boot screen apply
at the *next* power-on, not now. A settings dialog must never throw away the
program being typed, and cold-booting the machine because somebody changed what
it should start as would do exactly that. The screen says so in a note under the
controls.

**Start-up message** is worth a line of its own:

| Choice | What the screen shows at boot |
| --- | --- |
| Full | `*** HB-1 ***`, the ROM line, the copyright line, the disk line, `SYSTEM READY`, then the prompt |
| Short | `*** HB-1 ***` and the ROM line, then the prompt |
| Off | Nothing at all — the machine boots to an empty screen, as a quiet ROM would |

**Remember each ROM's program** is what makes switching ROMs feel like a machine
with ten cartridges rather than a tab bar. Switched off, the machine forgets all
ten at exit *and erases what it had already remembered*, so the next start cannot
show programs the setting says are not kept.

**Switch to SPLIT when a program draws** is the courtesy that stops a drawing
from happening where it cannot be seen. Switched off, a program in TEXT mode
draws invisibly to the canvas — which is occasionally exactly what a lesson wants
to demonstrate.

### Screen

| Setting | Values | Default | Effect |
| --- | --- | --- | --- |
| Monitor | Green, Amber, White, Ice | Green | The phosphor palette for the whole machine |
| Screen text size | 8–24 pt | 11 pt | OUTPUT and the prompt line |
| Program text size | 8–24 pt | 12 pt | The PROGRAM page |

The two text sizes are independent on purpose: a listing is read at a different
distance from a result, and the prompt line has to stay legible when the program
is small.

#### The phosphors are palettes, not tints

Choosing Amber does not put a filter over the green machine. It replaces every
colour the machine draws with:

| Role | What it paints |
| --- | --- |
| Background | Behind the machine, outside the screen frame |
| Panel | The header, the prompt bar and the other bezel panels |
| Border | Frames and control outlines |
| Text | Ordinary screen text |
| Bright | Headings and the ROM name |
| Dim | Captions and the status line |
| Accent | The prompt and the power lamp — what the eye should find first |
| Selection | Selection and hover fills |
| Canvas | The drawing surface behind the turtle |

The stylesheet, the PROGRAM editor's palette, the canvas surface and the CRT glass
all derive from that one table, which is why an amber machine is amber
everywhere rather than an amber rectangle in a green window.

### Glass

Five effects, each from 0 to 100 percent. Zero switches an effect off exactly, not
nearly.

| Effect | Default | What it is |
| --- | --- | --- |
| Scanlines | 13 % | Horizontal rules across the tube |
| Vignette | 7 % | Darkening at the edges |
| Curvature | 3 % | A rounded, darkened border suggesting a tube's face |
| Flicker | 0 % | A slow brightness breath |
| Phosphor glow | 8 % | A tint over everything, in the monitor's colour |

The glass is drawn *over* the screen and never takes the keyboard: every click and
keystroke goes to the machine underneath. Curvature is a bezel, not a shader — the
pixels are not distorted, because distorting them would mean rendering the whole
window through a shader to make a period effect, and that is a cost a teaching
machine should not pay. See
[`panels.hpp`](../../native/include/timewarp/ui/panels.hpp) for the honest comment.

---

## Where it is stored

The configuration is one group in the application's settings, written only when
the dialog is accepted:

| Key | Holds |
| --- | --- |
| `retroComputer/config/bootLanguage` | A `Language` value, or `-1` for *Last used ROM* |
| `retroComputer/config/bootScreen` | `0` TEXT, `1` GRAPHICS, `2` SPLIT, or `-1` for *Last used screen* |
| `retroComputer/config/banner` | `0` Full, `1` Short, `2` Off |
| `retroComputer/config/rememberPrograms` | `true` / `false` |
| `retroComputer/config/switchToSplit` | `true` / `false` |
| `retroComputer/config/historyDepth` | `0`–`500`; `0` is off |
| `retroComputer/config/voiceVolume` | The machine's own voice, 0–100 |
| `retroComputer/config/voiceMuted` | `true` / `false`. Muting is about sound, not history: a muted machine still counts what it did |
| `retroComputer/config/phosphor` | `0` Green, `1` Amber, `2` White, `3` Ice |
| `retroComputer/config/screenTextSize` | Point size, 8–24 |
| `retroComputer/config/editorTextSize` | Point size, 8–24 |
| `retroComputer/config/crtScanlines` | 0–100 |
| `retroComputer/config/crtVignette` | 0–100 |
| `retroComputer/config/crtCurvature` | 0–100 |
| `retroComputer/config/crtFlicker` | 0–100 |
| `retroComputer/config/crtTint` | Phosphor glow, 0–100 |

Session state — the ROM and screen the machine was left in, the window geometry
and the eleven program buffers — is a separate group, `retroComputer`, and is
described in the [Owner's Manual](README.md#what-the-machine-remembers). The split
matters: cancel a preview and nothing was written; close the machine and its
session is.

### Reading the file by hand

The settings file is ordinary Qt settings (`~/.config/TimeWarpStudio/TimeWarpStudio.conf`
in an installed build). Editing it is allowed and safe enough:

- a value outside the range is clamped (history to 0–500, text sizes to 8–24,
  glass to 0–100) rather than rejected;
- a history depth that is not one of the presets is still offered in the combo
  box, so a hand-edited value is visible rather than silently replaced;
- a boot ROM that this build does not have falls back to the saved ROM or BASIC.

---

## What CONFIG deliberately cannot do

- **It cannot change the ROM you are in.** That is the socket on the front panel
  and the machine's session state; CONFIG only decides what the *next* power-on
  does.
- **It cannot move the disk.** The volume lives with the rest of the Studio's
  data under `TIME_WARP_DATA_DIR`; the machine has exactly one disk, as the period
  did.
- **It cannot change a program.** No setting rewrites, reformats or re-indents
  what you typed.

---

## Where next

- [The ROM library](roms.md) — what each ROM says at boot.
- [HB DOS](hb-dos.md) — the disk the settings do not touch.
- [Inside the HB-1](inside.md) — where `retroPalette` and the preview live in code.
