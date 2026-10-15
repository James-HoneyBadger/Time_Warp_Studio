# Settings and Customization

Time Warp Studio exposes appearance and behavior options through its menus. Most editor behavior is fixed in the current build. Keyboard shortcuts cannot be remapped (see [Keyboard Shortcuts](07-shortcuts.md)).

## Accessing Settings

There is no single Settings dialog. Options are in the menus listed below. **File → Preferences** (**Ctrl+,**) opens only the Auto-Save dialog.

| Setting | Menu location |
|---------|---------------|
| Theme (33 themes) | **View → Theme** |
| Font family and size | **View → Font** |
| Auto-save and interval | **File → Preferences** (**Ctrl+,**) |
| Focus mode | **View → Zoom (Focus Mode)** (**F5**) |
| CRT effects | **View → CRT Effects** |
| Screen mode | **View → Screen Mode** |
| Minimap, outline, split editor, decks | **View** menu |
| Graphics speed | Speed slider on the toolbar |

Settings are saved automatically with Qt `QSettings`. See [Saved Values](#saved-values).

## Display Settings

### Themes

The IDE ships with 33 themes.

**Workstation and retro themes:** Neo-90s Workstation, Borland Turbo Vision 1991, NeXTSTEP Gunmetal, Amiga Workbench 2.0, VGA Amber Lab, Amber Monochrome, Green Monochrome, IBM PC CGA, Commodore 64, Apple II, DOS Blue, ZX Spectrum

**Dark themes:** Dracula, Monokai, VS Code Dark, GitHub Dark, Nord, One Dark Pro, Solarized Dark, Ocean, Catppuccin Mocha, Gruvbox Dark, Tokyo Night

**Light themes:** Gruvbox Light, Catppuccin Latte, VS Code Light, GitHub Light, Solarized Light, Spring

**Accessibility themes:** High Contrast Dark, High Contrast Light, Dyslexia Friendly, Accessible Dark Blue

**How to Change Theme:**

1. Open **View → Theme**.
2. Pick a theme. It applies immediately.

Other commands in the same menu:
- **Preview Themes…** opens a theme browser.
- **Cycle Workstation Theme** (**Shift+F10**) rotates through the workstation themes.
- **Edit Current Theme…** opens a colour editor. Colour schemes can be exported and imported as JSON.
- **Ctrl+Alt+H** toggles the high-contrast theme.

### Font

**View → Font** has two submenus:
- **Font Family** lists installed monospace fonts.
- **Font Size** sets the editor font size.

**Ctrl++** and **Ctrl+-** also change the editor font size.

**Tip:** Use a larger font when presenting to a class.

### CRT Effects

**View → CRT Effects** toggles the retro display effects. **CRT Settings…** adjusts scanline intensity, phosphor glow, screen curvature, and vignette.

### Screen Mode

**View → Screen Mode** switches between the normal graphics display and the retro text screen modes.

### Fixed Editor Behavior

The following are not configurable in the current build:

| Behavior | Current behavior |
|----------|------------------|
| Line numbers | Always shown |
| Indentation guides | Not available |
| Tab width | Fixed |
| Auto-indent | Built in per language |
| Word wrap | Not configurable |
| Line spacing | Fixed |
| Syntax colours | Follow the active theme |

## Language Settings

The default language is not configurable; blank files open as BASIC. To change the language of the current file, use **Ctrl+Shift+L** (Language picker). Language colours are part of the active theme.

## Output Settings

The output panel text size and wrapping are fixed in the current build.

## Canvas and Graphics

- **Speed:** the toolbar speed slider sets the delay for graphics output (0–500 ms per output line).
- **Size and background:** the canvas size follows the splitter layout, and the background follows the active theme. Neither is a separate setting.
- **Grid:** the canvas grid button toggles a coordinate grid overlay.

## Program Execution and Startup

- **Auto-save:** **File → Preferences** (**Ctrl+,**) enables or disables auto-save and sets the interval (10–3600 seconds).
- **Welcome tab:** shown at startup unless the stored `show_welcome` value is false.

## Interface Panels

- **View → Show Minimap** toggles the minimap.
- **F4** opens the document outline.
- **Ctrl+\** splits the editor; **Ctrl+Shift+\** closes the split.
- **Shift+F6** toggles the telemetry HUD.
- **Alt+1** to **Alt+4** switch workspace decks.
- **F10** / **Ctrl+Shift+P** open the command palette.

## Keyboard

Shortcuts cannot be remapped in this build. **Help → Keyboard Shortcuts** (**Ctrl+?**) shows the current bindings. See [Keyboard Shortcuts](07-shortcuts.md).

## Saved Values

IDE settings are stored with Qt `QSettings`, under the application's organization and application names. The storage location is platform-specific (on Linux, under `~/.config`). Examples of stored keys:

| Key | Contents |
|-----|----------|
| `theme` | Selected theme |
| `font_family`, `font_size` | Editor font |
| `show_welcome` | Welcome tab at startup |

Breakpoints are also stored in QSettings, so they persist across restarts.

## Export, Import, and Reset

- **Theme colours:** **View → Theme → Edit Current Theme…** exports and imports colour schemes as JSON.
- **Full settings export/import** and a global **Reset to Defaults** are not available in the current build.
- The CRT settings dialog applies changes only when you confirm it.

## Troubleshooting Settings

**Problem:** A theme or font change does not persist
- **Solution:** Confirm the Qt config directory (typically `~/.config`) is writable, then change the option again.

**Problem:** Theme or font looks wrong
- **Solution:** Reselect the theme in **View → Theme**, or the font in **View → Font**.

**Problem:** Keyboard shortcuts not working
- **Solution:** Check whether another application captured the key combination. See [Keyboard Shortcuts](07-shortcuts.md).

## Keyboard Shortcuts Reference

See [Keyboard Shortcuts Guide](07-shortcuts.md) for complete list.

## Tips for Optimization

1. **For learning** - Use a larger font and the Lessons panel (**Ctrl+F3**)
2. **For graphics** - Raise the speed slider to watch drawings faster
3. **For presentations** - Increase the font (**Ctrl++**) and use focus mode (**F5**)
4. **For focus** - Use the Dracula or Nord theme and turn on auto-save

## Next Steps

- Learn [Keyboard Shortcuts](07-shortcuts.md) for productivity
- Explore [Getting Started Guide](01-getting-started.md)
- Check [IDE Basics](02-ide-basics.md) for features
- Review [Troubleshooting Guide](08-troubleshooting.md) if issues arise

Happy customizing!
