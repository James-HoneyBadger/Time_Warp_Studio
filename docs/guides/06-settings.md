# IDE Settings and Customization

Time Warp IDE allows extensive customization of the editor, display, and behavior to suit your preferences.

## Accessing Settings

Settings are accessed through the **View → Settings** menu or by:
1. Click the **View** menu
2. Select **Settings**
3. Configure options as desired

Settings are automatically saved to `~/.Time_Warp/config.json`.

## Display Settings

### Themes

Change the IDE appearance with built-in themes:

**Available Themes:**

1. **Dracula** - Dark theme with purple and pink accents
   - Excellent for night coding
   - Easy on the eyes for long sessions

2. **Monokai** - Dark background with bright syntax colors
   - Popular in professional editors
   - High contrast for clarity

3. **Solarized Dark** - Science-backed dark theme
   - Reduced eye strain
   - Balanced colors

4. **Ocean** - Blue-based dark theme
   - Calming color palette
   - Good for focused work

5. **Spring** - Light, vibrant theme
   - Great for daytime use
   - Fresh, energetic feel

6. **Sunset** - Warm, orange/red tones
   - Comfortable for evening work
   - Stylish appearance

7. **Candy** - Bright, playful colors
   - Fun for learning environments
   - High visibility

8. **Forest** - Green-based natural theme
   - Restful to the eyes
   - Unique appearance

**How to Change Theme:**

1. Open **View → Settings**
2. Select **Theme**
3. Choose from available options
4. Theme applies immediately

### Font Settings

Customize editor font appearance:

- **Font Family** - Choose between available monospace fonts
  - Recommended: Consolas, Monaco, DejaVu Sans Mono
- **Font Size** - Adjust text size (8pt to 20pt)
  - Default: 12pt
  - Larger sizes improve readability for presentations
  - Smaller sizes show more code at once

**Tip:** Increase font size for teaching to students; decrease for working on large programs.

### Line Spacing

Adjust vertical spacing between lines:

- **Single Spacing** - Default, compact display
- **1.5x Spacing** - More comfortable reading
- **Double Spacing** - Maximum spacing for clarity

### Word Wrap

Enable or disable automatic line wrapping:

- **On** - Lines wrap at editor width (better for viewing)
- **Off** - Horizontal scrolling (better for editing)

## Editor Settings

### Show Line Numbers

Toggle line number display in the editor:

- **On** - Helpful for referencing specific lines
- **Off** - More space for code

### Show Indentation Guides

Display subtle guides for indentation levels:

- **On** - Helps see code structure
- **Off** - Cleaner appearance

### Tab Width

Set spaces per indentation level:

- **2 spaces** - Compact
- **4 spaces** - Standard (recommended)
- **8 spaces** - Spacious

### Auto-Indent

Enable automatic indentation:

- **On** - Matches previous line's indentation
- **Off** - Manual indentation only

### Code Formatting

Auto-format code when running:

- **On** - Clean, consistent formatting
- **Off** - Preserve custom formatting

## Language Settings

### Default Language

Choose the default programming language:

- **BASIC** - Classic line-numbered BASIC
- **PILOT** - Interactive teaching language
- **Logo** - Turtle graphics
- **Python** - General-purpose programming
- **C** - Systems programming
- **Pascal** - Structured programming
- **Prolog** - Logic programming

The selected language is remembered between sessions.

### Syntax Highlighting

Each language has customizable syntax colors:

- **Keywords** - Language-specific keywords
- **Strings** - Text in quotes
- **Comments** - Explanation text
- **Numbers** - Numeric literals
- **Operators** - Mathematical and logical operators

**To customize:**

1. Open **View → Settings**
2. Select **Language → Syntax Colors**
3. Choose a language
4. Adjust colors for each element

## Output Settings

### Output Text Size

Adjust the text size in the output panel:

- **Small** - More output visible at once
- **Medium** - Default size
- **Large** - Better readability

### Word Wrap in Output

Enable line wrapping in output panel:

- **On** - Wrap long lines
- **Off** - Horizontal scrolling

### Clear Output on Run

Automatically clear previous output when running:

- **On** - Fresh output each run (recommended)
- **Off** - Accumulate all output

### Auto-Scroll to Bottom

Automatically scroll output to show new content:

- **On** - Always see latest output
- **Off** - Manual scrolling

## Canvas Settings

### Canvas Size

Set the default turtle graphics canvas size:

- **Width** - Default: 800 pixels
- **Height** - Default: 600 pixels
- **Resizable** - Allow user to resize window

### Canvas Background

Set the default background color:

- **White** - Professional appearance
- **Black** - Good for graphics
- **Custom Color** - Choose any color

### Turtle Starting Position

Configure where the turtle begins:

- **Center** - Middle of canvas
- **Top-Left** - Upper left corner
- **Bottom-Left** - Lower left corner
- **Custom** - Specify coordinates

### Graphics Speed

Control animation speed for turtle graphics:

- **Slow** (100ms) - Watch every movement
- **Normal** (50ms) - Default speed
- **Fast** (10ms) - Quick execution
- **Instant** - No animation

## Program Execution

### Auto-Save Before Run

Automatically save files before running:

- **On** - Never lose unsaved work (recommended)
- **Off** - Manual save required

### Show Execution Time

Display how long a program took to run:

- **On** - Helpful for performance analysis
- **Off** - Cleaner output

### Run Timeout

Maximum seconds to run a program:

- Default: 30 seconds
- Prevents infinite loops from crashing IDE

### Input Timeout

Maximum seconds to wait for user input:

- Default: 60 seconds
- Useful in interactive programs

## Interface Settings

### Show Toolbar

Display the quick action toolbar:

- **On** - Quick access to common features
- **Off** - More vertical space

### Show Status Bar

Display status bar at bottom:

- **On** - See file info and position
- **Off** - Cleaner interface

### Window Size

Save and restore window size:

- **Remember Window Size** - Restore size on startup
- **Default Size** - Always start with default size

### Panel Layout

Customize which panels are visible:

- **Show Editor** - Code editor (always on)
- **Show Output** - Output panel
- **Show Canvas** - Turtle graphics canvas
- **Show File List** - Browse files

**Tip:** Customize layout for your workflow. Teachers might hide file list; graphics programmers might show canvas by default.

## Keyboard Settings

### Keyboard Layout

Choose keyboard layout:

- **US English** - Standard
- **DVORAK** - Alternative layout
- **Custom** - Map keys yourself

### Custom Keyboard Shortcuts

Map custom shortcuts:

1. Open **View → Settings**
2. Select **Keyboard Shortcuts**
3. Click on an action
4. Press the keys you want
5. Shortcuts save automatically

### Disable Key Bindings

Disable shortcuts you don't use:

1. Open **View → Settings**
2. Select **Keyboard Shortcuts**
3. Click an action
4. Press **Delete** or **Clear**

## Advanced Settings

### Log Level

Set verbosity of internal logs:

- **Error** - Only critical issues
- **Warning** - Warnings and errors
- **Info** - General information (default)
- **Debug** - Detailed debugging info

Logs appear in the Output panel during execution.

### Script Timeout

Set maximum execution time:

- **10 seconds** - Short scripts
- **30 seconds** - Default
- **60+ seconds** - Long-running programs

### Memory Limit

Maximum memory for running programs:

- **Default** - System dependent
- **Restricted** - For learning (prevents resource abuse)
- **Unlimited** - For serious programs

## Saving and Restoring Settings

### Export Settings

Save your current settings to a file:

1. Open **View → Settings**
2. Click **Export Settings**
3. Choose save location
4. Settings saved as `.json` file

### Import Settings

Load settings from a file:

1. Open **View → Settings**
2. Click **Import Settings**
3. Choose saved settings file
4. Settings load immediately

### Reset to Defaults

Restore all settings to factory defaults:

1. Open **View → Settings**
2. Click **Reset to Defaults**
3. Confirm the action
4. IDE restarts with defaults

**Warning:** This cannot be undone. Export settings first if you want to keep them.

## Settings File Format

Settings are stored in `~/.Time_Warp/config.json`:

```json
{
  "theme": "dracula",
  "font_size": 12,
  "font_family": "Consolas",
  "default_language": "python",
  "show_line_numbers": true,
  "canvas_width": 800,
  "canvas_height": 600,
  "auto_clear_output": true,
  "auto_save": true
}
```

You can edit this file directly in a text editor.

## Troubleshooting Settings

**Problem:** Settings won't save
- **Solution:** Check that `~/.Time_Warp/` directory exists and is writable
- **Solution:** Try exporting and importing settings

**Problem:** IDE looks wrong after changing theme
- **Solution:** Reset to defaults and try another theme
- **Solution:** Check that your system font is installed

**Problem:** Keyboard shortcuts not working
- **Solution:** Check if another application captured the shortcut
- **Solution:** Try remapping to a different key combination
- **Solution:** Restart the IDE to apply changes

## Keyboard Shortcuts Reference

See [Keyboard Shortcuts Guide](07-shortcuts.md) for complete list.

## Tips for Optimization

1. **For Learning** - Larger fonts, Show line numbers, Simple theme
2. **For Graphics** - Show canvas by default, Instant graphics speed
3. **For Performance** - Smaller fonts, Disable syntax highlighting
4. **For Presentations** - Large font, Candy theme, Hide file list
5. **For Development** - Line numbers, Auto-format, Dark theme

## Next Steps

- Learn [Keyboard Shortcuts](07-shortcuts.md) for productivity
- Explore [Getting Started Guide](01-getting-started.md)
- Check [IDE Basics](02-ide-basics.md) for features
- Review [Troubleshooting Guide](08-troubleshooting.md) if issues arise

Happy customizing!
