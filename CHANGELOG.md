# Changelog

## [Unreleased]

## [16.3.0] - 2026-10-20

The HB-1 arrives: a bespoke 1982 home computer inside the Studio, with a
ten-language ROM socket, its own CONFIG screen and HB DOS on a 512-sector volume.
The machines get their own manual set, the README is reframed for retro-computing
hobbyists, and CHIP-8 clears its hi-res screen correctly.

### Added

- **The HB-1.** Tools > HB-1 Computer… (`Ctrl+Alt+W`, or
  `./run.sh --computer`) opens a self-contained home computer: a boot intro at the
  top of one CRT, a prompt below it, a selectable language ROM for all ten
  languages, and TEXT/SPLIT/GRAPHICS screen modes. The text side is a real code
  editor on its PROGRAM page and the machine's boot/output screen on OUTPUT; the
  graphics side is the same turtle canvas and the same native engines the IDE
  runs. Each ROM keeps its own program, all ten buffers, the selected ROM and the
  screen mode persist between sessions, the prompt has history, and BASIC direct
  mode (numbered lines, `LIST`, `RUN`, `NEW`, `?`) works at the machine keyboard.
  The screen is covered by the existing CRT glass without intercepting input.
  **CONFIG** (`Ctrl+,`) opens a settings screen for the machine: boot ROM, boot
  screen and start-up message, prompt-history depth, whether each ROM's program is
  remembered, whether drawing switches a text screen to SPLIT, four phosphors
  (green, amber, white, ice) that recolour the whole machine, independent screen
  and program text sizes, and the five glass strengths. Changes preview live,
  Cancel puts the machine back, and the settings persist.
  **HB DOS 1.0** gives the machine a disk: one 512-sector volume with `CATALOG`,
  `SAVE`, `LOAD`, `RUN "name"`, `DELETE`, `RENAME`, `LOCK`/`UNLOCK`, `VERIFY`,
  `TYPE`, `STATUS` and a two-step `INIT`, catalogued in the style of Apple DOS 3.3
  in 256-byte sectors. A locked file refuses to be overwritten or deleted, a file
  that will not fit is refused rather than truncated, and each file remembers the
  ROM that can run it. The volume is one JSON image under the application's data
  directory, so programs survive a restart and a teacher can copy the disk.
  With `--computer`, `--run`, `--screenshot` and `--export` act on the machine
  rather than the editor, so a script captures the boot screen and the drawing the
  program made on the machine's canvas.

### Documentation

- **The machines have books.** [docs/MACHINES.md](docs/MACHINES.md) states the
  premise — a system from a parallel universe — what the HB-1 and the CHIP-8
  share, and the deliberate anachronisms item by item. `docs/hb1/` is the Owner's
  Manual, the ROM library, the HB DOS 1.0 reference, the CONFIG reference and an
  engineering companion; `docs/chip8/` is the machine manual, the assembler and
  the complete instruction set with its 1977 quirks. Both sets are installed with
  the application and listed in Help > Documentation.
- **The README leads with capability.** A capability inventory links every part of
  the Studio — editor, canvas, debugger, projects, SQL, lessons, automation,
  appearance, the help system and both machines — to the book that owns it. The
  IDE guide, FAQ, language guide, architecture notes, roadmap and the in-tree
  agent instructions follow the same map.
- **The README is framed for retro-computing hobbyists.** It leads with the
  Studio as a complete old-school programming environment — a full IDE, ten
  interpreters from 1964–1980, and the two machines — with the lesson curriculum
  reduced to a short "If you teach" section. A new "For the retro-computing mind"
  section states the kept quirks, the JSON disk image, the user-owned ROM policy,
  the theme and phosphor work, and the native/offline stance. The same framing
  follows the About box, `--help`, the package descriptions, the desktop entry,
  the container label and the release notes.
- **The guides are gated.** `make doc-links` — and the Docs workflow — checks
  every relative link and `#anchor` across `docs/`, `README.md` and
  `Examples/CATALOG.md`; the workflow now also requires the machine books to
  exist and runs when the checker changes.

### Fixed

- **CHIP-8 `CLS` in hi-res.** Clearing the screen used the lores buffer size, so a
  `00E0` after `00FF` truncated the 128x64 framebuffer; the next `DRW` could write
  past it and nothing below the sixteenth row could be painted. Clearing now uses
  the current grid, and `lang_tests` pins both the buffer size and a pixel on the
  bottom row.
- **Build: the SQLite target.** CMake 4.3 renamed `FindSQLite3`'s imported
  target from `SQLite::SQLite3` to `SQLite3::SQLite3` and deprecated the old
  spelling, so every configure on a newer CMake warned against the link line.
  The build now uses whichever name the module provides, keeping the CMake 3.20
  floor working without the warning.

## [16.2.0] - 2026-10-20

Projects, a persistent REPL with BASIC direct mode, SQL, watch expressions and
data breakpoints, a JSON-RPC automation API, and terminal compilation join the
IDE. These are additive changes; existing languages, programs, settings and
lesson ids are unchanged.

### Added

- **A showcase program for every language.** `Examples/<language>/showcase.<ext>`
  is one program per language that walks through that language's whole surface —
  its statements, its data, its control flow, the idioms that are its own, and a
  drawing at the end — so a reader can see what the language does before opening
  a manual. BASIC mixes PRINT, arrays, all three loops, SUB/FUNCTION, error
  handling, graphics and sound; Logo uses lists, procedures, recursion and the
  pen; PILOT goes through its text, answer, match, compute, branch and
  subroutine units; COMAL and REXX show their loop forms and string machinery;
  Scheme shows higher-order procedures and closures; APL shows each function
  family and the right-to-left rule; FOCAL shows its sign branch and function
  library; CHIP-8 draws a montage with the font, BCD, collision and hi-res;
  Smalltalk defines two classes and queries a collection three ways. All ten
  draw, so all ten are in `DRAWING_EXAMPLES`, and the parity gates cover them:
  483 programs, 0 differences.

- **Projects.** A 🗂 Projects dock and a `.twproject` model group programs into
  named groups without moving them on disk: create, open, add files, add a whole
  folder (its programs are grouped by subfolder), add and rename groups, drag
  between groups, and see a missing file rather than losing it silently. The model
  stores relative paths inside the root, saves atomically and refuses a newer
  format; the active project and recent list are session state. `project.addFolder`
  exposes the folder import to the API.
- **The REPL bar is wired.** A persistent session per language runs commands off
  the GUI thread and draws through the same canvas queue. BASIC, FOCAL, PILOT,
  COMAL, REXX, APL, Logo and Scheme keep state between entries; Scheme keeps its
  machine and echoes expression values, while Smalltalk and CHIP-8 run each entry
  as a whole program. BASIC's `NEW` clears the session's bindings; in the IDE it
  also clears the editor. A numbered BASIC line typed at the REPL is stored in
  the editor as a program line, `LIST` and `RUN` are direct-mode commands, and
  `?` is accepted as shorthand for `PRINT`.
- **Watch expressions and data breakpoints.** The Debug panel evaluates
  arithmetic over the paused bindings and stops a run when a named variable
  changes, for the line-loop languages, Scheme and the whole-program languages.
- **SQL.** A SQLite-backed 🗄 Database tab opens or creates a database, browses
  tables and schemas, runs scripts and shows results. Programs reach the same
  connection with `SQL ...` in the line-by-line languages and Logo, `(sql "...")`
  in Scheme and `SQL run: '...'` in Smalltalk; CHIP-8 has no SQL form.
- **Automation API.** `./api` speaks JSON-RPC 2.0 over newline-delimited JSON:
  `language.run`, `repl.*`, `debug.*`, `sql.*`, `project.*` and
  `watch.evaluate`, with batches, notifications and standard error codes. The
  API's drawing object now reports turtle `actions` (including turns and pen
  changes) beside ink `commands`.
  `docs/API.md` documents it and the linkable C++ libraries behind it; the new
  [user guide](docs/USER_GUIDE.md) covers the IDE itself.
- **Terminal tools.** `./run.sh` builds if needed and launches the IDE; `./twc`
  compiles a program into a standalone executable for every language, embedding
  the source, linking the engine, writing a drawing as SVG and taking `--db FILE`
  for a program that uses SQL.

### Fixed

- The command and REPL matrices accept the documented “not available” refusal
  from an optional backend (BASIC `SAY`/`SPEAK`), so the gates pass on a machine
  with no speech engine instead of failing on an optional dependency. The refusal
  is only accepted for rows marked `optional`; any other error still fails. A new
  `--python` option lets the command matrix choose the worker interpreter, which
  is how both the backed and unbacked paths are exercised.

## [16.1.0] - 2026-10-19

The port is finished: every feature the removed PySide6 application had is back,
each landed with the test that proves it, and `docs/PORTING_PLAN.md` records the
phases in the order they landed. The canvas can be scrubbed, recorded as a GIF,
printed and exported as vectors; a program can be profiled line by line, paused in
any of the ten languages, and handed to a class as a bundle, a lesson session or a
presentation. Nothing is removed and no promise changes, so this is a minor
release.

### Added

- **SVG drawing export.** File > Export Drawing (Ctrl+E) and `--export` gain
  `.svg` in a build that finds Qt6::Svg: `QSvgGenerator` paints the same ink the
  screen and the raster writer paint, so the vectors cannot drift from the
  picture, and it writes through `QSaveFile`, so a failed write leaves no half
  file where a good drawing was. One user unit is one widget pixel, which is why
  the export takes no scale factor, and two exports of one drawing are
  byte-identical. A build without the component does not list the format, fails
  `.svg` with a reason, and does not name it in `--help`; the `.deb` and `.rpm`
  gain the Svg package only when the binary links it.
- **Printing.** File > Print Code (Ctrl+P) sends the program to a printer as the
  same A4 document the PDF export writes — one HTML source, so a printed program
  and an exported one cannot disagree — and File > Print Drawing renders the
  canvas for the printer's own resolution and scales it to fit the page, keeping
  its aspect ratio. Both need Qt6::PrintSupport, which the build asks for as an
  optional component: without it neither entry is added, rather than one being
  shown dead, and the PDF and HTML exports still work. The page layout takes a
  `QPagedPaintDevice`, so the tests drive it with a `QPdfWriter` and with a real
  `QPrinter` writing PDF, with no print system or dialog. `printCode` has no
  error result on purpose: `QTextDocument::print` reports an unusable device only
  by printing nothing, and probing it first costs a blank page.
- **Per-line profiling.** View > Profile Run times every statement of a
  line-by-line program and, when the run ends, a report lists the lines longest
  first with hits, time, share and the source text. The clock starts after a
  debug pause, so waiting for Continue is not charged to the statement; the
  toggle is off at every launch because a clock read around every statement
  costs, and a remembered "on" would make every later run pay for a report
  nobody asked to see. A whole-program language (Logo, Scheme, Smalltalk,
  CHIP-8) has no statement loop to time, and the console says so rather than
  showing an empty table. A profiled run takes the same steps and prints the
  same output as an unprofiled one.
- **An opt-in embedded terminal.** Settings > Workspace adds a 💻 Terminal tab
  that writes each line to `$SHELL` (with the current document's folder as its
  working directory, and a silent `cd` when the tab changes) and appends what the
  shell prints. It is off by default because it runs whatever is typed with the
  user's permissions, and presentation and player modes remove the tab and stop
  the shell — a process nobody can see is a process nobody can stop. Closing the
  window stops it too. There is no PTY on purpose: prompts, job control and
  full-screen programs are absent, so `vim` fails rather than taking the IDE over.
- **Stepping every language.** The whole-program languages stop too: Logo before a
  top-level statement, Smalltalk before a message send, and CHIP-8 before every
  instruction (the assembler keeps the listing line of each opcode by address, so a
  gutter breakpoint names a line rather than an address; the display still repaints
  at the frame boundary). `DebugSession` gained a source-line pause point beside the
  statement-index one, with a once-per-visit rule so a line holding several sends
  does not re-pause on Continue but does pause again when a loop returns to it, and
  an atomic fast path so an ordinary CHIP-8 run pays one load per opcode. The
  Variables panel and the Debug tab are refreshed at a pause, and the Debug tab's
  buttons drive whichever runner is paused. Stop reaches these languages too: the
  session's stop flag is checked at the same pause points, so a looping CHIP-8 or
  Smalltalk program no longer runs to its own frame or send limit first.
- **CHIP-8 runs SuperChip hi-res.** The machine and the Python reference both
  implement the SuperChip 1.1 core: 00FE/00FF select 64x32 or 128x64 (clearing the
  screen), 00Cn/00FB/00FC scroll, 00FD exits, DXY0 draws a 16x16 sprite in hi-res
  (8x16 with doubled columns in the original mode), FX30 points at an 8x10 font
  derived from the 4x5 set, and FX75/FX85 round-trip the RPL flags. The emulator
  screen follows the mode, both modes filling the same 2:1 canvas. MegaChip's
  planes and audio are not implemented and are reported as unknown opcodes. A new
  [`hires.ch8`](Examples/chip8/hires.ch8) example draws and scrolls in the mode.
- **A CHIP-8 ROM library.** The emulator's ROM Library... browses a folder of
  programs recursively (.ch8, .c8, .rom, .bin), filters by name, and remembers the
  folder after a program is opened. Time Warp Studio ships no ROMs: most CHIP-8
  programs in circulation are somebody else's copyright, so the library reads a
  folder the user has the right to use rather than bundling one.
- **Record the canvas as an animated GIF.** File > Record Canvas as GIF starts
  sampling the canvas ten times a second; running a program records the drawing
  appearing, and stopping asks where to save it. The GIF writer is in tree
  (`ui/gif_encoder.{hpp,cpp}`: GIF89a, a fixed 6×6×6 palette and LZW), so this
  needs no Pillow and no extra package, and Qt reads GIF so the test decodes what
  the encoder wrote, pixel for pixel. A frame that did not change becomes extra
  time on the frame before it, and frames are taken at one size so resizing the
  window mid-recording cannot break the file.
- **Class bundles from the classroom page.** Save Bundle... writes the class list,
  its work and its settings to one archive, and Load Bundle... unpacks one (asking
  first whether files that are already there should be replaced). Both are the
  engine's own ZIP through `classroom-bundle`/`classroom-unbundle` requests, so the
  file the IDE writes is the one `teach.py unbundle` reads and vice versa -- there
  is no second archive format and no runtime `tar` dependency. A successful load
  re-reads the class on screen.
- **Lesson session export.** File > Export Lesson Session as Markdown or PDF
  writes one file describing the lesson being worked on: what it teaches, every
  checkpoint with the verdict it was given in this session (passed, not passed,
  and never tried are kept apart), the program in the editor and the console
  output. The panel now remembers every verdict it sees rather than only the one
  on screen, and Restart clears them. The Markdown fence outlasts any backtick run
  in the program, and the PDF goes through the same writer and metadata
  normalisation as Export Code as PDF, so two exports of one session are one file.
- **Presentation mode, for teaching from the front.** View > Presentation Mode
  (Shift+F11) is focus mode with an audience: the menu bar, both toolbars, the
  function-key row, the status bar and every dock go away, and the lesson panel
  and console grow to the size the class is projected at. The size and whether
  the window takes the whole screen come from `teach.py present`, so a teacher
  who ran `present 30 --windowed` on a shared desktop is not overruled by a
  button. It asks the bridge for the setting when it is not already known, and
  Escape leaves the mode, restoring exactly the chrome that was there.
- **A classroom page in the IDE.** The right column's sixth page is the teacher's
  side of the curriculum: the class list, what has been assigned and the note
  that goes with it, and one row of marks per learner per lesson with how many
  checkpoints were handed in and how many passed. Open Student Folder takes you
  to the selected learner's work. Everything is read through the language
  bridge from the files `teach.py` already writes -- `ClassroomMode` is the one
  reader of `roster.json`, so the marks the panel shows and `teach.py collect`
  reports are one function's answer. Refresh re-reads those files, so a class
  changed from the command line appears without restarting the IDE. An empty
  class is an empty page with a line saying so, not a failure, and opening the
  page does not create folders for anybody.
- **A document outline, a split editor, and a two-program comparison.** The
  **Document Outline** dock lists the named places in the current program --
  Logo procedures, BASIC subroutines and labels, COMAL `PROC`, REXX labels,
  PILOT `*labels`, Scheme `define`s and APL `∇` functions -- and one click jumps
  there. File > Split Editor Right (Ctrl+\\) moves the current document into a
  second pane, Close Split merges everything back, and Move Tab to Other Pane
  sends one document across; the split and which side each file was on come back
  after a restart. Run > Run Both Panes (Ctrl+Alt+B) runs both documents headless
  and prints one table of outcome, engine time and ink, followed by each
  program's own output under its own name. There is no shared variable bus: a
  benchmark that lets each program write the other's variables is not
  reproducible.
- **Breakpoints and pausing for the line-by-line languages.** Clicking the gutter
  sets a breakpoint (a dot beside the line number), and the points are kept per
  file, survive a restart and are pruned when the program is deleted. Running a
  BASIC, FOCAL, PILOT, COMAL, APL or REXX program stops *before* a breakpointed
  statement; the toolbar's Debug, Continue and Trace are live again, with Trace
  running exactly one statement and Continue going to the next breakpoint or the
  end. Stop unwinds a paused run cleanly rather than leaving a thread asleep.
  Scheme keeps its own debugger and panel; whole-program languages still run to
  completion, because they have no statement boundary to stop at.
- **Accessibility and display controls.** View > High Contrast (Ctrl+Alt+H) gives
  the *current* theme a maximum-contrast variant: the surface becomes black or
  white in the direction the theme already goes and every syntax accent is lifted
  until it clears the WCAG 7:1 floor, so the chosen theme survives and only its
  legibility changes. The A+ button and Ctrl+Alt+A cycle the text size through
  100%, 125% and 150% across the editor, the console logs and the panels, and the
  step is remembered. View > Focus Mode (F11) hides both toolbars, the
  function-key bar, the status bar and every dock, with Esc as the way out. View >
  CRT Settings adds vignette, curvature, flicker and a phosphor tint beside the
  scanlines, with a live preview on the real overlay and values saved under
  `display/crt/*`; View > Screen Mode pairs a theme with those values in one
  action (Normal, Amber, Green, White Phosphor).
- **Export the code as a PDF, and a whole session as one HTML page.** File >
  Export Code as PDF writes an A4 document with line numbers and the program's
  name and language in the header; `QPdfWriter` ships with Qt, so this added no
  dependency. File > Export Session as HTML writes a single self-contained page —
  the program, its output, and the drawing embedded as a PNG data URI — which is
  the file to hand in or project. Exports are deterministic: the PDF writer's
  creation dates and document UUID are normalised, so exporting one program twice
  produces the same bytes.
- **Scrubbing the drawing.** A playback strip under the canvas — play/pause and
  a frame slider — appears as soon as a run has left ink. The canvas keeps the
  primitives in draw order (strokes and shapes interleaved as they were drawn),
  and the strip only changes how many of them are *shown*: export, fit-to-content
  and the ink counters still use the whole drawing, so scrubbing cannot lose a
  picture. Playback advances at 60 Hz, with the step chosen so a long drawing
  still finishes in about ten seconds, and new ink rejoins the live view.
- **A guided tour and one-time coach marks.** A first run opens a four-step tour
  anchored to the real controls — Run, the canvas, the Lessons panel and the
  function-key bar — each step dimming the rest of the window and naming what the
  control is for. `tour/seen` in `QSettings` means a second window or launch does
  not repeat it; Help > Guided Tour (F12) shows it on demand, and Settings > Help
  re-arms it. Two hints use the same anchors once each: the Lessons panel and the
  history arrow on Run.
- **A live CHIP-8 emulator.** Tools > CHIP-8 Emulator (Ctrl+Shift+M) opens the
  current `.ch8` program, or one chosen from the toolbar or dropped on the
  window, as an interactive machine rather than a run to completion. It has a
  60 Hz clock with a speed control in opcodes per frame, real delay and sound
  timers, a clickable 4x4 keypad plus the `1-4`/`Q-R`/`A-F`/`Z-V` keys, Run/
  Pause/Step/Reset (F5/F6/F7), a buzzer while the sound timer is above zero with
  a Mute switch, and registers, timers, cycles, call stack and a memory hex dump
  beside the screen. `LD Vx, K` waits for a press here instead of reading the
  `; keys` directive. Run in the editor keeps its whole-program model, so the
  lessons and the parity gates still have one deterministic answer. The window
  opens on the bundled `Examples/chip8/paint.ch8`, and a **Listing** tab
  assembles typed source with Ctrl+Return; a listing that does not assemble
  reports the error there and leaves the running program untouched.
- **Binary CHIP-8 ROMs are runnable, and the opcode set is complete.** A `.ch8`
  file containing a NUL byte is a ROM rather than a listing: its bytes copy to
  `0x200` unchanged, up to the 3584 that fit before the end of memory, and the
  editor opens it read-only because a keystroke in a binary would be a corrupted
  program waiting to be saved. It is copied one-to-one, so saving writes back the
  program that was loaded. `SYS` (`0NNN`) now assembles and is ignored, as every
  interpreter since the original does; `JP V0, addr` refuses a register other
  than `V0` rather than encoding it into the address; and `LD [I], Vx` /
  `LD Vx, [I]` leave `I` one past the block, as the 1977 interpreter does. The
  reference engine and the port share all of these rules, and the parity gate
  covers them.
- **F1 context help and Ctrl+F1 keyword inspector.** F1 opens the current
  document's language tutorial, scrolled to the word under the cursor; a word
  the guide does not contain leaves the guide at the top rather than doing
  nothing. Ctrl+F1 lists every word the editor colours for that language,
  filterable, with the guide's own line beside it. The list is the highlighter's
  vocabulary rather than a second table, so a word added to the colouring cannot
  vanish from the inspector. F1 no longer opens the shortcut reference; that is
  Help > Keyboard Shortcuts, which keeps the function-key bar's Shift+F1.
- **Help > Documentation, and Getting Started.** A help window with a contents
  tree built from `docs/INDEX.md`'s own tables — so it cannot promise a page the
  index does not list — and a Markdown-rendered page beside it. Links between
  pages open in the window and external links in the user's browser. The docs are
  now installed to `share/doc/time-warp-studio`, and the resolver reads
  `TIME_WARP_DOCS`, then the installed tree, then the checkout's `docs/`, so a
  build and a package show the same pages. A missing documentation tree explains
  itself rather than showing an empty pane. Getting Started opens the student
  guide directly.
- **Autosave and Version History.** Every document with unsaved changes is
  snapshotted under the Studio's data directory, five copies per document, at an
  interval Settings controls (two minutes by default). Untitled buffers are
  covered too, which is the case with nothing on disk to recover from. The
  manifest is written through a temporary file and renamed, so a crash during a
  snapshot cannot lose the list of the others. At startup, a copy newer than its
  file is reported in the status bar — never a modal — and File > Version History
  (Ctrl+Alt+V) lists a document's copies newest first with a preview. Opening one
  puts it in a new, unsaved tab and never writes the file, so choosing the wrong
  version costs a tab rather than today's work.
- **Run Selection, Reset Program, and a list of what you ran.** Run >
  Run Selection (Ctrl+F9) executes just the selected lines, in the same engine,
  without writing them to a file; an empty selection is refused with a message
  rather than quietly running the whole program. Run > Reset Program
  (Ctrl+Shift+F9) stops anything running and clears the canvas and output while
  keeping the document. The Run button's popup now lists the last ten runs —
  path, language, and the first line of each — and clicking one reopens and runs
  it.
- **A minimap, and visible whitespace.** The strip at the right edge is the whole
  document sampled one row per pixel, with the current view marked; clicking or
  dragging it scrolls. It reserves its own margin, so it cannot cover the end of a
  long line, and switching it off gives the width back. `Show spaces and tabs`
  renders a middle dot per space and an arrow per tab, which is what makes an
  indent of four spaces and one of a tab look different. Both are in Settings and
  persist.
- **Edit > Insert Snippet (Ctrl+Shift+I).** A catalogue of small runnable programs
  for all ten languages — a loop, a procedure, a sprite, a turtle walk — filtered by
  language and by name, previewed, and inserted at the cursor. `$CURSOR$` marks
  where the caret lands and `$SELECTION$` wraps what is selected; a whole
  insertion is one undo step. The catalogue is `native/share/snippets.json`,
  installed with the application, and every construct in it appears in a program
  under `Examples/` or in the language guide — a snippet that teaches syntax the
  engine rejects is worse than an empty library. Rewritten from the removed
  `features/snippets.json`, three of whose six languages no longer exist.
- **Edit > Find in Files (Ctrl+Shift+F).** Searches the folder of the current
  document and opens a hit at its line. Skips binaries, files over 2 MB and build
  or VCS directories (`.git`, `build/`, `node_modules/`, `__pycache__/`), searches
  a selection by pre-filling the term, stops on request, and reports an invalid
  regular expression as an error rather than as "no matches". The scan is one
  function -- `searchInFiles` -- that both the dialog's thread and the tests call,
  so the skipping rules exist in one place.
- **Edit > Format Code (Ctrl+I).** Re-indents BASIC, COMAL, REXX and Logo by their
  block structure — the loop bodies, `IF`/`END IF` arms, `ELSE` branches, Logo
  procedures and bracketed blocks — using the editor's tab width. It changes
  nothing but the left margin: strip both versions and the text is identical, a
  second format does nothing, and the cursor returns to the same line and column.
  The other six languages say they have no blocks to indent rather than
  pretending to format. Ported from `utils/code_formatter.py`, with the bug that
  left every `ELSE` body one level out fixed.

## [16.0.0] - 2026-10-18

All ten languages now run in C++: nothing is routed to the Python engine at run
time, and it has been demoted to the reference the parity gates compare against.
The IDE that runs them gained the features the port had been missing — themes,
working function keys, a command palette, an editor that checks a program as it is
typed, sound that is audible, and ports a program can talk to. Programs can also
leave the IDE: `--compile` writes a standalone executable.

Two of those change what the product *promises* rather than what it can do. The
language lineup and the run-time dependency are the project's central claim, and
`SERIAL`/`PARALLEL` put device nodes within a program's reach — which is why they
are allow-listed, and why that is a security surface rather than a feature.

### Added

- **The 33 themes are back.** The removed PySide6 application shipped 33 named
  colour schemes — the retro sets (Amber Lab, Turbo Vision, NeXTSTEP, Amiga
  Workbench, CGA, C64, Apple II, DOS Blue, ZX Spectrum) as well as the modern
  editor palettes (Dracula, Nord, Solarized, Catppuccin, Tokyo Night) and the
  accessibility sets. `native/src/ui/themes.cpp` carries them as one record each,
  generated from the Python's table rather than re-invented, so a learner who had
  picked "Amiga Workbench 2.0" gets the same colours back. View > Theme lists all
  33, Shift+F10 cycles them, the choice is remembered, and applying one sets the
  palette, the window's own rules, the editor's syntax colours and the canvas
  paper — a theme that recoloured the chrome and left the code blue would not be
  a theme.
- **The function keys work.** The bar under the window had been drawing ten
  labelled keys since the port and doing nothing with them: its signal was
  connected to a *second signal* of the same name, so a click was re-emitted to
  nobody. It now has the Python's three layers (normal, shift, control), a badge
  that cycles them, and one dispatcher behind every key, the matching shortcut
  and the command palette. Thirty keys, each doing what its label says; the ones
  whose feature is not built (split editor, find in files) say so in the status
  bar rather than doing nothing.
- **A command palette** (F10, Ctrl+Shift+P) over every menu entry, filtered as
  you type and showing each command's shortcut. Built from the live menu bar, so
  it cannot list a command that does not exist or miss one that does.
- **Standalone executables.** `time_warp_ide --compile program.bas --output
  hello` writes a copy of the IDE with the program inside it: double-click it and
  it opens on the drawing, with the authoring UI hidden and the document
  read-only. The payload is a documented trailer (`native/include/timewarp/ui/
  standalone.hpp`), so the artifact is an ordinary executable that happens to
  have some bytes after its segments. `--force` replaces an existing file and
  nothing else does.
- **Keyboard reference** (F1, Shift+F1), built from the same key table the bar
  draws, so help and bar cannot disagree.
- **CRT scanlines** are switchable (View > CRT Scanlines, Shift+F7). The overlay
  has existed since the port and had no way to be turned on.
- **The editor is a real editor.** Language-specific checking as you type
  (`native/src/ui/lint.cpp`, one rule set per language, from the engine's own
  command inventory): unbalanced brackets, unterminated strings and comments,
  `FOR` without `NEXT`, `IF` without `THEN`, PILOT lines with no unit letter, and
  a keyword one letter from a real one, with the spelling suggested. Findings are
  a squiggle, a triangle in the gutter, and a tooltip with the sentence. Plus
  optional word completion over the language's keywords and the document's own
  names (Ctrl+Space asks even when the setting is off), auto-indent that follows
  block openers, Tab/Shift+Tab over a selection, and a Preferences dialog (Ctrl+,)
  with a live preview of a sample that is deliberately wrong in two places — so
  changing a setting shows what checking looks like. Font, size, tab width,
  gutter, current-line band, wrapping, indent, completion and checking are all
  remembered per user.
- **Sound that is actually audible.** `BEEP`, `SOUND` and `PLAY` were
  acknowledge-only: they printed "🔊 SOUND 440Hz for 55ms" and played silence,
  because the retired Python build made audio optional behind `pygame`. The
  native build synthesises the period-correct waves itself
  (`native/src/audio/music.cpp`: square, triangle, sine, noise; equal
  temperament; envelopes so notes do not click), parses `PLAY`'s notation — tempo,
  lengths, octaves, accidentals, dotted notes, rests, `N` note numbers — and
  hands a WAV to the first of `paplay`, `aplay`, `ffplay` or `play` it finds. No
  new build dependency; a machine with none of those is silent but not broken,
  and the language's acknowledgement is still the reference's text, so the parity
  gates are unchanged.
- **Serial and parallel ports** (`native/src/io/ports.cpp`), so a program can talk
  to a board: `SERIAL OPEN #1, "/dev/ttyUSB0", 9600`, `SERIAL WRITE`, `SERIAL
  READ #1, A$, 2000`, `SERIAL CLOSE`, and `PARALLEL OPEN/WRITE/BYTE/CLOSE` for the
  data lines. Raw POSIX `termios`, no library, timeouts on every read, writes
  that wait rather than drop bytes. The device families a school plugs in are
  allowed and everything else is refused with a sentence — a program that can
  open any device node can also open a disk — with `TIME_WARP_PORTS` as the
  teacher's escape hatch. Tested against a pseudo-terminal, which is a real
  device with the other end in the test.
- **`INKEY$` reads the keyboard.** It was the empty string by documented design,
  which made every game unwinnable. The canvas forwards typed keys to the running
  program through the host, one key per call, queued; a headless run still
  answers empty, which is what the gates record.
- **`DRAW` and `WAIT`.** `DRAW "U20 R20 D20 L20"` executes GW-BASIC's path string
  through the turtle (U/D, R/L, E/H, F/G, B, N, M, C), and `WAIT ms` paces a frame
  loop.
- **File > Open Example.** A fresh install used to open onto an empty window with
  nothing to run. The example programs now travel with the install tree
  (`/usr/share/time-warp-studio/examples`) and the File menu lists them by
  language, so the first thing a learner does after installing is one click. A
  build tree finds them in `Examples/`; `TIME_WARP_EXAMPLES` points the menu at a
  folder of your own.
- **Something a non-technical user can install.** A release now ships a `.deb`, an
  `.rpm`, an AppImage and a tarball for x86_64 and arm64, plus a container image —
  all built from one tagged tree by `.github/workflows/release.yml`, with
  checksums beside every file. The AppImage is the one-file answer: double-click
  it, no root, nothing installed. The packages declare their Qt 6 and `python3`
  dependencies and put the binary, engine, desktop entry and icon where the system
  expects them.

### Fixed

- **The X on a tab closed nothing.** `setTabsClosable(true)` draws the button and
  then emits `QTabWidget::tabCloseRequested`, which nothing was connected to: the
  port had wired only `ResizableTabWidget`'s own signal (middle-click and the
  context menu), so the one control a user reaches for first was painted and
  inert. Forwarded in the tab widget's constructor, so both tab groups get it, and
  `ui_layout_tests` now clicks the real button rather than the signal.
- A docstring on `FunctionKeyBar` and on the toolbar described ports that had not
  happened; the disabled controls carry the reason instead.
- F3 was bound twice (Open on the toolbar, Find Next in the Edit menu), which Qt
  resolves by warning and firing whichever it prefers. F3 is Find Next, as in
  every other editor; Open keeps Ctrl+O.
- The lesson panel's verdict was hard-coded green-on-white, which is unreadable
  on the green-phosphor and amber themes. It takes its colours from the theme.
- A new tab opened after a theme was applied was syntax-coloured for the previous
  one: the colours were set once, at construction.
- `SyntaxHighlighter::setColors` updated the formats but not the rules, which hold
  *copies* of them, so a colour change repainted nothing.
- The turtle-speed slider was removed rather than finished: nothing animates the
  display list, so it controlled an ability the application does not have. The
  Settings button that replaced it opens the editor preferences.

### Added: the language ports

- **BASIC runs in C++.** The largest port, in five files: `basic.cpp` (variables,
  the load-time scan, the drawing helpers), `basic_statements.cpp` (the dispatch),
  `basic_text.cpp` (the reference's string expression evaluator),
  `basic_features.cpp` (shapes, fractals, music, particles, speech) and
  `basic_library.cpp` (the geometry those features draw). The core language,
  narrowing types, `SUB`/`FUNCTION`, records, the file channel, the VGA
  statements, `ON ERROR`/`RESUME` and the reference's Logo fallback are all
  there: a `.bas` file's `FORWARD 15` is Logo's `FORWARD`, drawn by Logo's turtle
  code, exactly as the reference forwards it. 137 programs run through both
  engines with **0 differences**, and seven cases join `lang_tests`. **Every one
  of the ten languages now runs in process**; nothing is routed to the Python
  engine at run time.
- **BASIC's remaining string and clock surface is covered.** `CHR$`, `FORMAT$`
  and `REPEAT$` printed nothing because PRINT read any name containing `$(` as a
  string array; the same defect existed in the reference and is fixed there too.
  `FORMAT$` now understands both `%` conversions and `#` masks, and `TIME$`,
  `DATE$` and `TIMER` read the local clock instead of answering empty/0. Seven
  parity rows and the command matrix cover them.
- **Four more languages** — **APL**, **FOCAL**, **CHIP-8** and **Smalltalk**, taking the
  lineup to ten. APL and FOCAL run line by line; CHIP-8 and Smalltalk run whole
  program. All four draw, all four are in the example corpus, and each has a
  command reference in `docs/LANGUAGE_GUIDE.md` and a tutorial in `docs/tutorials/`.
- **Command coverage for every language.** `command_table.py` grew from 279 rows to
  437, and every language now has zero commands that are neither exercised by a row
  nor waived — the four older languages had accumulated gaps (21 in PILOT, 18 in
  REXX) that the new rows closed.
- **APL runs in C++.** `native/src/lang/apl.cpp` is the port: numbers, strings and
  nested arrays; the glyphs and the keyword spellings (`IOTA`, `RHO`, `REDUCE` …);
  pervasive scalar functions, so `1 2 3 + 10` is `11 12 13`; reduce and scan; the
  structural functions; `∇` definitions; and the turtle words with `TX`, `TY` and
  `THETA`. The same 46 command rows and example run through both engines with **0
  differences**, and eight cases join `lang_tests`. APL is no longer bridged.
- **REXX runs in C++.** `native/src/lang/rexx.cpp` is the port: typeless string
  values, abuttal and `||` concatenation, the full operator set, `PARSE` and
  `PULL`, `IF` and the `DO` forms (count, `TO`, `OVER`, `WHILE`, `UNTIL`,
  `FOREVER`), `SELECT`, labels with `CALL`/`RETURN`, the built-in functions and
  the turtle words. 35 programs run through both engines with **0 differences**,
  and seven cases join `lang_tests`. REXX is no longer bridged.
- **Smalltalk runs in C++.** The object system is four files: `smalltalk.cpp`
  (tokens, parser, sends, printing), `smalltalk_library.cpp` (classes and
  primitives), `smalltalk_driver.cpp` (chunks and the top-level echo) and
  `smalltalk_internal.hpp`. Classes and methods are created at run time, blocks
  are real closures, `^` is a non-local return, cascades send to the original
  receiver, and the class library covers the corpus — numbers, collections,
  strings, booleans, Transcript and Turtle. 22 programs run through both engines
  with **0 differences**, and seven cases join `lang_tests`. Smalltalk is no
  longer bridged.

### Fixed: the language ports

- **FOCAL** — `ASK`'s answer never reached the program: the statement wrote to a
  private dictionary while the engine substituted from `interpreter.variables`, so
  the prompt appeared and the variable stayed 0. `FPI` and `FRAN` no longer require
  the parentheses FOCAL does not write, and `ASK "prompt", N` now accepts the comma
  every other FOCAL statement uses.
- **APL** — `⍕` (FORMAT) printed a character code rather than digits (`⍕42` answered
  `*`), and the keyword spellings `+REDUCE` and `+SCAN` did not work at all.
- **CHIP-8** — `JP V0, addr` encoded the register into the address's high nibble, so
  a jump table landed at address 6.
- **PILOT** — `LINK` and `LOAD` could never run: a line beginning `L:` is the label
  declaration, so the unit returned before it was reached and the long forms remapped
  to exactly such a line. It now reports that lesson chaining is not implemented
  instead of returning nothing.
- **REXX** — `SPACE(string, n, pad)` ignored its count, so `SPACE('a b c', 0)`
  answered `a b c`.
- **`command_inventory.py`** compared its "not a command" list case-sensitively while
  the list held lowercase entries, so those entries did nothing and the completeness
  report asked for rows that could not exist.
- **Unresolved names are reported, not echoed.** `PRINT UNSET` printed `UNSET`,
  `PRINT NOSUCHFN(3)` printed `NOSUCHFN(3)` with no error, and after `DEFSTR A-Z`
  `PRINT A` printed `A`. An unset numeric variable is 0, as BASIC defines it; an
  unknown function is an error. The evaluator's function table is also per-instance
  now, so one language's `EOF` registration no longer leaks into the other nine.
- **BASIC's expression functions.** `X = PEEK(1000)` and `PRINT IN(60)` work, and
  `STICK(n)`/`STRIG(n)` read the gamepad `JOYINIT` starts. `TYPE$(x)` answers the
  interpreter's type instead of printing nothing, and `SPEAK` reaches the speech
  handler.
- **BASIC array and function reads in `PRINT`.** `MID$("HELLO", 1, 3)` printed
  nothing because the `$(` of an array read claimed every `$`-suffixed function;
  `BED$(I)` printed nothing because the index was read as a literal instead of
  evaluated, which blanked two columns of `community_garden.bas`; and a `PRINT`
  that neither evaluator could read used to answer 0, hiding a typo, where the
  reference reports the evaluator's message.
- **`RESUME NEXT` without line numbers** resumes after the statement that failed; the
  error handler now records the line index as well as the BASIC line number.
- **COMAL `FUNC` in an expression.** `PRINT DOUBLE(21)` answers 42 in both engines
  through a new per-interpreter function hook; a body the executor cannot read
  reports an error rather than answering 0.
- **Logo `OUTPUT` reaches its caller.** `PRINT DOUBLE 21` prints 42 and `SHOW P`
  prints the value, in both engines.

### Lessons, for students and teachers

- **Twenty lessons, sixty checkpoints** — two for each of the ten languages, with a
  plan, a worked answer and hints for every checkpoint.
- **Checked by running the learner's program** in a headless engine, so `PRINT 6 * 7`
  and `PRINT 42` are the same answer, and a drawing checkpoint cannot be passed by
  printing about drawing. `expect_lines`, `expect_turtle` (pen segments, filled shapes
  and closure) and `expect_error` are the three kinds of expectation.
- **The 🎓 Lessons panel** in the IDE, the one page of the right column that is a real
  port rather than a placeholder. It hosts no curriculum and runs nothing itself: it
  drives the bridge, which is what keeps a panel's verdict and a teacher's mark the same
  function's answer. Six new protocol messages, with the solutions deliberately *not*
  in the curriculum payload — a lesson whose answer travels with its questions has been
  spoiled before it is read.
- **`native/tools/learn.py`** — list, walk, check, hint, answer, progress, reset, for a
  learner at a terminal.
- **`native/tools/teach.py`** — lesson plans with objectives and teacher notes, answer
  keys, curriculum validation, a class roster, handout distribution that never
  overwrites started work, marking that writes nobody's progress, class progress
  reports, zip bundles a class can be moved in, a presentation size for teaching from
  the front, and a lesson scaffold whose skeleton the loader accepts.
- **The curriculum is checked, not trusted.** `teach.py validate` runs all sixty
  solutions and all sixty starters through the engine: a solution that does not pass is
  a lesson nobody can finish, and a starter that already passes has nothing to do. Both
  are invisible in a file listing, because lessons are data and nothing else runs them.
- **`docs/lessons/`** — a hub, a student guide, a teacher guide, a technical reference
  for the curriculum format and protocol, and generated plans for all ten languages.
- **`TIME_WARP_DATA_DIR`** now relocates everything the studio keeps for a user. It
  exists because a test recorded lesson hints into a real learner's `progress.json` —
  the hint counter being cumulative is how the leak surfaced.

### For contributors, and for the repository itself

- **Community files.** Issue forms for a bug, a feature and a language, a pull-request
  template, `SUPPORT.md`, `CODEOWNERS` and a deployment guide, so the questions a
  newcomer asks first have somewhere explicit to go. The bug form asks which
  environment the problem happened in, because a built-from-source IDE and an
  installed one take different paths to the bridge.
- **`docs.yml` now checks something.** It regenerates the lesson plans and fails on a
  diff, requires a tutorial for every language, requires a file for every guide
  `docs/INDEX.md` promises, and runs `teach.py validate`. A curriculum that has
  drifted from the plans a teacher would teach from is invisible in a file listing; it
  is a red check now.
- **Corrected counts.** Several documents still described five bridged languages and
  44 examples, the contributing guide listed six languages, and it offered a
  discussion board the project does not answer on.
- **A credential cannot be committed.** `.pre-commit-config.yaml` now runs
  `detect-secrets` against a committed baseline, so a key pasted into a file fails
  the commit instead of reaching GitHub. The baseline is what keeps the hook usable:
  it records what the tree already holds, so only a secret added *since* is reported
  — and a hook that cries wolf gets switched off. `CONTRIBUTING.md` says how to
  install the hooks and how to silence one false positive at a time; `SECURITY.md`
  says what to do if a secret does reach a commit: rotate it, then rewrite the
  history, because removing it in a later commit does not remove it from the clones.
- **The lint hook passes over the whole tree.** `native/tools/` had never been
  linted, and two findings surfaced when it was: a `lambda` assigned to a name in
  `scheme_differential_check.py`, and an ambiguous `l` in `teach.py`. Both are fixed
  rather than excluded, so `pre-commit run --all-files` is green.

### Syntax colouring in the editor

- **Per-language highlighting** — `native/src/ui/syntax_highlighter.cpp`, one rule
  table per language. The ten disagree about what a comment is: REXX has `--` and
  `/* ... */`, Smalltalk's comment is a **double**-quoted string while its string is a
  single-quoted one, FOCAL has no comment syntax at all, and PILOT's units are a letter
  and a colon at the start of a line. So the rules are data rather than one shared
  approximation with a keyword list bolted on, and the keyword tables are the engine's
  own command names — a highlighter that colours a word the engine would reject teaches
  the wrong vocabulary.
- **Twelve CTest suites.** `syntax_highlighter_tests` colours a bare `QTextDocument`
  and asserts the colour at a named position. That is the only kind of check that
  works here: coloured text one token out of place still looks like a working editor.

### Find, replace and go-to-line

- **`FindBar`** — a bar under the editor tabs, hidden until Edit > Find asks for it.
  Replace is the same bar with a second row. Enter finds the next match, Shift+Enter
  the previous one, Escape closes it and F3/Shift+F3 keep finding with it closed.
- **The searching lives on `CodeEditor`** — `findNext` (wrapping, which is what makes
  a search that started midway through the file reach the text above it), `matchCount`
  (re-counted whenever the document changes, so it cannot claim matches that have been
  deleted), `replaceAll` (one undo step: replacing a document in twelve pieces means
  twelve undos to get back) and `replaceCurrent`.
- **Go to Line** — Ctrl+L. Not Ctrl+G, which the canvas grid already owns: two actions
  on one key is an ambiguous shortcut that Qt resolves by guessing.
- The About box now names all ten languages instead of the original six.

### The last session comes back

- **Recent files** — File > Open Recent lists the last ten paths, most recent first,
  with a Clear Menu entry. Opening a file that is already open bumps it to the top
  rather than adding a second entry: raising a tab is using the file.
- **The session is restored at launch** — the documents that were open come back,
  unless a file was named on the command line (a file argument is a request for that
  file) or `--screenshot` is in use (a screenshot has to be reproducible from the
  command line, not from somebody's last session). A file that has been deleted or
  moved since is skipped silently, because a prompt at startup that cannot be
  dismissed is worse than a tab that is not there.
- **`saveLayout` and `saveSession` are separate** — `--reset-layout` puts the window
  back to its defaults without forgetting what was being edited.

### Save the drawing

- **File > Export Drawing** (Ctrl+E) writes PNG, JPEG or BMP; **Edit > Copy Drawing**
  (Ctrl+Shift+C) puts it on the clipboard; and `time_warp_ide --run program.logo
  --export drawing.png` does the same from a script, which is what a teacher rendering
  a class's work needs and what the tests use.
- **The export is the drawing, not a screenshot.** It goes through the same `paintInk`
  the window paints with, so the picture cannot drift from what is on screen, and it
  leaves out the grid and the turtle marker because those are chrome. It renders at
  twice the on-screen size, because a picture worth saving is worth the pixels.
- **SVG and PDF are not in this release.** They need the QtSvg and QtPrintSupport
  components, which the build does not ask for; PNG/JPEG/BMP need nothing that is not
  already linked.
- Exporting an empty canvas says so in the status bar rather than writing a blank
  picture.

### What a run actually did: Variables, Telemetry and Debug

The right column's three remaining placeholders are real panels now. Both
inspection panels read data the engine always had and nobody had asked it for.

- **🔍 Variables** lists every binding the engine can see, with the type it knows it by
  and its value, and filters by name. The bridge grew `time_warp/utils/inspection.py`
  to turn engine state into rows (`variable_rows`), so a learner sees `NAME = ada` and a
  programmer sees `REPCOUNT = 4` without either of them reading a debugger.
- **⚡ Telemetry HUD** shows what the run cost and what it did: steps used out of the
  cap, engine time, steps per second, last line, recursion depth, call stack depth,
  variables, turtle pose and pen, output lines and drawing commands, plus BASIC's
  16-byte memory window. Beside them sit the rows only the IDE can vouch for — wall
  clock, and how much ink reached the canvas — so a drawing the engine counted but the
  canvas never received reads as two rows that disagree.
- **🐞 Debug** drives `DebuggerRunner` from the panel: Continue, Pause, Step In, Step
  Over, Step Out, Stop, and a breakpoint list. Pausing shows the line, the procedure,
  the call stack and that frame's bindings. Only Scheme can be stepped — the nine
  bridged languages run to completion in another process with no pause protocol — so
  the panel disables itself with a sentence saying why rather than offering buttons that
  do nothing.
- **A run that never paused still leaves bindings.** `DebuggerRunner` now takes a final
  snapshot when a program ends, so the Variables panel shows what the program finished
  with instead of nothing, and `DebugVariable` carries the machine's own `type_name`
  rather than a guess made from the printed text.
- The three panels are emptied at the start of every run: a previous run's numbers
  sitting in a HUD while a new one starts read as this run's.

### FOCAL, PILOT, COMAL, CHIP-8 and Logo run in C++, and the engine for the rest

Five languages have left the bridge. The change that matters is not the ports
themselves but the seam they establish: an executor is now a C++ function, and the
IDE asks `executor_for(language)` whether it can run in process.

- **`timewarp_lang`** — a dependency-free C++20 library with the shared interpreter
  loop: program storage, variables, streamed output, synchronous input, the run
  budget and telemetry. An executor is a plain function
  `(Interpreter&, source, TurtleHost&) -> std::string`, the same shape as the
  Python engine's, because the reference's behaviour is what the port is checked
  against.
- **`TurtleHost`** — the drawing seam, deliberately Qt-free. A test records calls
  as text; the IDE's `NativeCanvasHost` turns them into `CanvasCommand`s.
- **FOCAL** is ported whole, including the rules a reimplementation gets wrong:
  branching by sign, `DO` returning at the end of the program, undefined variables
  as zero, and `!` suppressing a newline inside one `TYPE`.
- **PILOT** is ported whole: the unit letters and their `Y`/`N` conditions,
  labels and `U:`/`S:` subroutines with `E:` returns, `C:`'s string functions,
  `G:` graphics, `F:` files, hints, delays, pauses and the long-form keywords.
  Its `C:`/`B:` arithmetic runs on the native expression evaluator, so `timewarp_expr`
  is now linked into a Run rather than merely built.
- **COMAL** is ported whole: IF/ELIF/ELSE/ENDIF, FOR/NEXT, WHILE/ENDWHILE,
  REPEAT/UNTIL, LOOP/EXIT/ENDLOOP, PROC/FUNC with CALL/RETURN, DIM arrays,
  DATA/READ/RESTORE, GOTO/GOSUB, PRINT/INPUT and the turtle statements. Its loop
  frames live on an explicit stack, so `NEXT` finds its own `FOR` through blocks
  that have already closed.
- **CHIP-8** is ported whole: the two-pass assembler, all 35 opcodes, the 1977
  choices (Vx shifts in place, sprites wrap) and the frame model, with the same
  limits reported instead of silently applied.
- **Logo** is ported whole, and it is the one that is not a line-by-line language:
  a `TO ... END` block *defines* a procedure rather than running, and a bracketed
  command can span lines, so the statement boundaries are decided by a loader
  before anything executes. Everything the reference does is reproduced, including
  the parts that look like bugs: `LOCAL "X` really does make a variable called
  `"X`, a list printed through `SHOW` comes out as `['1.0', '2.0']`, and the
  splitter that starts a new statement at a procedure name once all of that
  procedure's arguments are present (the rule that kept a second call inside a
  `REPEAT` body from being swallowed, which is how a whole layer of
  `Examples/logo/mandala.logo` used to vanish).
- **The drawing seam grew the parts Logo needed** — a 3-D pose with the
  reference's Rodrigues rotations and its `1 + z/400` projection, so
  `turtle_3d.logo` still draws a helix instead of a flat spiral; `draw_polygon` for
  `STAMP`, `DOT` and `FILLED`; a font size on `draw_text` for `LABEL`; and `reset`
  for `CS`, which is not `CLEAN`: it puts the turtle, the pen and the canvas back
  the way a run starts.
- **A native run now starts on the reference's surface** — the dark background and
  the white pen — because a logo-style program may draw before it names a colour,
  and white ink on the IDE's white canvas is a run that reports four strokes and
  shows nothing.
- **The telemetry HUD measures whole-program runs.** An executor with its own
  statement loop reports its steps (`Interpreter::note_step`), so a Logo run's
  Steps row is a number rather than a standing zero.
- **Python-compatible randomness** — `FRAN` and `RND` run on a reproduction of
  CPython's Mersenne Twister, seeded the way `random.seed(int)` seeds it. A port
  with `std::mt19937` would be a different generator, and the differential harness
  could not tell a defect from a different sequence.
- **The IDE dispatches natively.** `MainWindow` gives a ported language to
  `NativeRunner`, a `QThread` that streams output, answers input prompts and feeds
  the canvas queue. The Variables and Telemetry panels are filled from the same
  run, so they do not know which engine produced the table.
- **A parity gate** — `native/tools/lang_differential_check.py` runs every
  command-table row and every example through both engines and compares output
  lines, every drawing call in order, and the final variables: **225 of 228
  programs agree, 0 differences**, the three that use a random function being
  reported and skipped. The corpus includes the nine Logo examples, among them the
  720- and 800-iteration fractal drawings, so the gate covers recursion, procedure
  restoration and the statement splitter as well as the primitives.
- BASIC, REXX, APL and Smalltalk still run through the bridge, and the bridge
  remains the thing to delete. Each port removes one path through it.

---

## [15.0.0] - 2026-10-16

The implementation was replaced wholesale: Time Warp Studio is now a C++20 / Qt6
desktop application, and the language lineup and packaging that went with the previous
implementation are gone. Both changes were made after `v14.1.2` and shipped together,
so both are recorded here.

### The Qt application is now the product

Time Warp Studio is a **C++20 / Qt6 desktop application** (`native/`). The PySide6
implementation that came before it has been **removed**; what remains of Python is a
temporary bridge the Qt application spawns for the five languages that are not ported
yet.

#### Added

- **Qt application** (`native/`) — `time_warp_ide`: documents and tabs, code editor
  with line-number gutter and current-line highlight, streaming output console with
  interpreter errors in their own tab, turtle canvas with zoom, pan, grid and
  auto-fit, menus, toolbar and status bar, and `make install` with a desktop entry
  and icon.
- **Native engines** — the expression evaluator (`timewarp_expr`) and Scheme
  (`timewarp_scheme`, an explicit-stack CEK machine), each with a parity gate against
  the Python reference (153 and 167 cases, zero unexplained divergence).
- **Language bridge** (`native/bridge/`) — a JSON-lines worker the application spawns
  for BASIC, PILOT, Logo, COMAL and REXX. It is scheduled for deletion one language at
  a time as each is ported.
- **Test and examination tooling** — 11 CTest suites, the bridge engine's own suite,
  `example_smoke.py` (all 44 examples through the real bridge), `command_matrix.py`
  with `command_table.py` (one row per documented command, 279 rows), and
  `command_inventory.py`, which extracts the implemented command set to check that
  table for completeness.
- **CI** (`.github/workflows/native.yml`) covering the build, the suites and the gates.

#### Removed

- The entire PySide6 application, its launchers (`run.py`, `run.sh`), its Docker and
  developer tooling, and the workflows that packaged and published it.
- The C, Pascal, Prolog, Forth, Brainfuck and Python executors.

#### Fixed during the port's examination

Examining every documented command through the real bridge turned up defects that no
gate had been looking for:

- **A keyword boundary bug in BASIC.** `startswith("DO")` also matched `DOG = 42`, so
  the `DO` statement swallowed the assignment: the variable was never set and `PRINT`
  then echoed the name back. Any variable named `DOUBLE`, `RUNNER`, `CLOSER`, `NEXTX`
  or similar silently lost its value. Eleven keywords were affected.
- **`ON TIMER` could never work.** The dispatcher sliced one character too many,
  discarding the `(`, so the handler rejected the format its own error message asked
  for.
- **`SHOW` and `TYPE` in Logo never evaluated their argument.** Both returned their
  source text, so `SHOW 1 + 2` displayed `1 + 2`. `PRINT` had always worked, so the
  two disagreed about the same expression.
- **A reporter could not be applied to a list literal** — `FIRST [1 2 3]` received
  just `[`, because argument consumption balanced parentheses but not brackets.
- **An input statement stopped the program.** The worker called `execute()` once, so
  every program that read input ended at its first prompt; resumed runs needed a loop.
- **The worker ran a program inline**, which meant `in-reply` and cooperative stop
  could never be delivered while a program was running. Stop now works.
- **`JOYINIT`** reported an absent *optional* backend with the error prefix, which the
  IDE presents as the program failing.

### Language lineup change — six active languages

Time Warp Studio now ships exactly **six** languages: **BASIC, PILOT, Logo,
COMAL, REXX, and Scheme**. The C, Pascal, Prolog, Forth, Brainfuck, and Python
executors have been removed from the product, along with their examples,
tutorials, snippets, lessons, and tests.

#### Added

- **COMAL** executor (`languages/comal.py`) — structured BASIC with
  `IF/ELIF/ELSE/ENDIF`, `FOR/NEXT`, `WHILE/ENDWHILE`, `REPEAT/UNTIL`,
  `LOOP/ENDLOOP`, `PROC/ENDPROC` + `CALL`, `DIM`, `DATA/READ`, `GOTO`/`GOSUB`,
  and turtle `MOVE`/`TURN` commands.
- **REXX** executor (`languages/rexx.py`) — typeless string processing with
  `SAY`, `PULL`, `PARSE`, `IF/THEN/ELSE`, `DO` (TO/BY/WHILE/UNTIL/FOREVER/
  OVER), `SELECT/WHEN`, labels + `CALL`/`ARG`/`RETURN`, `LEAVE`/`ITERATE`, a
  full expression evaluator (including `||` and abuttal concatenation) and 20+
  built-in string/number functions.
- **Scheme** executor (`languages/scheme.py`) — whole-program Lisp interpreter
  with lexical closures, `define`/`lambda`, `if`/`cond`/`case`/`when`/`unless`,
  `let`/`let*`/`letrec`/named `let`, `quote`/`quasiquote`, `set!`, list and
  vector primitives, `map`/`filter`/`foldl`/`apply`, and turtle bindings.
- New tutorials (`docs/tutorials/comal.md`, `rexx.md`, `scheme.md`), example
  programs (`Examples/comal/`, `Examples/rexx/`, `Examples/scheme/`), code
  snippets, and two guided lessons per new language.

#### Removed

- Executors: `brainfuck.py`, `c_lang_fixed.py`, `forth.py`, `pascal.py`,
  `prolog.py`, `python_lang.py`, plus their tests, examples, and tutorials.
- `Language` enum members `C`, `PASCAL`, `PROLOG`, `FORTH`, `BRAINFUCK` and
  `PYTHON_LANG`.

#### Changed

- `_WHOLE_PROGRAM_EXECUTORS` now contains Scheme only; the subprocess sandbox
  applies to Scheme.
- Syntax highlighting, comment toggling, keyword reference cards, document
  outlines, code formatters, snippet library, lessons, status-bar emoji,
  project icons, telemetry HUD, and output prompts all cover the new lineup.
- Example verifier default language map updated (`comal`, `rexx`, `scheme`).

All notable changes to Time Warp Studio will be documented in this file.

> **Scope note:** The current release actively supports **6 languages**:
> BASIC, PILOT, Logo, COMAL, REXX, and Scheme, and Python. Older
> entries below mention additional language executors (Smalltalk,
> Perl, Ruby, Erlang, Haskell, JavaScript, Lua, HyperTalk, PostScript,
> LISP/Scheme, Tcl, 6502 Assembly) that were built at the time but have since
> been removed from `time_warp/languages/` and are no longer part of the
> product. These entries are kept as historical record only.

## [14.1.2] - 2026-10-15

### Packaging

- Matched the Python distribution name to the existing PyPI project `timewarp-ide`.

## [14.1.1] - 2026-10-15

### Packaging

- Included the desktop and standalone editor entry-point modules in the Python wheel.
- Documented installation directly from the GitHub Release wheel.

## [14.1.0] - 2026-10-15

### Studio Improvements

- Added a lessons dock with built-in lessons and checkpoint verification.
- Added a Borland Turbo-style keymap and live Function-Key Bar.
- Expanded graphics, sound, and Prolog capabilities.
- Removed obsolete prototypes and aligned documentation and packaging with the supported Studio.

## [14.0.0] - 2026-10-01

### Editor and Workflow Improvements

- Added Go to Line navigation, clickable error locations, and error-only output filtering.
- Added a searchable document outline for supported procedure and function declarations.
- Added recovery for unsaved editor tabs after an unexpected shutdown.
- Made lesson authoring and file-dialog filters use the canonical nine-language registry.
- Corrected current release documentation and package metadata to match the active language set.

## [13.0.0] - 2026-06-01

### New Languages

- **Smalltalk executor** (`languages/smalltalk.py`) — Smalltalk-80 interpreter (~830 lines) supporting:
  - `Transcript show:/showCr:/print:` for output; temp variable declarations `| x y |`
  - Assignment `:=`, conditional `ifTrue:/ifFalse:/ifTrue:ifFalse:`
  - Iteration: `timesRepeat:`, `to:do:`, `to:by:do:`, `do:`, `whileTrue:`, `whileFalse:`
  - Collections: `OrderedCollection new`, `add:`, `at:put:`, `select:`, `collect:`, `reject:`, `inject:into:`
  - String messages: `,` (concat), `size`, `reversed`, `asUppercase`, `asLowercase`, `copyFrom:to:`
  - Block closures `[...]` with correct outer-scope variable sharing
  - Number messages: `factorial`, `sqrt`, `abs`, `max:`, `min:`, `raisedTo:`, `gcd:`
  - Turtle graphics via `Turtle forward:`, `Turtle right:`, etc.
  - 31/31 tests passing; 5 example programs in `Examples/smalltalk/`

### Syntax Highlighting

- **Smalltalk** — keyword/unary/binary message highlighting, `"..."` comment style, string literals

## [12.0.0] - 2026-06-01

### New Languages

- **Perl 5 executor** (`languages/perl.py`) — full Perl 5 interpreter (~2450 lines) supporting:
  - Scalar, array, and hash variables with proper scoping (`my`, `our`, `local`)
  - Control flow: `if`/`elsif`/`else`/`unless`, `while`/`until`, `for`/`foreach` (C-style and range), `do...while`
  - Statement modifiers: `print "x\n" if $cond;`, `last if $i > 3;`, `next if $i == 3;`
  - Subroutines with `@_` argument passing, closures, and recursive calls
  - Regular expressions: matching (`=~`), substitution (`s///`), global (`/g`), capture groups (`$1`–`$9`)
  - String builtins: `uc`, `lc`, `length`, `substr`, `index`, `rindex`, `reverse`, `sprintf`, `chomp`, `chop`
  - Array builtins: `push`, `pop`, `shift`, `unshift`, `splice`, `sort`, `reverse`, `map`, `grep`, `join`, `split`
  - Hash builtins: `keys`, `values`, `each`, `exists`, `delete`
  - Math builtins: `abs`, `int`, `sqrt`, `sin`, `cos`, `atan2`, `log`, `exp`, `rand`, `srand`
  - Turtle graphics: `forward`, `backward`, `left`, `right`, `penup`, `pendown`, `color`, `setheading`, `home`
  - 36/36 tests passing; 5 example programs in `Examples/perl/`

### Bug Fixes

- **Perl executor** — fixed 8 bugs found during test development:
  - `$i++` incorrectly parsed as binary `+` → infinite loops in `while` and C-style `for`
  - `_split_statements` did not split at `}` when outermost block closed → trailing statements after loops were silently dropped
  - Statement modifier loop ran after `print`/`last`/`next`/`return` handlers → `last if $cond` treated `if` as a label
  - `_contains_block` didn't detect unclosed string quotes → false modifier matches
  - `"a" . "b"` matched as a single double-quoted literal by `endswith('"')` check
  - `push @a, 3` extended array with wrong items when `@a` was expanded during arg evaluation
  - `split(/,/, $str)` failed because `/,/` was split at the comma by `_split_by_comma`
  - `_emit` split on `\n` eagerly → partial-line `print "$i "` calls produced separate output lines instead of accumulating

## [11.0.0] - 2026-06-01

### Major Features

- **Project File-Tree Panel** (`ui/project_tree.py`) — new `ProjectTreePanel` docked on the far left of the main window. Shows all tracked project files with language emoji icons; double-click to open in a new tab; right-click context menu: Add File, Remove from Project, Set as Main File. Auto-populates when a `.twsproj` project is opened and saves on any change.

- **Plugin Manifest Validation + Capability Sandboxing** (`features/plugin_system.py`) — plugins are now scanned with `ast.parse()` (no code execution) before loading. Plugins that import dangerous modules (`ctypes`, `cffi`, `subprocess`, `socket`, etc.) without declaring the corresponding capability in a `manifest.json` file are blocked. Blocked plugins are listed with a clear error message; safe plugins load unchanged.

- **Plugin Marketplace SQLite Persistence** (`features/plugin_marketplace.py`) — marketplace listings, reviews, releases, developers, and templates now survive application restarts via a SQLite database at `~/.time_warp/databases/marketplace.db`. All in-memory state is loaded from the database on startup and written on every mutation.

- **Real LAN Collaboration TCP Send/Receive** (`features/collaboration.py`) — `_send_message()` now actually transmits to peers over TCP using a 4-byte length-prefixed JSON framing protocol. Host binds on port 54321; joiner connects at session join. Background `_recv_loop` threads dispatch incoming messages (code updates, cursor moves, chat, sync requests) to registered event callbacks. `disconnect()` cleanly closes all sockets.

- **Student Roster + Assignment Distribution** (`features/classroom_mode.py`) — added `Student` and `StudentRoster` dataclasses, persisted to `~/.time_warp/classroom_roster.json`. New methods: `add_student()`, `remove_student()`, `save_roster()`, `distribute_assignment()` (writes bundle files to each student's work directory), and `collect_submissions()` (zips all student work into a timestamped archive in the bundles directory).

- **Persistent Terminal History** (`ui/terminal_widget.py`) — shell commands are now appended to `~/.time_warp/terminal_history` (plain text, one per line). On next launch the history is pre-loaded into the up/down arrow history buffer (up to 200 entries in memory, unlimited on disk).

### Previously Completed (v10.x)

- **Assembly 6502 executor** (`languages/asm6502.py`) — 2-pass assembler, 64 KB virtual machine, 40+ opcodes, all addressing modes
- **Code folding** (`ui/editor.py`) — ▼/▶ gutter glyphs, `toggle_fold()`, hides/shows QTextBlock ranges
- **Split editor** (`ui/main_window.py`) — `QSplitter`-based dual-pane editing (View → Split Editor)

### Bug Fixes

- **Ruby executor** (`languages/ruby.py`) — full initial implementation with extensive bug fixes:
  - `_rfind_op` now guards against treating `<<` (append operator) as two separate `<` operators — `r << x` no longer mis-splits into `r <` and `x`
  - `_split_statements` pre-expands semicolon-separated statements so `stmt1; stmt2` inside `{...}` blocks is correctly treated as two statements
  - `break if COND` / `next if COND` / `return if COND` modifier-only forms now handled correctly (no left-hand value before the keyword)
  - Negative array subscript access (`seq[-1]`, `seq[-2]`) now works via `_eval_chain`
  - Turtle API bindings corrected: `penup()`, `pendown()`, `setheading()`, `pencolor()`, `setpenwidth()`
  - Multi-line `do...end` blocks no longer split at binary operators (`\n` guard in `_eval`)
  - Multi-line array/hash literals now grouped correctly via `paren_depth` tracking in `_split_statements`
  - Assignment regex uses `re.DOTALL` so multi-line values are captured
  - String interpolation `#{}` correctly handles nested braces via brace-depth tracking in `_eval_dstring`
  - Block auto-splatting: `{|word, count| ...}` blocks automatically destructure single-array arguments
  - `||=` and `&&=` compound assignment operators now supported
  - All 5 Ruby demo programs pass; 217/217 coverage tests pass

- **Python executor** (`languages/python_lang.py`) — fixed all 4 Python demo programs

- **HyperTalk executor** (`languages/hypertalk.py`):
  - Fixed `BREAK`/`CONTINUE`/`RETURN` signals not propagating out of `if...end if` blocks — `exit repeat` inside an `if` block no longer causes the enclosing `repeat while` loop to run all 100,000 iterations
  - Fixed compound arithmetic expressions with embedded function calls (e.g. `put random(25) + 15 into var`) — the Python-eval fallback now pre-substitutes function calls before evaluation, so `random(n) + k` evaluates correctly
  - Fixed `_call_fn` RETURN value handling: `_exec_lines` unwraps `("RETURN", val)` tuples before returning, so call sites now check `if result is not None` instead of the tuple wrapper. User-defined functions now correctly return their computed values.
  - Fixed greedy function-call regex: replaced `^(\w+)\s*\((.*)\)$` with a balanced-parenthesis scan so expressions like `sin(r) / cos(r)` are no longer misidentified as a single function call.
  - Wrapped `float(args[0])` in `try/except` to prevent `ValueError` when a built-in receives a non-numeric first argument.
- **Erlang executor** — fixed boolean pattern matching: atoms `true`/`false` in `case` clauses now match Python `True`/`False` from built-in functions like `is_prime/1`
- **Erlang executor** — fixed `_split_fun_clauses` infinite loop caused by semicolons inside `if...end` blocks being incorrectly treated as clause separators; keyword-depth tracking (`if/case/begin/receive...end`) now prevents this

### Demo Program Fixes (timeout → 0 timeouts)

- **`Examples/erlang/number_theory.erl`** — reduced amicable-pairs search range (2000→300) and perfect-numbers loop (1..50→1..30) to complete within the 15-second test budget
- **`Examples/forth/mathematical_wonders.forth`** — fixed `SIEVE-MARK` stack corruption (`DUP *` → `DUP DUP *`), removed erroneous `ELSE DROP` in `BUILD-SIEVE`, added `4DUP` word definition, added `FVARIABLE`/`F@`/`F!`/`F+!` support to the Forth executor, reduced Hanoi threshold to avoid deep Python recursion
- **`Examples/lua/turtle_patterns.lua`** — reduced all spiral/curve step counts 10× (3000→300, 5000→500, etc.)
- **`Examples/hypertalk/adventure_game.htalk`** — resolved via the HyperTalk executor bug fixes above

### New Language Features

- **HyperTalk** — `find [whole|partial|string|chars|word] <text> [in <container>]` command: searches a variable or the output buffer and stores the match in `it` (roadmap Q2 2026)
- **HyperTalk** — `go [to] <card|next|prev|first|last>` navigation command (logs destination; no-op in desktop mode)
- **Prolog** — `\+(Goal)` negation-as-failure now parsed correctly from source (previously only worked when called from rule bodies, not in queries)
- **Prolog** — arithmetic comparison operators (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`) now accept arbitrary arithmetic expressions on both sides, not just bare variable names
- **Prolog** — new built-ins: `format/2` (with `~w`/`~d`/`~a`/`~n` directives), `string_concat/3`, `number_string/2`, `split_string/4`, `string_length/2`, `string_lower/2`, `string_upper/2`
- **Forth executor** — `FVARIABLE`, `F@`, `F!`, `F+!` for float variable storage

### Infrastructure

- **`run.py`** — launcher now installs core deps (PySide6, Pillow) first; if the full `requirements.txt` install fails (e.g. `asyncpg` compilation issues on some platforms), a warning is shown and the desktop IDE still launches successfully
- **`Examples/CATALOG.md`** — added `javascript/functional.js` and `javascript/async_patterns.js`

## [10.1.0] - 2026-04-25

### Examples (Round 2 — 20 new showcase programs)

- **BASIC** — `space_simulation.bas`: Solar system explorer with planetary data table, Kepler's third law, light travel times, escape velocities, and turtle orbital graphics
- **Logo** — `geometric_artistry.logo`: Koch snowflake grid, Islamic 8-star pattern, rosette mandala, hexagonal tiling, and optical art spiral
- **PILOT** — `math_drill.pilot`: 20-question arithmetic flashcard drill (addition, subtraction, multiplication, division) with letter grade scoring
- **C** — `game_of_life.c`: Conway's Game of Life with glider, blinker, toad, beacon, R-pentomino, and pulsar patterns on a toroidal grid
- **Pascal** — `grade_book.pas`: Student grade book for 20 students with letter grades, GPA, statistics (mean/median/std dev), and score histogram
- **Forth** — `cellular_automata.forth`: 1D elementary cellular automata — Rule 30 (chaos), Rule 90 (Sierpiński), Rule 110 (Turing-complete), Rule 184 (traffic)
- **Prolog** — `family_genealogy.pl`: 4-generation family tree (22 members) with relationship inference — cousins, aunts/uncles, ancestors, and generational distance
- **Python** — `statistics_suite.py`: Statistical analysis with mean/median/mode/quartiles/skewness/kurtosis, z-scores, normality checks, and turtle bar chart
- **Lua** — `turtle_patterns.lua`: Mathematical curves — spirograph (hypotrochoid/epitrochoid), rose curves, Lissajous figures, Archimedean spirals, star polygons
- **JavaScript** — `mandelbrot.js`: ASCII Mandelbrot set, 4 Julia sets, Burning Ship fractal, Tricorn; zoom views (Seahorse Valley, etc.) with density gradient chars
- **Ruby** — `word_games.rb`: Anagram finder, Scrabble scorer, palindrome checker (14 phrases), word frequency bar chart, Pig Latin translator, edit distance
- **Haskell** — `morse_code.hs`: Morse encoder/decoder with binary tree for O(log n) decode, timing analysis, dot/dash ratio, letter complexity, round-trip verification
- **Scheme** — `lambda_calculus.scm`: Lambda calculus evaluator — Church booleans/numerals/pairs/lists, Y combinator, beta reduction, SKI combinators
- **Erlang** — `number_theory.erl`: Number theory library — prime sieve, factorization, GCD/LCM, Euler's totient, amicable pairs, modular arithmetic, CRT, Collatz, Catalan
- **Smalltalk** — `ascii_patterns.st`: Pascal's triangle, number spiral, diamonds (solid and numbered), checkerboard, Sierpiński triangle, multiplication table, nested boxes
- **HyperTalk** — `calculator.htalk`: Scientific calculator — arithmetic, powers/roots, factorials, trig (sin/cos/tan in degrees), logarithms (log2/log10/ln), unit conversion, statistics
- **Rust** — `game_of_life.rs`: Conway's Game of Life with `Grid` struct/impl, toroidal boundaries, 7 classic patterns, period detection, and simulation statistics
- **Perl** — `text_analytics.pl`: Text analytics — Flesch-Kincaid readability, word frequency with bar chart, bigram/trigram n-grams, sentence statistics, KWIC concordance

### Documentation

- **Examples/CATALOG.md**: Updated from 87 to 107 programs; added all 20 Round 2 entries under each language section
- **README.md**: Updated Examples project tree from 67/19-language counts to 107/20-language counts with accurate per-language file totals

## [10.0.0] - 2026-04-25

### New Languages

- **Perl 5** — scalars/arrays/hashes, regex (`=~`, `s///`, `qw()`), subroutines, turtle graphics commands (`forward`, `back`, `left`, `right`, `penup`, `pendown`, `pencolor`, `setpos`, `circle`)

### Graphics Enhancements

- **SVGA mode** — `set_svga_mode(width, height)` on `TurtleState` enables a fixed-resolution virtual canvas (default 800×600) rendered with full anti-aliasing inside a chrome bezel; `clear_svga_mode()` returns to unlimited coordinates
- **Cubic Bezier curves** — `bezier_curve(cp1x, cp1y, cp2x, cp2y, end_x, end_y)` draws a smooth vector path from the current turtle position
- **Gradient fills** — `draw_rect_gradient()` and `draw_ellipse_gradient()` fill shapes with linear or radial colour gradients using stop-based colour maps
- **Sprite system** — `define_sprite(name, pixel_rows, palette)` defines named pixel-art sprites from string rows; `place_sprite(name, x, y, scale, rotation, z_order)` stamps them; `move_sprite(name, dx, dy)` translates the most recent instance
- **Advanced pen styles** — `set_pen_style(dash, cap, join)` controls dash patterns, line caps (`round`/`flat`/`square`), and joins (`round`/`miter`/`bevel`) for all subsequent shapes
- **Z-ordering** — shapes are now sorted by `z_order` before rendering; higher numbers paint on top
- Canvas: added `QLinearGradient`/`QRadialGradient`/`QPainterPath`/`QPolygonF` rendering paths; gradient-fill support extended to polygons and ellipses; sprite shape type rendered with per-pixel alpha, scale, and rotation transforms

### Sound / Music Enhancements

- **ADSR envelopes** — new `ADSREnvelope` dataclass with `attack_ms`, `decay_ms`, `sustain_level`, `release_ms`; automatically shapes amplitude for each note
- **New waveforms** — `pulse` (configurable duty cycle, `@Pn` MML command) and `noise` (white noise, `@W5`)
- **4-channel polyphony** — new `MusicChannel` dataclass; `MusicPlayer.mix_channels()` sums up to 4 independent synthesised streams into a single WAV; `parse_multichannel(dict)` convenience wrapper
- **Extended MML syntax** — `@Wn` (waveform 0–5), `@Datk,dcy,sus,rel` (ADSR), `@Pn` (pulse duty %), `@Cn` (channel 0–3), `[CEG]` chord notation for simultaneous notes
- 4 new built-in sound effects: `DRONE`, `BUZZ`, `STATIC`, `BEEP2`

### Language Executor Improvements

- **APL**: outer product operator (`∘.fn` e.g. `A ∘.× B`), format `⍕`, execute `⍎`
- **C**: `struct`/`union` type definitions, struct variable declarations, member access (`p.x`), `typedef` for scalar types

### UI Improvements

- **Canvas**: Added "Export SVG…" (🖼) toolbar button alongside existing "Save PNG…"
- SVG export via `PySide6.QtSvg.QSvgGenerator` with antialiasing and full turtle graphics rendering

### Examples & Documentation

- Added Ruby examples: `Examples/ruby/hello.rb`, `Examples/ruby/oop_demo.rb`
- Added Erlang examples: `Examples/erlang/hello.erl`, `Examples/erlang/pattern_match.erl`
- Added Rust examples: `Examples/rust/hello.rs`, `Examples/rust/structs.rs`
- Added tutorial docs: `docs/tutorials/ruby.md`, `docs/tutorials/erlang.md`, `docs/tutorials/rust.md`

### UI & Learning Experience

- Added a new Learning Hub with guided challenge launchers, remix actions, quick access to reference material, and one-click tutor help
- Added a Project Explorer with recent-project tracking, persistent workspace roots, and direct file opening from the IDE
- Added File menu and toolbar actions for opening project folders and revisiting recent projects
- Improved the AI Assistant with beginner-friendly tutor summaries for the current code context
- Updated welcome/help surfaces to highlight the new learning and project workflows

## [9.0.0] - 2026-03-28

### Security

- **Dockerfile**: Removed hardcoded `DATABASE_URL` and `REDIS_URL` credentials; environment variables now injected at runtime
- **Dockerfile.backend**: Added non-root `appuser` (UID 1000) with `USER` directive
- **Docker build stages**: Fixed lowercase `as` to uppercase `AS` per Dockerfile best practices
- **Docker Compose**: Removed deprecated `version` fields from `docker-compose.yml` and `docker/docker-compose.backend.yml`

### Code Quality

- **Interpreter**: Fixed 25 type annotation errors in `core/interpreter.py`
  - `__init__` signature now uses `Optional[Language] = None`
  - All instance attributes declared before `_init_state()` call
  - Added null guards for optional attributes
  - `execute()` accepts `TurtleState | None` with lazy creation
  - Removed unused `datetime` import
- **Version label**: Updated Dockerfile `VERSION` label from 8.0.0 to 9.0.0

### Documentation

- **ARCHITECTURE.md**: Rewrote Module Overview with all subpackages (core, languages, UI, graphics, features, utils, tests, tools)
- **README.md**: Fixed theme count (25→28), rewrote project structure tree to match actual layout
- **docs/INDEX.md**: Added missing Debugger Guide link in Feature Guides section
- **CHANGELOG.md**: Expanded 9.0.0 entry with detailed change descriptions
- **ROADMAP.md**: Rewrote with concrete milestones replacing generic filler content

### Infrastructure

- Added `*.log` pattern to `.gitignore`
- Removed stray `interpreter.log` files from repository
- Updated GitHub repository description and topics via API

## [8.1.0] - 2025-06-15

### Security

- **Pascal executor**: Replaced unsafe `eval()` with `ExpressionEvaluator` for safe math expression evaluation
- **JavaScript executor**: Fixed 8 bare `except:` clauses with specific exception types (`ValueError`, `TypeError`, `KeyError`, etc.)
- **Lua executor**: Fixed bare `except:` clause with specific exception types

### Interpreter Enhancements

- **HyperTalk**: Added `add/subtract/multiply/divide` math commands, `do`, `wait`, `visual effect`, `sort`, `beep`, `exit repeat`, `next repeat`, chunk expression ranges (`char i to j of`), built-in properties (`the date`, `the time`, `the ticks`, `the random`), new math functions (`average`, `sum`, `ln`, `log2`, `exp`, `atan`, `annuity`, `compound`)
- **PILOT**: Added `?` single-character wildcard and substring matching (SuperPILOT), match variables (`$LEFT`, `$MATCH`, `$RIGHT`), string functions (`LEN`, `UPPER`, `LOWER`, `TRIM`, `LEFT`, `RIGHT`, `MID`, `CONCAT`, `REPLACE`, `REVERSE`), enhanced graphics commands (`SETHEADING`, `HIDETURTLE`, `SHOWTURTLE`, `ARC`, `FILL`, `DOT`, `STAMP`, `TEXT`, `SPEED`), `H:` hint and `N:` no-match commands, tab support in `T:`
- **Lua**: Replaced stub coroutine implementation with threading-based yield/resume cycling using `threading.Event` synchronization with 5-second timeout
- **JavaScript**: Added `Symbol.hasInstance` and `Symbol.toPrimitive` support, fixed Symbol transpiler stripping

### UI Improvements

- **Find & Replace**: Added case sensitivity, regex, and whole word matching checkboxes; live match highlighting with match count display; regex-aware find and replace
- **Auto-Completion**: Enhanced to extract identifiers from the current document in addition to language keywords
- **Themes**: Added **Catppuccin Mocha** and **Gruvbox Dark** themes (28 themes total)

### Code Quality

- Removed 5 leftover debug print comments from BASIC and Logo executors
- All bare `except:` clauses replaced with specific exception types

### Demo Programs (new examples)

- **BASIC**: Maze generator with recursive backtracking
- **Logo**: Spirograph / hypotrochoid mathematical art
- **PILOT**: Typing speed tutor
- **C**: 3×3 matrix calculator
- **Pascal**: Number guessing game
- **Prolog**: Logic puzzle solver with unification
- **Forth**: Geometric turtle art
- **Brainfuck**: Fibonacci sequence generator
- **JavaScript**: Pub/Sub event system
- **HyperTalk**: Interactive quiz builder
- **Lua**: Traffic light state machine

### Documentation

- Updated README with v8.1.0 features
- Created CHANGELOG.md

---

## [8.0.0] - 2025-06-01

### Initial Release

- 12 programming language executors: BASIC, PILOT, Logo, COMAL, REXX, and Scheme, JavaScript, Lua, HyperTalk, Erlang
- PySide6 (Qt6) desktop IDE with editor, canvas, and output panels
- 26 built-in themes including retro CRT and accessibility options
- Integrated debugger with timeline recording and rewind
- Turtle graphics with 50+ drawing commands
- Lesson system with auto-verification
- AI assistant and error explainer panels
- Initial example programs across all 12 languages
- Comprehensive automated test suite
