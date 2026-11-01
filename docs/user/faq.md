# Questions

Short answers, with a pointer to the page that has the long one. If a question here needs a
procedure, the manual has it: [the Studio](the-studio.md) for the window,
[languages](../languages/README.md) for what each interpreter does,
[automation](automation.md) for the API.

---

## The product

**What is it?** A desktop programming environment for ten interpreters of 1964–1980, with
one editor, one debugger in every language and a live turtle canvas. It is a C++20 and Qt 6
application for Linux.

**Which languages?** BASIC, APL, Logo, PILOT, FOCAL, Forth, Scheme, COMAL, REXX and
Smalltalk. Each is an implementation of its own dialect, not a common core with different
keywords — the [reference](../languages/reference.md) lists every command.

**Why these ten?** They span the period when a programming language was designed around a
machine and a classroom: a beginner's language (BASIC, COMAL), a notation (APL), a
teaching tool (Logo, PILOT, FOCAL), a forerunner of structure (Forth, Scheme) and the first
object system (Smalltalk).

**Who is it for?** Someone who wants to write a program the way it was written then, and to
see it draw. The checked curriculum is there for teaching, but nothing in the application
assumes it will be used that way.

**Is it free?** MIT licence. The assistant is off until you configure it, and any cost is
whatever the service you choose charges — a model on your own machine charges nothing.

**Does it need a network?** No. The one network call in the application is the assistant,
and it only happens if you configure it and press a button.

---

## Running it

**What do I need?** A Linux distribution, and to build it: a C++20 compiler, CMake 3.20,
Qt 6.2 or newer, SQLite and zlib. Or take a release package, which needs none of them.
[Getting started](getting-started.md).

**Does it run on Windows or macOS?** No. It is built, tested and shipped for Linux only, and
there is no installer or supported path for any other system.

**How do I start it?** `./run.sh` from a checkout, or **Time Warp Studio** from the desktop
menu once installed.

**It crashes at startup with "Illegal instruction".** That is Qt compiled for a newer CPU
than the machine running it, not a fault in a program you wrote. Use a Qt built for that
machine, or a machine that is not a very old VM.

**Which extensions are recognised?**

| Language | Extensions |
| --- | --- |
| BASIC | `.bas`, `.bsc` |
| APL | `.apl`, `.dyalog` |
| Logo | `.logo`, `.lg` |
| PILOT | `.pilot`, `.plt` |
| FOCAL | `.foc`, `.focal` |
| Scheme | `.scm`, `.ss`, `.scheme` |
| COMAL | `.comal`, `.cml` |
| REXX | `.rexx`, `.rex` |
| Smalltalk | `.st`, `.smalltalk` |
| Forth | `.fs`, `.fth` |

An unknown extension opens as BASIC. The language picker in the toolbar changes it without
renaming the file.

---

## Using it

**How do I run a program?** F9. A selection runs with `Ctrl+F9`, and `Shift+F9` stops.

**Why does my program run forever?** It should not: every engine stops after two million
statements or ten seconds, whichever comes first, and the console says which limit ended
it. A run that ends that way is reported as stopped, not finished.

**How do I try a command without writing a program?** Type it in the REPL bar under the
editor. What survives between entries differs by language, and the
[manual](the-studio.md#the-repl-bar) has the table; BASIC also behaves as a direct-mode
interpreter there, storing numbered lines in the editor.

**Can I split the editor?** `Ctrl+\` opens a second pane, and `Ctrl+Alt+B` runs both and
reports them in one table.

**Is there a terminal?** Yes, off by default: **Settings > Workspace > Enable the embedded
terminal**. It is a real shell on a pseudo-terminal, and a text transcript rather than a
screen, so a full-screen curses program has its escape sequences stripped.

**Does the Studio send my code anywhere?** Only to the assistant, only when you ask it
something, and only what the [assistant page](assistant.md#what-leaves-the-machine) lists.

**What happens to unsaved changes when the window closes?** You are asked about every
document with unsaved changes, in both panes, before anything closes. Autosave also keeps
snapshots under `~/.time_warp`.

**Where are my settings and progress?** Qt stores preferences and window state; lesson
progress is `~/.time_warp/progress.json`, and a class's roster and handouts are under
`~/.time_warp/classroom`. `TIME_WARP_DATA_DIR` moves all of it, which is how the tests avoid
touching a real learner's files.

---

## Drawing

**Where does turtle output appear?** On the canvas beside the console. A program that prints
and draws keeps both in view.

**Why is the canvas blank after a turtle program?** Either the program drew nothing — a
turtle command that never ran, or a language whose turtle words you mistyped — or it named
a screen mode and drew outside it. The Turtle Log lists every drawing command the canvas
received, which answers the first question; the [reference](../languages/reference.md) says
which words each language has.

**How do I export a drawing?** `Ctrl+E`, as PNG, JPEG or BMP, or as SVG when the build has
Qt's Svg module.

**Which languages draw?** Logo, BASIC, COMAL and APL have turtle words, and Scheme and
Forth reach the same canvas through their own words. The reference lists them per language.

---

## Debugging

**Does it have a debugger?** Yes, in all ten. Breakpoints in the gutter, continue, pause,
step in, over and out, watch expressions, data breakpoints, a Variables panel and a
telemetry HUD. The [manual](the-studio.md#debugging) covers it.

**Can I go back in time?** A run can be walked backwards if you set **History** before it
starts: off, 100, 500 or 2000 statements. Scheme cannot — its state is an arena and a
continuation chain rather than a value — and Smalltalk has no checkpoint yet.

**Can I profile a program?** **View > Profile Run** times every statement of a line-by-line
program and reports line, hits, time and share, longest first.

---

## Lessons and teaching

**Where are the lessons?** In the Lessons panel, or `time_warp_learn` at a terminal. Twenty
lessons and sixty checkpoints, two per language, each checked by running the learner's
program rather than comparing it with an answer.

**Do lessons save my progress?** Yes, in `~/.time_warp/progress.json`. **Hand In** writes a
program into the class folder, which is where marking reads it.

**Is there a class list?** Yes: the Classroom panel, or `time_warp_teach roster add "Name"`.
[The curriculum](../lessons/README.md) covers rosters, handouts, marking and bundles.

---

## Appearance

**How many themes?** 35, including the retro CRT sets, plus a high-contrast variant derived
from whichever is active. `Shift+F10` cycles them.

**Can I change the font size?** Yes: the toolbar's text-size button steps the editor, the
console and the panels through 100%, 125% and 150% together, and `Ctrl+,` sets the editor
font itself.

---

## The code

**Is there an API?** A JSON-RPC service on stdin and stdout, and the same engines as C++
libraries. [Automation](automation.md).

**How do I turn a program into an executable?** `./twc program.bas -o program` links the
engine into one small binary that needs no interpreter. `--compile` instead produces the
player with the program embedded, which opens with a canvas.

**How do I contribute?** [CONTRIBUTING.md](../../CONTRIBUTING.md): setup, the gates, and the
recipe for adding a language, a command or a lesson.

**Where do I report a security problem?** [SECURITY.md](../../SECURITY.md), privately.

**What is not built yet?** An automatic update check, Snap packaging, an arm64 AppImage, a
fully static build and automatic publishing to a distribution repository.
[The roadmap](../maintainer/roadmap.md) records them, and `native/share/unbuilt.txt` is the
list the documentation gates check against.

**Is there a plugin system?** No. The automation API is the supported way to drive the
Studio from outside, and the C++ libraries are the supported way to embed its engines.
