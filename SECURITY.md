# Security policy

## Supported versions

| Version | Supported |
| --- | --- |
| 28.x | Yes |
| 27.x and earlier | No |

The application is the Qt build in `native/`, and every language it runs is a C++ executor
in that build. The Python reference engine the ports were judged against was deleted at
18.0; its answers are frozen in `native/tests/golden/lang_corpus.json.gz`, which is data
read by a command and executed by nothing.

## Reporting a vulnerability

Do **not** open a public issue for a security problem.

1. **GitHub private reporting** (preferred):
   [report a vulnerability](https://github.com/James-HoneyBadger/Time_Warp_Studio/security/advisories/new).
2. **Email** <james@honey-badger.org> with the subject `[SECURITY] Time Warp Studio`.

Include what the vulnerability is and what it affects, how to reproduce it, the version or
commit, and any mitigation you know of.

| Stage | Target |
| --- | --- |
| Acknowledgement | 48 hours |
| Initial assessment | 5 business days |
| Fix or mitigation | 30 days, depending on severity |

You will be told the report arrived, kept informed while it is worked on, credited in the
release notes unless you prefer otherwise, and the fix ships as a patch release.

---

## What the surface is

Time Warp Studio is an offline desktop application. It ships no server component, binds no
port and listens on nothing. Three things in it reach outside the process, and all three are
deliberate:

| Reaches | What, and when |
| --- | --- |
| **The network** | The assistant, and only it: one HTTPS request to the endpoint *you* configured, sent when you press a button. Off until configured. [The assistant page](docs/user/assistant.md) lists every field that leaves the machine |
| **A shell** | The embedded terminal, and only if you enable it in Settings. It runs your own login shell on a pseudo-terminal, with your privileges, in the current document's folder |
| **An audio device** | `SOUND` and `PLAY` write a WAV and hand it to `pw-play`, `paplay`, `aplay`, `ffplay` or `play` |

Nothing else opens a socket, starts a process or reads a device.

### A program is not a sandbox

A program runs in the application's process, with the user's privileges. It is not a
security boundary and is not presented as one. What it can do, and the fence around each:

- **Files.** BASIC's file channel (`OPEN`, `GET #`, `PUT #`) reads and writes what the user
  can, but only inside **one directory**: the process's working directory, which is the
  document's folder when the Studio starts on a file and the launch directory otherwise. A
  name is resolved against it, `..` is followed and then checked, and a path that lands
  outside — including a sibling whose name merely starts the same way — is refused with
  `Path '…' escapes sandbox directory` (`resolve_confined_path` in
  `native/src/lang/basic_file_statements.cpp`, asserted by `lang_tests`). That is a fence
  around one directory, not a privilege boundary: a program can still overwrite anything
  inside it, and a program that can write files can still lose you one. Run programs you
  trust.
- **Device nodes, through an allow-list.** `SERIAL` and `PARALLEL`
  (`native/src/io/ports.cpp`) open raw POSIX devices only from a family list —
  `/dev/ttyUSB*`, `/dev/ttyACM*`, `/dev/ttyAMA*`, `/dev/serial/by-id/*`, `/dev/lp*`,
  `/dev/parport*` and the pseudo-terminals — plus any exact path added with
  `TIME_WARP_PORTS` (`:`-separated). Everything else is refused by the engine with a
  sentence the program prints: `/dev/mem`, a disk, `/etc/passwd`, a relative path and a
  `..` path are all refused, and `media_tests` asserts it.
- **SQL.** `SQL` statements reach the SQLite connection the Database panel has open, which
  is a file the user chose.
- **The clock.** `TIME$`, `DATE$` and `TIMER` read the local clock.
- **Its own lifetime.** A runaway program is stopped by the run budget rather than by the
  operating system: 2,000,000 statements or 10 seconds of wall clock, whichever comes
  first, in every engine (`native/include/timewarp/lang/interpreter.hpp`). It ends with a
  sentence in the console rather than a hung window, and **Stop** ends it sooner.

What a program cannot do: open a network connection, start a shell, read memory or a block
device, or leave the directory it was given.

### The application's own surface

- **The automation API** (`./api`, `native/src/api/`) speaks JSON-RPC 2.0 over stdin and
  stdout. It is not a server: it binds nothing, listens on nothing, and a caller has to
  start the process itself. A caller that can write to its stdin can already run programs on
  the machine, which is why there is no authentication layer pretending otherwise.
- **A standalone artifact** is the player binary with the program appended. It contains the
  program's source in plain bytes — `strings hello` finds it — because the program is
  interpreted and nothing is hidden. Send one only to somebody who may see its source.
- **The parity corpus** (`native/tests/golden/lang_corpus.json.gz`) is data: program text
  and the reference's answers, read by `make golden-check` and executed by the native
  engines only. It is development tooling and is not installed.
- **The AI key** is a secret. It can come from `TIME_WARP_AI_API_KEY` and never be stored,
  it goes to the system keyring where the build found one, and it is stripped from every
  message and every error the client produces by `redact()`.

### Dependencies

C++20 with Qt 6 (Widgets, Core, Gui, Network for the assistant), SQLite, zlib for classroom
bundles, and optionally Qt Svg and Qt PrintSupport for drawing export and printing. Linux
only. There is no interpreter at run time and no third-party runtime beyond Qt and SQLite;
keep them updated. `security.yml` scans the tree with Trivy on every push and weekly, and
uploads the result as SARIF, so a finding appears in the repository's Security tab rather
than in a log nobody reads.

### Secrets stay out of the repository

A `detect-secrets` pre-commit hook checks staged files against a committed baseline
(`.secrets.baseline`), so a credential pasted into a file fails the commit instead of being
pushed. If a secret does reach a commit, treat it as compromised: rotate it, then rewrite
the history that contains it — removing the file in a later commit does not remove it from
the repository.

---

## For users

- Run the Studio in an ordinary user account. Nothing here needs elevated privileges.
- Keep Qt, SQLite and the rest of the system updated.
- Load programs from sources you trust, and read a program before running it — the editor
  is right there.
- Add a device path with `TIME_WARP_PORTS` only if you know what is plugged into it.
- The assistant is off until you configure it. A model on your own machine
  (`localhost`) sends nothing anywhere, which is the configuration for a classroom.

## Acknowledgements

Contributors who responsibly disclose a vulnerability are acknowledged here, and in the
release notes for the fix, unless they prefer to remain anonymous.
