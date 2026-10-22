# Security Policy

## Supported Versions

| Version | Supported           |
| ------- | ------------------- |
| 16.x    | :white_check_mark:  |
| < 16.0  | :x:                 |

The application is the Qt build in `native/`. The Python engine in
`native/bridge/` is a development reference used by the test gates and (until
17.0) the classroom page; it is not part of the shipped program, and nothing a
learner runs executes in it.

## Reporting a Vulnerability

If you discover a security vulnerability in Time Warp Studio, please report it responsibly.

**Do NOT open a public GitHub issue for security vulnerabilities.**

### How to Report

1. **GitHub Security Advisories (Preferred):**  
   Use [GitHub's private vulnerability reporting](https://github.com/James-HoneyBadger/Time_Warp_Studio/security/advisories/new) to submit a confidential report directly through the repository.

2. **Email:**  
   Send details to **<james@honey-badger.org>** with the subject line `[SECURITY] Time Warp Studio`.

### What to Include

- A description of the vulnerability and its potential impact
- Steps to reproduce the issue
- Affected version(s)
- Any suggested fixes or mitigations (if available)

### Response Timeline

- **Acknowledgement:** Within 48 hours of receipt
- **Initial assessment:** Within 5 business days
- **Fix or mitigation:** Targeted within 30 days, depending on severity

### What to Expect

- You will receive a confirmation that your report was received.
- We will work with you to understand and validate the issue.
- A fix will be developed and released as a patch version.
- You will be credited in the release notes (unless you prefer to remain anonymous).

## Threat model

Time Warp Studio is an **offline desktop application**. It opens no sockets, ships
no server component, and the only process it starts on the user's behalf is an
audio player. The surface that matters is a program: a `.bas` file is code, and
this document states exactly what code may do.

### A program is not a sandbox

A program runs in the application's process, with the user's privileges. It is not
a security boundary and is not presented as one. What it *can* do:

- **Files.** BASIC's file channel (`OPEN`, `GET #`, `PUT #`) reads and writes what
  the user can read and write, but only inside **one directory**: the process's
  working directory, which is the document's folder when the IDE starts on a file
  and the launch directory otherwise. A name is resolved against it, `..` is
  followed and then checked, and a path that lands outside — including a sibling
  whose name merely starts the same way — is refused with
  `Path '…' escapes sandbox directory` (`resolve_confined_path`,
  `native/src/lang/basic_features.cpp`, asserted by `lang_tests`). That is a fence
  around one directory, not a privilege boundary: a program can still overwrite
  anything inside it, and a program that can write files can still lose you one.
  Run programs you trust.
- **Device nodes, through an allow-list.** `SERIAL` and `PARALLEL`
  (`native/src/io/ports.cpp`) open raw POSIX devices only from a family list —
  `/dev/ttyUSB*`, `/dev/ttyACM*`, `/dev/ttyAMA*`, `/dev/serial/by-id/*`, `/dev/lp*`,
  `/dev/parport*`, and the pseudo-terminals — plus any exact path a teacher adds
  with `TIME_WARP_PORTS` (`:`-separated). Everything else is refused by the engine
  with a sentence the program prints: `/dev/mem`, a disk, `/etc/passwd`, a
  relative path and a `..` path are all refused, and `media_tests` asserts it.
- **SQL.** `SQL` statements reach the SQLite connection the Database tab has open,
  which is a file the user chose.
- **The clock.** `TIME$`, `DATE$` and `TIMER` read the local clock.
- **The filesystem's size.** A program that loops forever is stopped by the run
  budget, not by the operating system: 10 seconds of wall clock, 2,000,000
  statements and 100,000 program steps
  (`native/include/timewarp/lang/interpreter.hpp`). It ends with a sentence
  (`❌ Error: Execution timeout`) rather than a hung window, and the user can still
  press Stop.

What it cannot do: open a network connection, start a shell, read memory or a
block device, or escape the classroom and progress directories it is given.

### The application's own surface

- **The automation API** (`./api`, `native/src/api/`) speaks JSON-RPC 2.0 over
  stdin/stdout. It binds nothing and listens on nothing; a caller has to start the
  process itself.
- **A standalone artifact** is the IDE's binary with the program appended. It
  contains the source in plain bytes — `strings hello` finds it — because the
  program is interpreted and nothing is hidden. Send one only to somebody who may
  see its source.
- **The reference engine** in `native/bridge/` is development tooling. It is not
  installed and no learner program is routed to it.

### Dependencies

The application is C++20 with Qt 6 (Widgets, Core, Gui), SQLite, zlib (classroom
bundles), and optionally Qt Svg and Qt PrintSupport for drawing export and
printing. The Windows 7 artifact
uses Qt 5.15, which is the last Qt that runs on 7 SP1. There is no Python at run
time and no third-party runtime beyond Qt and SQLite. Keep them updated.

### Secrets stay out of the repository

A `detect-secrets` pre-commit hook checks staged files against a committed baseline
(`.secrets.baseline`), so a credential pasted into a file fails the commit instead
of being pushed. If a secret does reach a commit, treat it as compromised: rotate
it, then rewrite the history that contains it — removing the file in a later commit
does not remove it from the repository.

## Best Practices for Users

- Run Time Warp Studio in a standard user account (no elevated privileges required).
- Keep Qt, SQLite and the rest of the system updated.
- Only load programs from sources you trust, and read a program before you run it —
  the editor is right there.
- Add a device path with `TIME_WARP_PORTS` only if you know what is plugged into
  it.

## Acknowledgements

We appreciate the security research community's efforts in helping keep Time Warp Studio safe. Contributors who responsibly disclose vulnerabilities will be acknowledged here.

---

*This policy follows [GitHub's recommended security policy format](https://docs.github.com/en/code-security/getting-started/adding-a-security-policy-to-your-repository).*
