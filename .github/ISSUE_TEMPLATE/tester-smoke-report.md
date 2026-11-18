---
name: Tester smoke report
about: A fast compatibility report — where it installed, what ran, and what looked wrong
title: "[Smoke] "
labels: question
---

# Tester smoke report

Two minutes of ticking boxes and a few lines of prose. This is the report that turns
into a bug when enough of them agree.

**Where it ran**

- [ ] Arch
- [ ] Fedora
- [ ] Ubuntu
- [ ] Debian
- [ ] Another Linux (name it below)
- [ ] Windows — not a shipped target; say here if you ran it in WSL
- [ ] macOS — not a shipped target; say here if you ran it in a container
- [ ] In a browser, the WebAssembly build

Distribution and version, desktop and display server:

<!-- Ubuntu 24.04, GNOME on Wayland -->

**How it was installed**

- [ ] AppImage, run from where it was downloaded
- [ ] Flatpak bundle
- [ ] `.deb` or `.rpm` package
- [ ] Tarball unpacked into `/`
- [ ] Built with CMake from a checkout

**Which languages ran a program**

Tick the ones where a program actually produced output or a drawing, not the ones that
merely opened in the editor:

- [ ] BASIC
- [ ] APL
- [ ] Logo
- [ ] PILOT
- [ ] FOCAL
- [ ] Forth
- [ ] Scheme
- [ ] COMAL
- [ ] REXX
- [ ] Smalltalk

**Edge-case observations**

The interesting part. A statement that printed an unexpected value, a turtle that drew a
short line, a key that did nothing, a prompt that swallowed its input, a breakpoint that
stopped one line late. Say which language and what you typed.

```text

```

**The debugger, if it came up**

- [ ] A breakpoint stopped the run where it was set
- [ ] Continue resumed to the next one
- [ ] The Debug panel listed the variables the program had
- [ ] The REPL bar evaluated a statement in the same language

**Anything else**

The **Errors** tab, the terminal output, or a screenshot.
