---
name: Language runtime bug
about: An interpreter answered wrongly — a parse error, a wrong value, or a run that would not end
title: "[Runtime] "
labels: bug
---

# Language runtime bug

**The interpreter**

One of the ten, named as the language menu names it: BASIC, APL, Logo, PILOT, FOCAL,
Forth, Scheme, COMAL, REXX or Smalltalk.

**Where it happened**

- [ ] In the editor, running a file
- [ ] In the REPL bar
- [ ] In `twc`, the standalone compiler
- [ ] In the automation API, `./api`

**The smallest program that reproduces it**

The lines that fail and nothing else. A program of ten lines that fails is worth more
than a page that describes a page.

```text

```

**What you expected**

<!-- 42 -->

**What actually happened**

<!-- 41, or `❌ Unknown word`, or a prompt that never returns -->

**Is it the parser, the evaluator or the output?**

Whatever you can tell: a statement rejected before it ran, a value that came out wrong,
a loop that did not stop, a drawing that came out thin.

**How it ran**

Version, install method and operating system:

<!-- 30.0.0, AppImage, Ubuntu 24.04 -->

**Messages**

The **Errors** tab, or what the terminal printed.

```text

```
