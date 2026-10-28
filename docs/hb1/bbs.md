# WARP BOARD — the machine's telephone line

The HB-1 has a modem. `DIAL` at the prompt picks up the line, the machine prints the
noise a 300-baud modem made, and a caller arrives at **WARP BOARD**, a bulletin board
run by the sysop.

```text
READY.
DIAL
ATDT 555-0117
... DIALING ...
CONNECT 300
WELCOME TO WARP BOARD
RUN BY THE SYSOP  ·  1 MESSAGE(S)
ENTER A HANDLE, OR BYE TO HANG UP:
BBS> ada
HELLO, ADA.
TYPE H FOR HELP, B FOR THE BULLETINS, BYE TO HANG UP.
```

The board is **on the machine**, not on a network: a JSON file at
`~/.time_warp/bbs/board.json`, written the first time anybody dials in with a
welcome message from the sysop. A classroom can therefore demonstrate what a
bulletin board *was* -- a place other people had been -- without a single packet
leaving the room, and a learner who posts something can find it still there next
week.

## The words

Every letter works in either case.

| Word | What it does |
| --- | --- |
| `DIAL` (`BBS`) | Connect. The handle is asked for the first time and remembered after that |
| `H` or `?` | The menu, in the board's own words |
| `B` (`L`, `BULLETINS`) | List the messages: number, caller, subject, date |
| `R N` (`READ N`) | Read message `N` |
| `N` | How many messages there are |
| `P [SUBJECT]` (`POST`) | Start a message. Type the body, then a line with a single `.` |
| `WHO` | Who is online; on a one-line machine the answer is `1 CALLER:` and your handle |
| `BYE` (`G`, `Q`, `HANGUP`) | Hang up |

**Posting is two steps** because the prompt holds one line: `P A QUESTION` names the
subject, the lines after it are the message, and a line with one full stop ends it.
A blank line inside a message is a paragraph, not the end. An empty message is
refused -- `?AN EMPTY MESSAGE IS NOT A MESSAGE` -- and the board says so before
anything is written.

The handle is remembered in the machine's settings (`retroComputer/bbsHandle`), so a
second call opens with `WELCOME BACK, ADA.` and the menu. `NEW` clears the program,
not the handle; `INIT` clears the disk, not the board.

## Where it is kept

| Path | What it is |
| --- | --- |
| `~/.time_warp/bbs/board.json` | The board: name, sysop and the messages, written through `QSaveFile` |
| `TIME_WARP_DATA_DIR` | Moves the whole directory, as it does for the disk and the lessons |

A board that cannot be read is refused with `?NO CARRIER` and left exactly as it is:
overwriting a corrupt file would lose whatever somebody had written in it. A message
that cannot be written is refused at the moment of posting, not silently kept in
memory for a next caller who will never see it.

The file is plain JSON, so a teacher can seed a class's board by hand, back it up,
or open it in an editor:

```json
{
  "system": "WARP BOARD",
  "sysop": "THE SYSOP",
  "messages": [
    { "number": 1, "author": "THE SYSOP", "subject": "WELCOME TO THE MACHINE",
      "body": "hello, and welcome...", "posted": "2026-10-27 20:04" }
  ]
}
```

## What it will not do

- **No real telephone line.** There is no modem emulation, no dialling out and no
  serial port: `DIAL` connects to the board on this machine and nothing else. A
  machine that could reach a real network would be a machine this project would have
  to secure, and a classroom is not the place to learn that the hard way.
- **No private mail, no files, no doors.** One message area, read and posted by
  whoever is at the keyboard. The period had all three; this is the part that teaches
  what the others were for.
- **No real names.** A handle is what you typed, remembered so the board can greet
  you. There is no account, no password and no way to be anybody but the last caller.
