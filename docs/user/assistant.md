# The assistant

The Assistant page asks a language model about the program in front of you. It is a page
in the right column rather than a window over the editor, because the questions worth
asking are about the code you are looking at.

It is off until you configure it, nothing is sent until you press a button, and it is the
one place in the application that opens a network socket.

---

## The controls

Until it is configured the page shows a single button, **Configure the assistant…**, which
opens the same dialog as **Tools > AI Settings…**.

| Control | What it does |
| --- | --- |
| The question box | Ask anything. `Ctrl+Enter` sends, Enter alone is a new line |
| **Explain** | What the program does, in the order it runs, for someone learning the language |
| **Fix** | Find the faults and return the corrected program, with the last run's errors in view |
| **Comment** | The program back with comments added and no behaviour changed |
| **Translate** | Type the target language in the box first, then press it |
| **Write** | Type what the program should do in the box first, then press it |
| **Send**, **Stop** | Ask the question, or abandon the request in flight |
| **Clear** | Forget the conversation so far |
| **Insert**, **Copy** | Put the answer's code into the editor at the cursor, or the whole answer on the clipboard |
| **Settings…** | The endpoint, the model, the key and the limits |

While a request is in flight the question buttons are disabled, so two questions cannot be
half-asked at once, and **Stop** is the only enabled control. It abandons the request
rather than letting an answer arrive into a conversation that has moved on.

**Send the program** is ticked by default: the questions above are worth little without
the code. Untick it to ask a general question with nothing on screen going out. A
selection wins over the whole document, so selecting six lines and pressing **Explain** is
a question about six lines.

The panel keeps the last six exchanges, so "and the same in Forth?" works. **Clear**
forgets them.

**Tools > Ask About the Last Error** starts from the run rather than the program: the
lines selected in the Console, else the last error the Errors panel holds, else the last
line the program printed. What the run said becomes the question, and the program and the
output ride along as evidence, so the answer is about the failure that happened rather
than about a program in the abstract.

Two more entries are in **Tools** — **Explain This Program** and **Fix This Program** —
with no shortcuts, because a key pressed by accident spends a request.

### The suggested next line

With **Suggest the next line in the editor** ticked in AI Settings, a pause in typing
(about three quarters of a second) asks the endpoint for the next few lines of the
document up to the caret and shows the first as dimmed text after the cursor. **Tab**
takes it, **Esc** refuses it, and any other key dismisses it and is typed as usual.

It is off unless you turn it on, it never competes with the local word completion, and an
answer that arrives after the caret moved is dropped rather than pasted after the wrong
line. Unlike the panel it has a client of its own, because a suggestion must not cancel an
answer you are reading. A suggestion that does not arrive shows nothing: there is no
action a reader could take about it.

It asks only when the caret is at the end of the text it is asking from, because a
suggestion for the middle of a line would have to be inserted between two halves of a
statement.

---

## Configuring it

**Tools > AI Settings…** is the one place it is set up. The **Provider** dropdown fills
the fields below it from a preset; the fields stay editable, so a service that is not on
the list is one line away.

| Preset | Shape | Endpoint | Model it fills in |
| --- | --- | --- | --- |
| **OpenAI** | OpenAI-compatible | `https://api.openai.com/v1` | `gpt-4o-mini` |
| **DeepSeek** | OpenAI-compatible | `https://api.deepseek.com/v1` | `deepseek-chat` |
| **Anthropic** | Anthropic Messages | `https://api.anthropic.com/v1` | `claude-3-5-haiku-latest` |
| **Ollama (this machine)** | OpenAI-compatible | `http://localhost:11434/v1` | `llama3.2` |
| **LM Studio (this machine)** | OpenAI-compatible | `http://localhost:1234/v1` | `local-model` |
| **OpenRouter** | OpenAI-compatible | `https://openrouter.ai/api/v1` | `openrouter/auto` |
| **Custom endpoint** | whatever you pick | the fields as you left them | |

DeepSeek is a preset rather than a protocol because it speaks the OpenAI-compatible shape.
The *protocol* is how a request is built and the *provider* is who receives it: only two
protocols exist, and every service, including a server on your own machine, uses one of
them.

| Field | Meaning |
| --- | --- |
| **Protocol** | `OpenAI-compatible` posts to `/chat/completions` with a bearer token; `Anthropic` posts to `/messages` with `x-api-key` |
| **Endpoint** | The whole prefix, version path included. The protocol's own path is appended, and an endpoint that already names it is used as written |
| **Model** | The name the service knows it by. **Find models…** fills the list from the endpoint; a retired model name is the commonest cause of a 404 |
| **API key** | See below. A server on this machine usually needs none |
| **Reply ceiling** | The longest reply, 16 to 32768 tokens. 1024 is a page of explanation |
| **Temperature** | 0 is repeatable, 2 is inventive; 0 to 2 |
| **Give up after** | The request timeout, 5 to 600 seconds |
| **Send the current program by default** | What the panel's tick starts from |
| **Keep the conversation between sessions** | Off, a conversation dies with the window |
| **Suggest the next line in the editor** | Off, nothing is sent while you type |

**Find models…** asks the endpoint what it can run, `GET {base}/models`, the one discovery
call both request shapes share. A server on this machine is asked automatically when the
dialog opens, because the names a local server has are whatever was pulled last week.

**Test connection** sends one short question and reports the model's own answer. It is the
fastest way to tell a wrong key from a wrong endpoint from a retired model name, all three
of which otherwise fail the same way.

---

## The key

A key is a secret, and three things follow from that.

- **`TIME_WARP_AI_API_KEY` wins over anything stored.** Export it and leave the field
  empty, and the key is never written anywhere. That is the right answer for a shared
  machine.
- **Keeping the key is your choice.** With **Keep the key on this machine** unticked the
  key is used for that session and removed from the settings file, so a saved
  configuration can be handed on without the secret in it.
- **A stored key goes in the system's own store where there is one.** That is the
  freedesktop secret service, reached through `libsecret` when the build found it; a build
  or machine without it keeps the key in the settings file, restricted to its owner, and
  the dialog names the store it would use. `TIME_WARP_AI_KEYRING` chooses: `file` refuses
  the system store, `system` asks for it.

The item is one per user: a generic password with the service `Time Warp Studio` and the
account `assistant`. The key is never logged, never shown in a message, and stripped from
anything a service sends back, so a proxy that echoes the request in an error cannot put
it in the status bar.

Environment variables override the stored configuration:

| Variable | Overrides |
| --- | --- |
| `TIME_WARP_AI_API_KEY` | The key |
| `TIME_WARP_AI_BASE_URL` | The endpoint |
| `TIME_WARP_AI_MODEL` | The model |
| `TIME_WARP_AI_PROTOCOL` | `openai` or `anthropic` |
| `TIME_WARP_AI_ENABLED` | `1` or `0`: whether the assistant is on |
| `TIME_WARP_AI_KEYRING` | `file` or `system` |
| `TIME_WARP_AI_PERSONA` | `plain`, `professor`, `hacker` or `machine` |

---

## What leaves the machine

One request contains:

- your question, or the instruction a quick-action button stands for;
- the current document, or the selected lines, when **Send the program** is ticked;
- the document's file name and its language;
- for **Fix** and **Ask About the Last Error**, what the last run printed and any errors
  it reported;
- the last six exchanges of the conversation;
- with suggestions on, the document up to the caret, when typing pauses.

Nothing else: no other tab, no other file, no path, no list of open documents, no
telemetry, no version of the application beyond the request itself.

The program is capped at 12,000 characters. A larger document is cut there and the model
is told so in the text it receives, because a truncated program the model knows is
truncated is worth more than a request the service rejects. The same cap applies to a
selection and to the text sent for a suggestion. The last run's output is cut to its last
4,000 characters, because the end of a run is what a failure is at.

**Ollama** and **LM Studio** point at `localhost`: with either, nothing leaves the
computer, which is the configuration to recommend to a classroom with no policy for
sending student work anywhere. The panel decides which note to show from the endpoint's
host — `localhost`, `127.0.0.1`, `::1` or anything ending in `.localhost` — never from
which preset was chosen, so a custom local endpoint gets the same sentence.

### The two request shapes

| | OpenAI-compatible | Anthropic Messages |
| --- | --- | --- |
| Path | `POST {base}/chat/completions` | `POST {base}/messages` |
| Authentication | `Authorization: Bearer <key>` | `x-api-key: <key>` and `anthropic-version: 2023-06-01` |
| The standing instructions | a `system` turn first in `messages` | a top-level `system` field |
| The answer | `choices[0].message.content` | the `text` parts of `content[]` |

### What the model is told about the Studio

The system prompt is a constant in `native/src/ai/prompt.cpp`, and every sentence in it is
a mistake the assistant otherwise makes:

- the ten languages by name, and that they are interpreters written between 1964 and 1980
  running natively inside one application;
- that BASIC, COMAL and FOCAL are numbered lines, PILOT is letter-coded units, and Logo,
  Scheme, Smalltalk and Forth are whole programs;
- the turtle vocabulary, that heading 0 is north and increases clockwise, and that Logo
  has no `CIRCLE`, `RECTANGLE` or `POLYGON` — shapes are `REPEAT`;
- that no imports, packages or external libraries exist, so an answer must be one
  self-contained file;
- that code comes back in one fenced block labelled with the language key, and that the
  user's program is not quoted back unless it is being changed.

`ai_tests` reads the language table and fails if the prompt stops naming a language, so an
eleventh language cannot be added without the assistant learning about it.

### The voice

**Settings > Assistant > Voice** chooses how a reply reads. Every voice is appended to the
same standing instructions.

| Voice | What it does to a reply |
| --- | --- |
| **No persona** | The register above, with no era in it. The default |
| **1974: the lecturer** | A computer science teacher of that year: the machine, the listing, structured programming, flowcharts, the language's own specification |
| **1984: the bedroom programmer** | Terse and hands-on: type-in listings, tape and disk, memory you can count |
| **2026: the machine** | Precise and unhurried, current vocabulary welcome, and explicit when a limit is the interpreter's rather than the language's |

What a voice may not do is change the facts: each one says in its own words that a command
is real because it is in the list, not because it existed in that year. `ai_tests` asserts
both halves — that a persona adds to the base prompt rather than replacing it, and that
every voice still carries the rule against inventing commands.

---

## What it costs, and what it reports

The Studio bills nothing and has no account: the cost is whatever the service charges, and
a local server charges nothing. Three places report what was spent.

- **The status line** under the transcript after an answer: the model the service says
  answered, and the tokens in and out.
- **A session line** above it once anything has been asked: how many questions, and the
  totals. A service that sends no usage events leaves the counts at zero, and the line says
  so rather than showing three zeros as if they were a measurement.
- **The Telemetry panel**, as `Assistant asks` and `Assistant tokens` rows beside the run
  timings. The counters are per session; closing the window starts them again.

A remembered conversation is a file under the data directory's `assistant` leaf: the
newest twelve turns, written atomically and readable only by its owner, deleted when the
tick is removed or **Clear** is pressed. A damaged file is forgotten silently, because a
learner who never opened it does not need an error message about it.

---

## From a shell, and from a script

`time_warp_ai` is the same library with no window, reading the configuration the Studio
wrote:

```bash
./native/build/time_warp_ai check
./native/build/time_warp_ai config
./native/build/time_warp_ai explain --file Examples/logo/mandala.logo
./native/build/time_warp_ai ask "why does this draw nothing?" --file prog.bas
./native/build/time_warp_ai write "a number guessing game" --language BASIC
cat prog.logo | ./native/build/time_warp_ai explain --file -
```

| Command | What it answers |
| --- | --- |
| `check` | Whether the assistant is configured, and what is missing when it is not |
| `config` | The provider, protocol, endpoint and model — never the key |
| `models` | The models the endpoint can run, one per line, or `--json` |
| `ask`, `explain`, `fix`, `comment`, `translate`, `write` | The six questions, from a shell |

`--language`, `--model`, `--base-url`, `--protocol`, `--max-tokens` and `--timeout`
override the stored configuration for one run, and `--no-context` sends no program at all.
`--json PATH` writes a machine report — the answer, the model and the token counts, or the
error — and `--json -` writes it to standard output *instead* of the answer, so one stream
carries one thing. The answer goes to standard output alone; messages meant for a person
go to standard error. `check` exits 1 when the assistant is not configured, and a failed
request exits 1 with a sentence.

The same question is available over the automation API: `ai.ask` takes `prompt`, an
optional `system` and an optional `history`, and returns the answer with its model and
token counts. The handler waits for the finished answer, which is what a
one-line-in, one-line-out protocol can deliver, and gives up after the configured timeout
plus five seconds for a transport that accepts a request and then reports nothing. The
[automation](automation.md) page has the shapes.

Every failure is shown in the panel's status line, and the sentences are the ones the code
produces, so a screenshot can be read backwards into this list: no endpoint configured, a
key the service rejected, a model the service does not have, a network that did not answer,
a reply the application could not parse.
