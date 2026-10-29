# The AI Assistant

The Studio can explain a program, find what is wrong with it, comment it, rewrite it
in another language, or write a new one from a description — through a service you
choose and configure yourself.

It is **off until you configure it**, and nothing is sent until you press a button or
turn on the one setting that asks while you type. There is no bundled model, no
account, no default endpoint, and nothing on this page requires a particular vendor:
the assistant speaks two request shapes, and any service that speaks one of them
works.

- [What it does](#what-it-does)
- [Configuring it](#configuring-it)
- [The key, and what is sent](#the-key-and-what-is-sent)
- [What a request looks like](#what-a-request-looks-like)
- [What it costs, and what it reports](#what-it-costs-and-what-it-reports)
- [Running it from a shell](#running-it-from-a-shell)
- [When it does not work](#when-it-does-not-work)
- [Where it is built](#where-it-is-built)

## What it does

The assistant is the **🤖 Assistant** page of the right column, reached from
**Tools > 🤖 Assistant** (`Ctrl+Shift+A`). It is a conversation beside the editor
rather than a window over it, because the questions you want to ask are about the
program you are looking at. Until it is configured the page shows one button,
**Configure the assistant…**, which opens the same dialog as **Tools > AI Settings…**
— a page whose only useful control is a disabled question box is a dead end.

The buttons that ask about a program are disabled while a request is in flight, so
two questions cannot be half-asked at once. **Stop** is the only enabled control
then, and it abandons the request rather than letting an answer arrive into a
conversation that has moved on.

| Control | What it does |
| --- | --- |
| The question box | Ask anything. `Ctrl+Enter` sends; Enter alone is a new line |
| **Explain** | What the program does, in the order it runs, for someone learning the language |
| **Fix** | Find the faults and return the corrected program, with the last run's errors in view |
| **Comment** | The program back with comments added and no behaviour changed |
| **Translate** | Type the target language in the box first, then press it |
| **Write** | Type what the program should do in the box first, then press it |
| **Send** | Ask the question in the box |
| **Stop** | Abandon the request in flight |
| **Clear** | Forget the conversation so far |
| **Insert** | Put the answer's code into the editor at the cursor, replacing a selection |
| **Copy** | Put the whole answer on the clipboard |
| **Settings…** | The endpoint, the model, the key and the limits |

**Send the program** is ticked by default: the questions above are worth little
without the code. Untick it to ask a general question with nothing on screen going
out. A selection wins over the whole document, so selecting six lines and pressing
**Explain** is a question about six lines.

The panel keeps the last six exchanges so "and the same in Forth?" works. **Clear**
forgets them. Closing the application forgets them too unless **Keep the conversation
between sessions** is ticked in AI Settings: the turns are then written to
`assistant/conversation.json` under the application's data directory (`~/.time_warp`
unless `TIME_WARP_DATA_DIR` says otherwise), readable only by its owner, capped at the
same twelve turns the panel sends, and deleted the moment the tick is removed. A file
that has been damaged is forgotten silently rather than reported: a JSON file a learner
never opened is not something to put an error message in front of.

The same one-click questions are in **Tools** as **Explain This Program** and
**Fix This Program**, with no shortcut, because a key pressed by accident spends a
request. The command palette finds all of them.

**Tools > Ask About the Last Error** starts from the run rather than the program: the
lines selected in the Console, else the last error the Errors tab holds, else the last
line the program printed. What the run said becomes the question, and the program and
the run's output ride along as evidence, so the answer is about the failure that
happened rather than about a program in the abstract.

**The suggested next line** is the one thing here that is not a button. With
**Suggest the next line in the editor** ticked in AI Settings, a pause in typing
(about three quarters of a second) asks the endpoint for the next few lines of the
document up to the caret, and shows the first of them as dimmed text after the cursor.
**Tab** takes it, **Esc** refuses it, and any other key dismisses it and is typed as
usual. It is off unless you turn it on, it never competes with the local word list
(the suggestion is not shown while that popup is open), and an answer that arrives
after the caret moved is dropped rather than pasted after the wrong line. Unlike the
panel, it has a client of its own: a suggestion must not cancel an answer you are
reading.

Three limits on the ghost line are worth knowing, because each of them is a way a
code assistant usually goes wrong:

- **It asks only when the caret is at the end of the text it is asking from.** A
  suggestion for a caret in the middle of a line would have to be inserted between
  two halves of a statement, and nothing knows where that cut belongs.
- **It arrives only if nothing changed.** The editor keeps the text it sent, compares
  it with the text before the caret when the answer comes back, and discards the
  answer if a single keystroke landed in between.
- **A failure is silent.** A suggestion that does not arrive is not something the
  reader can act on, so it shows no status line — unlike the panel, where a failed
  question is the whole exchange.

## Configuring it

**Tools > AI Settings…** is the one place it is set up. The dialog's **Provider**
dropdown fills the three fields below it from a preset; the fields stay editable, so
a service that is not on the list is one line away.

| Preset | Shape | Endpoint | Model it fills in |
| --- | --- | --- | --- |
| **OpenAI** | OpenAI-compatible | `https://api.openai.com/v1` | `gpt-4o-mini` |
| **DeepSeek** | OpenAI-compatible | `https://api.deepseek.com/v1` | `deepseek-chat` |
| **Anthropic** | Anthropic Messages | `https://api.anthropic.com/v1` | `claude-3-5-haiku-latest` |
| **Ollama (this machine)** | OpenAI-compatible | `http://localhost:11434/v1` | `llama3.2` |
| **LM Studio (this machine)** | OpenAI-compatible | `http://localhost:1234/v1` | `local-model` |
| **OpenRouter** | OpenAI-compatible | `https://openrouter.ai/api/v1` | `openrouter/auto` |
| **Custom endpoint** | whatever you pick | the fields as you left them | |

DeepSeek is a preset rather than a protocol because it speaks the OpenAI-compatible
shape: the *protocol* is how a request is built, and the *provider* is who receives
it. Only two protocols exist — OpenAI-compatible and Anthropic — and every other
service, including a server on your own machine, uses one of them.

| Field | What it means |
| --- | --- |
| **Protocol** | `OpenAI-compatible` posts to `/chat/completions` with a bearer token; `Anthropic` posts to `/messages` with `x-api-key` |
| **Endpoint** | The whole prefix, version path included. The protocol's own path is appended, and an endpoint that already names it is used as written |
| **Model** | The name the service knows it by. **Find models…** fills the list from the endpoint; retired model names are the most common cause of a 404 |
| **API key** | See below. A server on this machine usually needs none |
| **Reply ceiling** | The longest reply, in tokens. 1024 is a page of explanation; the field accepts 16 to 32768 |
| **Temperature** | 0 is repeatable, 2 is inventive; the field accepts 0 to 2 |
| **Give up after** | The request timeout, in seconds; the field accepts 5 to 600 |
| **Send the current program by default** | What the panel's tick starts from |
| **Keep the conversation between sessions** | Off, a conversation dies with the window. On, it is kept in the data directory and deleted when this is unticked |
| **Suggest the next line in the editor** | Off, nothing is sent while you type. On, a pause in typing asks for the next line and shows it as ghost text |

**Find models…** asks the endpoint what it can run -- `GET {base}/models`, the one
discovery call both request shapes share -- and offers the answer in a dropdown. A
server on this machine is asked automatically when the dialog opens, because the
names a local model server has are whatever was pulled last week. A service that
does not implement the call answers with nothing, and the dialog says to type the
name instead.

**Test connection** sends one short question and reports what came back, with the
model's own answer. It is the fastest way to tell a wrong key from a wrong endpoint
from a retired model name — all three otherwise fail the same way, with a red
sentence in the panel.

## The key, and what is sent

A key is a secret. Three things follow, and all three are deliberate.

- **`TIME_WARP_AI_API_KEY` wins over anything stored.** Export it and leave the
  field empty and the key is never written anywhere. This is the right answer for a
  shared machine.
- **Keeping the key is your choice.** With **Keep the key on this machine** unticked
  the key is used for that session and *removed* from the settings file, so a saved
  configuration can be handed to another machine without the secret in it.
- **A stored key goes in the operating system's own store where there is one.** On
  Windows that is the Credential Manager, on macOS the Keychain, and on Linux the
  freedesktop secret service when the build found `libsecret`. A build or a machine
  without one keeps the key in the settings file, restricted to its owner, exactly as
  before -- and the settings dialog names the store it would use. The choice is
  `TIME_WARP_AI_KEYRING`: `file` refuses the system store, `system` asks for it, and a
  build that has none answers `file` either way rather than pretending.

  The item is one per user and is named the same way everywhere it is visible: a
  `TimeWarpStudio/assistant` target in the Windows Credential Manager, a generic
  password with the service `Time Warp Studio` and the account `assistant` in the
  macOS Keychain, and the same account name in the freedesktop secret service. A
  reader who wants to remove a key by hand can find it; a reader who wants to know
  where it went does not have to guess.

The key is never logged, never shown in a message, and stripped from anything a
service sends back — a proxy that echoes the request in an error still cannot put
the key in the status bar.

The environment variables that override the stored configuration:

| Variable | Overrides |
| --- | --- |
| `TIME_WARP_AI_API_KEY` | The key |
| `TIME_WARP_AI_BASE_URL` | The endpoint |
| `TIME_WARP_AI_MODEL` | The model |
| `TIME_WARP_AI_PROTOCOL` | `openai` or `anthropic` |
| `TIME_WARP_AI_ENABLED` | `1`/`0`: whether the assistant is on |
| `TIME_WARP_AI_KEYRING` | `file` or `system`: where a stored key lives |

### What exactly leaves the machine

One request contains:

- your question, or the instruction a quick-action button stands for;
- the current document, or the selected lines, when **Send the program** is ticked;
- the document's file name and its language;
- for **Fix** and **Ask About the Last Error**, what the last run printed and any
  errors it reported;
- the last six exchanges of the conversation;
- with inline suggestions on, the document up to the caret, when typing pauses.

Nothing else. No other tab, no other file, no path, no list of what you have open,
no telemetry, no version of this application beyond the request itself.

**The program is capped at 12,000 characters.** A larger document is cut at that
point and the model is told so in the text it receives — a truncated program the
model knows is truncated is worth more than a request the service rejects for size.
The same cap applies to a selection and to the text sent for an inline suggestion.
The last run's output is cut to its last 4,000 characters, because the end of a run
is what a failure is at.

**The selected lines win.** If a selection exists, the request carries the selection
*instead of* the document — six selected lines and **Explain** is a question about
six lines. The file name stays with it, so an answer can still say "in
`mandala.logo`".

`Ollama (this machine)` and `LM Studio (this machine)` point at `localhost`: with
either of those nothing leaves the computer at all, which is the configuration to
recommend to a classroom with no network policy for sending student work anywhere.
The panel decides which note to show by looking at the endpoint's host — `localhost`,
`127.0.0.1`, `::1` or anything ending in `.localhost` — and never by remembering which
preset was chosen, so a custom endpoint on this machine gets the same sentence.

## What a request looks like

There are two shapes, and the difference is small enough to state in full.

| | OpenAI-compatible | Anthropic Messages |
| --- | --- | --- |
| Path | `POST {base}/chat/completions` | `POST {base}/messages` |
| Authentication | `Authorization: Bearer <key>` | `x-api-key: <key>` and `anthropic-version: 2023-06-01` |
| The standing instructions | A `system` turn first in `messages` | A top-level `system` field |
| The answer | `choices[0].message.content` | the `text` parts of `content[]` |
| Token counts | `usage.prompt_tokens` / `completion_tokens` | `usage.input_tokens` / `output_tokens` |

Both bodies carry `model`, `max_tokens`, `temperature` and `"stream": true`. The
OpenAI-compatible body also asks for `stream_options.include_usage`, without which a
streamed answer carries no token counts at all and the panel's `412 in, 96 out` would
silently become zero.

Four details are worth writing down because each of them is a bug the obvious version
of this code has:

- **A base URL that already ends in the protocol's path is used as written.** A
  provider whose documentation hands out `https://host/v1/chat/completions` works,
  and so does `https://host/v1`; appending `/chat/completions` to the first would
  produce a 404 nobody can explain. Trailing slashes are removed first, for the same
  reason.
- **Anthropic requires turns to alternate and to begin with a user turn.** A
  conversation with two questions in a row is legal here (the first one may have
  failed) and a 400 there, so adjacent turns with the same role are joined with a
  blank line rather than refused.
- **The numbers are clamped on the way in and out.** The reply ceiling is held to
  16–32768 tokens, the timeout to 5–600 seconds and the temperature to 0–2, whatever
  a settings file says. A hand-edited `ai/maxTokens=10` is corrected rather than
  honoured.
- **There are no retries and no cache.** One request in flight, one answer or one
  sentence. Sending again while one is in flight cancels the first: two answers
  arriving out of order into one transcript is worse than a lost question.

Every answer streams. The panel replaces the partial text as each piece arrives
rather than logging fragments, so what is on screen while waiting is the answer being
written; **Stop** ends a half-written one. A service that ignores `stream` and answers
with one JSON document is read from the bytes it did send, so nothing is lost either
way. A reply that finishes after it was cancelled is recognised and dropped rather
than reported as a failed request.

**Insert takes the program out of the answer.** The longest fenced block is what
goes into the editor; a model that says "here is the fix" and then the code has both,
and the code is what the button means. When there is no fence at all, the whole reply
is inserted — which is what a one-line answer wants, and what makes it obvious the
model did not follow the instruction.

### What the model is told about this Studio

The system prompt is a constant in `native/src/ai/prompt.cpp`, and every sentence in
it is a mistake the assistant otherwise makes:

- the eleven languages, by name, and that they are interpreters written between 1964
  and 1980 running natively inside one application;
- that BASIC, COMAL and FOCAL are numbered lines, PILOT is letter-coded units, and
  Logo, Scheme, Smalltalk, Forth and CHIP-8 are whole programs;
- the turtle vocabulary the language guide documents, that heading 0 is north and
  increases clockwise, and that Logo has no `CIRCLE`, `RECTANGLE` or `POLYGON` —
  shapes are `REPEAT`;
- that a CHIP-8 program is assembly for the assembler in this application;
- that no imports, packages or external libraries exist, so an answer must be one
  self-contained file;
- that code comes back in one fenced block labelled with the language key, and that
  the user's program should not be quoted back unless it is being changed.

`ai_tests` reads the language table and fails if the prompt stops naming a language,
so a twelfth language cannot be added without the assistant learning about it.

## What it costs, and what it reports

The Studio has no account and bills nothing: the cost is whatever the service you
chose charges, and a local server charges nothing. What the application does report
is what it spent, in three places:

- **The status line** under the transcript, after an answer: `✅ deepseek-chat · 412
  in, 96 out`. The model is the service's own name for what answered, which is not
  always the one asked for.
- **A session line** above it once anything has been asked: `This session: 3
  question(s), 1180 tokens in, 402 out`. A service that sends no usage events leaves
  the counts at zero, and the line says `token counts not reported` rather than
  showing three zeros as if they were a measurement.
- **The Telemetry panel**, as `Assistant asks` and `Assistant tokens` rows, beside the
  run timings. The counters are for the session: closing the window starts them
  again, because they describe what this session spent.

A remembered conversation is a file, and it is treated as one: `conversation.json`
under the data directory's `assistant` leaf, the newest twelve turns, written through
`QSaveFile` and restricted to its owner, deleted when the tick is removed or when
**Clear** is pressed. A damaged file is forgotten silently — a learner who never
opened it does not need an error message about it.

## Running it from a shell

`time_warp_ai` is the same library with no window. It reads the configuration the
IDE wrote, and it is the quickest way to answer "does this key work?":

```bash
./native/build/time_warp_ai check
./native/build/time_warp_ai config
./native/build/time_warp_ai explain --file Examples/logo/mandala.logo
./native/build/time_warp_ai ask "why does this draw nothing?" --file prog.bas
./native/build/time_warp_ai write "a number guessing game" --language BASIC
```

| Command | What it answers |
| --- | --- |
| `check` | Whether the assistant is configured, and what is missing when it is not |
| `config` | The provider, protocol, endpoint and model — never the key |
| `models` | The models the endpoint can run, one per line (or `--json`) |
| `ask` | Answer a question about a program |
| `explain` | Explain a program |
| `fix` | Find and correct faults |
| `comment` | Comment a program without changing it |
| `translate` | Rewrite a program, naming the target language in the words |
| `write` | Write a new program from a description |

`--file -` reads the program from standard input, so
`cat prog.logo | ./native/build/time_warp_ai explain --file -` works. `--language`,
`--model`, `--base-url`, `--protocol`, `--max-tokens` and `--timeout` override the
stored configuration for one run, and `--no-context` sends no program at all.

`--json PATH` writes a machine report — the answer, the model and the token counts,
or `ok` and the error — to `PATH`; `--json -` writes it to standard output *instead*
of the answer, so one stream carries one thing. That is the shape a script wants:

```bash
./native/build/time_warp_ai explain --file prog.logo --json - |
  jq -r .text
```

The answer goes to standard output alone, so a pipeline can have it; messages a
person reads go to standard error. `check` exits 1 when the assistant is not
configured, and a failed request exits 1 with `❌` and a sentence.

The same question is available over the automation API, for a script that already
speaks JSON-RPC to the Studio: `ai.ask` takes `prompt`, an optional `system` and an
optional `history`, and returns the answer with its model and token counts.
`docs/API.md` has the request and response shapes. It reads the same configuration,
so `TIME_WARP_AI_API_KEY` and the `TIME_WARP_AI_*` overrides apply there too, and the
request streams through the same client the panel uses -- the handler waits for the
finished answer, which is what a one-line-in, one-line-out protocol can deliver.

Because the call blocks, it gives up after the configured **Give up after** plus a
five-second net for a transport that accepts the request and then reports nothing at
all; the answer then arrives as an `-32000` error with the sentence that explains
it. A caller that needs a shorter deadline changes the timeout in AI Settings — the
same stored configuration the handler reads — rather than wrapping the call.

## When it does not work

Every one of these is shown in the panel's status line and, for a network failure,
in the conversation as well. The sentences are the ones the code produces, so this
table can be read backwards from a screenshot.

**Before anything is sent**

| What you see | What to do |
| --- | --- |
| `❌ the assistant is switched off` | Tick **Use the AI assistant** in AI Settings |
| `❌ no endpoint is configured` | Choose a preset, or type the base URL |
| `❌ the endpoint must be an http or https address` | The endpoint needs its scheme and a host: `https://api.deepseek.com/v1`. A bare host name and a `file:` path are refused here rather than at the socket |
| `❌ no model is named` | Fill in **Model**, or pick one with **Find models…** |

**The service said no**

| What you see | What to do |
| --- | --- |
| `❌ the service rejected the request: …` | Usually a field the service does not accept. The words after the colon are the service's own |
| `❌ the key was rejected — check the key and that it belongs to this endpoint` | A key for one vendor will not work at another vendor's URL, and a key with a stray space is the same thing |
| `❌ the endpoint was not found — the base URL should end at the version path, such as https://api.openai.com/v1` | The base URL should end at the version path, not at the bare domain and not at the whole `/chat/completions`. A retired model name can also produce this from some services |
| `❌ the program is too large for this model` | Select the part you are asking about; a selection replaces the document |
| `❌ the service is rate-limiting requests — wait a moment and try again` | Wait, or use a smaller model |
| `❌ the service reported an internal error` | The service's fault, not the configuration's. Try again |
| `❌ the service answered HTTP 418` | Any status without a sentence of its own is reported by number |
| `❌ the service returned no text` | A successful exchange with an empty answer, such as a refusal. The service's own explanation follows the colon when it sent one |
| `❌ the service did not answer with JSON` | The endpoint is not the API it was taken for — a proxy's login page, or a web address rather than an API one |

**The request never arrived**

| What you see | What to do |
| --- | --- |
| `❌ the connection was refused — is the server running?` | A local server is not running: start Ollama or LM Studio, then press **Test connection** again |
| `❌ the host could not be found — check the endpoint address` | The address is wrong, or this machine has no DNS |
| `❌ the request timed out — the service did not answer in time` | Raise **Give up after**, or the service is busy |
| `❌ the TLS handshake failed — the endpoint's certificate could not be verified` | A corporate proxy or an expired certificate. There is no "ignore the certificate" switch, deliberately |
| `❌ the proxy refused the connection` | The proxy settings, or the credentials it wants |
| `❌ the network request failed` | No route to the service: a firewall, or a machine that is offline |
| `❌ the request was cancelled` | Only when something other than **Stop** ended it — a request that is stopped reports nothing |

A failed request exits `time_warp_ai` with status 1 and the same sentence on
standard error, so a script does not need to parse the panel to find out what
happened.

## Where it is built

| Part | Where |
| --- | --- |
| Configuration, prompts and the client | `native/include/timewarp/ai/config.hpp`, `native/src/ai/config.cpp`, `native/include/timewarp/ai/prompt.hpp`, `native/src/ai/prompt.cpp`, `native/include/timewarp/ai/client.hpp`, `native/src/ai/client.cpp` |
| The stored conversation | `native/include/timewarp/ai/history.hpp`, `native/src/ai/history.cpp` |
| Where the key lives | `native/include/timewarp/ai/keyring.hpp`, `native/src/ai/keyring.cpp` -- the Credential Manager, the Keychain, `libsecret`, or the settings file |
| The page | `native/include/timewarp/ui/ai_panel.hpp`, `native/src/ui/ai_panel.cpp` |
| The settings dialog | `native/include/timewarp/ui/ai_settings.hpp`, `native/src/ui/ai_settings.cpp` |
| The dialog the two machines open | `native/include/timewarp/ui/assistant_dialog.hpp`, `native/src/ui/assistant_dialog.cpp` |
| The command | `native/tools/ai_cli.cpp` |
| The suite | `native/tests/ai_tests.cpp` |

The library is `timewarp::ai`, built when Qt's Network module is present. A Qt
without Network does not get a broken assistant: it does not get the page, the menu
entries or the command at all, because a client with no transport is not a smaller
feature.

The interesting half is testable without a network: what to ask and how to shape a
request are pure functions, and the client is given a transport of the test's own.
`ai_tests` covers, at the time of writing:

- **configuration** — endpoint validation, path assembly for both protocols, protocol
  names and their aliases, the DeepSeek preset, what a request needs before it is
  sent, the settings round trip, where the key lives, the environment beating the
  file, and a key that appears in an error message being removed;
- **prompts** — that the system prompt names every language in the table, that
  `Explain` sends the program exactly once, that a selection replaces the document,
  that `Fix` and `Diagnose` carry what the run said, that `Write` sends no document,
  that an oversized program is cut and says so, and how code is taken out of an
  answer;
- **the wire** — the OpenAI-compatible body and the Anthropic body, the authentication
  header each one uses, reading an answer in each envelope, a refusal, the transport
  failures as sentences, model-list parsing, a hosted endpoint streaming, a service
  that ignores `stream`, **Stop** ending a hosted answer, and a second send
  superseding the first.

What a real model *says* is not a property of this program and is not asserted: the
suite proves the request is well formed and the reply is read, not that the answer is
good.

```bash
cmake --build native/build --target ai_tests
ctest --test-dir native/build -R ai_tests --output-on-failure
```

The prompt is where the assistant is told what this IDE is: the eleven languages,
which of them are line-numbered and which are whole programs, the turtle vocabulary,
and the rule that code comes back in one fenced block labelled with the language key.
`ai_tests` reads the language table and fails if the prompt stops naming a language.

### What it deliberately does not do

- **It streams, and every request streams** — what that means for a service that
  ignores the request, for the token counts and for **Stop** is
  [above](#what-a-request-looks-like). `timewarp::ai::Client::delta` is what a caller
  receives while it waits; `ai::is_local_endpoint` is not about streaming at all — it
  is the privacy question, and the note beside the question box answers it.
- **It does not run what it writes.** Press **Insert**, then **Run** (F9): the
  program reaches the editor first, where you can read it. An answer that executed
  itself would be a language model with a keyboard.
- **It does not talk to the machine.** No files, no shell, no debugger: it is asked
  about text and answers in text.
- **It does not see the drawing, the variables or the debugger.** Two machines can ask
  it about their own program (below), and none of the three sends anything else.
- **It is not in the standalone player.** `time_warp_player` opens a compiled
  program, and a program that opens a socket to an AI service is not what a
  compiled program is for.

### The two machines

The HB-1 and the CHIP-8 each open the same page in a dialog, because those windows are
machines of their own rather than the IDE: the HB-1's 🤖 button in its header and
**🤖 Assistant...** on the CHIP-8's toolbar. What they send is the program the machine
is holding -- the current ROM's listing, or the text in the CHIP-8's Listing tab -- and
nothing else: not the screen, not the memory, not the disk. A CHIP-8 binary ROM has no
text to send, so it sends none.

The dialog is the same widget the IDE shows, so the settings, the conversation limits
and the privacy note are one implementation and cannot drift. It keeps no conversation
of its own: closing it forgets what was asked, and the tick that remembers a
conversation applies here too.
