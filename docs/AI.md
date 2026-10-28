# The AI Assistant

The Studio can explain a program, find what is wrong with it, comment it, rewrite it
in another language, or write a new one from a description — through a service you
choose and configure yourself.

It is **off until you configure it**, and it never sends anything until you press a
button. There is no bundled model, no account, no default endpoint, and nothing on
this page requires a particular vendor: the assistant speaks two request shapes, and
any service that speaks one of them works.

- [What it does](#what-it-does)
- [Configuring it](#configuring-it)
- [The key, and what is sent](#the-key-and-what-is-sent)
- [Running it from a shell](#running-it-from-a-shell)
- [When it does not work](#when-it-does-not-work)
- [Where it is built](#where-it-is-built)

## What it does

The assistant is the **🤖 Assistant** page of the right column, reached from
**Tools > 🤖 Assistant** (`Ctrl+Shift+A`). It is a conversation beside the editor
rather than a window over it, because the questions you want to ask are about the
program you are looking at.

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
| **Reply ceiling** | The longest reply, in tokens. 1024 is a page of explanation |
| **Temperature** | 0 is repeatable, 2 is inventive |
| **Give up after** | The request timeout, in seconds |
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
- for **Fix**, what the last run printed and any errors it reported;
- the last six exchanges of the conversation;
- with inline suggestions on, the document up to the caret, when typing pauses.

Nothing else. No other tab, no other file, no path, no list of what you have open,
no telemetry, no version of this application beyond the request itself.

`Ollama (this machine)` and `LM Studio (this machine)` point at `localhost`: with
either of those nothing leaves the computer at all, which is the configuration to
recommend to a classroom with no network policy for sending student work anywhere.

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

## When it does not work

Every one of these is shown in the panel's status line and, for a network failure,
in the conversation as well.

| What you see | What to do |
| --- | --- |
| `❌ the assistant is switched off` | Tick **Use the AI assistant** in AI Settings |
| `❌ no model is named` | Fill in **Model** |
| `❌ the endpoint must be an http or https address` | The endpoint needs its scheme: `https://api.deepseek.com/v1` |
| `❌ the key was rejected` | Check the key, and that it belongs to *this* endpoint. A key for one vendor will not work at another's URL |
| `❌ the endpoint was not found` | The base URL should end at the version path — `https://api.openai.com/v1`, not the whole `/chat/completions` and not the bare domain |
| `❌ the host could not be found` | The address is wrong, or this machine has no DNS |
| `❌ the request timed out` | Raise **Give up after**, or the service is busy |
| `❌ the service is rate-limiting requests` | Wait, or use a smaller model |
| `❌ the network request failed` | No route to the service: a proxy, a firewall, or a machine that is offline |
| `❌ the model does not exist` (in the service's words) | The model name has been retired; the settings dialog's **Test connection** is the quickest check |

A local server that answers `❌ the connection was refused` is not running: start
Ollama or LM Studio, then press **Test connection** again.

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
`ai_tests` exercises a 200 in each protocol's envelope, a 401 with an error object, a
body that is not JSON, a refused connection and a cancellation that arrives after
the button was pressed — every branch an endpoint can take, without an endpoint.
What a real model *says* is not a property of this program and is not asserted.

```bash
cmake --build native/build --target ai_tests
ctest --test-dir native/build -R ai_tests --output-on-failure
```

The prompt is where the assistant is told what this IDE is: the eleven languages,
which of them are line-numbered and which are whole programs, the turtle vocabulary,
and the rule that code comes back in one fenced block labelled with the language key.
`ai_tests` reads the language table and fails if the prompt stops naming a language.

### What it deliberately does not do

- **It streams, and every request streams.** Both shapes send the answer token by
  token, so the page shows it being written, replacing the partial text as each piece
  arrives rather than logging fragments, and **Stop** ends an answer that is half
  written instead of waiting for it. The token counts survive streaming because both
  protocols carry them inside it -- OpenAI in a final `usage` chunk, Anthropic in
  `message_start` and `message_delta`. A service that ignores the request to stream
  and answers with one JSON document is read by the ordinary reader on the bytes it
  kept, so nothing is lost either way. `timewarp::ai::Client::delta` is what a caller
  receives while it waits; `ai::is_local_endpoint` is no longer about streaming at all
  -- it is the privacy question, and the note beside the question box answers it.
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
