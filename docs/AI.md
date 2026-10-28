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
forgets them, and closing the application forgets them too: a conversation is a
session, not a setting.

The same three one-click questions are in **Tools** as **Explain This Program** and
**Fix This Program**, with no shortcut, because a key pressed by accident spends a
request. The command palette finds all of them.

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
- **A stored key is protected as far as the platform allows.** After a key is
  written the settings file is restricted to its owner on Linux and macOS.

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

### What exactly leaves the machine

One request contains:

- your question, or the instruction a quick-action button stands for;
- the current document, or the selected lines, when **Send the program** is ticked;
- the document's file name and its language;
- for **Fix**, what the last run printed and any errors it reported;
- the last six exchanges of the conversation.

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
| The page | `native/include/timewarp/ui/ai_panel.hpp`, `native/src/ui/ai_panel.cpp` |
| The settings dialog | `native/include/timewarp/ui/ai_settings.hpp`, `native/src/ui/ai_settings.cpp` |
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

- **It does not stream.** A reply arrives when it is complete. A partial answer
  arriving into a transcript is a feature with a cancel button in the middle of it,
  and this one has a Stop button instead.
- **It does not run what it writes.** Press **Insert**, then **Run** (F9): the
  program reaches the editor first, where you can read it. An answer that executed
  itself would be a language model with a keyboard.
- **It does not talk to the machine.** No files, no shell, no debugger: it is asked
  about text and answers in text.
- **It is not in the standalone player.** `time_warp_player` opens a compiled
  program, and a program that opens a socket to an AI service is not what a
  compiled program is for.
