# QUICK START: Time Warp IDE for Apple macOS

## Build

```bash
cd Apple
./build.sh
```

## Run

```bash
./target/release/time_warp_apple
```

## Features

- Native macOS GUI (eframe/egui)
- Turtle graphics canvas (500x400)
- Live console output
- File operations (Open, Save, Quick Save PNG)
- Built-in examples
- Keyboard shortcuts (Cmd+R, Cmd+S, Cmd+Q)

## Packaging

To create a `.app` bundle for distribution:

```bash
cargo install cargo-bundle
cargo bundle --release
```

## Example

```text
REPEAT 4
  FORWARD 100
  LEFT 90
ENDREPEAT
```

## License

MIT
