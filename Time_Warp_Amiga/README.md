# Time Warp (Amiga)

Experimental Amiga port of Time Warp — a tiny, console-based interpreter skeleton.

Status: minimal REPL and single-command mode with a small command set:

- BASIC-like: `PRINT <text>`, `ECHO <text>` → `✅ <text>`
- Logo-like: `FORWARD|FD <n>`, `RIGHT <deg>`, `LEFT <deg>` → `🐢 ...`
- PILOT-like: `T:<text>`, `A:<text>` → `ℹ️ ...` / `📝 ...`
- `QUIT`/`EXIT` to leave REPL

## Build

Host (Linux/macOS, for quick testing):

```bash
cd Time_Warp_Amiga
make host
./timewarp_amiga ECHO Hello
```

Amiga m68k (requires cross toolchain `m68k-amigaos-gcc` on PATH):

```bash
cd Time_Warp_Amiga
make amiga
# copy ./timewarp_amiga to your Amiga system/emulator to run
```

Amiga m68k via Docker (no local toolchain needed):

```bash
cd Time_Warp_Amiga
make amiga-docker
# copy ./timewarp_amiga to your Amiga system/emulator to run
```

This uses the public bebbo Amiga GCC image (`ghcr.io/bebbo/amiga-gcc:latest`) to compile inside a container and writes the artifact into this folder via a bind mount.

If you see a permissions error pulling from GHCR, authenticate once:

```bash
docker login ghcr.io
# Use a GitHub personal access token with read:packages
```

Fallback: build a local toolchain image (slower, no GHCR required):

```bash
cd Time_Warp_Amiga
make amiga-docker-local
# first build can take 20–40 minutes depending on your machine
```

This builds Bebbo's toolchain from source inside a Docker image based on Debian, then compiles the project with it. Subsequent runs will be much faster thanks to cached layers.

No-network fallback (mount host toolchain):

```bash
# 1) Clone toolchain on the host (using your network config/credentials)
git clone https://github.com/bebbo/amiga-gcc "$HOME/amiga-gcc"

# 2) Build deps-only image, mount the host toolchain, and compile
cd Time_Warp_Amiga
make amiga-docker-mount-toolchain TOOLCHAIN_DIR="$HOME/amiga-gcc"
```

The first run builds the toolchain inside the container using the host checkout (no network inside the container). Once it completes, `./timewarp_amiga` is produced in this folder.

## Run

Single command:

```bash
./timewarp_amiga PRINT "Hello Amiga"
```

Interactive REPL:

```bash
./timewarp_amiga
> PRINT Hello
✅ Hello
> FD 50
🐢 FD 50.00
> QUIT
```

Notes:

- Emoji prefixes are used by convention; classic Amiga consoles may render them as fallback squares.
- The interpreter is stateless and returns formatted text; no UI state is stored in executors.
