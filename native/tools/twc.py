#!/usr/bin/env python3
# SPDX-License-Identifier: MIT
"""Compile a Time Warp Studio program into a standalone executable.

The Studio's languages are interpreted, so "compiling" here means producing a
small native launcher that embeds the program and links the same interpreter the
IDE runs. The result is a real executable: it needs neither Python nor the Studio
at run time, and a program that draws writes an SVG rather than silently exiting
with nothing to show.

Usage:
    ./twc Examples/basic/hello.bas -o hello
    ./twc Examples/logo/mandala.logo --svg mandala.svg
    ./twc program.scm -o program

Run `./twc --help` for every option.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

# Mirrors native/src/ui/language.cpp. The keys are the names the C++ enum uses
# (and the ones the runner generator emits); the stems are the extensions a
# program can arrive with.
LANGUAGES = {
    "basic": ("Basic", ("bas", "bsc")),
    "pilot": ("Pilot", ("pilot", "plt")),
    "logo": ("Logo", ("logo", "lg")),
    "comal": ("Comal", ("comal", "cml")),
    "rexx": ("Rexx", ("rexx", "rex")),
    "scheme": ("Scheme", ("scm", "ss", "scheme")),
    "focal": ("Focal", ("foc", "focal")),
    "chip8": ("Chip8", ("ch8", "c8")),
    "apl": ("Apl", ("apl", "dyalog")),
    "smalltalk": ("Smalltalk", ("st", "smalltalk")),
}

DISPLAY = {
    "Basic": "BASIC",
    "Pilot": "PILOT",
    "Logo": "Logo",
    "Comal": "COMAL",
    "Rexx": "REXX",
    "Scheme": "Scheme",
    "Focal": "FOCAL",
    "Chip8": "CHIP-8",
    "Apl": "APL",
    "Smalltalk": "Smalltalk",
}


def repo_root() -> Path:
    """Return the repository root from this tool's own location."""
    return Path(__file__).resolve().parents[2]


def detect_language(path: Path, override: str | None) -> tuple[str, str]:
    """Return the language key and C++ enum name for a program or override."""
    if override:
        wanted = override.strip().lower()
        for key, (enum, _) in LANGUAGES.items():
            if wanted in (key, enum.lower(), DISPLAY[enum].lower()):
                return key, enum
        raise SystemExit(f"twc: unknown language '{override}'")
    suffix = path.suffix.lower().lstrip(".")
    for key, (enum, extensions) in LANGUAGES.items():
        if suffix in extensions:
            return key, enum
    raise SystemExit(f"twc: cannot tell the language of {path.name!r}; pass --language")


def find_library(build_dir: Path, name: str) -> Path | None:
    """Find a built static library, directly or in a nested build directory."""
    direct = build_dir / f"lib{name}.a"
    if direct.exists():
        return direct
    for candidate in build_dir.rglob(f"lib{name}.a"):
        return candidate
    return None


def build_engine(root: Path, build_dir: Path, needs_scheme: bool) -> dict[str, Path]:
    """Configure and build the libraries a generated launcher must link."""
    targets = ["timewarp_scheme"] if needs_scheme else ["timewarp_lang"]
    if not (build_dir / "CMakeCache.txt").exists():
        print("twc: configuring the engine build", file=sys.stderr)
        subprocess.run(
            [
                "cmake",
                "-S",
                str(root / "native"),
                "-B",
                str(build_dir),
                "-DCMAKE_BUILD_TYPE=Release",
            ],
            check=True,
        )
    libraries = {}
    for name in targets + ([] if needs_scheme else ["timewarp_expr"]):
        library = find_library(build_dir, name)
        if library is None:
            print(f"twc: building {name}", file=sys.stderr)
            subprocess.run(
                ["cmake", "--build", str(build_dir), "--target", name, "-j"],
                check=True,
            )
            library = find_library(build_dir, name)
            if library is None:
                raise SystemExit(f"twc: {name} did not produce a library")
        libraries[name] = library
    return libraries


def cxx_string(text: str) -> str:
    """Quote text as a C++ string literal."""
    return '"' + text.replace("\\", "\\\\").replace('"', '\\"') + '"'


def generate_runner(source: bytes, enum: str, default_svg: str) -> str:
    """Return the C++ launcher source for one embedded program."""
    bytes_text = ",".join(str(byte) for byte in source)
    scheme = enum == "Scheme"
    includes = (
        '#include "timewarp/scheme/machine.hpp"\n'
        if scheme
        else '#include "timewarp/lang/interpreter.hpp"\n'
    )
    main_body = (
        """
    timewarp::scheme::Arena arena;
    timewarp::scheme::Machine machine{arena, host};
    const std::string text = machine.execute(source);
    std::fwrite(text.data(), 1, text.size(), stdout);
    ok = text.rfind("\\u274C", 0) != 0;
"""
        if scheme
        else f"""
    timewarp::lang::Interpreter interpreter{{timewarp::lang::Language::{enum}, host}};
    interpreter.set_sql_callback([&database](std::string_view sql) {{
        return database.run(sql);
    }});
    interpreter.set_output_callback([](const std::string& line) {{
        std::fwrite(line.data(), 1, line.size(), stdout);
        std::fputc('\\n', stdout);
    }});
    interpreter.set_input_callback([](const std::string& prompt) {{
        std::fputs(prompt.c_str(), stdout);
        std::fflush(stdout);
        std::string answer;
        if (!std::getline(std::cin, answer)) {{
            answer.clear();
        }}
        return answer;
    }});
    interpreter.load_program(source);
    interpreter.run();
    ok = interpreter.error().empty() && !interpreter.timed_out();
"""
    )
    return f"""// Generated by twc. The program is embedded as bytes, not as a quoted string:
// a CHIP-8 ROM is binary and a source file may contain any byte the editor kept.
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <string_view>

#include "twc_host.hpp"
#include "twc_sql.hpp"
{includes}
namespace {{
const unsigned char kProgram[] = {{{bytes_text}}};
constexpr auto kDefaultSvg = {cxx_string(default_svg)};

void usage(const char* name) {{
    std::printf("Usage: %s [--svg FILE] [--no-svg]\\n", name);
    std::printf("Runs the embedded {DISPLAY[enum]} program.\\n");
    std::printf("  --svg FILE  write the drawing to FILE\\n");
    std::printf("  --no-svg    discard the drawing\\n");
    std::printf("  --db FILE   SQL database (default: $TIME_WARP_SQL_DB or :memory:)\\n");
}}
}}  // namespace

int main(int argc, char** argv) {{
    std::string svg = kDefaultSvg;
    std::string db_path = ":memory:";
    if (const char* from_environment = std::getenv("TIME_WARP_SQL_DB");
        from_environment != nullptr && *from_environment != '\0') {{
        db_path = from_environment;
    }}
    bool want_svg = true;
    for (int index = 1; index < argc; ++index) {{
        const std::string argument = argv[index];
        if (argument == "--help" || argument == "-h") {{
            usage(argv[0]);
            return 0;
        }}
        if (argument == "--svg" && index + 1 < argc) {{
            svg = argv[++index];
            want_svg = true;
            continue;
        }}
        if (argument == "--no-svg") {{
            want_svg = false;
            continue;
        }}
        if (argument == "--db" && index + 1 < argc) {{
            db_path = argv[++index];
            continue;
        }}
        std::fprintf(stderr, "%s: unknown argument %s\\n", argv[0], argument.c_str());
        usage(argv[0]);
        return 2;
    }}

    TwcHost host;
    TwcDatabase database;
    std::string database_error;
    if (!database.open(db_path, &database_error)) {{
        std::fprintf(stderr, "%s: cannot open %s: %s\\n", argv[0], db_path.c_str(),
                     database_error.c_str());
        return 1;
    }}
    host.set_sql_runner([&database](std::string_view sql) {{
        return database.run(sql);
    }});

    const std::string_view source{{reinterpret_cast<const char*>(kProgram),
                                   sizeof kProgram}};
    bool ok = true;
{main_body}
    std::fflush(stdout);
    if (host.drew() && want_svg) {{
        if (!host.write_svg(svg)) {{
            std::fprintf(stderr, "%s: the drawing could not be written to %s\\n",
                         argv[0], svg.c_str());
            return 1;
        }}
        std::fprintf(stderr, "%s: %zu drawing command(s), saved to %s\\n", argv[0],
                     host.drawingCalls(), svg.c_str());
    }} else if (host.drew()) {{
        std::fprintf(stderr,
                     "%s: the program drew %zu command(s); pass --svg FILE to save "
                     "the drawing\\n",
                     argv[0], host.drawingCalls());
    }}
    return ok ? 0 : 1;
}}
"""


def compile_launcher(
    libraries: dict[str, Path],
    runner: str,
    output: Path,
    cxx: str,
    keep: bool,
) -> int:
    """Write the generated C++ and compile it; return the compiler's status."""
    root = repo_root()
    with tempfile.TemporaryDirectory(prefix="twc-") as temporary:
        runner_path = Path(temporary) / "twc_program.cpp"
        runner_path.write_text(runner, encoding="utf-8")
        if keep:
            kept = output.with_suffix(".cpp")
            shutil.copyfile(runner_path, kept)
            print(f"twc: kept {kept}", file=sys.stderr)

        command = [
            cxx,
            "-std=c++20",
            "-O2",
            "-I",
            str(root / "native" / "include"),
            "-I",
            str(root / "native" / "tools" / "twc"),
            str(runner_path),
            *[str(path) for path in libraries.values()],
            "-o",
            str(output),
        ]
        if os.name != "nt":
            command.append("-pthread")
            command.append("-lsqlite3")
        result = subprocess.run(command, capture_output=True, text=True, check=False)
        if result.returncode != 0:
            print("twc: compilation failed", file=sys.stderr)
            print(result.stderr, file=sys.stderr)
            print(" ".join(command), file=sys.stderr)
            return result.returncode
    return 0


def main() -> int:
    """Parse the command line, build the launcher and return its exit status."""
    parser = argparse.ArgumentParser(
        prog="twc",
        description="Compile a Time Warp Studio program into a standalone executable.",
        epilog="The generated executable accepts --svg FILE, --no-svg and, for a "
        "program that uses SQL, --db FILE.",
    )
    parser.add_argument("program", help="the Studio program to compile")
    parser.add_argument("-o", "--output", help="the executable to write")
    parser.add_argument("--language", help="override the language (e.g. basic, logo)")
    parser.add_argument("--build-dir", help="the native build tree to link against")
    parser.add_argument("--keep", action="store_true", help="keep the generated C++")
    parser.add_argument(
        "--cxx", default=os.environ.get("CXX", "c++"), help="the C++ compiler to use"
    )
    args = parser.parse_args()

    program = Path(args.program)
    if not program.is_file():
        print(f"twc: no such program: {program}", file=sys.stderr)
        return 1
    _, enum = detect_language(program, args.language)
    source = program.read_bytes()

    output = Path(args.output) if args.output else Path(program.stem)
    default_svg = str(output.with_suffix(".svg"))

    root = repo_root()
    build_dir = Path(args.build_dir) if args.build_dir else root / "native" / "build"
    libraries = build_engine(root, build_dir, needs_scheme=enum == "Scheme")

    runner = generate_runner(source, enum, default_svg)
    status = compile_launcher(libraries, runner, output, args.cxx, args.keep)
    if status != 0:
        return status

    print(f"twc: {program} -> {output} ({DISPLAY[enum]})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
