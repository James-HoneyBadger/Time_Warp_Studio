# SPDX-License-Identifier: MIT
#
# Included by `native/CMakeLists.txt`. `include()` rather than `add_subdirectory()`
# on purpose: one directory scope keeps every source path in this file relative to
# `native/`, which is what the file's thousands of `src/...` and `tools/...` entries
# assume. The modules are a way to read the build one part at a time; when the
# sources move into per-module directories they can become subdirectories.
#
# The order matters and is the order in `CMakeLists.txt`: a library before what links
# it.
#
# The command-line tools that need no Qt: the run report the gates drive, the gates
# themselves, and the icon generator.

# ---------------------------------------------------------------------------
# The run report the gates drive
# ---------------------------------------------------------------------------
#
# `time_warp_check` runs one program through the native engines and says what it
# did. The example, command and REPL harnesses used to ask `native/bridge/worker.py`
# instead, which made a Python interpreter part of this repository's test suite.
# The engines are the same ones the IDE runs, so a question about what a program
# did is answered by running it here rather than by asking a second interpreter.

add_executable(time_warp_check tools/check.cpp)
target_link_libraries(time_warp_check PRIVATE ${TIME_WARP_QT}::Core timewarp::lang timewarp::scheme)
timewarp_enable_warnings(time_warp_check)

# ---------------------------------------------------------------------------
# The gates
# ---------------------------------------------------------------------------
#
# The harnesses that ask the built binaries what each program did: the example
# corpus, the command table, the same rows through the REPL, and the frozen
# reference corpora. They were five Python scripts until 22.0.0; the questions are
# the same and they are asked here, in the language the answers come from.
#
# `TIME_WARP_SOURCE_DIR` is compiled in so a build tree outside the repository
# still finds the checkout it was configured from -- the row data, the examples and
# the corpora all live there.

add_executable(time_warp_gates
    tools/gates/main.cpp
    tools/gates/locate.cpp
    tools/gates/report.cpp
    tools/gates/runner.cpp
    tools/gates/judge.cpp
    tools/gates/smoke.cpp
    tools/gates/commands.cpp
    tools/gates/repl.cpp
    tools/gates/golden.cpp
    tools/gates/inventory.cpp
    tools/gates/tree.cpp
    tools/gates/registry.cpp
    tools/gates/structure.cpp
)
# The gates read `timewarp/lang/registry.hpp` for the language table -- the header is
# a constexpr table with no library behind it, so this is an include path, not a link.
target_include_directories(time_warp_gates PRIVATE tools/gates include)
target_compile_definitions(time_warp_gates PRIVATE
    TIME_WARP_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/..")
find_package(ZLIB REQUIRED)
target_link_libraries(time_warp_gates PRIVATE ${TIME_WARP_QT}::Core ZLIB::ZLIB)
set_target_properties(time_warp_gates PROPERTIES
    AUTOMOC OFF)
timewarp_enable_warnings(time_warp_gates)

# ---------------------------------------------------------------------------
# The icon generator
# ---------------------------------------------------------------------------
#
# `time_warp_icon` draws the application icon and writes the committed asset the
# package carries -- `packaging/linux/time-warp-studio.png`.
# It replaced `Scripts/generate_icon.py`, and it is the one tool here that links
# no Qt at all: its only dependency is zlib, for the CRC-32 and the deflate stream
# a PNG is made of. `--check` compares what it would write with what is on disk so
# a release can assert the committed icons are current.

add_executable(time_warp_icon tools/icon/main.cpp)
# The tools' shared command line, which is a header with no library behind it.
target_include_directories(time_warp_icon PRIVATE include)
target_compile_definitions(time_warp_icon PRIVATE
    TIME_WARP_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/..")
target_link_libraries(time_warp_icon PRIVATE ZLIB::ZLIB)
timewarp_enable_warnings(time_warp_icon)
