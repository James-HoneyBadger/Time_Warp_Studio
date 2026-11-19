# SPDX-License-Identifier: AGPL-3.0-only
#
# Standalone build for the Time Warp Studio native engines, gates and Qt
# application. No Python is involved in building or running any of it.
#
#   cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
#   cmake --build native/build -j
#   ctest --test-dir native/build --output-on-failure
#
# Deliberately dependency-free: no gtest, no Catch2, no fmt.  A C++20 compiler
# and CMake 3.20+ are all that is required.

cmake_minimum_required(VERSION 3.20)

# The warning policy and the suite macro, used by every target below. Kept as a
# module rather than inline because it is one fact -- what "warnings on" means here --
# and it used to be repeated at each of the 37 targets.
include(cmake/warnings.cmake)

# The WebAssembly target, off by default. Its toolchain has to be chosen before
# `project()` enables a language, which is why the option and the search for the
# toolchain sit above that call rather than with the other build parts below. A
# desktop configure reads four lines and a `return()`.
option(TW_BUILD_WASM "Build the IDE for Qt for WebAssembly (browser) instead of the desktop" OFF)
include(cmake/wasm-toolchain.cmake)

# The CMake project version of this target set. It is *not* the product version: the
# product version is read from the repository's VERSION file further down and compiled
# into the UI as TIME_WARP_VERSION, so the window title and the packaged release cannot
# disagree. Do not set this to the release number expecting the About box to follow.
project(timewarp_expr
    VERSION 1.0.0
    DESCRIPTION "Time Warp Studio expression lexer and evaluator (C++20 port)"
    LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# A compilation database: clangd and the C/C++ extension resolve the Qt include
# paths from it instead of reporting every QWidget include as missing.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(TIME_WARP_EXPR_WERROR "Treat compiler warnings as errors" ON)

# Binary hardening for a shipped build. Ubuntu's toolchain gives PIE by default
# and nothing else, so a release without these is a release whose only protection
# against a memory bug in an interpreter is that nobody wrote one. `_FORTIFY_SOURCE`
# needs an optimising build, which is why the block skips Debug rather than applying
# to every configuration; `-Wl,-z,now` resolves every symbol at load, which is what makes
# a RELRO segment actually read-only. The browser build's wasm toolchain knows
# none of these flags, which is what the `TW_BUILD_WASM` guard excludes, and
# `release.yml` verifies the result with `readelf`, so this block and that gate
# are one decision in two places.
if(NOT TW_BUILD_WASM AND NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_compile_options(
        -fstack-protector-strong
        -fPIE
        -D_FORTIFY_SOURCE=3
        -fno-plt
    )
    add_link_options(-pie -Wl,-z,relro,-z,now)
endif()
option(TIME_WARP_EXPR_BUILD_TESTS "Build the test suites" ON)

# `-Wnull-dereference` is dropped for GCC before 14: GCC 13 reports a false
# positive inside libstdc++'s own `__shared_ptr` copy constructor for every
# `std::vector<std::shared_ptr<T>>::insert`, which is a thing this code does
# deliberately (the Smalltalk AST holds shared nodes). Nothing at the call site
# can silence it, and removing the flag does not leave a real null dereference
# unwatched: the AddressSanitizer and UndefinedBehaviorSanitizer jobs treat one as
# a crash rather than as a warning, and they run on every push.
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_VERSION VERSION_LESS 14)
    set(TIME_WARP_NULL_DEREFERENCE "")
else()
    set(TIME_WARP_NULL_DEREFERENCE "-Wnull-dereference")
endif()

# Two of the flags below are GCC's own and Clang rejects them as unknown, which
# `-Werror` turns into a failed build. They are named once here rather than nine
# times below, so a compiler that does not know them simply does not get them.
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(TIME_WARP_GNU_WARNING_FLAGS -Wlogical-op -Wuseless-cast)
else()
    set(TIME_WARP_GNU_WARNING_FLAGS "")
endif()

# ---------------------------------------------------------------------------
# The build, in parts
# ---------------------------------------------------------------------------
#
# Each module is one answer: the Qt-free libraries, the Qt probe, the tools that
# need no Qt, the Qt libraries, and the packages. They are included in this order
# because a library has to be defined before what links it.

include(cmake/version.cmake)
# Before the libraries: its memory figures are read when the executable is finalised,
# and a variable set afterwards would arrive too late.
include(cmake/wasm.cmake)
include(cmake/libraries.cmake)
include(cmake/qt.cmake)
# The command-line tools and their gates are host programs: a browser has no command
# line, and each one would link the engines into another module the page never loads.
if(NOT TW_BUILD_WASM)
    include(cmake/tools.cmake)
endif()
include(cmake/libraries-qt.cmake)
include(cmake/packaging.cmake)
