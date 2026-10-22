# SPDX-License-Identifier: MIT
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

# The IDE's Windows build carries a resource script -- the icon and the version
# block -- and RC is the language that compiles it. It exists only there, so it
# is enabled only there; `project(LANGUAGES CXX)` does not turn it on.
if(WIN32)
    enable_language(RC)
endif()

if(MSVC)
    # The sources are UTF-8: the output prefixes are emoji and several messages
    # use typographic punctuation. Without /utf-8 MSVC reads the files in the
    # system code page, which turns every one of those characters into a question
    # mark in the window and warns C4819 on fifty-odd files.
    #
    # C++ only: the Windows resource compiler shares the directory's option list
    # and does not understand /utf-8, so handing it one fails the build.
    add_compile_options($<$<COMPILE_LANGUAGE:CXX>:/utf-8>)
endif()

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# A compilation database: clangd and the C/C++ extension resolve the Qt include
# paths from it instead of reporting every QWidget include as missing.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(TIME_WARP_EXPR_WERROR "Treat compiler warnings as errors" ON)
option(TIME_WARP_QT5 "Build against Qt 5.15 (the Windows 7 build) rather than Qt 6" OFF)
option(TIME_WARP_WIN7 "Compile against the Windows 7 API level" OFF)
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
# `-Werror` turns into a failed build -- and Clang is what builds the macOS disk
# image. They are named once here rather than nine times below, so a compiler
# that does not know them simply does not get them.
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(TIME_WARP_GNU_WARNING_FLAGS -Wlogical-op)
    if(NOT TIME_WARP_QT5)
        # Qt 5.15's moc emits `static_cast<_t>(...)` that GCC calls useless, so
        # the flag is kept for the Qt 6 build and dropped for the Windows 7 one.
        list(APPEND TIME_WARP_GNU_WARNING_FLAGS -Wuseless-cast)
    endif()
else()
    set(TIME_WARP_GNU_WARNING_FLAGS "")
endif()

# The Windows 7 build states its own API level. `WINVER` and `_WIN32_WINNT` are
# what the SDK headers test, so a call to anything newer than Windows 7 becomes a
# compile error here instead of a missing entry point on the user's machine --
# which is the only way to be sure without a Windows 7 machine to try it on. The
# subsystem version the linker writes (6.00 by default) is already below the 6.1
# Windows 7 accepts, so nothing has to be forced there.
if(TIME_WARP_WIN7)
    add_compile_definitions(WINVER=0x0601 _WIN32_WINNT=0x0601)
endif()

# ---------------------------------------------------------------------------
# The build, in parts
# ---------------------------------------------------------------------------
#
# Each module is one answer: the Qt-free libraries, the probe that decides which Qt
# this is, the tools that need none, the Qt libraries, and the packages. They are
# included in this order because a library has to be defined before what links it.

include(cmake/version.cmake)
include(cmake/libraries.cmake)
include(cmake/qt.cmake)
include(cmake/tools.cmake)
include(cmake/libraries-qt.cmake)
include(cmake/packaging.cmake)
