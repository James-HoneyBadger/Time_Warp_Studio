# SPDX-License-Identifier: MIT
#
# Standalone build for the native C++20 expression engine extracted from the
# Time Warp Studio Python interpreter.
#
#   cmake -S native -B native/build -DCMAKE_BUILD_TYPE=Release
#   cmake --build native/build -j
#   ctest --test-dir native/build --output-on-failure
#
# Deliberately dependency-free: no gtest, no Catch2, no fmt.  A C++20 compiler
# and CMake 3.20+ are all that is required.

cmake_minimum_required(VERSION 3.20)

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

# ---------------------------------------------------------------------------
# Library
# ---------------------------------------------------------------------------

add_library(timewarp_expr
    src/token.cpp
    src/lexer.cpp
    src/evaluator.cpp
)
add_library(timewarp::expr ALIAS timewarp_expr)

target_include_directories(timewarp_expr
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

target_compile_features(timewarp_expr PUBLIC cxx_std_20)

if(MSVC)
    target_compile_options(timewarp_expr PRIVATE /W4 /permissive-)
else()
    target_compile_options(timewarp_expr PRIVATE
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast
        -Wlogical-op ${TIME_WARP_NULL_DEREFERENCE}
    )
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(timewarp_expr PRIVATE -Werror)
    endif()
endif()

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

if(TIME_WARP_EXPR_BUILD_TESTS)
    enable_testing()

    foreach(suite lexer_tests evaluator_tests)
        add_executable(${suite} tests/${suite}.cpp)
        target_link_libraries(${suite} PRIVATE timewarp::expr)
        target_include_directories(${suite} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(${suite} PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(${suite} PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME ${suite} COMMAND ${suite})
    endforeach()
endif()

# ---------------------------------------------------------------------------
# Differential-test CLI
#
# Evaluates expressions read from stdin so tools/differential_check.py can diff
# the C++ port against the original Python implementation over a corpus.
# ---------------------------------------------------------------------------

add_executable(expr_cli tools/eval_cli.cpp)
target_link_libraries(expr_cli PRIVATE timewarp::expr)
if(NOT MSVC)
    target_compile_options(expr_cli PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(expr_cli PRIVATE -Werror)
    endif()
endif()

# ---------------------------------------------------------------------------
# Scheme evaluator
#
# A second library rather than part of timewarp_expr: the two share nothing but
# the coding conventions, and keeping them separate means the expression engine
# can be embedded without pulling in an interpreter.
# ---------------------------------------------------------------------------

add_library(timewarp_scheme
    src/scheme/value.cpp
    src/scheme/reader.cpp
    src/scheme/printer.cpp
    src/scheme/machine.cpp
    src/scheme/builtins.cpp
)
add_library(timewarp::scheme ALIAS timewarp_scheme)

target_include_directories(timewarp_scheme
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

target_compile_features(timewarp_scheme PUBLIC cxx_std_20)

if(MSVC)
    target_compile_options(timewarp_scheme PRIVATE /W4 /permissive-)
else()
    target_compile_options(timewarp_scheme PRIVATE
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast
        -Wlogical-op ${TIME_WARP_NULL_DEREFERENCE}
    )
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(timewarp_scheme PRIVATE -Werror)
    endif()
endif()

if(TIME_WARP_EXPR_BUILD_TESTS)
    add_executable(scheme_tests tests/scheme_tests.cpp)
    target_link_libraries(scheme_tests PRIVATE timewarp::scheme)
    target_include_directories(scheme_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    if(NOT MSVC)
        target_compile_options(scheme_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(scheme_tests PRIVATE -Werror)
        endif()
    endif()
    add_test(NAME scheme_tests COMMAND scheme_tests)
endif()

# Reads a whole program on stdin and prints its output, so
# tools/scheme_differential_check.py can diff the port against the reference.
add_executable(scheme_cli tools/scheme_cli.cpp)
target_link_libraries(scheme_cli PRIVATE timewarp::scheme)
if(NOT MSVC)
    target_compile_options(scheme_cli PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(scheme_cli PRIVATE -Werror)
    endif()
endif()

# Depth and throughput measurement: every workload here exceeds what the
# reference could run at all.
add_executable(scheme_bench tools/scheme_bench.cpp)
target_link_libraries(scheme_bench PRIVATE timewarp::scheme)
if(NOT MSVC)
    target_compile_options(scheme_bench PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(scheme_bench PRIVATE -Werror)
    endif()
endif()

# ---------------------------------------------------------------------------
# Native language executors
#
# The C++ counterparts of the Python executors in `bridge/time_warp/languages`.
# A language lives here once it is ported; until then the bridge still runs it,
# and `executor_for()` returns null so the caller can tell the difference. This
# is why the library is separate from `timewarp_scheme`: Scheme has its own
# machine, while these share one interpreter loop and one executor interface.
# ---------------------------------------------------------------------------

add_library(timewarp_lang
    src/audio/music.cpp
    src/io/ports.cpp
    src/lang/value.cpp
    src/lang/language.cpp
    src/lang/interpreter.cpp
    src/lang/python_random.cpp
    src/lang/basic.cpp
    src/lang/basic_statements.cpp
    src/lang/basic_features.cpp
    src/lang/basic_library.cpp
    src/lang/basic_text.cpp
    src/lang/chip8.cpp
    src/lang/focal.cpp
    src/lang/pilot.cpp
    src/lang/comal.cpp
    src/lang/apl.cpp
    src/lang/rexx.cpp
    src/lang/smalltalk.cpp
    src/lang/smalltalk_library.cpp
    src/lang/smalltalk_driver.cpp
    src/lang/logo.cpp
)
add_library(timewarp::lang ALIAS timewarp_lang)
# A statement's arithmetic is the ported expression evaluator, not a second
# parser inside every executor.
target_link_libraries(timewarp_lang PUBLIC timewarp::expr)

target_include_directories(timewarp_lang
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)

target_compile_features(timewarp_lang PUBLIC cxx_std_20)

if(MSVC)
    target_compile_options(timewarp_lang PRIVATE /W4 /permissive- /fp:precise)
else()
    target_compile_options(timewarp_lang PRIVATE
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast
        -Wlogical-op ${TIME_WARP_NULL_DEREFERENCE}
        # No fused multiply-add. The reference is CPython, which rounds every
        # multiplication and addition separately; contracting `start + i*step`
        # into one fma changes the last bits of a shape's vertices, and a
        # vertex that is 6e-17 away from where Python puts it is a different
        # drawing. Compiling without contraction is what makes the comparison
        # about behaviour rather than about rounding.
        -ffp-contract=off
    )
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(timewarp_lang PRIVATE -Werror)
    endif()
endif()

if(TIME_WARP_EXPR_BUILD_TESTS)
    add_executable(lang_tests tests/lang_tests.cpp)
    target_link_libraries(lang_tests PRIVATE timewarp::lang)
    target_include_directories(lang_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    if(NOT MSVC)
        target_compile_options(lang_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(lang_tests PRIVATE -Werror)
        endif()
    endif()
    add_test(NAME lang_tests COMMAND lang_tests)
endif()

# One native program in, what it printed and drew out, for the differential
# harness. See tools/lang_differential_check.py.
add_executable(lang_cli tools/lang_cli.cpp)
target_link_libraries(lang_cli PRIVATE timewarp::lang)
if(NOT MSVC)
    target_compile_options(lang_cli PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(lang_cli PRIVATE -Werror)
    endif()
endif()

# ---------------------------------------------------------------------------
# Micro-benchmark
# ---------------------------------------------------------------------------

add_executable(expr_bench tools/bench.cpp)
target_link_libraries(expr_bench PRIVATE timewarp::expr)
if(NOT MSVC)
    target_compile_options(expr_bench PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(expr_bench PRIVATE -Werror)
    endif()
endif()

# ---------------------------------------------------------------------------
# Canvas command queue (Qt-free core)
#
# The bridge between an interpreter thread and the GUI thread.  Deliberately
# Qt-free so it can be unit tested, and so timewarp_canvas stays usable from a
# headless or non-Qt host.
# ---------------------------------------------------------------------------

find_package(Threads REQUIRED)

add_library(timewarp_canvas
    src/canvas/command.cpp
    src/canvas/command_queue.cpp
)
add_library(timewarp::canvas ALIAS timewarp_canvas)

target_include_directories(timewarp_canvas
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)
target_compile_features(timewarp_canvas PUBLIC cxx_std_20)
target_link_libraries(timewarp_canvas PUBLIC Threads::Threads)

if(MSVC)
    target_compile_options(timewarp_canvas PRIVATE /W4 /permissive-)
else()
    target_compile_options(timewarp_canvas PRIVATE
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast
        -Wlogical-op ${TIME_WARP_NULL_DEREFERENCE}
    )
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(timewarp_canvas PRIVATE -Werror)
    endif()
endif()

if(TIME_WARP_EXPR_BUILD_TESTS)
    add_executable(canvas_queue_tests tests/canvas_queue_tests.cpp)
    target_link_libraries(canvas_queue_tests PRIVATE timewarp::canvas)
    target_include_directories(canvas_queue_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    if(NOT MSVC)
        target_compile_options(canvas_queue_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(canvas_queue_tests PRIVATE -Werror)
        endif()
    endif()
    add_test(NAME canvas_queue_tests COMMAND canvas_queue_tests)
endif()

# Throughput and coalescing measurement.
add_executable(canvas_queue_bench tools/canvas_queue_bench.cpp)
target_link_libraries(canvas_queue_bench PRIVATE timewarp::canvas)
if(NOT MSVC)
    target_compile_options(canvas_queue_bench PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    if(TIME_WARP_EXPR_WERROR)
        target_compile_options(canvas_queue_bench PRIVATE -Werror)
    endif()
endif()

# ---------------------------------------------------------------------------
# Qt6 bridge (optional)
#
# Built only when Qt6 is present.  The core queue above never depends on this,
# so a machine without Qt still builds and tests everything else.
# ---------------------------------------------------------------------------

find_package(Qt6 QUIET COMPONENTS Core)

if(Qt6_FOUND)
    message(STATUS "Qt6 ${Qt6Core_VERSION} found: building the canvas bridge")

    add_library(timewarp_canvas_qt
        src/canvas/qt_bridge.cpp
        # Listed so AUTOMOC scans it: the Q_OBJECT class lives in the header,
        # which is outside this target's source directory.
        include/timewarp/canvas/qt_bridge.hpp
    )
    add_library(timewarp::canvas_qt ALIAS timewarp_canvas_qt)

    target_link_libraries(timewarp_canvas_qt PUBLIC timewarp::canvas Qt6::Core)
    set_target_properties(timewarp_canvas_qt PROPERTIES AUTOMOC ON)

    if(MSVC)
        target_compile_options(timewarp_canvas_qt PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_canvas_qt PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_canvas_qt PRIVATE -Werror)
        endif()
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(canvas_bridge_tests tests/canvas_bridge_tests.cpp)
        target_link_libraries(canvas_bridge_tests PRIVATE timewarp::canvas_qt)
        target_include_directories(canvas_bridge_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(canvas_bridge_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(canvas_bridge_tests PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME canvas_bridge_tests COMMAND canvas_bridge_tests)
    endif()
else()
    message(STATUS "Qt6 not found: skipping the canvas bridge (core queue still builds)")
endif()

# ---------------------------------------------------------------------------
# Debugger surface (Qt-free half)
#
# Source maps, resumable execution and the call-stack/variable inspection API all
# live in timewarp_scheme; this target covers them directly.
# ---------------------------------------------------------------------------

if(TIME_WARP_EXPR_BUILD_TESTS)
    add_executable(debug_tests tests/debug_tests.cpp)
    target_link_libraries(debug_tests PRIVATE timewarp::scheme)
    target_include_directories(debug_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    if(NOT MSVC)
        target_compile_options(debug_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(debug_tests PRIVATE -Werror)
        endif()
    endif()
    add_test(NAME debug_tests COMMAND debug_tests)
endif()

# ---------------------------------------------------------------------------
# Background debugger runner (Qt)
#
# A QThread whose run() *is* the interpreter loop, driven by an atomic command
# channel.  Built only where Qt6 exists.
# ---------------------------------------------------------------------------

if(Qt6_FOUND)
    message(STATUS "Qt6 ${Qt6Core_VERSION} found: building the debugger runner")

    add_library(timewarp_debugger
        src/debugger/debugger_runner.cpp
        # Listed so AUTOMOC scans it: the Q_OBJECT class lives in the header,
        # which is outside this target's source directory.
        include/timewarp/debugger/debugger_runner.hpp
    )
    add_library(timewarp::debugger ALIAS timewarp_debugger)

    target_link_libraries(timewarp_debugger PUBLIC timewarp::scheme Qt6::Core)
    set_target_properties(timewarp_debugger PROPERTIES AUTOMOC ON)

    if(MSVC)
        target_compile_options(timewarp_debugger PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_debugger PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_debugger PRIVATE -Werror)
        endif()
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(debugger_runner_tests tests/debugger_runner_tests.cpp)
        target_link_libraries(debugger_runner_tests PRIVATE timewarp::debugger)
        target_include_directories(debugger_runner_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(debugger_runner_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(debugger_runner_tests PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME debugger_runner_tests COMMAND debugger_runner_tests)
    endif()
else()
    message(STATUS "Qt6 not found: skipping the debugger runner")
endif()

# ---------------------------------------------------------------------------
# Python bridge (Qt)
#
# Runs the Python implementations of the languages that have no C++ interpreter
# yet, in a child process, and turns what it draws into canvas commands. Qt6Core
# only: it is a pipe and a JSON parser, and the UI is one of its callers rather
# than its owner.
# ---------------------------------------------------------------------------

if(Qt6_FOUND)
    message(STATUS "Qt6 ${Qt6Core_VERSION} found: building the Python bridge")

    add_library(timewarp_bridge
        src/bridge/python_worker.cpp
        # Listed so AUTOMOC scans it: the Q_OBJECT class lives in the header.
        include/timewarp/bridge/python_worker.hpp
    )
    add_library(timewarp::bridge ALIAS timewarp_bridge)

    target_include_directories(timewarp_bridge
        PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )
    target_link_libraries(timewarp_bridge PUBLIC timewarp::canvas Qt6::Core)
    set_target_properties(timewarp_bridge PROPERTIES AUTOMOC ON)

    if(MSVC)
        target_compile_options(timewarp_bridge PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_bridge PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast
            -Wlogical-op ${TIME_WARP_NULL_DEREFERENCE}
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_bridge PRIVATE -Werror)
        endif()
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(python_bridge_tests tests/python_bridge_tests.cpp)
        target_link_libraries(python_bridge_tests PRIVATE timewarp::bridge)
        target_include_directories(python_bridge_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(python_bridge_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(python_bridge_tests PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME python_bridge_tests COMMAND python_bridge_tests)
        # The suite runs the real worker, which imports the Python package from
        # the repository. PYTHONPATH is passed through the environment rather than
        # hard-coded in the test, so the same suite works from an installed tree.
        set_tests_properties(python_bridge_tests PROPERTIES
            ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_SOURCE_DIR}/bridge"
        )
    endif()
else()
    message(STATUS "Qt6 not found: skipping the Python bridge")
endif()

# ---------------------------------------------------------------------------
# Desktop UI (Qt Widgets)
#
# The main-window layout. Separate from the debugger runner because it needs
# Widgets rather than just Core: a machine with Qt6Core but no Qt6Widgets (a
# headless build host) still gets everything else.
# ---------------------------------------------------------------------------

if(Qt6_FOUND)
    find_package(Qt6 QUIET COMPONENTS Widgets)

    if(Qt6Widgets_FOUND)
        message(STATUS "Qt6 ${Qt6Widgets_VERSION} found: building the widget UI")

        # Some distributions ship Qt built for one configuration only -- Arch's
        # qt6-base 6.12 provides RelWithDebInfo. An imported target with no
        # location for the requested configuration then makes CMake's generate
        # step fail with "IMPORTED_LOCATION or IMPORTED_IMPLIB not set" before
        # anything is compiled. Map the requested configuration to one the
        # package provides rather than making the user change build type.
        get_target_property(_qt_imported_configs Qt6::Gui IMPORTED_CONFIGURATIONS)
        if(_qt_imported_configs AND CMAKE_BUILD_TYPE)
            string(TOUPPER "${CMAKE_BUILD_TYPE}" _qt_wanted_config)
            if(NOT _qt_wanted_config IN_LIST _qt_imported_configs)
                list(GET _qt_imported_configs 0 _qt_fallback_config)
                foreach(_qt_target Qt6::Core Qt6::Gui Qt6::Widgets)
                    if(TARGET ${_qt_target})
                        set_property(TARGET ${_qt_target} PROPERTY
                            MAP_IMPORTED_CONFIG_${_qt_wanted_config} ${_qt_fallback_config})
                    endif()
                endforeach()
                message(STATUS
                    "Qt6 has no ${CMAKE_BUILD_TYPE} configuration; using ${_qt_fallback_config}")
            endif()
        endif()

        # The window title carries the version, and it has to be the same string
        # the packaging scripts use -- so it is read from the repository's VERSION
        # file at configure time rather than duplicated in the source.
        file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION" TIME_WARP_VERSION_RAW)
        string(STRIP "${TIME_WARP_VERSION_RAW}" TIME_WARP_VERSION)
        # `file(READ)` does *not* register a configure dependency the way
        # `configure_file` does, so without this a version bump would not re-run
        # CMake and the window title would keep the old number until somebody
        # deleted the build tree.
        set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
            "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION"
        )

        add_library(timewarp_ui
            src/ui/app.cpp
            src/ui/app_paths.cpp
            src/ui/autosave.cpp
            src/ui/chip8_emulator.cpp
            src/ui/chip8_screen.cpp
            src/ui/chip8_window.cpp
            src/ui/code_editor.cpp
            src/ui/command_palette.cpp
            src/ui/find_bar.cpp
            src/ui/find_in_files.cpp
            src/ui/formatter.cpp
            src/ui/help_browser.cpp
            src/ui/lint.cpp
            src/ui/language.cpp
            src/ui/main_window.cpp
            src/ui/native_runner.cpp
            src/ui/panels.cpp
            src/ui/preferences.cpp
            src/ui/resizable_tab_widget.cpp
            src/ui/scrubber.cpp
            src/ui/snippet_dialog.cpp
            src/ui/snippets.cpp
            src/ui/sound_player.cpp
            src/ui/standalone.cpp
            src/ui/syntax_highlighter.cpp
            src/ui/themes.cpp
            src/ui/tour.cpp
            src/ui/turtle_canvas.cpp
            src/ui/version_history.cpp
            src/ui/ui_style.cpp
            # Listed so AUTOMOC scans them: the Q_OBJECT classes live in headers
            # outside this target's source directory.
            include/timewarp/ui/code_editor.hpp
            include/timewarp/ui/chip8_emulator.hpp
            include/timewarp/ui/chip8_screen.hpp
            include/timewarp/ui/chip8_window.hpp
            include/timewarp/ui/command_palette.hpp
            include/timewarp/ui/find_bar.hpp
            include/timewarp/ui/find_in_files.hpp
            include/timewarp/ui/help_browser.hpp
            include/timewarp/ui/lint.hpp
            include/timewarp/ui/snippet_dialog.hpp
            include/timewarp/ui/main_window.hpp
            include/timewarp/ui/native_runner.hpp
            include/timewarp/ui/panels.hpp
            include/timewarp/ui/preferences.hpp
            include/timewarp/ui/resizable_tab_widget.hpp
            include/timewarp/ui/scrubber.hpp
            include/timewarp/ui/sound_player.hpp
            include/timewarp/ui/standalone.hpp
            include/timewarp/ui/syntax_highlighter.hpp
            include/timewarp/ui/themes.hpp
            include/timewarp/ui/tour.hpp
            include/timewarp/ui/turtle_canvas.hpp
            include/timewarp/ui/version_history.hpp
            include/timewarp/ui/ui_style.hpp
        )
        add_library(timewarp::ui ALIAS timewarp_ui)

        target_include_directories(timewarp_ui
            PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        )
        target_compile_features(timewarp_ui PUBLIC cxx_std_20)
        target_link_libraries(timewarp_ui PUBLIC Qt6::Widgets)
        # The window owns the execution pipeline: the interpreter runs on a
        # `DebuggerRunner` thread, pushes turtle commands into the canvas queue,
        # and `CanvasBridge` drains them on the GUI thread. Guarded because the
        # canvas bridge and the debugger are only built when their own Qt
        # components were found.
        if(TARGET timewarp::canvas_qt)
            target_link_libraries(timewarp_ui PRIVATE timewarp::canvas_qt)
        endif()
        if(TARGET timewarp::debugger)
            target_link_libraries(timewarp_ui PRIVATE timewarp::debugger)
        endif()
        # The ported languages, in process. Built unconditionally: it has no
        # dependency beyond the standard library.
        target_link_libraries(timewarp_ui PRIVATE timewarp::lang)
        # The Python bridge, which is how the four languages without a C++
        # interpreter run at all.
        if(TARGET timewarp::bridge)
            target_link_libraries(timewarp_ui PRIVATE timewarp::bridge)
        endif()
        target_compile_definitions(timewarp_ui PRIVATE
            TIME_WARP_VERSION="${TIME_WARP_VERSION}"
        )
        set_target_properties(timewarp_ui PROPERTIES AUTOMOC ON)

        if(MSVC)
            target_compile_options(timewarp_ui PRIVATE /W4 /permissive-)
        else()
            target_compile_options(timewarp_ui PRIVATE
                -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual -Wuseless-cast
                -Wlogical-op ${TIME_WARP_NULL_DEREFERENCE}
            )
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(timewarp_ui PRIVATE -Werror)
            endif()
        endif()

        # The application, not a harness: it shows the window, opens the files it
        # is given, and can render itself to a PNG for the layout checks.
        add_executable(time_warp_ide tools/time_warp_ide.cpp)
        target_link_libraries(time_warp_ide PRIVATE timewarp::ui)
        if(NOT MSVC)
            target_compile_options(time_warp_ide PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(time_warp_ide PRIVATE -Werror)
            endif()
        endif()

        # ---- install ------------------------------------------------------
        # Three things have to arrive together for an installed tree to work:
        #   bin/time_warp_ide                 the application
        #   share/time_warp/                  the bridge, which is where
        #                                     locateWorker() looks for it
        #                                     (`../share/time_warp` relative to
        #                                     the binary), so an installed build
        #                                     needs no path configuration
        #   share/applications + share/icons  the desktop entry
        include(GNUInstallDirs)
        install(TARGETS time_warp_ide RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
        install(
            DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bridge/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/time_warp
            PATTERN "__pycache__" EXCLUDE
            PATTERN "*.pyc" EXCLUDE
            PATTERN ".ruff_cache" EXCLUDE
            PATTERN ".mypy_cache" EXCLUDE
            PATTERN ".pytest_cache" EXCLUDE
            PATTERN "tests" EXCLUDE
        )
        # The snippet catalogue: `snippetRoot()` looks here first, so a package
        # and a build tree offer the same snippets.
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/share/snippets.json
            DESTINATION ${CMAKE_INSTALL_DATADIR}/time-warp-studio
        )
        # The documentation browser reads these. `docsRoot()` looks in
        # `share/doc/time-warp-studio` first, so a package and a checkout show the
        # same pages; without this rule an installed help window would explain
        # that the documentation is missing.
        install(
            DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../docs/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/doc/time-warp-studio
        )
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/../packaging/linux/time-warp-studio.desktop
            DESTINATION ${CMAKE_INSTALL_DATADIR}/applications
        )
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/../packaging/linux/time-warp-studio.png
            DESTINATION ${CMAKE_INSTALL_DATADIR}/icons/hicolor/256x256/apps
        )
        # The example programs travel with the install tree rather than beside
        # the tarball, because a package has no second place to put them and a
        # fresh install that opens onto an empty window is a poor first
        # impression. This is the directory File > Open Example reads.
        # The trailing slash matters: `DIRECTORY Examples` installs the directory
        # itself, giving `examples/Examples/logo`, while `Examples/` installs its
        # contents and gives `examples/logo`, which is the shape the menu reads.
        install(
            DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../Examples/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/time-warp-studio/examples
            PATTERN "__pycache__" EXCLUDE
        )

        if(TIME_WARP_EXPR_BUILD_TESTS)
            add_executable(ui_layout_tests tests/ui_layout_tests.cpp)
            target_link_libraries(ui_layout_tests PRIVATE timewarp::ui)
            target_include_directories(ui_layout_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(ui_layout_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(ui_layout_tests PRIVATE -Werror)
                endif()
            endif()
            # The test sets QT_QPA_PLATFORM=offscreen itself, so it runs with no
            # display and belongs in the default CTest set.
            add_test(NAME ui_layout_tests COMMAND ui_layout_tests)

            # The document lifecycle gets its own binary: it is about what happens
            # to a user's file, not about where the widgets sit, and the two fail
            # for unrelated reasons.
            add_executable(ui_document_tests tests/ui_document_tests.cpp)
            target_link_libraries(ui_document_tests PRIVATE timewarp::ui)
            target_include_directories(ui_document_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(ui_document_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(ui_document_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME ui_document_tests COMMAND ui_document_tests)

            # The canvas is a separate binary from the documents because it has
            # no window: it is a model and a painter, and testing the model
            # without constructing a MainWindow keeps a failure attributable.
            add_executable(turtle_canvas_tests tests/turtle_canvas_tests.cpp)
            target_link_libraries(turtle_canvas_tests PRIVATE timewarp::ui)
            target_include_directories(turtle_canvas_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(turtle_canvas_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(turtle_canvas_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME turtle_canvas_tests COMMAND turtle_canvas_tests)

            # Syntax highlighting is a document transform, not a widget: it is
            # tested against a bare QTextDocument so a failure names the rule
            # that broke rather than showing up as "the colours look wrong".
            # Sound and ports: arithmetic and a pseudo-terminal, so both halves
            # run on a machine with no speaker and no board attached.
            add_executable(media_tests tests/media_tests.cpp)
            target_link_libraries(media_tests PRIVATE timewarp::lang)
            target_include_directories(media_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(media_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(media_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME media_tests COMMAND media_tests)

            add_executable(syntax_highlighter_tests tests/syntax_highlighter_tests.cpp)
            target_link_libraries(syntax_highlighter_tests PRIVATE timewarp::ui)
            target_include_directories(syntax_highlighter_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(syntax_highlighter_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(syntax_highlighter_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME syntax_highlighter_tests COMMAND syntax_highlighter_tests)

            # The IDE's text services: formatting a document, and (as they land)
            # the snippet library, find-in-files and autosave. One binary because
            # they share a concern -- text in, text out, no program run -- and
            # none of them needs a window.
            add_executable(editor_services_tests tests/editor_services_tests.cpp)
            target_link_libraries(editor_services_tests PRIVATE timewarp::ui)
            target_include_directories(editor_services_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(editor_services_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(editor_services_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME editor_services_tests COMMAND editor_services_tests)

            # The live CHIP-8 machine and its window: frames, timers, the
            # keypad that changes while a program runs, pause/step and the
            # controls. The instruction semantics stay in lang_tests.
            add_executable(chip8_emulator_tests tests/chip8_emulator_tests.cpp)
            target_link_libraries(chip8_emulator_tests PRIVATE timewarp::ui)
            target_include_directories(chip8_emulator_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            # The bundled-program test reads the checkout's Examples/ whatever
            # directory CMake put the build in.
            target_compile_definitions(chip8_emulator_tests PRIVATE
                TIME_WARP_TEST_EXAMPLES="${CMAKE_CURRENT_SOURCE_DIR}/../Examples")
            if(NOT MSVC)
                target_compile_options(chip8_emulator_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(chip8_emulator_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME chip8_emulator_tests COMMAND chip8_emulator_tests)

            # The help browser is a data-driven document view: the resolver,
            # the index parser and the tree are all testable without a window,
            # and the dead-link check is the reason it has a suite at all.
            add_executable(help_browser_tests tests/help_browser_tests.cpp)
            target_link_libraries(help_browser_tests PRIVATE timewarp::ui)
            target_include_directories(help_browser_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(help_browser_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(help_browser_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME help_browser_tests COMMAND help_browser_tests)
        endif()
    else()
        message(STATUS "Qt6 Widgets not found: skipping the widget UI")
    endif()
endif()

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------
#
# `cpack` in the build directory writes a `.deb`; `cpack -G RPM` also writes an
# `.rpm` where `rpmbuild` is installed. `release.yml` runs both, builds an
# AppImage beside them with `linuxdeploy` (the Qt bundling an AppImage needs is a
# tool rather than a CPack generator), and attaches the three to the release.
#
# The packages install into `/usr`, not into the build's prefix: a `.deb` that
# puts a binary in `/usr/local/bin` is a `.deb` whose files no package manager
# owns. Everything the application needs at run time is already installed by the
# `install()` rules above -- the binary, the bridge, the snippets, the docs, the
# desktop entry and the icon -- so the package is simply those paths.
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION" TIME_WARP_PACKAGE_VERSION_RAW)
string(STRIP "${TIME_WARP_PACKAGE_VERSION_RAW}" TIME_WARP_PACKAGE_VERSION)

set(CPACK_PACKAGE_NAME "time-warp-studio")
set(CPACK_PACKAGE_VENDOR "James Temple")
set(CPACK_PACKAGE_CONTACT "James Temple <james@honey-badger.org>")
set(CPACK_PACKAGE_VERSION "${TIME_WARP_PACKAGE_VERSION}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "Educational IDE for ten programming languages, with turtle graphics")
set(CPACK_PACKAGE_DESCRIPTION
    "Time Warp Studio teaches BASIC, APL, Logo, PILOT, FOCAL, Scheme, CHIP-8, \
COMAL, REXX and Smalltalk in one window, each with an integrated turtle-graphics \
canvas. Every language runs natively in C++; the bundled Python engine is the \
reference the lesson check uses.")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/James-HoneyBadger/Time_Warp_Studio")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE")

# `python3` is not optional: the Lessons panel runs the bundled engine, which is
# Python. The Qt packages are the libraries the binary links against plus the
# platform plugins it loads at run time -- without the plugins the application
# starts and immediately reports that it cannot load a platform plugin.
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "James Temple <james@honey-badger.org>")
set(CPACK_DEBIAN_PACKAGE_SECTION "education")
set(CPACK_DEBIAN_PACKAGE_DEPENDS
    "libqt6widgets6, libqt6gui6, libqt6core6, qt6-qpa-plugins, python3")
set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
# Named explicitly rather than discovered: CPackDeb asks `dpkg` for the
# architecture and falls back to i386 when it is absent, which is how a build on a
# machine without dpkg produced a `.deb` whose name has no architecture in it.
if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64|amd64")
    set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
    set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "arm64")
endif()

set(CPACK_RPM_PACKAGE_LICENSE "MIT")
set(CPACK_RPM_PACKAGE_GROUP "Education")
set(CPACK_RPM_PACKAGE_REQUIRES "qt6-qtbase-gui, python3")
# The same pin as the DEB's, for a sharper reason: CPackRPM asks rpmbuild for
# `%{_target_cpu}` and, when the answer does not come back, writes the *system
# name* instead -- which is how a release produced
# `time-warp-studio-16.0.0-Linux.rpm`, an architecture no rpm-based system has.
# The architecture also becomes rpmbuild's `--target`, so it is not only a file
# name.
if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64|amd64")
    set(CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
    set(CPACK_RPM_PACKAGE_ARCHITECTURE "aarch64")
endif()
# Named rather than left to the default, so the file name in the release notes
# and the file on the release page cannot drift apart.
set(CPACK_RPM_PACKAGE_RELEASE "1")
# And *this* one is what makes the architecture appear in the name at all. Unless
# `CPACK_RPM_FILE_NAME` is the literal string `RPM-DEFAULT`, CPackRPM falls back
# to the old format -- `${CPACK_PACKAGE_FILE_NAME}.rpm`, and CPack builds that
# from the *system name*, hence `time-warp-studio-16.0.0-Linux.rpm`. With
# RPM-DEFAULT it stops pinning a name and lets rpmbuild's own
# `<name>-<version>-<release>.<arch>.rpm` rule apply, where the architecture is
# the one pinned above.
set(CPACK_RPM_FILE_NAME "RPM-DEFAULT")

# DEB only by default: `rpmbuild` is not installed everywhere, and a generator
# that fails makes `cpack` fail for somebody who only wanted the deb.
set(CPACK_GENERATOR "DEB")

include(CPack)
