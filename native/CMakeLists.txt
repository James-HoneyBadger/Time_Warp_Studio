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

option(TIME_WARP_EXPR_WERROR "Treat compiler warnings as errors" ON)
option(TIME_WARP_EXPR_BUILD_TESTS "Build the test suites" ON)

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
        -Wlogical-op -Wnull-dereference
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
        -Wlogical-op -Wnull-dereference
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
        -Wlogical-op -Wnull-dereference
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
            -Wlogical-op -Wnull-dereference
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
            src/ui/code_editor.cpp
            src/ui/find_bar.cpp
            src/ui/language.cpp
            src/ui/main_window.cpp
            src/ui/panels.cpp
            src/ui/resizable_tab_widget.cpp
            src/ui/syntax_highlighter.cpp
            src/ui/turtle_canvas.cpp
            src/ui/ui_style.cpp
            # Listed so AUTOMOC scans them: the Q_OBJECT classes live in headers
            # outside this target's source directory.
            include/timewarp/ui/code_editor.hpp
            include/timewarp/ui/find_bar.hpp
            include/timewarp/ui/main_window.hpp
            include/timewarp/ui/panels.hpp
            include/timewarp/ui/resizable_tab_widget.hpp
            include/timewarp/ui/syntax_highlighter.hpp
            include/timewarp/ui/turtle_canvas.hpp
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
        # The Python bridge, which is how the five languages without a C++
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
                -Wlogical-op -Wnull-dereference
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
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/../packaging/linux/time-warp-studio.desktop
            DESTINATION ${CMAKE_INSTALL_DATADIR}/applications
        )
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/../packaging/linux/time-warp-studio.png
            DESTINATION ${CMAKE_INSTALL_DATADIR}/icons/hicolor/256x256/apps
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
        endif()
    else()
        message(STATUS "Qt6 Widgets not found: skipping the widget UI")
    endif()
endif()
