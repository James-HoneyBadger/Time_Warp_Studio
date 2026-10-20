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

# The IDE's Windows build carries a resource script -- the icon and the version
# block -- and RC is the language that compiles it. It exists only there, so it
# is enabled only there; `project(LANGUAGES CXX)` does not turn it on.
if(WIN32)
    enable_language(RC)
endif()

if(APPLE)
    # Qt 6.8 and 6.9 link `-framework AGL` on every Apple build: their shipped
    # lib/cmake/Qt6/FindWrapOpenGL.cmake asks find_library for AGL and, when the
    # SDK has none, falls back to the literal flag. macOS 26's SDK has none, so
    # every executable fails to link -- "ld: framework 'AGL' not found" -- before
    # any of this project's code is involved. Qt 6.10 stopped doing it.
    #
    # Answering that lookup with OpenGL.framework keeps the link line valid and
    # costs nothing: AGL was OpenGL's legacy entry point, Qt 6 never calls it, and
    # the answer is only given when the SDK genuinely has no AGL -- on a macOS 12
    # to 15 SDK the real framework is found and Qt links exactly what it shipped.
    find_library(TIME_WARP_AGL_FRAMEWORK NAMES AGL)
    if(NOT TIME_WARP_AGL_FRAMEWORK)
        find_library(TIME_WARP_OPENGL_FRAMEWORK NAMES OpenGL)
        if(TIME_WARP_OPENGL_FRAMEWORK)
            set(WrapOpenGL_AGL "${TIME_WARP_OPENGL_FRAMEWORK}" CACHE FILEPATH
                "Stand-in for the AGL framework macOS 26 removed")
        endif()
    endif()
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
option(TIME_WARP_EXPR_BUILD_TESTS "Build the test suites" ON)

# ---------------------------------------------------------------------------
# Product version
# ---------------------------------------------------------------------------
# Read here rather than at the packaging section, because three things need it
# before it: the Windows version resource, the macOS bundle's Info.plist and
# CPack's own version. It is the repository's VERSION file and not the CMake
# `project()` version, because the window title and the packaged release must
# not be able to disagree.
file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION" TIME_WARP_PACKAGE_VERSION_RAW)
string(STRIP "${TIME_WARP_PACKAGE_VERSION_RAW}" TIME_WARP_PACKAGE_VERSION)
set(TIME_WARP_VERSION "${TIME_WARP_PACKAGE_VERSION}")

# `file(READ)` does *not* register a configure dependency the way
# `configure_file` does, so without this a version bump would not re-run CMake
# and the window title would keep the old number until somebody deleted the
# build tree.
set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION"
)

# The Windows resource wants four numeric fields; a version missing a component
# (`17.0`) still has to produce a valid FILEVERSION, so absent parts are zero.
string(REPLACE "." ";" _time_warp_version_parts "${TIME_WARP_PACKAGE_VERSION}")
list(LENGTH _time_warp_version_parts _time_warp_version_count)
foreach(_time_warp_index RANGE 0 3)
    math(EXPR _time_warp_field "${_time_warp_index} + 1")
    if(_time_warp_version_count GREATER _time_warp_index)
        list(GET _time_warp_version_parts ${_time_warp_index} _time_warp_part)
    else()
        set(_time_warp_part 0)
    endif()
    set(TIME_WARP_VERSION_PART_${_time_warp_field} "${_time_warp_part}")
endforeach()
set(TIME_WARP_VERSION_COMMA
    "${TIME_WARP_VERSION_PART_1},${TIME_WARP_VERSION_PART_2},${TIME_WARP_VERSION_PART_3},${TIME_WARP_VERSION_PART_4}")
set(TIME_WARP_VERSION_DOTTED
    "${TIME_WARP_VERSION_PART_1}.${TIME_WARP_VERSION_PART_2}.${TIME_WARP_VERSION_PART_3}.${TIME_WARP_VERSION_PART_4}")

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
    set(TIME_WARP_GNU_WARNING_FLAGS -Wuseless-cast -Wlogical-op)
else()
    set(TIME_WARP_GNU_WARNING_FLAGS "")
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
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
        ${TIME_WARP_NULL_DEREFERENCE}
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
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
        ${TIME_WARP_NULL_DEREFERENCE}
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
    src/lang/debug.cpp
    src/lang/watch.cpp
    src/lang/repl.cpp
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
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
        ${TIME_WARP_NULL_DEREFERENCE}
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
        -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
        ${TIME_WARP_NULL_DEREFERENCE}
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
# SQL (Qt Core + SQLite)
#
# SQLite is embedded, so a learner's database is a file rather than a server.
# The library is required: the Database tab and every language's `SQL` statement
# are part of the product now, and a build without them is a build missing a
# documented feature.
# ---------------------------------------------------------------------------

if(Qt6_FOUND)
    find_package(SQLite3 REQUIRED)
    # FindSQLite3 called its imported target SQLite::SQLite3 until CMake 4.3,
    # when SQLite3::SQLite3 arrived and the old spelling was deprecated. Follow
    # whichever this CMake provides: 3.20 (the project's floor) has only the
    # first, and a 4.4 build should not print an author warning on every
    # configure for a name we chose years ago.
    if(TARGET SQLite3::SQLite3)
        set(TIME_WARP_SQLITE_TARGET SQLite3::SQLite3)
    else()
        set(TIME_WARP_SQLITE_TARGET SQLite::SQLite3)
    endif()
    add_library(timewarp_sql
        src/sql/database.cpp
        include/timewarp/sql/database.hpp
    )
    add_library(timewarp::sql ALIAS timewarp_sql)

    target_include_directories(timewarp_sql
        PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )
    target_compile_features(timewarp_sql PUBLIC cxx_std_20)
    target_link_libraries(timewarp_sql PUBLIC Qt6::Core ${TIME_WARP_SQLITE_TARGET})

    if(MSVC)
        target_compile_options(timewarp_sql PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_sql PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
            ${TIME_WARP_NULL_DEREFERENCE}
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_sql PRIVATE -Werror)
        endif()
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(sql_tests tests/sql_tests.cpp)
        target_link_libraries(sql_tests PRIVATE timewarp::sql)
        target_include_directories(sql_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(sql_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(sql_tests PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME sql_tests COMMAND sql_tests)
    endif()
endif()

# ---------------------------------------------------------------------------
# Projects (Qt Core)
#
# A project is a JSON view over programs that already exist, so it needs no
# widget and no engine: Qt Core's JSON and file classes are the whole
# dependency. The panel that draws one lives in timewarp_ui.
# ---------------------------------------------------------------------------

if(Qt6_FOUND)
    add_library(timewarp_project
        src/project/project.cpp
        include/timewarp/project/project.hpp
    )
    add_library(timewarp::project ALIAS timewarp_project)

    target_include_directories(timewarp_project
        PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )
    target_compile_features(timewarp_project PUBLIC cxx_std_20)
    target_link_libraries(timewarp_project PUBLIC Qt6::Core)

    if(MSVC)
        target_compile_options(timewarp_project PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_project PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
            ${TIME_WARP_NULL_DEREFERENCE}
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_project PRIVATE -Werror)
        endif()
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(project_tests tests/project_tests.cpp)
        target_link_libraries(project_tests PRIVATE timewarp::project)
        target_include_directories(project_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(project_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(project_tests PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME project_tests COMMAND project_tests)
    endif()
else()
    message(STATUS "Qt6 not found: skipping the project model")
endif()

# ---------------------------------------------------------------------------
# Automation API (Qt Core)
#
# JSON-RPC over newline-delimited JSON. The library has no transport of its own:
# `time_warp_api` supplies stdin/stdout, and a test supplies string streams. That
# is what lets the IDE, a plugin and a test share one implementation.
# ---------------------------------------------------------------------------

if(Qt6_FOUND AND TARGET timewarp::sql)
    add_library(timewarp_api
        src/api/server.cpp
        src/api/debug.cpp
        include/timewarp/api/server.hpp
        include/timewarp/api/debug.hpp
        include/timewarp/graphics/recording_host.hpp
    )
    add_library(timewarp::api ALIAS timewarp_api)
    target_include_directories(timewarp_api
        PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )
    target_compile_features(timewarp_api PUBLIC cxx_std_20)
    target_link_libraries(timewarp_api PUBLIC
        Qt6::Core timewarp::lang timewarp::scheme timewarp::sql timewarp::project
    )
    # The debugger API drives the same Scheme runner the IDE's Debug panel does.
    if(TARGET timewarp::debugger)
        target_link_libraries(timewarp_api PRIVATE timewarp::debugger)
    endif()
    # The version file is read later in this block for the window title, so the
    # API target reads it here rather than depending on the order of the file.
    # Declared as a configure dependency too: `file(READ)` alone does not make CMake
    # re-run when the file changes, so `make` after a release bump would quietly
    # keep compiling the old number into the binary.
    set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
                 "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION")
    file(READ "${CMAKE_CURRENT_SOURCE_DIR}/../VERSION" TIME_WARP_API_VERSION_RAW)
    string(STRIP "${TIME_WARP_API_VERSION_RAW}" TIME_WARP_API_VERSION)
    target_compile_definitions(timewarp_api PRIVATE
        TIME_WARP_VERSION="${TIME_WARP_API_VERSION}"
    )
    if(MSVC)
        target_compile_options(timewarp_api PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_api PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
            ${TIME_WARP_NULL_DEREFERENCE}
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_api PRIVATE -Werror)
        endif()
    endif()

    add_executable(time_warp_api tools/time_warp_api.cpp)
    target_link_libraries(time_warp_api PRIVATE timewarp::api)
    if(NOT MSVC)
        target_compile_options(time_warp_api PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(api_tests tests/api_tests.cpp)
        target_link_libraries(api_tests PRIVATE timewarp::api)
        target_include_directories(api_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        if(NOT MSVC)
            target_compile_options(api_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(api_tests PRIVATE -Werror)
            endif()
        endif()
        add_test(NAME api_tests COMMAND api_tests)
    endif()
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
            -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
            ${TIME_WARP_NULL_DEREFERENCE}
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

        # Two components are wanted but not required. `Svg` is QSvgGenerator, for
        # vector export; `PrintSupport` is QPrinter and QPrintDialog, for the two
        # Print entries. A build without either still runs: the format and the
        # entries are absent rather than broken, and the save dialog and the
        # `--export` help do not name what the build cannot do.
        find_package(Qt6 QUIET COMPONENTS Svg PrintSupport)

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

        # The window title carries the version read at the top of this file, from
        # the repository's VERSION, so the title and the packages cannot drift.

        add_library(timewarp_ui
            src/ui/app.cpp
            src/ui/app_paths.cpp
            src/ui/benchmark.cpp
            src/ui/autosave.cpp
            src/ui/breakpoints.cpp
            src/ui/chip8_emulator.cpp
            src/ui/chip8_library.cpp
            src/ui/chip8_screen.cpp
            src/ui/chip8_window.cpp
            src/ui/code_editor.cpp
            src/ui/command_palette.cpp
            src/ui/export.cpp
            src/ui/find_bar.cpp
            src/ui/find_in_files.cpp
            src/ui/formatter.cpp
            src/ui/gif_encoder.cpp
            src/ui/hb_dos.cpp
            src/ui/help_browser.cpp
            src/ui/lint.cpp
            src/ui/language.cpp
            src/ui/main_window.cpp
            src/ui/native_runner.cpp
            src/ui/outline.cpp
            src/ui/panels.cpp
            src/ui/preferences.cpp
            src/ui/profile_report.cpp
            src/ui/project_panel.cpp
            src/ui/repl_runner.cpp
            src/ui/resizable_tab_widget.cpp
            src/ui/retro_computer.cpp
            src/ui/retro_settings.cpp
            src/ui/scrubber.cpp
            src/ui/snippet_dialog.cpp
            src/ui/snippets.cpp
            src/ui/sound_player.cpp
            src/ui/sql_panel.cpp
            src/ui/standalone.cpp
            src/ui/syntax_highlighter.cpp
            src/ui/terminal_widget.cpp
            src/ui/themes.cpp
            src/ui/tour.cpp
            src/ui/turtle_canvas.cpp
            src/ui/version_history.cpp
            src/ui/ui_style.cpp
            # Listed so AUTOMOC scans them: the Q_OBJECT classes live in headers
            # outside this target's source directory.
            include/timewarp/ui/benchmark.hpp
            include/timewarp/ui/breakpoints.hpp
            include/timewarp/ui/code_editor.hpp
            include/timewarp/ui/chip8_emulator.hpp
            include/timewarp/ui/chip8_library.hpp
            include/timewarp/ui/chip8_screen.hpp
            include/timewarp/ui/chip8_window.hpp
            include/timewarp/ui/command_palette.hpp
            include/timewarp/ui/export.hpp
            include/timewarp/ui/find_bar.hpp
            include/timewarp/ui/find_in_files.hpp
            include/timewarp/ui/hb_dos.hpp
            include/timewarp/ui/help_browser.hpp
            include/timewarp/ui/lint.hpp
            include/timewarp/ui/snippet_dialog.hpp
            include/timewarp/ui/main_window.hpp
            include/timewarp/ui/native_runner.hpp
            include/timewarp/ui/panels.hpp
            include/timewarp/ui/preferences.hpp
            include/timewarp/ui/profile_report.hpp
            include/timewarp/ui/project_panel.hpp
            include/timewarp/ui/repl_runner.hpp
            include/timewarp/ui/resizable_tab_widget.hpp
            include/timewarp/ui/retro_computer.hpp
            include/timewarp/ui/retro_settings.hpp
            include/timewarp/ui/scrubber.hpp
            include/timewarp/ui/sound_player.hpp
            include/timewarp/ui/sql_panel.hpp
            include/timewarp/ui/standalone.hpp
            include/timewarp/ui/syntax_highlighter.hpp
            include/timewarp/ui/terminal_widget.hpp
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
        # Projects: a JSON model over files, no widget and no engine.
        target_link_libraries(timewarp_ui PRIVATE timewarp::project)
        target_link_libraries(timewarp_ui PRIVATE timewarp::sql)
        # The Python bridge, which is how the four languages without a C++
        # interpreter run at all.
        if(TARGET timewarp::bridge)
            target_link_libraries(timewarp_ui PRIVATE timewarp::bridge)
        endif()
        target_compile_definitions(timewarp_ui PRIVATE
            TIME_WARP_VERSION="${TIME_WARP_VERSION}"
        )
        # Public because the save-dialog filter and the `--export` help text live
        # in the executable and the tests, and they must not offer a format this
        # build cannot write.
        if(Qt6Svg_FOUND)
            target_link_libraries(timewarp_ui PUBLIC Qt6::Svg)
            target_compile_definitions(timewarp_ui PUBLIC TIME_WARP_HAVE_SVG=1)
            message(STATUS "Qt6 Svg ${Qt6Svg_VERSION} found: SVG drawing export enabled")
        else()
            message(STATUS "Qt6 Svg not found: SVG drawing export disabled")
        endif()
        # Public for the same reason as Svg: the menu entries and the tests both
        # have to know whether this build can print.
        if(Qt6PrintSupport_FOUND)
            target_link_libraries(timewarp_ui PUBLIC Qt6::PrintSupport)
            target_compile_definitions(timewarp_ui PUBLIC TIME_WARP_HAVE_PRINT=1)
            message(STATUS "Qt6 PrintSupport ${Qt6PrintSupport_VERSION} found: printing enabled")
        else()
            message(STATUS "Qt6 PrintSupport not found: printing disabled")
        endif()
        set_target_properties(timewarp_ui PROPERTIES AUTOMOC ON)

        if(MSVC)
            target_compile_options(timewarp_ui PRIVATE /W4 /permissive-)
        else()
            target_compile_options(timewarp_ui PRIVATE
                -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
                ${TIME_WARP_NULL_DEREFERENCE}
            )
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(timewarp_ui PRIVATE -Werror)
            endif()
        endif()

        # The application, not a harness: it shows the window, opens the files it
        # is given, and can render itself to a PNG for the layout checks.
        #
        # On Windows it is a GUI-subsystem executable -- there is no console
        # window beside the IDE -- and `main()` attaches to the parent console
        # when one started it, so `--version` and `--help` still print when run
        # from a prompt. On macOS it is a .app bundle, because that is what a
        # Finder window, a Dock icon and a drag-installed DMG all require.
        if(WIN32)
            add_executable(time_warp_ide WIN32 tools/time_warp_ide.cpp)
        elseif(APPLE)
            add_executable(time_warp_ide MACOSX_BUNDLE tools/time_warp_ide.cpp)
        else()
            add_executable(time_warp_ide tools/time_warp_ide.cpp)
        endif()
        target_link_libraries(time_warp_ide PRIVATE timewarp::ui)
        if(NOT MSVC)
            target_compile_options(time_warp_ide PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(time_warp_ide PRIVATE -Werror)
            endif()
        endif()

        # ---- platform identity --------------------------------------------
        # The icon and the version are what make an executable look like a
        # product in Explorer and in the Finder; both come from the same VERSION
        # file the window title uses.
        if(WIN32)
            file(TO_CMAKE_PATH
                "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/windows/time-warp-studio.ico"
                TIME_WARP_ICON_ICO)
            configure_file(
                "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/windows/time-warp-studio.rc.in"
                "${CMAKE_CURRENT_BINARY_DIR}/time-warp-studio.rc"
                @ONLY)
            target_sources(time_warp_ide PRIVATE
                "${CMAKE_CURRENT_BINARY_DIR}/time-warp-studio.rc")
            # The resource compiler resolves the icon relative to the .rc file,
            # and the path has to be Windows-shaped for it either way.
            set_source_files_properties(
                "${CMAKE_CURRENT_BINARY_DIR}/time-warp-studio.rc"
                PROPERTIES OBJECT_DEPENDS "${TIME_WARP_ICON_ICO}")
        elseif(APPLE)
            # The bundle is what Finder shows, so it carries the product's name
            # rather than the target's.
            set(TIME_WARP_BUNDLE_NAME "Time Warp Studio")
            set(TIME_WARP_BUNDLE_EXECUTABLE "${TIME_WARP_BUNDLE_NAME}")
            # The plist is configured here rather than through CMake's
            # MACOSX_BUNDLE_* substitution, because it needs keys those
            # properties do not reach: the minimum macOS, the high-resolution
            # flag and the category. A plist that is already complete is still
            # a valid MACOSX_BUNDLE_INFO_PLIST template -- there is simply
            # nothing left in it to substitute.
            configure_file(
                "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/macos/Info.plist.in"
                "${CMAKE_CURRENT_BINARY_DIR}/Info.plist"
                @ONLY)
            set_target_properties(time_warp_ide PROPERTIES
                OUTPUT_NAME "${TIME_WARP_BUNDLE_NAME}"
                MACOSX_BUNDLE_BUNDLE_NAME "${TIME_WARP_BUNDLE_NAME}"
                MACOSX_BUNDLE_GUI_IDENTIFIER "org.honey-badger.timewarpstudio"
                MACOSX_BUNDLE_BUNDLE_VERSION "${TIME_WARP_VERSION}"
                MACOSX_BUNDLE_SHORT_VERSION_STRING "${TIME_WARP_VERSION}"
                MACOSX_BUNDLE_INFO_STRING
                    "Time Warp Studio ${TIME_WARP_PACKAGE_VERSION}: an old-school programming environment"
                MACOSX_BUNDLE_COPYRIGHT "MIT licensed. James Temple."
                MACOSX_BUNDLE_ICON_FILE "time-warp-studio.icns"
                MACOSX_BUNDLE_INFO_PLIST
                    "${CMAKE_CURRENT_BINARY_DIR}/Info.plist")
            # A bundle resource rather than an installed file: CMake copies it
            # into Contents/Resources as part of the build, so the .app in the
            # build tree is as complete as the one on the disk image.
            target_sources(time_warp_ide PRIVATE
                "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/macos/time-warp-studio.icns")
            set_source_files_properties(
                "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/macos/time-warp-studio.icns"
                PROPERTIES MACOSX_PACKAGE_LOCATION "Resources")
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
        #
        # A macOS bundle is self-contained instead: the same payload lives under
        # Contents/Resources/share, which is where `app_paths.cpp`,
        # `python_worker.cpp`, `snippets.cpp` and `help_browser.cpp` look first
        # when they are inside a .app. Installing it anywhere else would leave a
        # user who dragged the app to /Applications with no examples and no help.
        include(GNUInstallDirs)
        if(APPLE)
            set(TIME_WARP_SHARE_DIR
                "${TIME_WARP_BUNDLE_NAME}.app/Contents/Resources/share")
            install(TARGETS time_warp_ide BUNDLE DESTINATION .)
        else()
            set(TIME_WARP_SHARE_DIR "${CMAKE_INSTALL_DATADIR}")
            install(TARGETS time_warp_ide
                    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
        endif()
        install(TARGETS time_warp_api
                RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
        install(
            DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/bridge/
            DESTINATION ${TIME_WARP_SHARE_DIR}/time_warp
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
            DESTINATION ${TIME_WARP_SHARE_DIR}/time-warp-studio
        )
        # The documentation browser reads these. `docsRoot()` looks in
        # `share/doc/time-warp-studio` first, so a package and a checkout show the
        # same pages; without this rule an installed help window would explain
        # that the documentation is missing.
        install(
            DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../docs/
            DESTINATION ${TIME_WARP_SHARE_DIR}/doc/time-warp-studio
        )
        if(NOT APPLE)
            install(
                FILES ${CMAKE_CURRENT_SOURCE_DIR}/../packaging/linux/time-warp-studio.desktop
                DESTINATION ${CMAKE_INSTALL_DATADIR}/applications
            )
            install(
                FILES ${CMAKE_CURRENT_SOURCE_DIR}/../packaging/linux/time-warp-studio.png
                DESTINATION ${CMAKE_INSTALL_DATADIR}/icons/hicolor/256x256/apps
            )
        endif()
        # The example programs travel with the install tree rather than beside
        # the tarball, because a package has no second place to put them and a
        # fresh install that opens onto an empty window is a poor first
        # impression. This is the directory File > Open Example reads.
        # The trailing slash matters: `DIRECTORY Examples` installs the directory
        # itself, giving `examples/Examples/logo`, while `Examples/` installs its
        # contents and gives `examples/logo`, which is the shape the menu reads.
        install(
            DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../Examples/
            DESTINATION ${TIME_WARP_SHARE_DIR}/time-warp-studio/examples
            PATTERN "__pycache__" EXCLUDE
        )

        # ---- Qt deployment ------------------------------------------------
        # A packaged application is useless on a machine that has no Qt, so the
        # Qt tools copy the libraries and platform plugins beside the binary as
        # part of the install. Doing it here rather than in the release script
        # means `make install` on a development machine produces the same tree
        # that CPack packages -- a package that differs from a local install is
        # a package nobody tested.
        if(TARGET Qt6::qmake)
            get_target_property(_time_warp_qmake Qt6::qmake IMPORTED_LOCATION)
            get_filename_component(_time_warp_qt_bin "${_time_warp_qmake}" DIRECTORY)
        endif()
        if(WIN32)
            find_program(TIME_WARP_WINDEPLOYQT
                NAMES windeployqt
                HINTS "${_time_warp_qt_bin}"
            )
            if(TIME_WARP_WINDEPLOYQT)
                # `--no-opengl-sw` leaves out the software OpenGL fallback (about
                # 30 MB) that a raster-rendered widget application never loads.
                install(CODE "
                    execute_process(
                        COMMAND \"${TIME_WARP_WINDEPLOYQT}\"
                            --no-translations --no-system-d3d-compiler
                            --no-opengl-sw
                            \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/time_warp_ide.exe\"
                        RESULT_VARIABLE _time_warp_deploy_result)
                    if(NOT _time_warp_deploy_result EQUAL 0)
                        message(FATAL_ERROR
                            \"windeployqt failed (\${_time_warp_deploy_result}); the installed tree would not run without Qt installed\")
                    endif()
                ")
            else()
                message(WARNING
                    "windeployqt was not found: the installed tree and the packages will need Qt installed separately")
            endif()
        elseif(APPLE)
            find_program(TIME_WARP_MACDEPLOYQT
                NAMES macdeployqt
                HINTS "${_time_warp_qt_bin}"
            )
            if(TIME_WARP_MACDEPLOYQT)
                # An unsigned bundle is refused outright on Apple silicon, and
                # `-` is the ad-hoc signature that makes a locally built or
                # downloaded-and-unquarantined application start. It is not a
                # Developer ID signature: Gatekeeper still asks about a bundle
                # that arrived over the network until somebody right-clicks Open.
                install(CODE "
                    execute_process(
                        COMMAND \"${TIME_WARP_MACDEPLOYQT}\"
                            \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${TIME_WARP_BUNDLE_NAME}.app\"
                            -always-overwrite
                        RESULT_VARIABLE _time_warp_deploy_result)
                    if(NOT _time_warp_deploy_result EQUAL 0)
                        message(FATAL_ERROR
                            \"macdeployqt failed (\${_time_warp_deploy_result}); the bundle would not run without Qt installed\")
                    endif()
                    execute_process(
                        COMMAND /usr/bin/codesign --force --deep --sign -
                            \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${TIME_WARP_BUNDLE_NAME}.app\"
                        RESULT_VARIABLE _time_warp_sign_result)
                    if(NOT _time_warp_sign_result EQUAL 0)
                        message(FATAL_ERROR
                            \"codesign failed (\${_time_warp_sign_result}); the bundle would be killed at launch on Apple silicon\")
                    endif()
                ")
            else()
                message(WARNING
                    "macdeployqt was not found: the bundle will need Qt installed separately")
            endif()
        endif()

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

            # The HB-1 is a whole little machine: ROM socket, screen
            # modes, the prompt's direct mode and the same native engines the
            # editor runs. It gets its own binary because a failure should name
            # the machine, not the IDE layout around it.
            add_executable(retro_computer_tests tests/retro_computer_tests.cpp)
            target_link_libraries(retro_computer_tests PRIVATE timewarp::ui)
            target_include_directories(retro_computer_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            if(NOT MSVC)
                target_compile_options(retro_computer_tests PRIVATE -Wall -Wextra -Wpedantic -Wshadow)
                if(TIME_WARP_EXPR_WERROR)
                    target_compile_options(retro_computer_tests PRIVATE -Werror)
                endif()
            endif()
            add_test(NAME retro_computer_tests COMMAND retro_computer_tests)
        endif()
    else()
        message(STATUS "Qt6 Widgets not found: skipping the widget UI")
    endif()
endif()

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------
#
# `cpack` in the build directory writes the artifact each platform installs
# with: a `.deb` (and `-G RPM` an `.rpm` where `rpmbuild` is installed), a NSIS
# `.exe` installer and a `.zip` on Windows, and a `.dmg` on macOS. `release.yml`
# builds all of them from a tag, and builds the AppImage beside them with
# `linuxdeploy` -- the Qt bundling an AppImage needs is a tool rather than a CPack
# generator.
#
# The Linux packages install into `/usr`, not into the build's prefix: a `.deb`
# that puts a binary in `/usr/local/bin` is a `.deb` whose files no package
# manager owns. Everything the application needs at run time is already installed
# by the `install()` rules above -- the binary, the bridge, the snippets, the
# docs, the desktop entry and the icon -- so the package is simply those paths.
# The version was read at the top of this file, before any of that needed it.

set(CPACK_PACKAGE_NAME "time-warp-studio")
set(CPACK_PACKAGE_VENDOR "James Temple")
set(CPACK_PACKAGE_CONTACT "James Temple <james@honey-badger.org>")
set(CPACK_PACKAGE_VERSION "${TIME_WARP_PACKAGE_VERSION}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "Old-school programming environment: ten languages, an IDE, and two complete machines")
set(CPACK_PACKAGE_DESCRIPTION
    "Time Warp Studio is a workbench for old-school programming: BASIC, APL, Logo, \
PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX and Smalltalk in one window, each with an \
integrated turtle-graphics canvas and a debugger. Inside it are two complete \
machines: the HB-1, a bespoke 1982 home computer from a parallel universe with a \
ten-language ROM socket, a CRT and its own disk operating system, and a live \
CHIP-8 with an assembler, keypad and register inspector. Every language runs \
natively in C++; the bundled Python engine is the reference the lesson check uses.")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/James-HoneyBadger/Time_Warp_Studio")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE")

# `python3` is not optional: the Lessons panel runs the bundled engine, which is
# Python. The Qt packages are the libraries the binary links against plus the
# platform plugins it loads at run time -- without the plugins the application
# starts and immediately reports that it cannot load a platform plugin.
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "James Temple <james@honey-badger.org>")
set(CPACK_DEBIAN_PACKAGE_SECTION "education")
set(CPACK_DEBIAN_PACKAGE_DEPENDS
    "libqt6widgets6, libqt6gui6, libqt6core6, qt6-qpa-plugins, python3, libsqlite3-0")
# Only when this build was made with it: a package that names a library the
# binary does not link is a needless install, and one that omits a library the
# binary does link fails to start.
if(Qt6Svg_FOUND)
    string(APPEND CPACK_DEBIAN_PACKAGE_DEPENDS ", libqt6svg6")
endif()
if(Qt6PrintSupport_FOUND)
    string(APPEND CPACK_DEBIAN_PACKAGE_DEPENDS ", libqt6printsupport6")
    # The RPM needs nothing extra: Fedora ships libQt6PrintSupport in
    # qt6-qtbase-gui, which CPACK_RPM_PACKAGE_REQUIRES already names.
endif()
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
set(CPACK_RPM_PACKAGE_REQUIRES "qt6-qtbase-gui, python3, sqlite-libs")
if(Qt6Svg_FOUND)
    string(APPEND CPACK_RPM_PACKAGE_REQUIRES ", qt6-qtsvg")
endif()
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

if(WIN32)
    # NSIS is the installer, and the ZIP beside it is the portable answer for
    # somebody who would rather not run one. `CPACK_PACKAGE_FILE_NAME` is set
    # rather than derived: on Windows the default carries `win64` or `win32`
    # from the host, which is right today and would quietly stop being right on
    # a different runner.
    set(CPACK_GENERATOR "NSIS;ZIP")
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        set(_time_warp_platform_tag "win64")
    else()
        set(_time_warp_platform_tag "win32")
    endif()
    set(CPACK_PACKAGE_FILE_NAME
        "time-warp-studio-${TIME_WARP_PACKAGE_VERSION}-${_time_warp_platform_tag}")

    set(CPACK_PACKAGE_INSTALL_DIRECTORY "Time Warp Studio")
    set(CPACK_NSIS_DISPLAY_NAME "Time Warp Studio")
    set(CPACK_NSIS_PACKAGE_NAME "Time Warp Studio")
    set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
    # NSIS runs its own resource compiler over this file, so the path is
    # Windows-shaped rather than CMake-shaped.
    file(TO_NATIVE_PATH
        "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/windows/time-warp-studio.ico"
        _time_warp_nsis_icon)
    set(CPACK_NSIS_MUI_ICON "${_time_warp_nsis_icon}")
    set(CPACK_NSIS_INSTALLED_ICON_NAME "bin\\time_warp_ide.exe")
    # One Start Menu entry and one desktop shortcut, both named after the
    # application rather than after the executable.
    set(CPACK_PACKAGE_EXECUTABLES "time_warp_ide" "Time Warp Studio")
    set(CPACK_CREATE_DESKTOP_LINKS "time_warp_ide")
    set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)
    set(CPACK_NSIS_MODIFY_PATH OFF)
    # A URL in the Add/Remove Programs entry, and the same page the Linux
    # packages name.
    set(CPACK_NSIS_URL_INFO_ABOUT
        "https://github.com/James-HoneyBadger/Time_Warp_Studio")
elseif(APPLE)
    # DragNDrop is CPack's .dmg generator: it puts the installed bundle and an
    # /Applications symlink on one disk image, which is the shape a Mac user
    # expects to drag from.
    set(CPACK_GENERATOR "DragNDrop")
    set(CPACK_PACKAGE_FILE_NAME
        "Time_Warp_Studio-${TIME_WARP_PACKAGE_VERSION}-macOS-universal")
    set(CPACK_DMG_VOLUME_NAME "Time Warp Studio")
    set(CPACK_DMG_FORMAT "UDZO")
    set(CPACK_DMG_DISABLE_APPLICATIONS_SYMLINK OFF)
    set(CPACK_PACKAGING_INSTALL_PREFIX "/")
else()
    # DEB only by default: `rpmbuild` is not installed everywhere, and a
    # generator that fails makes `cpack` fail for somebody who only wanted the deb.
    set(CPACK_GENERATOR "DEB")
endif()

include(CPack)
