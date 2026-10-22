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
# Qt-free libraries: the expression engine and its probes, the Scheme machine,
# the language executors, the canvas queue and the two micro-benchmarks.

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
        timewarp_enable_warnings(${suite})
        add_test(NAME ${suite} COMMAND ${suite})
    endforeach()
endif()

# ---------------------------------------------------------------------------
# Expression probe
#
# Evaluates expressions read from stdin, one per line. It was the C++ half of a
# differential check against the Python reference; the reference is gone, so it is
# now a hand tool. `evaluator_tests` is the gate.
# ---------------------------------------------------------------------------

add_executable(expr_cli tools/eval_cli.cpp)
target_link_libraries(expr_cli PRIVATE timewarp::expr)
timewarp_enable_warnings(expr_cli)

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
    timewarp_enable_warnings(scheme_tests)
    add_test(NAME scheme_tests COMMAND scheme_tests)
endif()

# Reads a whole program on stdin and prints its output, so
# tools/scheme_differential_check.py can diff the port against the reference.
add_executable(scheme_cli tools/scheme_cli.cpp)
target_link_libraries(scheme_cli PRIVATE timewarp::scheme)
timewarp_enable_warnings(scheme_cli)

# Depth and throughput measurement: every workload here exceeds what the
# reference could run at all.
add_executable(scheme_bench tools/scheme_bench.cpp)
target_link_libraries(scheme_bench PRIVATE timewarp::scheme)
timewarp_enable_warnings(scheme_bench)

# ---------------------------------------------------------------------------
# Native language executors
#
# One executor per line-by-line or whole-program language, plus the shared
# expression evaluator, canvas host and I/O ports they run against. This is why
# the library is separate from `timewarp_scheme`: Scheme has its own machine,
# while these share one interpreter loop and one executor interface. Forth is
# whole-program like Logo and Smalltalk, and it lives here too.
#
# `executor_for()` answers for every language in this library and returns null
# only for Scheme, which is the machine rather than an executor.
# ---------------------------------------------------------------------------

add_library(timewarp_lang
    src/audio/music.cpp
    src/audio/mixer.cpp
    src/io/ports.cpp
    src/io/gamepad.cpp
    src/lang/value.cpp
    src/lang/raster.cpp
    src/lang/screen_mode.cpp
    src/lang/language.cpp
    src/lang/debug.cpp
    src/lang/watch.cpp
    src/lang/repl.cpp
    src/lang/interpreter.cpp
    src/lang/mersenne_random.cpp
    src/lang/basic.cpp
    src/lang/basic_statements.cpp
    src/lang/basic_features.cpp
    src/lang/basic_library.cpp
    src/lang/basic_text.cpp
    src/lang/chip8.cpp
    src/lang/focal.cpp
    src/lang/forth.cpp
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
    # The shipped examples are a gate as well as a sample: a game that runs and
    # draws nothing is the failure this project treats as the bug, so the test
    # reads the file from the checkout wherever the build tree is.
    target_compile_definitions(lang_tests PRIVATE
        TIME_WARP_TEST_EXAMPLES="${CMAKE_CURRENT_SOURCE_DIR}/../Examples")
    timewarp_enable_warnings(lang_tests)
    add_test(NAME lang_tests COMMAND lang_tests)

    # The mixer is Qt-free and has no device, so its suite runs anywhere the
    # engine builds -- a machine with no sound card is the normal case in CI.
    add_executable(audio_tests tests/audio_tests.cpp)
    target_link_libraries(audio_tests PRIVATE timewarp::lang)
    target_include_directories(audio_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
    timewarp_enable_warnings(audio_tests)
    add_test(NAME audio_tests COMMAND audio_tests)
endif()

# One native program in, what it printed and drew out, for the differential
# harness. See tools/lang_differential_check.py.
add_executable(lang_cli tools/lang_cli.cpp)
target_link_libraries(lang_cli PRIVATE timewarp::lang)
timewarp_enable_warnings(lang_cli)

# ---------------------------------------------------------------------------
# Micro-benchmark
# ---------------------------------------------------------------------------

add_executable(expr_bench tools/bench.cpp)
target_link_libraries(expr_bench PRIVATE timewarp::expr)
timewarp_enable_warnings(expr_bench)

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
    timewarp_enable_warnings(canvas_queue_tests)
    add_test(NAME canvas_queue_tests COMMAND canvas_queue_tests)
endif()

# Throughput and coalescing measurement.
add_executable(canvas_queue_bench tools/canvas_queue_bench.cpp)
target_link_libraries(canvas_queue_bench PRIVATE timewarp::canvas)
timewarp_enable_warnings(canvas_queue_bench)
