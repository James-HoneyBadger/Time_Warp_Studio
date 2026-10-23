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
    src/scheme/machine_apply.cpp
    src/scheme/machine_forms.cpp
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
# Audio and I/O: two libraries that used to be inside the language one
# ---------------------------------------------------------------------------
#
# They were files in `timewarp_lang`'s source list, which meant a change to BASIC's
# `PRINT` recompiled the mixer and the gamepad reader. They are separate libraries
# now, both Qt-free, and the executors that reach them link them: `music.cpp` is
# where `SOUND` and `PLAY` get their tones, `ports.cpp` is `SERIAL`, `gamepad.cpp`
# is `STICK`/`STRIG`. A consumer of the language library still sees their symbols,
# because the link is PUBLIC -- nothing else had to change.

add_library(timewarp_audio
    src/audio/music.cpp
    src/audio/mixer.cpp
)
add_library(timewarp::audio ALIAS timewarp_audio)
target_include_directories(timewarp_audio
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)
target_compile_features(timewarp_audio PUBLIC cxx_std_20)
timewarp_enable_warnings(timewarp_audio)

add_library(timewarp_io
    src/io/ports.cpp
    src/io/gamepad.cpp
)
add_library(timewarp::io ALIAS timewarp_io)
target_include_directories(timewarp_io
    PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
)
target_compile_features(timewarp_io PUBLIC cxx_std_20)
timewarp_enable_warnings(timewarp_io)
if(NOT MSVC)
    # The two flags the rest of the engine needs and the warnings function cannot
    # know about: `-ffp-contract=off` because a fused multiply-add moves a vertex by
    # 6e-17 and the drawings are compared with the reference, and the GCC-only
    # warnings that Clang rejects.
    target_compile_options(timewarp_audio PRIVATE ${TIME_WARP_GNU_WARNING_FLAGS}
        ${TIME_WARP_NULL_DEREFERENCE} -ffp-contract=off)
    target_compile_options(timewarp_io PRIVATE ${TIME_WARP_GNU_WARNING_FLAGS}
        ${TIME_WARP_NULL_DEREFERENCE})
endif()

# ---------------------------------------------------------------------------
# The language core, and one object library per language
# ---------------------------------------------------------------------------
#
# The library is the interpreter loop, the value type, dispatch and the registry.
# Each language is an OBJECT library: the same link structure as before, but editing
# the Logo executor recompiles the Logo executor instead of every language in the
# tree, and adding a language is a block of three lines in the list below rather
# than a line in the middle of one long source list.
#
# `executor_for()` answers for every language here and returns null only for Scheme,
# which is the machine rather than an executor.

add_library(tw_lang_core OBJECT
    src/lang/value.cpp
    src/lang/raster.cpp
    src/lang/screen_mode.cpp
    src/lang/language.cpp
    src/lang/debug.cpp
    src/lang/watch.cpp
    src/lang/repl.cpp
    src/lang/interpreter.cpp
    src/lang/mersenne_random.cpp
)

add_library(tw_lang_basic OBJECT
    src/lang/basic.cpp
    src/lang/basic_statements_helpers.cpp
    src/lang/basic_statements_dispatch.cpp
    src/lang/basic_statements_print.cpp
    src/lang/basic_statements_variables.cpp
    src/lang/basic_statements_flow.cpp
    src/lang/basic_statements_blocks.cpp
    src/lang/basic_statements_errors.cpp
    src/lang/basic_draw_statements.cpp
    src/lang/basic_file_statements.cpp
    src/lang/basic_screen_statements.cpp
    src/lang/basic_sprite_statements.cpp
    src/lang/basic_system_statements.cpp
    src/lang/basic_library.cpp
    src/lang/basic_text.cpp
)
add_library(tw_lang_logo OBJECT
    src/lang/logo.cpp
    src/lang/logo_support.cpp
    src/lang/logo_machine.cpp
    src/lang/logo_motion.cpp
    src/lang/logo_control.cpp
    src/lang/logo_data.cpp
)
add_library(tw_lang_chip8 OBJECT src/lang/chip8.cpp)
add_library(tw_lang_focal OBJECT src/lang/focal.cpp)
add_library(tw_lang_pilot OBJECT src/lang/pilot.cpp)
add_library(tw_lang_comal OBJECT src/lang/comal.cpp)
add_library(tw_lang_apl OBJECT
    src/lang/apl.cpp
    src/lang/apl_eval.cpp
    src/lang/apl_parse.cpp
    src/lang/apl_structural.cpp
    src/lang/apl_tables.cpp
    src/lang/apl_values.cpp
)
add_library(tw_lang_rexx OBJECT
    src/lang/rexx.cpp
    src/lang/rexx_parser.cpp
    src/lang/rexx_support.cpp
)
add_library(tw_lang_smalltalk OBJECT
    src/lang/smalltalk.cpp
    src/lang/smalltalk_library.cpp
    src/lang/smalltalk_driver.cpp
)
add_library(tw_lang_forth OBJECT src/lang/forth.cpp)

set(TIME_WARP_LANGUAGE_OBJECTS
    tw_lang_core
    tw_lang_basic
    tw_lang_logo
    tw_lang_chip8
    tw_lang_focal
    tw_lang_pilot
    tw_lang_comal
    tw_lang_apl
    tw_lang_rexx
    tw_lang_smalltalk
    tw_lang_forth
)

# One loop over the object libraries so the include path, the standard and the
# warnings are said once. A new language adds a name to the list above and nothing
# else.
foreach(objects IN LISTS TIME_WARP_LANGUAGE_OBJECTS)
    target_include_directories(${objects} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
    target_compile_features(${objects} PUBLIC cxx_std_20)
    target_link_libraries(${objects} PUBLIC timewarp::expr timewarp::audio timewarp::io)
    timewarp_enable_warnings(${objects})
    if(MSVC)
        target_compile_options(${objects} PRIVATE /permissive- /fp:precise)
    else()
        target_compile_options(${objects} PRIVATE ${TIME_WARP_GNU_WARNING_FLAGS}
            ${TIME_WARP_NULL_DEREFERENCE}
            # No fused multiply-add. The reference rounds every multiplication and
            # addition separately; contracting `start + i*step` into one fma changes
            # the last bits of a shape's vertices, and a vertex 6e-17 away from where
            # the reference puts it is a different drawing.
            -ffp-contract=off)
    endif()
endforeach()

# The library itself is the union of those objects: one link target for everything
# that consumes the engines, and the same symbols as before. A static library whose
# only sources are the object files is the shape CMake expects -- an INTERFACE
# library cannot carry them.
add_library(timewarp_lang STATIC)
add_library(timewarp::lang ALIAS timewarp_lang)
target_link_libraries(timewarp_lang PUBLIC timewarp::expr timewarp::audio timewarp::io)
target_include_directories(timewarp_lang PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)
target_compile_features(timewarp_lang PUBLIC cxx_std_20)
foreach(objects IN LISTS TIME_WARP_LANGUAGE_OBJECTS)
    target_sources(timewarp_lang PRIVATE $<TARGET_OBJECTS:${objects}>)
endforeach()
if(TIME_WARP_EXPR_BUILD_TESTS)
    add_executable(lang_tests
        tests/lang_tests.cpp
        tests/lang_apl_rexx_smalltalk_tests.cpp
        tests/lang_basic_tests.cpp
        tests/lang_chip8_assembler_tests.cpp
        tests/lang_chip8_machine_tests.cpp
        tests/lang_focal_pilot_comal_tests.cpp
        tests/lang_forth_tests.cpp
        tests/lang_logo_tests.cpp
        tests/lang_repl_tests.cpp
    )
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

    # The command-line convention every tool in tools/ speaks. It is a header with
    # no library behind it -- the documentation gates build without Qt, and they
    # include it -- so this suite links nothing but its own source.
    add_executable(cli_tests tests/cli_tests.cpp)
    target_include_directories(cli_tests PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/tests
        ${CMAKE_CURRENT_SOURCE_DIR}/include)
    timewarp_enable_warnings(cli_tests)
    add_test(NAME cli_tests COMMAND cli_tests)
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
