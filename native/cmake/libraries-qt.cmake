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
# The Qt libraries: the canvas bridge, the debugger, SQL, projects, lessons and their
# commands, and the JSON-RPC API.

# ---------------------------------------------------------------------------
# Qt bridge (optional)
#
# Built only when Qt is present.  The core queue above never depends on this,
# so a machine without Qt still builds and tests everything else.

if(TIME_WARP_QT_FOUND)
    message(STATUS "${TIME_WARP_QT} ${TIME_WARP_QT_CORE_VERSION} found: building the canvas bridge")

    add_library(timewarp_canvas_qt
        src/canvas/qt_bridge.cpp
        # Listed so AUTOMOC scans it: the Q_OBJECT class lives in the header,
        # which is outside this target's source directory.
        include/timewarp/canvas/qt_bridge.hpp
    )
    add_library(timewarp::canvas_qt ALIAS timewarp_canvas_qt)

    target_link_libraries(timewarp_canvas_qt PUBLIC timewarp::canvas ${TIME_WARP_QT}::Core)
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
        timewarp_enable_warnings(canvas_bridge_tests)
        add_test(NAME canvas_bridge_tests COMMAND canvas_bridge_tests)
    endif()
else()
    message(STATUS "Qt not found: skipping the canvas bridge (core queue still builds)")
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
    timewarp_enable_warnings(debug_tests)
    add_test(NAME debug_tests COMMAND debug_tests)
endif()

# ---------------------------------------------------------------------------
# Background debugger runner (Qt)
#
# A QThread whose run() *is* the interpreter loop, driven by an atomic command
# channel.  Built only where Qt exists.
# ---------------------------------------------------------------------------

if(TIME_WARP_QT_FOUND)
    message(STATUS "${TIME_WARP_QT} ${TIME_WARP_QT_CORE_VERSION} found: building the debugger runner")

    add_library(timewarp_debugger
        src/debugger/debugger_runner.cpp
        # Listed so AUTOMOC scans it: the Q_OBJECT class lives in the header,
        # which is outside this target's source directory.
        include/timewarp/debugger/debugger_runner.hpp
    )
    add_library(timewarp::debugger ALIAS timewarp_debugger)

    target_link_libraries(timewarp_debugger PUBLIC timewarp::scheme ${TIME_WARP_QT}::Core)
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
        timewarp_enable_warnings(debugger_runner_tests)
        add_test(NAME debugger_runner_tests COMMAND debugger_runner_tests)
    endif()
else()
    message(STATUS "Qt not found: skipping the debugger runner")
endif()

# ---------------------------------------------------------------------------
# SQL (Qt Core + SQLite)
#
# SQLite is embedded, so a learner's database is a file rather than a server.
# The library is required: the Database tab and every language's `SQL` statement
# are part of the product now, and a build without them is a build missing a
# documented feature.
# ---------------------------------------------------------------------------

if(TIME_WARP_QT_FOUND)
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
    target_link_libraries(timewarp_sql PUBLIC ${TIME_WARP_QT}::Core ${TIME_WARP_SQLITE_TARGET})

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
        timewarp_enable_warnings(sql_tests)
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

if(TIME_WARP_QT_FOUND)
    add_library(timewarp_project
        src/project/project.cpp
        include/timewarp/project/project.hpp
    )
    add_library(timewarp::project ALIAS timewarp_project)

    target_include_directories(timewarp_project
        PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )
    target_compile_features(timewarp_project PUBLIC cxx_std_20)
    target_link_libraries(timewarp_project PUBLIC ${TIME_WARP_QT}::Core)

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
        timewarp_enable_warnings(project_tests)
        add_test(NAME project_tests COMMAND project_tests)
    endif()
else()
    message(STATUS "Qt not found: skipping the project model")
endif()

# ---------------------------------------------------------------------------
# The AI assistant (Qt Core and Qt Network)
#
# Where the endpoint is, how a conversation is composed, and the one class that
# carries it. The IDE's panel and settings dialog are views over this, and
# `time_warp_ai` is the same thing from a terminal.
#
# Qt Network is not optional the way Svg and PrintSupport are: a client with no
# transport is not a smaller assistant, it is none. So a Qt built without Network
# gets no assistant at all -- the panel is not created and its menu entries are
# absent -- rather than a dial that reports a failure every time it is used.
# ---------------------------------------------------------------------------

if(TIME_WARP_QT_FOUND)
    find_package(${TIME_WARP_QT} QUIET COMPONENTS Network)

    if(${TIME_WARP_QT}Network_FOUND)
        add_library(timewarp_ai
            src/ai/client.cpp
            src/ai/config.cpp
            src/ai/history.cpp
            src/ai/keyring.cpp
            src/ai/prompt.cpp
            # Listed so AUTOMOC scans them: the Q_OBJECT class lives in the header,
            # which is outside this target's source directory.
            include/timewarp/ai/client.hpp
            include/timewarp/ai/config.hpp
            include/timewarp/ai/history.hpp
            include/timewarp/ai/keyring.hpp
            include/timewarp/ai/prompt.hpp
        )
        add_library(timewarp::ai ALIAS timewarp_ai)

        target_include_directories(timewarp_ai
            PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        )
        target_compile_features(timewarp_ai PUBLIC cxx_std_20)
        target_link_libraries(timewarp_ai PUBLIC
            ${TIME_WARP_QT}::Core ${TIME_WARP_QT}::Network)
        set_target_properties(timewarp_ai PROPERTIES AUTOMOC ON)

        # The key's home on this platform, when it has one. Each is optional: a
        # build with none of them keeps the key in the settings file as it always
        # did, and `TIME_WARP_AI_KEYRING=file` refuses the system store on any
        # platform.
        if(WIN32)
            target_link_libraries(timewarp_ai PRIVATE advapi32)
        elseif(APPLE)
            target_link_libraries(timewarp_ai PRIVATE "-framework Security"
                "-framework CoreFoundation")
        else()
            find_package(PkgConfig QUIET)
            if(PkgConfig_FOUND)
                pkg_check_modules(LIBSECRET IMPORTED_TARGET libsecret-1)
            endif()
            if(LIBSECRET_FOUND)
                target_link_libraries(timewarp_ai PRIVATE PkgConfig::LIBSECRET)
                target_compile_definitions(timewarp_ai PRIVATE TIME_WARP_HAVE_LIBSECRET=1)
                message(STATUS "libsecret found: the assistant key goes in the system keyring")
            else()
                message(STATUS "libsecret not found: the assistant key stays in the settings file")
            endif()
        endif()

        if(MSVC)
            target_compile_options(timewarp_ai PRIVATE /W4 /permissive-)
        else()
            target_compile_options(timewarp_ai PRIVATE
                -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
                ${TIME_WARP_NULL_DEREFERENCE}
            )
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(timewarp_ai PRIVATE -Werror)
            endif()
        endif()

        # The same assistant from a shell, for a script and for a person who wants
        # to know whether the key works without opening a window.
        add_executable(time_warp_ai tools/ai_cli.cpp)
        target_link_libraries(time_warp_ai PRIVATE timewarp::ai)
        timewarp_enable_warnings(time_warp_ai)

        if(TIME_WARP_EXPR_BUILD_TESTS)
            add_executable(ai_tests tests/ai_tests.cpp)
            target_link_libraries(ai_tests PRIVATE timewarp::ai)
            target_include_directories(ai_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            timewarp_enable_warnings(ai_tests)
            add_test(NAME ai_tests COMMAND ai_tests)
        endif()
    else()
        message(STATUS "${TIME_WARP_QT} Network not found: skipping the AI assistant")
    endif()
endif()

# ---------------------------------------------------------------------------
# Lessons (Qt Core)
#
# The curriculum and the verifier the Lessons panel, `time_warp_teach` and
# `time_warp_learn` all ask. Qt Core for the JSON and nothing else: the languages
# are the engine's, and `RecordingHost` is the surface, so a lesson can be judged
# on a machine with no window at all.
# ---------------------------------------------------------------------------

if(TIME_WARP_QT_FOUND)
    # zlib for classroom bundles: a zip a school can open with the tools it
    # already has. Qt does not expose the copy it links internally, so this is a
    # real dependency of the lessons library, and the packaging ships it.
    find_package(ZLIB REQUIRED)
    add_library(timewarp_lessons
        src/lessons/classroom.cpp
        src/lessons/curriculum.cpp
        src/lessons/progress.cpp
        src/lessons/plans.cpp
        src/lessons/verify.cpp
        include/timewarp/lessons/classroom.hpp
        include/timewarp/lessons/curriculum.hpp
        include/timewarp/lessons/plans.hpp
        include/timewarp/lessons/progress.hpp
        include/timewarp/lessons/verify.hpp
    )
    add_library(timewarp::lessons ALIAS timewarp_lessons)

    target_include_directories(timewarp_lessons
        PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    )
    target_compile_features(timewarp_lessons PUBLIC cxx_std_20)
    target_link_libraries(timewarp_lessons
        PUBLIC ${TIME_WARP_QT}::Core timewarp::lang timewarp::scheme ZLIB::ZLIB
    )

    if(MSVC)
        target_compile_options(timewarp_lessons PRIVATE /W4 /permissive-)
    else()
        target_compile_options(timewarp_lessons PRIVATE
            -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
            ${TIME_WARP_NULL_DEREFERENCE}
        )
        if(TIME_WARP_EXPR_WERROR)
            target_compile_options(timewarp_lessons PRIVATE -Werror)
        endif()
    endif()

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(lessons_tests tests/lessons_tests.cpp)
        target_link_libraries(lessons_tests PRIVATE timewarp::lessons)
        target_include_directories(lessons_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        # The shipped curriculum, from the checkout: a lesson whose answer does
        # not pass is a curriculum bug, and this test is what finds it.
        target_compile_definitions(lessons_tests PRIVATE
            TIME_WARP_TEST_LESSONS="${CMAKE_CURRENT_SOURCE_DIR}/share/lessons.json")
        timewarp_enable_warnings(lessons_tests)
        add_test(NAME lessons_tests COMMAND lessons_tests)

        add_executable(classroom_tests tests/classroom_tests.cpp)
        target_link_libraries(classroom_tests PRIVATE timewarp::lessons)
        target_include_directories(classroom_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        timewarp_enable_warnings(classroom_tests)
        add_test(NAME classroom_tests COMMAND classroom_tests)
    endif()

    # The learner's command, from the same library the panel uses.
    add_executable(time_warp_learn tools/learn.cpp)
    target_link_libraries(time_warp_learn PRIVATE timewarp::lessons)
    timewarp_enable_warnings(time_warp_learn)

    # The teacher's command: validate the curriculum, and (with --write) the
    # lesson plans the documentation job compares against it.
    add_executable(time_warp_teach tools/teach.cpp)
    target_link_libraries(time_warp_teach PRIVATE timewarp::lessons)
    timewarp_enable_warnings(time_warp_teach)
else()
    message(STATUS "Qt not found: skipping the lessons library")
endif()

# ---------------------------------------------------------------------------
# Automation API (Qt Core)
#
# JSON-RPC over newline-delimited JSON. The library has no transport of its own:
# `time_warp_api` supplies stdin/stdout, and a test supplies string streams. That
# is what lets the IDE, a plugin and a test share one implementation.
# ---------------------------------------------------------------------------

if(TIME_WARP_QT_FOUND AND TARGET timewarp::sql)
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
        ${TIME_WARP_QT}::Core timewarp::lang timewarp::scheme timewarp::sql timewarp::project
    )
    # `ai.ask` exists in every build; a build without the assistant answers with a
    # sentence saying so rather than "unknown method".
    if(TARGET timewarp::ai)
        target_link_libraries(timewarp_api PUBLIC timewarp::ai)
        target_compile_definitions(timewarp_api PRIVATE TIME_WARP_HAVE_AI=1)
    endif()
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
    timewarp_enable_warnings(time_warp_api)

    if(TIME_WARP_EXPR_BUILD_TESTS)
        add_executable(api_tests tests/api_tests.cpp)
        target_link_libraries(api_tests PRIVATE timewarp::api)
        target_include_directories(api_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
        timewarp_enable_warnings(api_tests)
        add_test(NAME api_tests COMMAND api_tests)
    endif()
endif()


# ---------------------------------------------------------------------------
# Desktop UI (Qt Widgets)
#
# The main-window layout. Separate from the debugger runner because it needs
# Widgets rather than just Core: a machine with QtCore but no QtWidgets (a
# headless build host) still gets everything else.
# ---------------------------------------------------------------------------

if(TIME_WARP_QT_FOUND)
    find_package(${TIME_WARP_QT} QUIET COMPONENTS Widgets)

    if(${TIME_WARP_QT}Widgets_FOUND)
        message(STATUS "${TIME_WARP_QT} ${${TIME_WARP_QT}Widgets_VERSION} found: building the widget UI")

        # Two components are wanted but not required. `Svg` is QSvgGenerator, for
        # vector export; `PrintSupport` is QPrinter and QPrintDialog, for the two
        # Print entries. A build without either still runs: the format and the
        # entries are absent rather than broken, and the save dialog and the
        # `--export` help do not name what the build cannot do.
        find_package(${TIME_WARP_QT} QUIET COMPONENTS Svg PrintSupport)

        # Some distributions ship Qt built for one configuration only -- Arch's
        # qt6-base 6.12 provides RelWithDebInfo. An imported target with no
        # location for the requested configuration then makes CMake's generate
        # step fail with "IMPORTED_LOCATION or IMPORTED_IMPLIB not set" before
        # anything is compiled. Map the requested configuration to one the
        # package provides rather than making the user change build type.
        get_target_property(_qt_imported_configs ${TIME_WARP_QT}::Gui IMPORTED_CONFIGURATIONS)
        if(_qt_imported_configs AND CMAKE_BUILD_TYPE)
            string(TOUPPER "${CMAKE_BUILD_TYPE}" _qt_wanted_config)
            if(NOT _qt_wanted_config IN_LIST _qt_imported_configs)
                list(GET _qt_imported_configs 0 _qt_fallback_config)
                foreach(_qt_target ${TIME_WARP_QT}::Core ${TIME_WARP_QT}::Gui ${TIME_WARP_QT}::Widgets)
                    if(TARGET ${_qt_target})
                        set_property(TARGET ${_qt_target} PROPERTY
                            MAP_IMPORTED_CONFIG_${_qt_wanted_config} ${_qt_fallback_config})
                    endif()
                endforeach()
                message(STATUS
                    "${TIME_WARP_QT} has no ${CMAKE_BUILD_TYPE} configuration; using ${_qt_fallback_config}")
            endif()
        endif()

        # The window title carries the version read at the top of this file, from
        # the repository's VERSION, so the title and the packages cannot drift.

        # ---- what a program needs to be *shown* ----------------------------
        # The canvas, the runner that fills it, the sounds it makes and the
        # palette it is drawn in. Split out of `timewarp_ui` so `time_warp_player`
        # can link the showing without the authoring: an editor, a REPL bar, a
        # session restore and eleven docks are not part of opening a game, and
        # until 18.0 the only way to ship a program was to ship all of it.
        add_library(timewarp_ui_core
            src/ui/language.cpp
            src/ui/native_runner.cpp
            src/ui/sound_player.cpp
            src/ui/standalone.cpp
            src/ui/themes.cpp
            src/ui/turtle_canvas_commands.cpp
            src/ui/turtle_canvas_painting.cpp
            src/ui/turtle_canvas_raster.cpp
            src/ui/turtle_canvas_view.cpp
            src/ui/ui_style.cpp
            include/timewarp/ui/language.hpp
            include/timewarp/ui/native_runner.hpp
            include/timewarp/ui/sound_player.hpp
            include/timewarp/ui/standalone.hpp
            include/timewarp/ui/themes.hpp
            include/timewarp/ui/turtle_canvas.hpp
            include/timewarp/ui/ui_style.hpp
        )
        add_library(timewarp::ui_core ALIAS timewarp_ui_core)
        target_include_directories(timewarp_ui_core
            PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        )
        target_compile_features(timewarp_ui_core PUBLIC cxx_std_20)
        target_link_libraries(timewarp_ui_core PUBLIC ${TIME_WARP_QT}::Widgets)
        if(TARGET timewarp::canvas_qt)
            target_link_libraries(timewarp_ui_core PRIVATE timewarp::canvas_qt)
        endif()
        # The engines. A player with no interpreter has nothing to play.
        target_link_libraries(timewarp_ui_core PRIVATE timewarp::lang)
        target_compile_definitions(timewarp_ui_core PRIVATE
            TIME_WARP_VERSION="${TIME_WARP_VERSION}"
        )
        # Public for the same reason `timewarp_ui` makes Svg public: the save
        # dialog's filter and the export help text are compiled into the
        # executable and the tests.
        if(${TIME_WARP_QT}Svg_FOUND)
            target_link_libraries(timewarp_ui_core PUBLIC ${TIME_WARP_QT}::Svg)
            target_compile_definitions(timewarp_ui_core PUBLIC TIME_WARP_HAVE_SVG=1)
        endif()
        set_target_properties(timewarp_ui_core PROPERTIES AUTOMOC ON)
        if(MSVC)
            target_compile_options(timewarp_ui_core PRIVATE /W4 /permissive-)
        else()
            target_compile_options(timewarp_ui_core PRIVATE
                -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual ${TIME_WARP_GNU_WARNING_FLAGS}
                ${TIME_WARP_NULL_DEREFERENCE}
            )
            if(TIME_WARP_EXPR_WERROR)
                target_compile_options(timewarp_ui_core PRIVATE -Werror)
            endif()
        endif()

        add_library(timewarp_ui
            src/ui/app.cpp
            src/ui/app_paths.cpp
            src/ui/benchmark.cpp
            src/ui/autosave.cpp
            src/ui/breakpoints.cpp
            src/ui/classroom_marker.cpp
            src/ui/chip8_emulator.cpp
            src/ui/chip8_library.cpp
            src/ui/chip8_screen.cpp
            src/ui/chip8_window.cpp
            src/ui/code_editor.cpp
            src/ui/console.cpp
            src/ui/command_palette.cpp
            src/ui/command_reference.cpp
            src/ui/diff.cpp
            src/ui/export.cpp
            src/ui/find_bar.cpp
            src/ui/find_in_files.cpp
            src/ui/gallery.cpp
            src/ui/notebook.cpp
            src/ui/notebook_window.cpp
            src/ui/formatter.cpp
            src/ui/gif_encoder.cpp
            src/ui/hb_bbs.cpp
            src/ui/hb_dos.cpp
            src/ui/hb_clock.cpp
            src/ui/hb_memory.cpp
            src/ui/hb_tube.cpp
            src/ui/machine_image.cpp
            src/ui/machine_voice.cpp
            src/ui/help_browser.cpp
            src/ui/lint.cpp
            src/ui/main_window_construction.cpp
            src/ui/main_window_menus.cpp
            src/ui/main_window_appearance.cpp
            src/ui/main_window_actions.cpp
            src/ui/main_window_chrome.cpp
            src/ui/main_window_data.cpp
            src/ui/main_window_session.cpp
            src/ui/main_window_panels.cpp
            src/ui/main_window_run.cpp
            src/ui/outline.cpp
            src/ui/panels_status.cpp
            src/ui/panels_classroom.cpp
            src/ui/panels_function_keys.cpp
            src/ui/panels_crt.cpp
            src/ui/panels_lessons.cpp
            src/ui/panels_inspectors.cpp
            src/ui/preferences.cpp
            src/ui/profile_report.cpp
            src/ui/project_panel.cpp
            src/ui/repl_runner.cpp
            src/ui/resizable_tab_widget.cpp
            src/ui/retro_computer_basic.cpp
            src/ui/retro_computer_bbs.cpp
            src/ui/retro_computer_boot.cpp
            src/ui/retro_computer_dos.cpp
            src/ui/retro_computer_machine.cpp
            src/ui/retro_computer_settings.cpp
            src/ui/retro_computer_support.cpp
            src/ui/retro_computer_ui.cpp
            src/ui/retro_settings.cpp
            src/ui/scrubber.cpp
            src/ui/snippet_dialog.cpp
            src/ui/snippets.cpp
            src/ui/sql_panel.cpp
            src/ui/syntax_highlighter.cpp
            src/ui/terminal_widget.cpp
            src/ui/tour.cpp
            src/ui/version_history.cpp
            # Listed so AUTOMOC scans them: the Q_OBJECT classes live in headers
            # outside this target's source directory.
            include/timewarp/ui/benchmark.hpp
            include/timewarp/ui/breakpoints.hpp
            include/timewarp/ui/code_editor.hpp
            include/timewarp/ui/chip8_emulator.hpp
            include/timewarp/ui/chip8_library.hpp
            include/timewarp/ui/classroom_marker.hpp
            include/timewarp/ui/chip8_screen.hpp
            include/timewarp/ui/chip8_window.hpp
            include/timewarp/ui/command_palette.hpp
            include/timewarp/ui/command_reference.hpp
            include/timewarp/ui/diff.hpp
            include/timewarp/ui/export.hpp
            include/timewarp/ui/find_bar.hpp
            include/timewarp/ui/find_in_files.hpp
            include/timewarp/ui/hb_bbs.hpp
            include/timewarp/ui/hb_dos.hpp
            include/timewarp/ui/hb_clock.hpp
            include/timewarp/ui/hb_memory.hpp
            include/timewarp/ui/hb_tube.hpp
            include/timewarp/ui/machine_image.hpp
            include/timewarp/ui/machine_voice.hpp
            include/timewarp/ui/help_browser.hpp
            include/timewarp/ui/gallery.hpp
            include/timewarp/ui/notebook.hpp
            include/timewarp/ui/notebook_window.hpp
            include/timewarp/ui/lint.hpp
            include/timewarp/ui/snippet_dialog.hpp
            include/timewarp/ui/main_window.hpp
            include/timewarp/ui/panels.hpp
            include/timewarp/ui/preferences.hpp
            include/timewarp/ui/profile_report.hpp
            include/timewarp/ui/project_panel.hpp
            include/timewarp/ui/repl_runner.hpp
            include/timewarp/ui/resizable_tab_widget.hpp
            include/timewarp/ui/retro_computer.hpp
            include/timewarp/ui/retro_settings.hpp
            include/timewarp/ui/scrubber.hpp
            include/timewarp/ui/sql_panel.hpp
            include/timewarp/ui/syntax_highlighter.hpp
            include/timewarp/ui/terminal_widget.hpp
            include/timewarp/ui/tour.hpp
            include/timewarp/ui/version_history.hpp
        )
        add_library(timewarp::ui ALIAS timewarp_ui)

        target_include_directories(timewarp_ui
            PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        )
        target_compile_features(timewarp_ui PUBLIC cxx_std_20)
        target_link_libraries(timewarp_ui PUBLIC ${TIME_WARP_QT}::Widgets)
        # The canvas, the runner and the palette. `timewarp_ui` is the authoring
        # shell around them; `time_warp_player` links only the core.
        target_link_libraries(timewarp_ui PUBLIC timewarp::ui_core)
        # The window owns the execution pipeline: the interpreter runs on a
        # `DebuggerRunner` thread, pushes turtle commands into the canvas queue,
        # and `CanvasBridge` drains them on the GUI thread. Guarded because the
        # canvas bridge and the debugger are only built when their own Qt
        # components were found.
        if(TARGET timewarp::canvas_qt)
            target_link_libraries(timewarp_ui PRIVATE timewarp::canvas_qt)
        endif()
        # The Lessons panel is a view over the curriculum and the learner's
        # progress, checked in process -- no worker round trip.
        if(TARGET timewarp::lessons)
            target_link_libraries(timewarp_ui PRIVATE timewarp::lessons)
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
        target_compile_definitions(timewarp_ui PRIVATE
            TIME_WARP_VERSION="${TIME_WARP_VERSION}"
        )
        # Public because the save-dialog filter and the `--export` help text live
        # in the executable and the tests, and they must not offer a format this
        # build cannot write.
        if(${TIME_WARP_QT}Svg_FOUND)
            target_link_libraries(timewarp_ui PUBLIC ${TIME_WARP_QT}::Svg)
            target_compile_definitions(timewarp_ui PUBLIC TIME_WARP_HAVE_SVG=1)
            message(STATUS "Qt Svg ${${TIME_WARP_QT}Svg_VERSION} found: SVG drawing export enabled")
        else()
            message(STATUS "Qt Svg not found: SVG drawing export disabled")
        endif()
        # Public for the same reason as Svg: the menu entries and the tests both
        # have to know whether this build can print.
        if(${TIME_WARP_QT}PrintSupport_FOUND)
            target_link_libraries(timewarp_ui PUBLIC ${TIME_WARP_QT}::PrintSupport)
            target_compile_definitions(timewarp_ui PUBLIC TIME_WARP_HAVE_PRINT=1)
            message(STATUS "Qt PrintSupport ${${TIME_WARP_QT}PrintSupport_VERSION} found: printing enabled")
        else()
            message(STATUS "Qt PrintSupport not found: printing disabled")
        endif()
        set_target_properties(timewarp_ui PROPERTIES AUTOMOC ON)

        # The assistant's page and its settings dialog exist only where there is a
        # transport to put behind them. `TIME_WARP_HAVE_AI` is PUBLIC because a
        # test asks `MainWindow::panelKeys()` how many pages the right column has,
        # and that count is different in a build with no Qt Network.
        if(TARGET timewarp::ai)
            target_sources(timewarp_ui PRIVATE
                src/ui/ai_panel.cpp
                src/ui/ai_settings.cpp
                src/ui/assistant_dialog.cpp
                include/timewarp/ui/ai_panel.hpp
                include/timewarp/ui/ai_settings.hpp
                include/timewarp/ui/assistant_dialog.hpp
            )
            target_link_libraries(timewarp_ui PRIVATE timewarp::ai)
            target_compile_definitions(timewarp_ui PUBLIC TIME_WARP_HAVE_AI=1)
        endif()

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

        # ---- the player, not the workshop ---------------------------------
        # One program, the canvas it draws on and the output it prints. It links
        # `timewarp::ui_core` rather than `timewarp::ui`, so an artifact compiled
        # for a friend carries the showing and not the authoring.
        if(WIN32)
            add_executable(time_warp_player WIN32 tools/player.cpp)
        elseif(APPLE)
            add_executable(time_warp_player MACOSX_BUNDLE tools/player.cpp)
        else()
            add_executable(time_warp_player tools/player.cpp)
        endif()
        target_link_libraries(time_warp_player PRIVATE timewarp::ui_core)
        # The version string is a definition rather than a header constant, so a
        # target that prints it has to carry it: `--version` would otherwise be the
        # one place in the product with no number in it.
        target_compile_definitions(time_warp_player PRIVATE
            TIME_WARP_VERSION="${TIME_WARP_VERSION}")
        if(TARGET ${TIME_WARP_QT}::WinMain)
            target_link_libraries(time_warp_player PRIVATE ${TIME_WARP_QT}::WinMain)
        endif()
        timewarp_enable_warnings(time_warp_player)

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
        # Qt 5 provides the WinMain that a WIN32-subsystem executable needs to
        # reach `main`; Qt 6 folded it into Qt6::EntryPoint and links it itself.
        if(TARGET ${TIME_WARP_QT}::WinMain)
            target_link_libraries(time_warp_ide PRIVATE ${TIME_WARP_QT}::WinMain)
        endif()
        timewarp_enable_warnings(time_warp_ide)

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
            # The bundle's frameworks live in Contents/Frameworks, and the
            # dependencies name them through `@rpath`. Without this the install
            # step strips the build tree's rpath and leaves
            # `@rpath/QtSvg.framework/...` pointing at nothing, which is a dyld
            # abort the moment the staged bundle is run: macdeployqt copies the
            # frameworks in but rewrites only what it can resolve.
            set_target_properties(time_warp_ide PROPERTIES
                INSTALL_RPATH "@executable_path/../Frameworks")
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
        #   bin/time_warp_ide                         the application
        #   share/time-warp-studio/                    the examples, the lesson
        #                                              curriculum and the snippet
        #                                              catalogue, which is where
        #                                              `app_paths.cpp`,
        #                                              `snippets.cpp` and
        #                                              `help_browser.cpp` look
        #                                              (`../share/time-warp-studio`
        #                                              relative to the binary), so an
        #                                              installed build needs no path
        #                                              configuration
        #   share/applications + share/icons           the desktop entry
        #
        # A macOS bundle is self-contained instead: the same payload lives under
        # Contents/Resources/share, which is the second place those three look when
        # they are inside a .app. Installing it anywhere else would leave a user who
        # dragged the app to /Applications with no examples and no help.
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
        # The teacher's and the learner's commands, beside the IDE. A package that
        # shipped only the window would make "the panel and the tool agree" a
        # claim about a checkout.
        if(TARGET time_warp_teach)
            install(TARGETS time_warp_teach RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
            install(TARGETS time_warp_learn RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
        endif()
        # A command, not development scaffolding: "check this program" is one of the
        # things a person who writes programs in eleven languages wants from a terminal,
        # and it is the same report the gates read.
        install(TARGETS time_warp_check RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
        install(TARGETS time_warp_api
                RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
        # The player goes beside the IDE because `--compile` looks for it there:
        # an artifact is a copy of whichever of the two it finds, and a package
        # without it would compile programs into copies of the whole IDE.
        if(TARGET time_warp_player)
            if(APPLE)
                install(TARGETS time_warp_player BUNDLE DESTINATION .)
            else()
                install(TARGETS time_warp_player
                        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
            endif()
        endif()
        # The gates and their data are deliberately not installed: they are
        # development tooling, in the checkout, and no part of the product. A
        # package that carried them would be shipping a second thing to keep green.
        # The snippet catalogue: `snippetRoot()` looks here first, so a package
        # and a build tree offer the same snippets.
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/share/snippets.json
            DESTINATION ${TIME_WARP_SHARE_DIR}/time-warp-studio
        )
        # The curriculum is shipped data too: the Lessons panel and the teacher's
        # command read the same file, so a package and a checkout teach the same
        # course.
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/share/lessons.json
            DESTINATION ${TIME_WARP_SHARE_DIR}/time-warp-studio
        )
        # The command table: the Reference page reads it, and it is the same file
        # `time_warp_gates commands` drives, so a package's reference shows what the
        # gate proved. It was development data until the page existed.
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/share/command_rows.json
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
        # The MIT notice travels with the application rather than in a dialog in
        # front of the disk image (see CPACK_DMG_SLA_USE_RESOURCE_FILE_LICENSE),
        # so this copy is the one a user can read after installing.
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE
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
        if(TARGET ${TIME_WARP_QT}::qmake)
            get_target_property(_time_warp_qmake ${TIME_WARP_QT}::qmake IMPORTED_LOCATION)
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
            # The player, spawned as a process: a compiled program is only "sent
            # to a friend" if the executable that opens it exists and draws, and
            # neither half is reachable from a unit test of the window.
            add_executable(player_tests tests/player_tests.cpp)
            target_link_libraries(player_tests PRIVATE timewarp::ui_core)
            target_include_directories(player_tests PRIVATE
                ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            target_compile_definitions(player_tests PRIVATE
                TIME_WARP_TEST_PLAYER="$<TARGET_FILE:time_warp_player>"
                TIME_WARP_TEST_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/..")
            add_dependencies(player_tests time_warp_player)
            timewarp_enable_warnings(player_tests)
            add_test(NAME player_tests COMMAND player_tests)

            add_executable(ui_layout_tests
                tests/ui_layout_tests.cpp
                tests/ui_layout_appearance_tests.cpp
                tests/ui_layout_editor_tests.cpp
                tests/ui_layout_menu_tests.cpp
                tests/ui_layout_panel_tests.cpp
                tests/ui_layout_run_tests.cpp
                tests/ui_layout_shell_tests.cpp
                tests/ui_layout_turtle_tests.cpp
            )
            target_link_libraries(ui_layout_tests PRIVATE timewarp::ui)
            # The assistant's page is exercised end to end against a stub endpoint on
            # loopback, which needs Qt Network in this binary as well as in the
            # library -- `timewarp::ai` links it privately.
            if(TARGET timewarp::ai)
                target_link_libraries(ui_layout_tests PRIVATE ${TIME_WARP_QT}::Network)
            endif()
            target_include_directories(ui_layout_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            # The snippet picker's suite reads the checkout's catalogue. The
            # resolver walks up from the executable, which reaches the checkout
            # from `native/build` but from no other build directory, so the path
            # is recorded here -- the arrangement chip8_emulator_tests uses for
            # its Examples. An installed tree still finds its own copy first.
            target_compile_definitions(ui_layout_tests PRIVATE
                TIME_WARP_TEST_SNIPPETS="${CMAKE_CURRENT_SOURCE_DIR}/share/snippets.json"
                TIME_WARP_TEST_COMMANDS="${CMAKE_CURRENT_SOURCE_DIR}/share/command_rows.json")
            timewarp_enable_warnings(ui_layout_tests)
            # The test sets QT_QPA_PLATFORM=offscreen itself, so it runs with no
            # display and belongs in the default CTest set.
            add_test(NAME ui_layout_tests COMMAND ui_layout_tests)

            # The document lifecycle gets its own binary: it is about what happens
            # to a user's file, not about where the widgets sit, and the two fail
            # for unrelated reasons.
            add_executable(ui_document_tests
                tests/ui_document_tests.cpp
                tests/ui_document_debug_tests.cpp
                tests/ui_document_edit_tests.cpp
                tests/ui_document_language_tests.cpp
                tests/ui_document_lifecycle_tests.cpp
                tests/ui_document_run_tests.cpp
                tests/ui_document_session_tests.cpp
            )
            target_link_libraries(ui_document_tests PRIVATE timewarp::ui)
            target_include_directories(ui_document_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            timewarp_enable_warnings(ui_document_tests)
            add_test(NAME ui_document_tests COMMAND ui_document_tests)

            # The canvas is a separate binary from the documents because it has
            # no window: it is a model and a painter, and testing the model
            # without constructing a MainWindow keeps a failure attributable.
            add_executable(turtle_canvas_tests tests/turtle_canvas_tests.cpp)
            target_link_libraries(turtle_canvas_tests PRIVATE timewarp::ui)
            target_include_directories(turtle_canvas_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            timewarp_enable_warnings(turtle_canvas_tests)
            add_test(NAME turtle_canvas_tests COMMAND turtle_canvas_tests)

            # Syntax highlighting is a document transform, not a widget: it is
            # tested against a bare QTextDocument so a failure names the rule
            # that broke rather than showing up as "the colours look wrong".
            # Sound and ports: arithmetic and a pseudo-terminal, so both halves
            # run on a machine with no speaker and no board attached.
            add_executable(media_tests tests/media_tests.cpp)
            target_link_libraries(media_tests PRIVATE timewarp::lang)
            target_include_directories(media_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            timewarp_enable_warnings(media_tests)
            add_test(NAME media_tests COMMAND media_tests)

            add_executable(syntax_highlighter_tests tests/syntax_highlighter_tests.cpp)
            target_link_libraries(syntax_highlighter_tests PRIVATE timewarp::ui)
            target_include_directories(syntax_highlighter_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            timewarp_enable_warnings(syntax_highlighter_tests)
            add_test(NAME syntax_highlighter_tests COMMAND syntax_highlighter_tests)

            # The IDE's text services: formatting a document, and (as they land)
            # the snippet library, find-in-files and autosave. One binary because
            # they share a concern -- text in, text out, no program run -- and
            # none of them needs a window.
            add_executable(editor_services_tests tests/editor_services_tests.cpp)
            target_link_libraries(editor_services_tests PRIVATE timewarp::ui)
            target_include_directories(editor_services_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            # The shipped catalogue, from the checkout: see ui_layout_tests.
            target_compile_definitions(editor_services_tests PRIVATE
                TIME_WARP_TEST_SNIPPETS="${CMAKE_CURRENT_SOURCE_DIR}/share/snippets.json")
            timewarp_enable_warnings(editor_services_tests)
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
            timewarp_enable_warnings(chip8_emulator_tests)
            add_test(NAME chip8_emulator_tests COMMAND chip8_emulator_tests)

            # The help browser is a data-driven document view: the resolver,
            # the index parser and the tree are all testable without a window,
            # and the dead-link check is the reason it has a suite at all.
            add_executable(help_browser_tests tests/help_browser_tests.cpp)
            target_link_libraries(help_browser_tests PRIVATE timewarp::ui)
            target_include_directories(help_browser_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            # The guides, from the checkout, for the same reason: the dead-link
            # check has to point at a real docs tree wherever the build is.
            target_compile_definitions(help_browser_tests PRIVATE
                TIME_WARP_TEST_DOCS="${CMAKE_CURRENT_SOURCE_DIR}/../docs")
            timewarp_enable_warnings(help_browser_tests)
            add_test(NAME help_browser_tests COMMAND help_browser_tests)

            # The HB-1 is a whole little machine: ROM socket, screen
            # modes, the prompt's direct mode and the same native engines the
            # editor runs. It gets its own binary because a failure should name
            # the machine, not the IDE layout around it.
            add_executable(retro_computer_tests
                tests/retro_computer_tests.cpp
                tests/retro_basic_tests.cpp
                tests/retro/paper_tests.cpp
                tests/retro/bbs_tests.cpp
                tests/retro_boot_tests.cpp
                tests/retro_config_tests.cpp
                tests/retro_dos_tests.cpp
                tests/retro_hardware_tests.cpp
                tests/retro_image_tests.cpp
                tests/retro_memory_tests.cpp
            )
            target_link_libraries(retro_computer_tests PRIVATE timewarp::ui)
            target_include_directories(retro_computer_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests)
            # One case opens the demo volume shipped in `Examples/hb1`, so the test
            # needs the checkout it is running from -- the same definition the
            # player suite uses for the same reason.
            target_compile_definitions(retro_computer_tests PRIVATE
                TIME_WARP_TEST_SOURCE_DIR="${CMAKE_CURRENT_SOURCE_DIR}/..")
            timewarp_enable_warnings(retro_computer_tests)
            add_test(NAME retro_computer_tests COMMAND retro_computer_tests)
        endif()
    else()
        message(STATUS "${TIME_WARP_QT} Widgets not found: skipping the widget UI")
    endif()
endif()
