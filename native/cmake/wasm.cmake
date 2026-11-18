# SPDX-License-Identifier: AGPL-3.0-only
#
# Included by `native/CMakeLists.txt` before the libraries, and only when
# `TW_BUILD_WASM` is ON. It holds the three facts that have to be known before a target
# is created -- how much memory the module may ask for, where its output goes, and that
# the suites are off -- plus the one function a target calls afterwards.
#
# Why the memory figures are set here rather than in the toolchain: Qt reads
# `QT_WASM_INITIAL_MEMORY` and `QT_WASM_MAXIMUM_MEMORY` at finalisation, so they must be
# ordinary variables in the directory scope, and the toolchain file is included before
# there is a directory scope this module can see.

if(NOT TW_BUILD_WASM)
    return()
endif()

# The language engines catch exceptions to turn invalid programs into useful
# output. Emscripten disables exception catching by default, so compile the
# Studio's C++ code with its JavaScript-based exception support. Qt itself stays
# on the prebuilt package's default exception model.
add_compile_options(-fexceptions)

# The Qt packages for WebAssembly come in two flavours, and this build needs the
# threaded one: every runner in the IDE is a `QThread`, and Qt built without thread
# support compiles `QThread::start()` to an empty function. A single-threaded module
# therefore loads, paints and edits, and does nothing at all when Run is pressed --
# the silent failure this project treats as its characteristic bug. The threaded
# module needs `SharedArrayBuffer`, which a page may use only when it is
# cross-origin isolated; `wasm/coi-serviceworker.js` supplies the two headers on a
# host that cannot set them.
set(QT_WASM_PTHREAD_POOL_SIZE 4 CACHE STRING
    "Worker threads the module starts with, for the interpreters")

# 256 MB initial and a 1 GB ceiling. Emscripten fixes the heap at the initial figure
# unless growth is enabled, so this is the amount the IDE has to work in rather than a
# hint: 256 MB opens a window, runs a program and draws on the canvas with room to
# spare. Growth past the ceiling aborts the module, which is why it is a number in the
# page's troubleshooting section rather than an accident.
set(QT_WASM_INITIAL_MEMORY 268435456 CACHE STRING
    "Initial WebAssembly memory in bytes (256 MB)")
set(QT_WASM_MAXIMUM_MEMORY 1073741824 CACHE STRING
    "Maximum WebAssembly memory in bytes (1 GB)")

# A browser tab cannot run `ctest`: there is no process to spawn, no exit code to read
# and no filesystem the suites could write a temporary directory into. Turning them off
# here rather than letting them fail is what keeps a wasm configure honest rather than
# a configure that produces 23 tests nobody can execute.
set(TIME_WARP_EXPR_BUILD_TESTS OFF CACHE BOOL
    "Test suites are off in the WebAssembly build" FORCE)

# Where the deployable directory is assembled. `wasm/index.html` is the page and
# `wasm/coi-serviceworker.js` is the worker that makes the page cross-origin isolated;
# Qt writes `time_warp_ide.js`, `time_warp_ide.wasm` and `qtloader.js` beside them. The
# build copies the two page files here, so the directory is complete on its own: it is
# what a server serves, what the workflow uploads, and the only thing a tag publishes.
set(TIME_WARP_WASM_OUTPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../wasm/dist"
    CACHE PATH "Where the WebAssembly build is assembled for deployment")
set(TIME_WARP_WASM_PAGE_DIR "${CMAKE_CURRENT_LIST_DIR}/../../wasm")
file(MAKE_DIRECTORY "${TIME_WARP_WASM_OUTPUT_DIR}")

# Called by the UI module on the executable it just created. `qt_add_executable` has
# already run Qt's finalisation by the time a target exists, so this only adds what this
# project needs on top: the filesystem library behind IndexedDB, the resource the example
# programs travel in, and the page beside the module.
function(timewarp_enable_wasm target)
    # `FS.mount(IDBFS, ...)` in wasm_storage.cpp needs the library that implements it.
    # Emscripten does not link a filesystem backend that a C++ translation unit never
    # names, and the name is inside an `EM_JS` block the linker cannot see; without
    # this the mount throws at run time and the volume is never persisted.
    target_link_options(${target} PRIVATE
        "SHELL:-lidbfs.js"
        "SHELL:-sASYNCIFY"
        -Os
        -fexceptions
    )

    # `qt_add_executable` puts its output in the build directory's root unless told
    # otherwise; the deployment directory is named so `wasm/index.html` can sit beside
    # the module.
    set_target_properties(${target} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${TIME_WARP_WASM_OUTPUT_DIR}"
    )

    # Qt writes the module, its glue and the loader from its own finalisation; the page
    # and the isolation worker are this project's and have to arrive in the same
    # directory. `.nojekyll` turns off GitHub Pages' Jekyll pass, which silently drops
    # files and directories whose name begins with an underscore.
    add_custom_command(TARGET ${target} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${TIME_WARP_WASM_PAGE_DIR}/index.html"
                "${TIME_WARP_WASM_OUTPUT_DIR}/index.html"
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${TIME_WARP_WASM_PAGE_DIR}/coi-serviceworker.js"
                "${TIME_WARP_WASM_OUTPUT_DIR}/coi-serviceworker.js"
        COMMAND ${CMAKE_COMMAND} -E touch "${TIME_WARP_WASM_OUTPUT_DIR}/.nojekyll"
        COMMENT "Assembling the WebAssembly page in ${TIME_WARP_WASM_OUTPUT_DIR}")

    # The example programs travel inside the module: the page has no checkout beside it
    # and no install directory to read, so a gallery that looked for `Examples/` on a
    # disk would show nothing, and the browser build would open on an empty editor. The
    # glob is deliberate -- the corpus is data, and a new program should not need a
    # second edit to be listed -- and a fresh configure picks up a new file.
    file(GLOB_RECURSE examples "${TIME_WARP_WASM_PAGE_DIR}/../Examples/*")
    if(examples)
        qt_add_resources(${target} "examples"
            PREFIX "/examples"
            BASE "${TIME_WARP_WASM_PAGE_DIR}/../Examples"
            FILES ${examples})
        list(LENGTH examples example_count)
        message(STATUS "TW_BUILD_WASM: ${example_count} example programs in the module")
    endif()

    # The three data files and the guides are read at run time by `shareFile()` and
    # `docsRoot()`, which look beside the binary on a desktop. A page has nothing beside
    # it, so the same files travel inside the module and both resolvers check their
    # resource first: the lessons panel, the snippet catalogue, the command reference and
    # **Help > Documentation** all work without a filesystem.
    qt_add_resources(${target} "share"
        PREFIX "/share"
        BASE "${CMAKE_CURRENT_SOURCE_DIR}/share"
        FILES
            "${CMAKE_CURRENT_SOURCE_DIR}/share/command_rows.json"
            "${CMAKE_CURRENT_SOURCE_DIR}/share/lessons.json"
            "${CMAKE_CURRENT_SOURCE_DIR}/share/snippets.json")

    file(GLOB_RECURSE guides "${CMAKE_CURRENT_SOURCE_DIR}/../docs/*")
    if(guides)
        qt_add_resources(${target} "docs"
            PREFIX "/docs"
            BASE "${CMAKE_CURRENT_SOURCE_DIR}/../docs"
            FILES ${guides})
        list(LENGTH guides guide_count)
        message(STATUS "TW_BUILD_WASM: ${guide_count} guides in the module")
    endif()

    message(STATUS "TW_BUILD_WASM: ${target} for the browser, output ${TIME_WARP_WASM_OUTPUT_DIR}")
endfunction()
