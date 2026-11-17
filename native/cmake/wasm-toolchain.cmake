# SPDX-License-Identifier: AGPL-3.0-only
#
# Included by `native/CMakeLists.txt` *before* `project()`, because a toolchain file
# has to be chosen before any language is enabled. Nothing here runs for a desktop
# build: `TW_BUILD_WASM` defaults to OFF and the module returns immediately.
#
# Qt for WebAssembly needs three things that are all the toolchain's business: the
# emsdk compilers, a host Qt that supplies `moc` and `rcc`, and the WebAssembly Qt
# that the result links against. Qt ships the file that wires the three together --
# `qt.toolchain.cmake`, installed with the *WebAssembly* Qt, which finds the host
# through `QT_HOST_PATH` -- so this module locates that file and hands it to CMake
# instead of repeating its contents. Repeating them is how a build ends up with the
# host's `moc` generating for the host's headers, which links and then fails in the
# browser.

if(NOT TW_BUILD_WASM)
    return()
endif()

if(CMAKE_TOOLCHAIN_FILE)
    # Naming one explicitly is how the workflow and a developer with an unusual
    # layout point this at their own; the search below is only a fallback.
    message(STATUS "TW_BUILD_WASM: using the toolchain file already given: ${CMAKE_TOOLCHAIN_FILE}")
else()
    set(TIME_WARP_WASM_TOOLCHAIN "")

    if(DEFINED ENV{QT_WASM_TOOLCHAIN} AND EXISTS "$ENV{QT_WASM_TOOLCHAIN}")
        set(TIME_WARP_WASM_TOOLCHAIN "$ENV{QT_WASM_TOOLCHAIN}")
    elseif(DEFINED ENV{QT_WASM}
           AND EXISTS "$ENV{QT_WASM}/lib/cmake/Qt6/qt.toolchain.cmake")
        # The variable the Qt for WebAssembly container sets to its own install, so a
        # build inside that image needs no arguments at all.
        set(TIME_WARP_WASM_TOOLCHAIN "$ENV{QT_WASM}/lib/cmake/Qt6/qt.toolchain.cmake")
    endif()

    if(NOT TIME_WARP_WASM_TOOLCHAIN)
        message(FATAL_ERROR
            "TW_BUILD_WASM=ON needs Qt for WebAssembly, and no toolchain file was\n"
            "found. Pass\n"
            "  -DCMAKE_TOOLCHAIN_FILE=<wasm Qt>/lib/cmake/Qt6/qt.toolchain.cmake\n"
            "or set QT_WASM to that install, or QT_WASM_TOOLCHAIN to the file itself.\n"
            "The wasm.yml workflow builds in an image that sets QT_WASM;\n"
            "docs/maintainer/wasm.md has the commands for a local build.")
    endif()

    set(CMAKE_TOOLCHAIN_FILE "${TIME_WARP_WASM_TOOLCHAIN}" CACHE FILEPATH
        "The Qt for WebAssembly toolchain file" FORCE)
endif()

message(STATUS "TW_BUILD_WASM: toolchain ${CMAKE_TOOLCHAIN_FILE}")

# Qt's toolchain refuses to configure without a host Qt: a cross build can compile, but
# only the host's `moc` and `rcc` can generate code for it. The Qt for WebAssembly
# container installs the host beside the wasm ones (`<prefix>/gcc_64` next to
# `<prefix>/wasm_singlethread` and `<prefix>/wasm_multithread`), and its own `qt-cmake`
# wrapper passes that path, so the same layout is derived here rather than requiring
# every caller to repeat it. A `QT_HOST_PATH` in the environment is promoted to the
# CMake variable, because Qt's toolchain reads the variable.
if(NOT QT_HOST_PATH AND DEFINED ENV{QT_HOST_PATH})
    set(QT_HOST_PATH "$ENV{QT_HOST_PATH}" CACHE PATH
        "The desktop Qt that supplies moc and rcc" FORCE)
endif()

if(NOT QT_HOST_PATH)
    get_filename_component(prefix "${CMAKE_TOOLCHAIN_FILE}" DIRECTORY)
    get_filename_component(prefix "${prefix}" DIRECTORY)
    get_filename_component(prefix "${prefix}" DIRECTORY)
    get_filename_component(prefix "${prefix}" DIRECTORY)
    if(EXISTS "${prefix}/../gcc_64/lib/cmake/Qt6")
        set(QT_HOST_PATH "${prefix}/../gcc_64" CACHE PATH
            "The desktop Qt that supplies moc and rcc" FORCE)
        message(STATUS "TW_BUILD_WASM: host Qt ${QT_HOST_PATH}")
    endif()
endif()
