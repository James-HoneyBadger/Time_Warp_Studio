# SPDX-License-Identifier: MIT
#
# The documentation gates as a project of their own.
#
#   cmake -S native/tools/docs -B native/tools/docs/build
#   cmake --build native/tools/docs/build -j
#   ./native/tools/docs/build/time_warp_docs links
#
# Why this is not part of native/CMakeLists.txt: these three checks run on every
# documentation change, and a documentation pull request must not wait for Qt to be
# installed, found and linked. Nothing here includes a Qt header, links a library,
# opens a socket or reads a clock; a C++20 compiler and CMake are the whole
# requirement, which is what makes the gate finish in seconds on a runner that has
# no desktop stack at all.
#
# It is also not a subdirectory of the native build, because the moment it was one
# it would inherit the Qt option set and the dependency on the IDE's configuration
# that the split exists to avoid.

cmake_minimum_required(VERSION 3.20)

project(time_warp_docs
    VERSION 1.0.0
    DESCRIPTION "Time Warp Studio documentation gates (links, claims, version, markdown)"
    LANGUAGES CXX
)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

option(TIME_WARP_DOCS_WERROR "Treat compiler warnings as errors" ON)

# The sources are UTF-8 because the messages carry the project's emoji prefixes.

add_executable(time_warp_docs
    src/main.cpp
    src/docs/text.cpp
    src/docs/pipes.cpp
    src/docs/links.cpp
    src/docs/claims.cpp
    src/docs/version.cpp
    src/docs/md_heading_rules.cpp
    src/docs/md_inline_rules.cpp
    src/docs/md_code_rules.cpp
    src/docs/md_lines.cpp
    src/docs/md_lint.cpp
    src/docs/md_list_rules.cpp
    src/docs/md_whitespace_rules.cpp
)

# `../../include` is where the tools' shared command line lives
# (`timewarp/tools/cli.hpp`); `src` is this project's own headers.
target_include_directories(time_warp_docs PRIVATE src ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
target_compile_features(time_warp_docs PRIVATE cxx_std_20)

target_compile_options(time_warp_docs PRIVATE
    -Wall -Wextra -Wpedantic -Wshadow -Wcast-qual
)
if(TIME_WARP_DOCS_WERROR)
    target_compile_options(time_warp_docs PRIVATE -Werror)
endif()
