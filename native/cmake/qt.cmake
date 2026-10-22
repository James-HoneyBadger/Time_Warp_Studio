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
# Which Qt: 6, or 5.15 for the Windows 7 artifact.

# ---------------------------------------------------------------------------
# Qt: version 6, or 5.15
# ---------------------------------------------------------------------------
# Qt 6 is what the releases ship and what every gate builds. Qt 5.15 is the
# Windows 7 build: Qt 6 requires Windows 10, and 5.15 is the last Qt that runs on
# 7 SP1. The two are source-compatible here apart from a handful of names, which
# `timewarp/core/qt_compat.hpp` supplies, so the choice is a configure option
# rather than a fork.
if(TIME_WARP_QT5)
    find_package(Qt5 5.15 QUIET COMPONENTS Core)
    set(TIME_WARP_QT Qt5)
else()
    find_package(Qt6 QUIET COMPONENTS Core)
    set(TIME_WARP_QT Qt6)
endif()
set(TIME_WARP_QT_FOUND "${${TIME_WARP_QT}_FOUND}")
set(TIME_WARP_QT_CORE_VERSION "${${TIME_WARP_QT}Core_VERSION}")
