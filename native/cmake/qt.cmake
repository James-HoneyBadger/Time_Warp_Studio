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
# Which Qt: 6. There is one.

# ---------------------------------------------------------------------------
# Qt 6
# ---------------------------------------------------------------------------
# The rest of the build names it `${TIME_WARP_QT}::Widgets` rather than
# `Qt6::Widgets`. That spelling was once a choice between Qt 6 and Qt 5.15, which
# existed only to build the Windows 7 artifact; with that artifact retired there is
# one answer, and the variable is kept so the target names in the module files stay
# readable beside the source lists.
find_package(Qt6 QUIET COMPONENTS Core)
set(TIME_WARP_QT Qt6)
set(TIME_WARP_QT_FOUND "${${TIME_WARP_QT}_FOUND}")
set(TIME_WARP_QT_CORE_VERSION "${${TIME_WARP_QT}Core_VERSION}")
