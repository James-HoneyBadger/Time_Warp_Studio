# SPDX-License-Identifier: MIT
#
# The product version, read from the repository's `VERSION` file.
#
# Separate from the CMake `project()` version on purpose: the window title, the
# desktop entry and CPack all read this, and they must not be able to disagree with
# the file a release bumps. Included early, because packaging and the UI both need
# it.

# ---------------------------------------------------------------------------
# Product version
# ---------------------------------------------------------------------------
# Read here rather than at the packaging section, because the UI and CPack both
# need it before that point. It is the repository's VERSION file and not the CMake
# `project()` version, because the window title and the packaged release must not
# be able to disagree.
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
