# SPDX-License-Identifier: MIT
#
# The product version, read from the repository's `VERSION` file.
#
# Separate from the CMake `project()` version on purpose: the window title, the
# Windows version resource, the macOS bundle's Info.plist and CPack all read this,
# and they must not be able to disagree with the file a release bumps. Included
# early, because packaging and the UI both need it.

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
