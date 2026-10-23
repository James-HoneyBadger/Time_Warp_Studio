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
# Install rules, the desktop entry and CPack: what a package contains.

# ---------------------------------------------------------------------------
# Packages
# ---------------------------------------------------------------------------
#
# `cpack` in the build directory writes the artifact each platform installs
# with: a `.deb` (and `-G RPM` an `.rpm` where `rpmbuild` is installed), a NSIS
# `.exe` installer and a `.zip` on Windows, and a `.dmg` on macOS. `release.yml`
# builds all of them from a tag, and builds the AppImage beside them with
# `linuxdeploy` -- the Qt bundling an AppImage needs is a tool rather than a CPack
# generator.
#
# The Linux packages install into `/usr`, not into the build's prefix: a `.deb`
# that puts a binary in `/usr/local/bin` is a `.deb` whose files no package
# manager owns. Everything the application needs at run time is already installed
# by the `install()` rules above -- the binary, the bridge, the snippets, the
# docs, the desktop entry and the icon -- so the package is simply those paths.
# The version was read at the top of this file, before any of that needed it.

set(CPACK_PACKAGE_NAME "time-warp-studio")
set(CPACK_PACKAGE_VENDOR "James Temple")
set(CPACK_PACKAGE_CONTACT "James Temple <james@honey-badger.org>")
set(CPACK_PACKAGE_VERSION "${TIME_WARP_PACKAGE_VERSION}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "Old-school programming environment: eleven languages, an IDE, and two complete machines")
set(CPACK_PACKAGE_DESCRIPTION
    "Time Warp Studio is a workbench for old-school programming: BASIC, APL, Logo, \
PILOT, FOCAL, Scheme, CHIP-8, COMAL, REXX and Smalltalk in one window, each with an \
integrated turtle-graphics canvas and a debugger. Inside it are two complete \
machines: the HB-1, a bespoke 1982 home computer from a parallel universe with a \
eleven-language ROM socket, a CRT and its own disk operating system, and a live \
CHIP-8 with an assembler, keypad and register inspector. Every language runs \
natively in C++, including the lesson check and the classroom.")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/James-HoneyBadger/Time_Warp_Studio")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/../LICENSE")

# No Python: the lesson check and the classroom are in the binary. `zlib1g` is
# what classroom bundles are written with -- a class zip a school can open is
# worth one library every desktop already has. The Qt packages are the libraries
# the binary links against plus the platform plugins it loads at run time --
# without the plugins the application starts and immediately reports that it
# cannot load a platform plugin.
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "James Temple <james@honey-badger.org>")
set(CPACK_DEBIAN_PACKAGE_SECTION "education")
set(CPACK_DEBIAN_PACKAGE_DEPENDS
    "libqt6widgets6, libqt6gui6, libqt6core6, qt6-qpa-plugins, libsqlite3-0, zlib1g")
# Only when this build was made with it: a package that names a library the
# binary does not link is a needless install, and one that omits a library the
# binary does link fails to start.
if(${TIME_WARP_QT}Svg_FOUND)
    string(APPEND CPACK_DEBIAN_PACKAGE_DEPENDS ", libqt6svg6")
endif()
if(${TIME_WARP_QT}PrintSupport_FOUND)
    string(APPEND CPACK_DEBIAN_PACKAGE_DEPENDS ", libqt6printsupport6")
    # The RPM needs nothing extra: Fedora ships libQt6PrintSupport in
    # qt6-qtbase-gui, which CPACK_RPM_PACKAGE_REQUIRES already names.
endif()
set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
# Named explicitly rather than discovered: CPackDeb asks `dpkg` for the
# architecture and falls back to i386 when it is absent, which is how a build on a
# machine without dpkg produced a `.deb` whose name has no architecture in it.
if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64|amd64")
    set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
    set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "arm64")
endif()

set(CPACK_RPM_PACKAGE_LICENSE "MIT")
set(CPACK_RPM_PACKAGE_GROUP "Education")
set(CPACK_RPM_PACKAGE_REQUIRES "qt6-qtbase-gui, sqlite-libs, zlib")
if(${TIME_WARP_QT}Svg_FOUND)
    string(APPEND CPACK_RPM_PACKAGE_REQUIRES ", qt6-qtsvg")
endif()
# The same pin as the DEB's, for a sharper reason: CPackRPM asks rpmbuild for
# `%{_target_cpu}` and, when the answer does not come back, writes the *system
# name* instead -- which is how a release produced
# `time-warp-studio-16.0.0-Linux.rpm`, an architecture no rpm-based system has.
# The architecture also becomes rpmbuild's `--target`, so it is not only a file
# name.
if(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|AMD64|amd64")
    set(CPACK_RPM_PACKAGE_ARCHITECTURE "x86_64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
    set(CPACK_RPM_PACKAGE_ARCHITECTURE "aarch64")
endif()
# Named rather than left to the default, so the file name in the release notes
# and the file on the release page cannot drift apart.
set(CPACK_RPM_PACKAGE_RELEASE "1")
# And *this* one is what makes the architecture appear in the name at all. Unless
# `CPACK_RPM_FILE_NAME` is the literal string `RPM-DEFAULT`, CPackRPM falls back
# to the old format -- `${CPACK_PACKAGE_FILE_NAME}.rpm`, and CPack builds that
# from the *system name*, hence `time-warp-studio-16.0.0-Linux.rpm`. With
# RPM-DEFAULT it stops pinning a name and lets rpmbuild's own
# `<name>-<version>-<release>.<arch>.rpm` rule apply, where the architecture is
# the one pinned above.
set(CPACK_RPM_FILE_NAME "RPM-DEFAULT")

if(WIN32)
    # NSIS is the installer, and the ZIP beside it is the portable answer for
    # somebody who would rather not run one. `CPACK_PACKAGE_FILE_NAME` is set
    # rather than derived: on Windows the default carries `win64` or `win32`
    # from the host, which is right today and would quietly stop being right on
    # a different runner.
    set(CPACK_GENERATOR "NSIS;ZIP")
    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        set(_time_warp_platform_tag "win64")
    else()
        set(_time_warp_platform_tag "win32")
    endif()
    # Two installers can exist for one architecture, so which Windows they want
    # is in the name rather than only in the release notes.
    if(TIME_WARP_WIN7)
        string(APPEND _time_warp_platform_tag "-win7")
    endif()
    set(CPACK_PACKAGE_FILE_NAME
        "time-warp-studio-${TIME_WARP_PACKAGE_VERSION}-${_time_warp_platform_tag}")

    set(CPACK_PACKAGE_INSTALL_DIRECTORY "Time Warp Studio")
    set(CPACK_NSIS_DISPLAY_NAME "Time Warp Studio")
    set(CPACK_NSIS_PACKAGE_NAME "Time Warp Studio")
    set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES64")
    # Forward slashes, deliberately. These two values are written into
    # `CPackConfig.cmake` as CMake strings without being re-escaped, so a native
    # Windows path is a parse error the moment it contains a `\a` or a `\t` --
    # which is exactly what a checkout under `D:\a\...` produces, and how the
    # first release attempt died with "Invalid character escape '\a'". NSIS
    # accepts both separators, so nothing is lost by the CMake-shaped path.
    file(TO_CMAKE_PATH
        "${CMAKE_CURRENT_SOURCE_DIR}/../packaging/windows/time-warp-studio.ico"
        _time_warp_nsis_icon)
    set(CPACK_NSIS_MUI_ICON "${_time_warp_nsis_icon}")
    set(CPACK_NSIS_INSTALLED_ICON_NAME "bin/time_warp_ide.exe")
    # One Start Menu entry and one desktop shortcut, both named after the
    # application rather than after the executable.
    set(CPACK_PACKAGE_EXECUTABLES "time_warp_ide" "Time Warp Studio")
    set(CPACK_CREATE_DESKTOP_LINKS "time_warp_ide")
    set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)
    set(CPACK_NSIS_MODIFY_PATH OFF)
    # A URL in the Add/Remove Programs entry, and the same page the Linux
    # packages name.
    set(CPACK_NSIS_URL_INFO_ABOUT
        "https://github.com/James-HoneyBadger/Time_Warp_Studio")
elseif(APPLE)
    # DragNDrop is CPack's .dmg generator: it puts the installed bundle and an
    # /Applications symlink on one disk image, which is the shape a Mac user
    # expects to drag from.
    set(CPACK_GENERATOR "DragNDrop")
    set(CPACK_PACKAGE_FILE_NAME
        "Time_Warp_Studio-${TIME_WARP_PACKAGE_VERSION}-macOS-universal")
    set(CPACK_DMG_VOLUME_NAME "Time Warp Studio")
    set(CPACK_DMG_FORMAT "UDZO")
    set(CPACK_DMG_DISABLE_APPLICATIONS_SYMLINK OFF)
    # CPack 3.22 and older -- and any project whose policy level predates
    # CMP0133, which is this one -- put CPACK_RESOURCE_FILE_LICENSE on the image
    # as an agreement hdiutil shows before the volume mounts. That is a dialog
    # between a user and a MIT licence they receive in full inside the bundle,
    # and it breaks every non-interactive mount: `hdiutil attach` prints the
    # agreement and then cancels when there is no terminal to accept it, and
    # `-acceptlicense`, its one escape, no longer exists. The deb, rpm and
    # installer generators still read the license from CPACK_RESOURCE_FILE_LICENSE.
    set(CPACK_DMG_SLA_USE_RESOURCE_FILE_LICENSE OFF)
    set(CPACK_PACKAGING_INSTALL_PREFIX "/")
else()
    # DEB only by default: `rpmbuild` is not installed everywhere, and a
    # generator that fails makes `cpack` fail for somebody who only wanted the deb.
    set(CPACK_GENERATOR "DEB")
endif()

include(CPack)
