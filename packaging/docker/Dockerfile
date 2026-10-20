# syntax=docker/dockerfile:1
#
# The container image published to GitHub Packages (ghcr.io).
#
# Two stages, because the build needs a compiler and the runtime does not: the
# published image carries the binary, the Qt libraries it links against, the
# Python engine the lesson check runs, and nothing else. Arch is the base because
# it is what the project is developed on, and because a rolling base means the Qt
# in the image is the Qt the code was written against rather than a two-year-old
# one.
#
#   docker build -f packaging/docker/Dockerfile -t time-warp-studio .
#   docker run --rm -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix time-warp-studio
#
# Headless, for a teacher rendering pictures from a script:
#
#   docker run --rm -v "$PWD:/work" time-warp-studio \
#     --run --export /work/mandala.png /work/mandala.logo

FROM archlinux:base AS build

# The version is stamped into the binary's window title and `--version`, so it
# travels as a build argument rather than being read from a file that a release
# might not have updated.
ARG VERSION=0.0.0

# `--needed` so a rebuild does not re-resolve packages that are already present.
RUN pacman -Syu --noconfirm --needed \
        base-devel cmake ninja \
        qt6-base \
    && pacman -Scc --noconfirm

WORKDIR /src
# The whole repository: the CMake build reads VERSION from the parent directory,
# installs the bridge from `native/bridge`, and the desktop entry and icon from
# `packaging/linux`.
COPY . .

# Tests are not built into the image -- they would double it and cannot run there
# without a display for the UI suites. The suites run in `native.yml` and again in
# the release workflow, before this image is pushed.
RUN cmake -S native -B native/build -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=/usr \
        -DTIME_WARP_EXPR_BUILD_TESTS=OFF \
    && cmake --build native/build \
    && DESTDIR=/out cmake --install native/build

FROM archlinux:base

# Re-declared because a build argument belongs to the stage that declares it, and
# `LABEL` below runs in this one.
ARG VERSION=0.0.0

LABEL org.opencontainers.image.title="Time Warp Studio" \
      org.opencontainers.image.description="An old-school programming environment: ten languages, an IDE, an HB-1 home computer and a live CHIP-8" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.version="${VERSION}"

# Qt6 brings the platform plugins and the xcb libraries a window needs. `python`
# is not optional: the lesson check runs the bundled engine, which is Python, and
# without it the Lessons panel reports an engine that will not start. `alsa-utils`
# is what `SOUND` and `PLAY` hand their WAV to -- a container with no sound device
# is silent, but it is silent for the right reason.
RUN pacman -Syu --noconfirm --needed \
        qt6-base python fontconfig ttf-dejavu alsa-utils \
    && pacman -Scc --noconfirm \
    && useradd --create-home --shell /bin/bash studio

COPY --from=build /out/usr /usr
# The example programs are already in that tree, at
# /usr/share/time-warp-studio/examples, because the install rules carry them --
# which is also where File > Open Example looks. Copying /src/Examples over the
# top as well would be a second source of truth for one set of files.
COPY --from=build /src/LICENSE /usr/share/licenses/time-warp-studio/LICENSE

# A non-root user with a home directory: QSettings, lesson progress and the
# classroom roster all write under $HOME, and writing them as root leaves a
# ~/.time_warp a later run cannot open.
USER studio
ENV HOME=/home/studio
WORKDIR /home/studio

# Offscreen by default would be wrong for a desktop user; the environment decides.
# What matters is that the image runs without --privileged and without a writable
# /usr.
ENTRYPOINT ["time_warp_ide"]
CMD ["--help"]
