# syntax=docker/dockerfile:1
#
# The container image published to GitHub Packages (ghcr.io).
#
# Two stages, because the build needs a compiler and the runtime does not: the
# published image carries the binary, the Qt libraries it links against and nothing
# else. There is no interpreter in the image, because there is none in the tree.
#
# Ubuntu 24.04 is the base because it is what the Linux job in `native.yml` builds
# and tests against -- the same compiler and the same Qt 6.4 -- and because it
# publishes both architectures the image is built for: linux/amd64 and
# linux/arm64, one manifest list under each tag. It used to be Arch, which is the
# distribution the project is developed on, but Arch publishes no arm64 image, so
# an Arch base could only ever be amd64: an ARM laptop or a Raspberry Pi had to
# emulate every instruction.
#
#   docker build -f packaging/docker/Dockerfile -t time-warp-studio .
#   docker run --rm -e DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix time-warp-studio
#
# Headless, for a teacher rendering pictures from a script:
#
#   docker run --rm -v "$PWD:/work" time-warp-studio \
#     --run --export /work/mandala.png /work/mandala.logo

FROM ubuntu:24.04 AS build

# The version is stamped into the binary's window title and `--version`, so it
# travels as a build argument rather than being read from a file that a release
# might not have updated.
ARG VERSION=0.0.0

# The package set the Linux job in `native.yml` installs, because the image should
# be the configuration that job proved rather than a second one.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential cmake ninja-build \
        qt6-base-dev qt6-base-dev-tools libsqlite3-dev \
        libgl1-mesa-dev libxkbcommon-dev \
    && rm -rf /var/lib/apt/lists/*

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

FROM ubuntu:24.04

# Re-declared because a build argument belongs to the stage that declares it, and
# `LABEL` below runs in this one.
ARG VERSION=0.0.0

LABEL org.opencontainers.image.title="Time Warp Studio" \
      org.opencontainers.image.description="An old-school programming environment: eleven languages, an IDE, an HB-1 home computer and a live CHIP-8" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.version="${VERSION}"

# Qt6 brings the platform plugins and the xcb libraries a window needs, and
# `libqt6sql6-sqlite` is the driver the Database tab opens its connection with.
# `zlib1g` is what classroom bundles are written with. There is no Python: the
# lesson check and the classroom run in the binary.
# `alsa-utils` is what `SOUND` and `PLAY` hand their WAV to -- a container with no
# sound device is silent, but it is silent for the right reason.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libqt6widgets6 libqt6gui6 libqt6core6 libqt6sql6-sqlite \
        libqt6printsupport6 libsqlite3-0 zlib1g \
        fontconfig fonts-dejavu-core alsa-utils libgl1 \
    && rm -rf /var/lib/apt/lists/*

# `studio` takes uid 1000 rather than whatever `useradd` would pick: the base
# image already spends 1000 on its own `ubuntu` user, and 1000 is the first user
# of nearly every desktop. The documented `-v "$PWD:/work"` hands the container a
# directory owned by that user, and the 1001 it would otherwise get cannot write
# the PNG it was asked to export there. The base image's user is deleted to free
# the id, and to leave the image one account rather than two.
RUN userdel --remove ubuntu 2>/dev/null || true; \
    groupdel ubuntu 2>/dev/null || true; \
    useradd --uid 1000 --create-home --shell /bin/bash studio

COPY --from=build /out/usr /usr
# The example programs are already in that tree, at
# /usr/share/time-warp-studio/examples, because the install rules carry them --
# which is also where File > Open Example looks. Copying /src/Examples over the
# top as well would be a second source of truth for one set of files.
COPY --from=build /src/LICENSE /usr/share/licenses/time-warp-studio/LICENSE

# A library the binary links and this stage forgot fails here, as a build that
# names it, rather than in a user's `docker run`.
RUN set -eu; \
    missing="$(ldd /usr/bin/time_warp_ide | grep 'not found' || true)"; \
    if [ -n "$missing" ]; then \
        echo "the runtime stage is missing libraries:" >&2; \
        echo "$missing" >&2; \
        exit 1; \
    fi

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
