# Time_Warp_Amiga Makefile
# Build host-native (for quick testing) or m68k Amiga binary if toolchain is available.

APP := timewarp_amiga
SRC := src/main.c src/interpreter.c
INC := -Isrc

# Default to host build with gcc
CC_HOST ?= gcc
CFLAGS_HOST ?= -O2 -std=c11 -Wall -Wextra -pedantic $(INC)
LDFLAGS_HOST ?=

# Amiga cross (requires m68k-amigaos-gcc on PATH)
CC_AMIGA ?= m68k-amigaos-gcc
CFLAGS_AMIGA ?= -O2 -std=c11 -Wall -Wextra -pedantic $(INC)
LDFLAGS_AMIGA ?=

# Dockerized cross-build using bebbo's toolchain image
DOCKER_IMAGE ?= ghcr.io/bebbo/amiga-gcc:latest
LOCAL_IMAGE ?= timewarp/amiga-toolchain:local

.PHONY: all host amiga amiga-docker amiga-docker-local amiga-docker-mount-toolchain clean

all: host

host: $(SRC)
	$(CC_HOST) $(CFLAGS_HOST) -o $(APP) $(SRC) $(LDFLAGS_HOST)
	@echo "Built ./$(APP) (host)"

amiga: $(SRC)
	$(CC_AMIGA) $(CFLAGS_AMIGA) -o $(APP) $(SRC) $(LDFLAGS_AMIGA)
	@echo "Built ./$(APP) (Amiga m68k)"

# Build using Docker (no local toolchain required)
amiga-docker:
	@command -v docker >/dev/null 2>&1 || { echo "❌ Docker is required for 'amiga-docker' target."; exit 1; }
	docker run --rm -v $(CURDIR):/src -w /src $(DOCKER_IMAGE) make amiga

# Build a local toolchain image from source and then build the Amiga binary.
# This is a fallback when GHCR pulls are blocked. First build is slow.
amiga-docker-local: Dockerfile.local-toolchain
	@command -v docker >/dev/null 2>&1 || { echo "❌ Docker is required for 'amiga-docker-local' target."; exit 1; }
	docker build -t $(LOCAL_IMAGE) -f Dockerfile.local-toolchain .
	docker run --rm -v $(CURDIR):/src -w /src $(LOCAL_IMAGE) make amiga

# Use a deps-only image and a host-prepared toolchain mounted into the container.
# Provide TOOLCHAIN_DIR=/absolute/path/to/amiga-gcc (host clone/build).
amiga-docker-mount-toolchain:
	@command -v docker >/dev/null 2>&1 || { echo "❌ Docker is required for 'amiga-docker-mount-toolchain' target."; exit 1; }
	@if [ -z "$(TOOLCHAIN_DIR)" ]; then echo "❌ Set TOOLCHAIN_DIR to your host amiga-gcc path (e.g., $$HOME/amiga-gcc)"; exit 2; fi
	@if [ ! -d "$(TOOLCHAIN_DIR)" ]; then echo "❌ TOOLCHAIN_DIR does not exist: $(TOOLCHAIN_DIR)"; exit 2; fi
	docker build -t timewarp/amiga-deps -f Dockerfile.local-toolchain --target deps-only .
	docker run --rm -v $(TOOLCHAIN_DIR):/opt/amiga-gcc -v $(CURDIR):/src -w /src timewarp/amiga-deps bash -lc 'make -C /opt/amiga-gcc -j"$$(nproc)" all && make amiga'

clean:
	rm -f $(APP)
