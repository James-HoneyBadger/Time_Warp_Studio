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

.PHONY: all host amiga amiga-docker clean

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

clean:
	rm -f $(APP)
