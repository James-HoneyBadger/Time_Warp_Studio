# Portable Makefile for OS/2 Warp 4 build
# Targets: emx (GCC/EMX), watcom (OpenWatcom)

APP_NAME = timewarp_os2
BIN_DIR  = bin
OBJ_DIR  = build
SRC_DIR  = src
RES_DIR  = resources

SRCS = \
	$(SRC_DIR)/main.c \
	$(SRC_DIR)/pm_app.c

OBJS_EMX   = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/emx/%.o)
OBJS_WAT   = $(SRCS:$(SRC_DIR)/%.c=$(OBJ_DIR)/wat/%.obj)

RC_SRC     = $(RES_DIR)/timewarp.rc
RES_EMX    = $(OBJ_DIR)/emx/timewarp.res
RES_WAT    = $(OBJ_DIR)/wat/timewarp.res

all: help

.PHONY: help
help:
	@echo "Targets:"
	@echo "  make emx     # Build with EMX/GCC on OS/2"
	@echo "  wmake watcom # Build with OpenWatcom (or make watcom with ow make)"

# =========================
# EMX / GCC (OS/2) build
# =========================
CC_EMX = gcc
CFLAGS_EMX = -Zomf -O2 -Wall -Wextra -I$(SRC_DIR)
LDFLAGS_EMX = -Zomf
LIBS_EMX = -lpmwin -lpmgpi
RC_EMX = rc.exe

emx: prepdirs $(BIN_DIR)/$(APP_NAME).exe

$(BIN_DIR)/$(APP_NAME).exe: $(OBJS_EMX) $(RES_EMX)
	$(CC_EMX) $(LDFLAGS_EMX) -o $@ $(OBJS_EMX) $(RES_EMX) $(LIBS_EMX)

$(OBJ_DIR)/emx/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC_EMX) $(CFLAGS_EMX) -c $< -o $@

$(RES_EMX): $(RC_SRC)
	@mkdir -p $(dir $@)
	$(RC_EMX) -r -fo=$@ $<

# =========================
# OpenWatcom build
# =========================
WCL = wcl386
WRC = wrc
CFLAGS_WAT = -bt=os2 -d2 -I$(SRC_DIR)
LIBS_WAT = pmwin.lib pmgpi.lib

watcom: prepdirs $(BIN_DIR)/$(APP_NAME).exe.wat

$(BIN_DIR)/$(APP_NAME).exe.wat: $(OBJS_WAT) $(RES_WAT)
	$(WCL) $(CFLAGS_WAT) -fe=$@ $(OBJS_WAT) $(RES_WAT) $(LIBS_WAT)

$(OBJ_DIR)/wat/%.obj: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(WCL) $(CFLAGS_WAT) -c $< -fo=$@

$(RES_WAT): $(RC_SRC)
	@mkdir -p $(dir $@)
	$(WRC) -bt=os2 -r -fo=$@ $<

.PHONY: prepdirs clean
prepdirs:
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)/emx $(OBJ_DIR)/wat

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)
