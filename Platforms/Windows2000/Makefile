# Time Warp IDE for Windows 2000
# Complete implementation targeting Windows 2000 (WINVER 0x0500)

CC = i686-w64-mingw32-gcc
WINDRES = i686-w64-mingw32-windres
CFLAGS = -DWINVER=0x0500 -D_WIN32_WINNT=0x0500 -D_WIN32_IE=0x0500 -Wall -O2 -mwindows
LIBS = -lcomctl32 -lcomdlg32 -lgdi32 -luser32 -lkernel32
TARGET = TimeWarpIDE.exe

SRCS = src/main.c src/editor.c src/canvas.c src/console.c \
       src/basic_interpreter.c src/pilot_interpreter.c \
       src/logo_interpreter.c src/pascal_interpreter.c \
       src/forth_interpreter.c src/debugger.c src/file_ops.c

OBJS = $(SRCS:.c=.o)
RES_OBJ = resources/timewarp.res.o

all: $(TARGET)

$(TARGET): $(OBJS) $(RES_OBJ)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(RES_OBJ) $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(RES_OBJ): resources/timewarp.rc
	$(WINDRES) -i $< -o $@

clean:
	rm -f $(OBJS) $(RES_OBJ) $(TARGET)

.PHONY: all clean
