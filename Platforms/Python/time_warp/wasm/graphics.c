/**
 * WASM Graphics Interface
 * Turtle graphics primitives for Logo and graphics-based languages
 * Communicates with JavaScript canvas rendering
 */

#include <math.h>
#include <string.h>
#include "common.h"

// Turtle graphics state
typedef struct {
  double x;
  double y;
  double angle; // 0-360 degrees
  int pen_down;
  uint32_t pen_color;
  double pen_width;
  int visible;
} TurtleState;

// Graphics command buffer
typedef struct {
  char type[32]; // "line", "circle", "text", etc.
  double x1, y1;
  double x2, y2;
  uint32_t color;
  double width;
  char text[256];
} GraphicsCommand;

#define MAX_GRAPHICS_COMMANDS 10000
#define GRAPHICS_BUFFER_SIZE (MAX_GRAPHICS_COMMANDS * sizeof(GraphicsCommand))

// Global graphics state
static struct {
  TurtleState turtle;
  GraphicsCommand* commands;
  int command_count;
  int buffer_size;
  int initialized;
  // Canvas dimensions (from JS)
  double canvas_width;
  double canvas_height;
} graphics_state;

/**
 * Initialize graphics system
 */
void graphics_init() {
  if (graphics_state.initialized) return;

  memset(&graphics_state, 0, sizeof(graphics_state));

  // Initialize turtle
  graphics_state.turtle.x = 0;
  graphics_state.turtle.y = 0;
  graphics_state.turtle.angle = 0;
  graphics_state.turtle.pen_down = 1;
  graphics_state.turtle.pen_color = 0x000000; // Black
  graphics_state.turtle.pen_width = 1.0;
  graphics_state.turtle.visible = 1;

  // Allocate command buffer
  graphics_state.commands = (GraphicsCommand*)malloc(GRAPHICS_BUFFER_SIZE);
  graphics_state.command_count = 0;
  graphics_state.buffer_size = MAX_GRAPHICS_COMMANDS;
  graphics_state.canvas_width = 640;
  graphics_state.canvas_height = 480;

  graphics_state.initialized = 1;
}

/**
 * Add graphics command to buffer
 */
static void add_command(const GraphicsCommand* cmd) {
  if (!graphics_state.initialized) {
    graphics_init();
  }

  if (graphics_state.command_count < graphics_state.buffer_size) {
    memcpy(
      &graphics_state.commands[graphics_state.command_count],
      cmd,
      sizeof(GraphicsCommand)
    );
    graphics_state.command_count++;
  }
}

/**
 * Turtle forward
 */
void turtle_forward(double distance) {
  if (!graphics_state.initialized) graphics_init();

  double rad = graphics_state.turtle.angle * 3.14159265359 / 180.0;
  double new_x = graphics_state.turtle.x + distance * cos(rad);
  double new_y = graphics_state.turtle.y + distance * sin(rad);

  // Draw line if pen is down
  if (graphics_state.turtle.pen_down) {
    GraphicsCommand cmd;
    strcpy(cmd.type, "line");
    cmd.x1 = graphics_state.turtle.x;
    cmd.y1 = graphics_state.turtle.y;
    cmd.x2 = new_x;
    cmd.y2 = new_y;
    cmd.color = graphics_state.turtle.pen_color;
    cmd.width = graphics_state.turtle.pen_width;
    add_command(&cmd);
  }

  graphics_state.turtle.x = new_x;
  graphics_state.turtle.y = new_y;
}

/**
 * Turtle backward
 */
void turtle_back(double distance) {
  turtle_forward(-distance);
}

/**
 * Turtle turn right
 */
void turtle_right(double angle) {
  graphics_state.turtle.angle += angle;
  if (graphics_state.turtle.angle >= 360) {
    graphics_state.turtle.angle -= 360;
  }
}

/**
 * Turtle turn left
 */
void turtle_left(double angle) {
  turtle_right(-angle);
}

/**
 * Pen down
 */
void turtle_pendown() {
  graphics_state.turtle.pen_down = 1;
}

/**
 * Pen up
 */
void turtle_penup() {
  graphics_state.turtle.pen_down = 0;
}

/**
 * Set pen color
 */
void turtle_setcolor(uint32_t color) {
  graphics_state.turtle.pen_color = color;
}

/**
 * Set pen width
 */
void turtle_setwidth(double width) {
  graphics_state.turtle.pen_width = width;
}

/**
 * Clear canvas
 */
void turtle_clear() {
  GraphicsCommand cmd;
  strcpy(cmd.type, "clear");
  cmd.color = 0xFFFFFF; // White background
  add_command(&cmd);

  graphics_state.command_count = 0;
  graphics_state.turtle.x = 0;
  graphics_state.turtle.y = 0;
  graphics_state.turtle.angle = 0;
}

/**
 * Get turtle state
 */
TurtleState* turtle_get_state() {
  if (!graphics_state.initialized) graphics_init();
  return &graphics_state.turtle;
}

/**
 * Set turtle position (absolute)
 */
void turtle_setpos(double x, double y) {
  if (!graphics_state.initialized) graphics_init();

  if (graphics_state.turtle.pen_down) {
    GraphicsCommand cmd;
    strcpy(cmd.type, "line");
    cmd.x1 = graphics_state.turtle.x;
    cmd.y1 = graphics_state.turtle.y;
    cmd.x2 = x;
    cmd.y2 = y;
    cmd.color = graphics_state.turtle.pen_color;
    cmd.width = graphics_state.turtle.pen_width;
    add_command(&cmd);
  }

  graphics_state.turtle.x = x;
  graphics_state.turtle.y = y;
}

/**
 * Set turtle heading (absolute angle)
 */
void turtle_setheading(double angle) {
  graphics_state.turtle.angle = fmod(angle, 360);
}

/**
 * Draw filled circle
 */
void turtle_circle(double radius) {
  GraphicsCommand cmd;
  strcpy(cmd.type, "circle");
  cmd.x1 = graphics_state.turtle.x;
  cmd.y1 = graphics_state.turtle.y;
  cmd.x2 = radius;
  cmd.color = graphics_state.turtle.pen_color;
  add_command(&cmd);
}

/**
 * Draw filled rectangle
 */
void turtle_rect(double width, double height) {
  GraphicsCommand cmd;
  strcpy(cmd.type, "rect");
  cmd.x1 = graphics_state.turtle.x;
  cmd.y1 = graphics_state.turtle.y;
  cmd.x2 = width;
  cmd.y2 = height;
  cmd.color = graphics_state.turtle.pen_color;
  add_command(&cmd);
}

/**
 * Get graphics command buffer
 */
GraphicsCommand* graphics_get_commands(int* count) {
  if (!graphics_state.initialized) graphics_init();

  *count = graphics_state.command_count;
  return graphics_state.commands;
}

/**
 * Reset graphics command buffer
 */
void graphics_reset_buffer() {
  graphics_state.command_count = 0;
}

/**
 * Set canvas dimensions (called from JavaScript)
 */
void graphics_set_canvas_size(double width, double height) {
  graphics_state.canvas_width = width;
  graphics_state.canvas_height = height;
}

/**
 * Get canvas dimensions
 */
void graphics_get_canvas_size(double* width, double* height) {
  *width = graphics_state.canvas_width;
  *height = graphics_state.canvas_height;
}

/**
 * Cleanup graphics
 */
void graphics_cleanup() {
  if (graphics_state.initialized) {
    if (graphics_state.commands) {
      free(graphics_state.commands);
      graphics_state.commands = NULL;
    }
    memset(&graphics_state, 0, sizeof(graphics_state));
    graphics_state.initialized = 0;
  }
}
