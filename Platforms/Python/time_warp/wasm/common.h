/**
 * WASM Common Header
 * Shared definitions and utilities for WASM interpreters
 */

#ifndef WASM_COMMON_H
#define WASM_COMMON_H

#include <stddef.h>
#include <stdint.h>

// Memory allocation functions (provided by WASM runtime)
extern void* malloc(size_t size);
extern void free(void* ptr);

// Standard library functions
extern int strlen(const char* s);
extern int strcmp(const char* s1, const char* s2);
extern int strncmp(const char* s1, const char* s2, size_t n);
extern char* strcpy(char* dest, const char* src);
extern char* strncpy(char* dest, const char* src, size_t n);
extern void* memset(void* s, int c, size_t n);
extern void* memcpy(void* dest, const void* src, size_t n);
extern double atof(const char* nptr);
extern int atoi(const char* nptr);
extern int snprintf(char* str, size_t size, const char* format, ...);

// Math functions
extern double sin(double x);
extern double cos(double x);
extern double tan(double x);
extern double sqrt(double x);
extern double pow(double x, double y);
extern double floor(double x);
extern double ceil(double x);
extern double fabs(double x);

// WASM specific - these will be exported
void init_interpreter();
int execute_code(const char* code, int code_len);
const char* get_output(int* length);
const char* get_error(int* code);
void cleanup();

// Graphics interface (for turtle graphics)
typedef struct {
  double x;
  double y;
  double angle;
  int pen_down;
  uint32_t pen_color;
} Turtle;

// Exported graphics functions (language interpreters will call these)
extern void turtle_forward(double distance);
extern void turtle_back(double distance);
extern void turtle_right(double angle);
extern void turtle_left(double angle);
extern void turtle_pendown();
extern void turtle_penup();
extern void turtle_setcolor(uint32_t color);
extern void turtle_clear();
extern Turtle* turtle_get_state();

// Output interface
typedef void (*output_callback_t)(const char* text);
extern void set_output_callback(output_callback_t callback);

#endif // WASM_COMMON_H
