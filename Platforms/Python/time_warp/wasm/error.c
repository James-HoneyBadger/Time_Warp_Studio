/**
 * WASM Error Handling
 * Unified error handling across all WASM interpreters
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"

#define MAX_ERROR_MSG 512
#define MAX_ERROR_STACK 64

// Error types
typedef enum {
  ERR_NONE = 0,
  ERR_SYNTAX = 1,
  ERR_RUNTIME = 2,
  ERR_MEMORY = 3,
  ERR_STACK = 4,
  ERR_TYPE = 5,
  ERR_UNDEFINED = 6,
  ERR_INDEX = 7,
  ERR_TIMEOUT = 8,
  ERR_UNKNOWN = 255
} ErrorCode;

// Error context
typedef struct {
  ErrorCode code;
  char message[MAX_ERROR_MSG];
  int line_number;
  int column_number;
  char context[256];
} ErrorContext;

// Global error state
static struct {
  ErrorContext stack[MAX_ERROR_STACK];
  int depth;
  char last_error[MAX_ERROR_MSG];
} error_state;

/**
 * Initialize error handler
 */
void error_init() {
  memset(&error_state, 0, sizeof(error_state));
}

/**
 * Push error onto stack
 */
int error_push(ErrorCode code, const char* message,
               int line, int column) {
  if (error_state.depth >= MAX_ERROR_STACK) {
    return 1; // Stack overflow
  }

  ErrorContext* ctx = &error_state.stack[error_state.depth++];
  ctx->code = code;
  ctx->line_number = line;
  ctx->column_number = column;

  if (message) {
    strncpy(ctx->message, message, MAX_ERROR_MSG - 1);
  } else {
    ctx->message[0] = '\0';
  }
  ctx->context[0] = '\0';

  // Update last error
  snprintf(error_state.last_error, sizeof(error_state.last_error),
    "[Line %d:%d] Error %d: %s",
    line, column, code, message ? message : "");

  return 0;
}

/**
 * Pop error from stack
 */
int error_pop(ErrorContext* ctx) {
  if (error_state.depth <= 0) {
    return 1;
  }

  if (ctx) {
    *ctx = error_state.stack[--error_state.depth];
  } else {
    error_state.depth--;
  }

  return 0;
}

/**
 * Get current error
 */
const ErrorContext* error_current() {
  if (error_state.depth > 0) {
    return &error_state.stack[error_state.depth - 1];
  }
  return NULL;
}

/**
 * Get error depth (number of active errors)
 */
int error_depth() {
  return error_state.depth;
}

/**
 * Format error message
 */
const char* error_format(ErrorCode code, const char* details) {
  static char buffer[MAX_ERROR_MSG];

  const char* code_str = "Unknown";
  switch (code) {
    case ERR_SYNTAX: code_str = "Syntax Error"; break;
    case ERR_RUNTIME: code_str = "Runtime Error"; break;
    case ERR_MEMORY: code_str = "Memory Error"; break;
    case ERR_STACK: code_str = "Stack Error"; break;
    case ERR_TYPE: code_str = "Type Error"; break;
    case ERR_UNDEFINED: code_str = "Undefined"; break;
    case ERR_INDEX: code_str = "Index Error"; break;
    case ERR_TIMEOUT: code_str = "Timeout"; break;
    default: code_str = "Unknown Error"; break;
  }

  snprintf(buffer, sizeof(buffer), "❌ %s: %s",
    code_str, details ? details : "");

  return buffer;
}

/**
 * Get last error message
 */
const char* error_last() {
  return error_state.last_error;
}

/**
 * Clear all errors
 */
void error_clear() {
  error_state.depth = 0;
  error_state.last_error[0] = '\0';
}

/**
 * Check for errors
 */
int error_has() {
  return error_state.depth > 0;
}

/**
 * Print error stack to buffer
 */
int error_sprint(char* buffer, int buffer_size) {
  if (!buffer) return 0;

  buffer[0] = '\0';
  int written = 0;

  for (int i = 0; i < error_state.depth && written < buffer_size - 1; i++) {
    ErrorContext* ctx = &error_state.stack[i];
    const char* code_str = "Unknown";

    switch (ctx->code) {
      case ERR_SYNTAX: code_str = "Syntax"; break;
      case ERR_RUNTIME: code_str = "Runtime"; break;
      case ERR_MEMORY: code_str = "Memory"; break;
      case ERR_STACK: code_str = "Stack"; break;
      case ERR_TYPE: code_str = "Type"; break;
      case ERR_UNDEFINED: code_str = "Undefined"; break;
      case ERR_INDEX: code_str = "Index"; break;
      case ERR_TIMEOUT: code_str = "Timeout"; break;
      default: break;
    }

    int len = snprintf(&buffer[written], buffer_size - written,
      "❌ %s Error at %d:%d: %s\n",
      code_str, ctx->line_number, ctx->column_number, ctx->message);

    if (len > 0) written += len;
  }

  return written;
}

/**
 * Panic - unrecoverable error
 */
void error_panic(const char* message) {
  error_push(ERR_UNKNOWN, message, 0, 0);
  // In WASM, we can't actually exit, so we just record the error
}

/**
 * Assert macro helper
 */
int error_assert(int condition, ErrorCode code, const char* message) {
  if (!condition) {
    error_push(code, message, 0, 0);
    return 1;
  }
  return 0;
}

/**
 * Memory error helper
 */
int error_out_of_memory() {
  return error_assert(0, ERR_MEMORY, "Out of memory");
}

/**
 * Stack overflow helper
 */
int error_stack_overflow() {
  return error_assert(0, ERR_STACK, "Stack overflow");
}

/**
 * Stack underflow helper
 */
int error_stack_underflow() {
  return error_assert(0, ERR_STACK, "Stack underflow");
}

/**
 * Syntax error helper
 */
int error_syntax(const char* details, int line, int col) {
  char msg[256];
  snprintf(msg, sizeof(msg), "Syntax: %s", details);
  return error_push(ERR_SYNTAX, msg, line, col);
}

/**
 * Type error helper
 */
int error_type_mismatch(const char* expected, const char* actual) {
  char msg[256];
  snprintf(msg, sizeof(msg), "Type mismatch: expected %s, got %s",
    expected, actual);
  return error_push(ERR_TYPE, msg, 0, 0);
}

/**
 * Undefined error helper
 */
int error_undefined(const char* name) {
  char msg[256];
  snprintf(msg, sizeof(msg), "Undefined: %s", name);
  return error_push(ERR_UNDEFINED, msg, 0, 0);
}

/**
 * Index error helper
 */
int error_index_out_of_bounds(int index, int size) {
  char msg[256];
  snprintf(msg, sizeof(msg), "Index %d out of bounds (size %d)", index, size);
  return error_push(ERR_INDEX, msg, 0, 0);
}

/**
 * Runtime error helper
 */
int error_runtime(const char* details) {
  return error_push(ERR_RUNTIME, details, 0, 0);
}

/**
 * Division by zero helper
 */
int error_div_by_zero() {
  return error_push(ERR_RUNTIME, "Division by zero", 0, 0);
}
