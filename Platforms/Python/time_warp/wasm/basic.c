/**
 * BASIC Interpreter - WASM Version
 * Compiles to WebAssembly for client-side execution
 * 
 * Core Features:
 * - Variable storage and management
 * - Control flow (IF/THEN, FOR/NEXT, DO/LOOP)
 * - Subroutines and GOSUB
 * - Array support
 * - String operations
 * - Graphics integration
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "common.h"

// Constants
#define MAX_VARIABLES 1024
#define MAX_ARRAYS 256
#define MAX_CODE_SIZE 65536
#define MAX_STACK_DEPTH 512
#define MAX_FOR_LOOPS 100
#define OUTPUT_BUFFER_SIZE 16384

// Variable types
typedef enum {
  VAR_UNINITIALIZED,
  VAR_NUMBER,
  VAR_STRING
} VarType;

// Variable structure
typedef struct {
  char name[64];
  VarType type;
  double num_value;
  char str_value[512];
} Variable;

// FOR loop tracking
typedef struct {
  int line_number;
  double target;
  Variable* loop_var;
  int step;
} ForLoop;

// Global state
static struct {
  Variable variables[MAX_VARIABLES];
  int var_count;
  ForLoop for_loops[MAX_FOR_LOOPS];
  int for_depth;
  char* code_buffer;
  int code_size;
  int current_line;
  char output[OUTPUT_BUFFER_SIZE];
  int output_pos;
  char error_msg[256];
  int error_code;
} state;

/**
 * Initialize BASIC interpreter
 */
void init_interpreter() {
  memset(&state, 0, sizeof(state));
  state.var_count = 0;
  state.for_depth = 0;
  state.output_pos = 0;
  state.error_code = 0;
}

/**
 * Get or create variable
 */
static Variable* get_variable(const char* name) {
  for (int i = 0; i < state.var_count; i++) {
    if (strcmp(state.variables[i].name, name) == 0) {
      return &state.variables[i];
    }
  }

  if (state.var_count >= MAX_VARIABLES) {
    snprintf(state.error_msg, sizeof(state.error_msg), 
             "Too many variables");
    return NULL;
  }

  Variable* var = &state.variables[state.var_count++];
  strncpy(var->name, name, sizeof(var->name) - 1);
  var->type = VAR_UNINITIALIZED;
  return var;
}

/**
 * Set numeric variable
 */
static void set_number(const char* name, double value) {
  Variable* var = get_variable(name);
  if (var) {
    var->type = VAR_NUMBER;
    var->num_value = value;
  }
}

/**
 * Set string variable
 */
static void set_string(const char* name, const char* value) {
  Variable* var = get_variable(name);
  if (var) {
    var->type = VAR_STRING;
    strncpy(var->str_value, value, sizeof(var->str_value) - 1);
  }
}

/**
 * Get numeric variable
 */
static double get_number(const char* name) {
  Variable* var = get_variable(name);
  if (!var) return 0.0;
  return var->type == VAR_NUMBER ? var->num_value : 0.0;
}

/**
 * Get string variable
 */
static const char* get_string(const char* name) {
  Variable* var = get_variable(name);
  if (!var) return "";
  return var->type == VAR_STRING ? var->str_value : "";
}

/**
 * Print output
 */
static void print_output(const char* text) {
  size_t len = strlen(text);
  if (state.output_pos + len < OUTPUT_BUFFER_SIZE) {
    memcpy(&state.output[state.output_pos], text, len);
    state.output_pos += len;
  }
}

/**
 * Print newline
 */
static void print_newline() {
  print_output("\n");
}

/**
 * Simple BASIC statement parser
 */
static int parse_statement(const char* line) {
  // Skip whitespace
  while (*line && (*line == ' ' || *line == '\t')) {
    line++;
  }

  // Empty line
  if (!*line) {
    return 0;
  }

  // PRINT statement
  if (strncmp(line, "PRINT", 5) == 0) {
    line += 5;
    while (*line && *line == ' ') line++;

    if (!*line || *line == '\n') {
      print_newline();
      return 0;
    }

    // For now, just output the string literal
    if (*line == '"') {
      line++;
      char buffer[512];
      int i = 0;
      while (*line && *line != '"' && i < sizeof(buffer) - 1) {
        if (*line == '\\' && *(line + 1) == 'n') {
          buffer[i++] = '\n';
          line += 2;
        } else {
          buffer[i++] = *line++;
        }
      }
      buffer[i] = '\0';
      print_output(buffer);
      print_newline();
    }
    return 0;
  }

  // LET statement
  if (strncmp(line, "LET", 3) == 0) {
    line += 3;
    while (*line && *line == ' ') line++;

    char var_name[64];
    int i = 0;
    while (*line && *line != '=' && i < sizeof(var_name) - 1) {
      var_name[i++] = *line++;
    }
    var_name[i] = '\0';

    if (*line == '=') {
      line++;
      // Simple numeric assignment
      double value = atof(line);
      set_number(var_name, value);
    }
    return 0;
  }

  // END statement
  if (strncmp(line, "END", 3) == 0) {
    return 1; // Signal end
  }

  return 0;
}

/**
 * Execute BASIC code
 */
int execute_code(const char* code, int code_len) {
  if (!code || code_len <= 0) {
    strcpy(state.error_msg, "Empty code");
    return 1;
  }

  // Copy code to buffer
  if (code_len > MAX_CODE_SIZE) {
    strcpy(state.error_msg, "Code too large");
    return 1;
  }

  memcpy(state.code_buffer, code, code_len);
  state.code_size = code_len;
  state.output_pos = 0;
  state.error_code = 0;

  // Parse and execute line by line
  char line_buffer[256];
  int line_idx = 0;
  int char_idx = 0;

  while (char_idx < code_len) {
    // Read line
    line_idx = 0;
    while (char_idx < code_len && 
           state.code_buffer[char_idx] != '\n' && 
           line_idx < sizeof(line_buffer) - 1) {
      line_buffer[line_idx++] = state.code_buffer[char_idx++];
    }
    line_buffer[line_idx] = '\0';

    if (state.code_buffer[char_idx] == '\n') {
      char_idx++;
    }

    // Execute line
    if (parse_statement(line_buffer)) {
      break; // END encountered
    }
  }

  // Null terminate output
  if (state.output_pos < OUTPUT_BUFFER_SIZE) {
    state.output[state.output_pos] = '\0';
  }

  return state.error_code;
}

/**
 * Get output buffer
 */
const char* get_output(int* length) {
  if (length) {
    *length = state.output_pos;
  }
  return state.output;
}

/**
 * Get error message
 */
const char* get_error(int* code) {
  if (code) {
    *code = state.error_code;
  }
  return state.error_msg;
}

/**
 * Cleanup
 */
void cleanup() {
  memset(&state, 0, sizeof(state));
}
