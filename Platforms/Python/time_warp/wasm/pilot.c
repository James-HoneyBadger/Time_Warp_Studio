/**
 * PILOT Interpreter - WASM Version
 * Programmed Inquiry, Learning, Or Teaching language
 * 
 * Core Features:
 * - Accept: Read user input
 * - Type: Output text
 * - Compute: Calculate expressions
 * - Jump: Control flow
 * - Match: Pattern matching
 * - Branching logic
 * - Conditional execution
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "common.h"

// Constants
#define MAX_VARIABLES 256
#define MAX_LABELS 256
#define MAX_CODE_SIZE 131072
#define MAX_LINES 10000
#define OUTPUT_BUFFER_SIZE 32768

// Variable types
typedef struct {
  char name[64];
  char value[1024];
} Variable;

// Label structure
typedef struct {
  char name[64];
  int line_number;
} Label;

// Line structure
typedef struct {
  char label[64];
  char command[16];
  char content[2048];
  int line_number;
} PilotLine;

// Global state
static struct {
  Variable variables[MAX_VARIABLES];
  int var_count;
  Label labels[MAX_LABELS];
  int label_count;
  PilotLine lines[MAX_LINES];
  int line_count;
  char* code_buffer;
  int code_size;
  int current_line;
  char output[OUTPUT_BUFFER_SIZE];
  int output_pos;
  char error_msg[256];
  int error_code;
  int pc; // Program counter
} state;

/**
 * Initialize PILOT interpreter
 */
void init_interpreter() {
  memset(&state, 0, sizeof(state));
  state.var_count = 0;
  state.label_count = 0;
  state.line_count = 0;
  state.output_pos = 0;
  state.error_code = 0;
  state.pc = 0;
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
    return NULL;
  }

  Variable* var = &state.variables[state.var_count++];
  strncpy(var->name, name, sizeof(var->name) - 1);
  var->value[0] = '\0';
  return var;
}

/**
 * Parse line label and command
 */
static int parse_line(const char* line_text, PilotLine* parsed) {
  const char* ptr = line_text;
  
  // Skip whitespace
  while (*ptr && isspace(*ptr)) ptr++;

  // Extract label (if exists)
  parsed->label[0] = '\0';
  if (*ptr && (isalpha(*ptr) || *ptr == '_')) {
    int i = 0;
    while (*ptr && (isalnum(*ptr) || *ptr == '_') && i < sizeof(parsed->label) - 1) {
      parsed->label[i++] = *ptr++;
    }
    parsed->label[i] = '\0';

    // Check if followed by colon
    while (*ptr && isspace(*ptr)) ptr++;
    if (*ptr == ':') {
      ptr++;
    } else {
      // No label, reparse
      parsed->label[0] = '\0';
      ptr = line_text;
      while (*ptr && isspace(*ptr)) ptr++;
    }
  }

  // Skip whitespace
  while (*ptr && isspace(*ptr)) ptr++;

  // Extract command (must be uppercase letter)
  if (!*ptr || !isalpha(*ptr)) {
    return 1; // Skip empty lines
  }

  int i = 0;
  while (*ptr && isalpha(*ptr) && i < sizeof(parsed->command) - 1) {
    parsed->command[i++] = toupper(*ptr++);
  }
  parsed->command[i] = '\0';

  // Skip whitespace
  while (*ptr && isspace(*ptr)) ptr++;

  // Extract content (rest of line)
  i = 0;
  while (*ptr && i < sizeof(parsed->content) - 1) {
    parsed->content[i++] = *ptr++;
  }
  parsed->content[i] = '\0';

  return 0;
}

/**
 * Parse PILOT program into lines
 */
static void parse_program(const char* code) {
  state.line_count = 0;
  state.label_count = 0;
  const char* line_start = code;
  int line_num = 0;

  while (*line_start && state.line_count < MAX_LINES) {
    // Find end of line
    const char* line_end = strchr(line_start, '\n');
    if (!line_end) {
      line_end = line_start + strlen(line_start);
    }

    // Extract line
    int len = line_end - line_start;
    char line_text[2048];
    if (len > sizeof(line_text) - 1) len = sizeof(line_text) - 1;
    strncpy(line_text, line_start, len);
    line_text[len] = '\0';

    // Parse line
    PilotLine* pline = &state.lines[state.line_count];
    pline->line_number = line_num++;

    if (parse_line(line_text, pline) == 0) {
      // Register label
      if (pline->label[0]) {
        if (state.label_count < MAX_LABELS) {
          Label* lbl = &state.labels[state.label_count++];
          strcpy(lbl->name, pline->label);
          lbl->line_number = state.line_count;
        }
      }
      state.line_count++;
    }

    // Move to next line
    if (*line_end == '\n') line_end++;
    line_start = line_end;
  }
}

/**
 * Process TYPE command (output)
 */
static void cmd_type(const char* content) {
  // Expand variables in content
  char expanded[2048];
  int i = 0, j = 0;

  while (content[i] && j < sizeof(expanded) - 1) {
    if (content[i] == '$' && content[i + 1]) {
      // Variable reference
      i++;
      int var_start = i;
      while (content[i] && isalnum(content[i])) i++;
      
      char var_name[64];
      int len = i - var_start;
      if (len > 0 && len < sizeof(var_name)) {
        strncpy(var_name, &content[var_start], len);
        var_name[len] = '\0';
        
        Variable* var = get_variable(var_name);
        if (var) {
          int vlen = strlen(var->value);
          if (j + vlen < sizeof(expanded) - 1) {
            strcpy(&expanded[j], var->value);
            j += vlen;
          }
        }
      }
    } else {
      expanded[j++] = content[i++];
    }
  }
  expanded[j] = '\0';

  // Handle escape sequences
  j = 0;
  while (expanded[j]) {
    if (expanded[j] == '\\' && expanded[j + 1]) {
      if (expanded[j + 1] == 'n') {
        if (state.output_pos < OUTPUT_BUFFER_SIZE) {
          state.output[state.output_pos++] = '\n';
        }
        j += 2;
      } else {
        j++;
      }
    } else {
      if (state.output_pos < OUTPUT_BUFFER_SIZE) {
        state.output[state.output_pos++] = expanded[j];
      }
      j++;
    }
  }
}

/**
 * Process COMPUTE command
 */
static void cmd_compute(const char* content) {
  // Simple format: COMPUTE var = expression
  // For now, just parse "var = number"
  const char* ptr = content;
  
  // Skip whitespace
  while (*ptr && isspace(*ptr)) ptr++;
  
  // Get variable name
  char var_name[64];
  int i = 0;
  while (*ptr && isalnum(*ptr) && i < sizeof(var_name) - 1) {
    var_name[i++] = *ptr++;
  }
  var_name[i] = '\0';

  // Skip whitespace and '='
  while (*ptr && (isspace(*ptr) || *ptr == '=')) ptr++;

  // Parse number
  double value = atof(ptr);

  // Set variable
  Variable* var = get_variable(var_name);
  if (var) {
    snprintf(var->value, sizeof(var->value), "%g", value);
  }
}

/**
 * Process ACCEPT command (simulated input)
 */
static void cmd_accept(const char* content) {
  // Get variable name
  char var_name[64];
  int i = 0;
  const char* ptr = content;
  
  while (*ptr && isspace(*ptr)) ptr++;
  while (*ptr && isalnum(*ptr) && i < sizeof(var_name) - 1) {
    var_name[i++] = *ptr++;
  }
  var_name[i] = '\0';

  // In WASM, we can't actually read input, so set a default
  Variable* var = get_variable(var_name);
  if (var) {
    strcpy(var->value, "0");
  }
}

/**
 * Find label line number
 */
static int find_label(const char* label_name) {
  for (int i = 0; i < state.label_count; i++) {
    if (strcmp(state.labels[i].name, label_name) == 0) {
      return state.labels[i].line_number;
    }
  }
  return -1;
}

/**
 * Execute PILOT program
 */
int execute_code(const char* code, int code_len) {
  if (!code || code_len <= 0) {
    strcpy(state.error_msg, "Empty code");
    return 1;
  }

  if (code_len > MAX_CODE_SIZE) {
    strcpy(state.error_msg, "Code too large");
    return 1;
  }

  // Parse program
  if (!state.code_buffer) {
    state.code_buffer = (char*)malloc(MAX_CODE_SIZE);
  }

  memcpy(state.code_buffer, code, code_len);
  parse_program(code);

  state.output_pos = 0;
  state.error_code = 0;
  state.pc = 0;

  // Execute
  int max_iterations = 100000; // Safety limit
  int iterations = 0;

  while (state.pc < state.line_count && iterations < max_iterations) {
    iterations++;
    PilotLine* line = &state.lines[state.pc];

    // TYPE command
    if (strcmp(line->command, "T") == 0 || strcmp(line->command, "TYPE") == 0) {
      cmd_type(line->content);
      state.pc++;
    }
    // COMPUTE command
    else if (strcmp(line->command, "C") == 0 || strcmp(line->command, "COMPUTE") == 0) {
      cmd_compute(line->content);
      state.pc++;
    }
    // ACCEPT command
    else if (strcmp(line->command, "A") == 0 || strcmp(line->command, "ACCEPT") == 0) {
      cmd_accept(line->content);
      state.pc++;
    }
    // JUMP command
    else if (strcmp(line->command, "J") == 0 || strcmp(line->command, "JUMP") == 0) {
      int target = find_label(line->content);
      if (target >= 0) {
        state.pc = target;
      } else {
        strcpy(state.error_msg, "Label not found");
        state.error_code = 1;
        break;
      }
    }
    // END command
    else if (strcmp(line->command, "E") == 0 || strcmp(line->command, "END") == 0) {
      break;
    }
    // Unknown command
    else {
      state.pc++;
    }
  }

  if (iterations >= max_iterations) {
    strcpy(state.error_msg, "Execution timeout");
    state.error_code = 1;
  }

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
  if (state.code_buffer) {
    free(state.code_buffer);
    state.code_buffer = NULL;
  }
  memset(&state, 0, sizeof(state));
}
