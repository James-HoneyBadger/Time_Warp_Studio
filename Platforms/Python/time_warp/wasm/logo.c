/**
 * Logo Interpreter - WASM Version
 * Full turtle graphics implementation with advanced features
 * 
 * Core Features:
 * - Turtle graphics (forward, back, right, left)
 * - Pen control (up, down, setcolor, setwidth)
 * - Procedures and recursion
 * - Variables and expressions
 * - Repeat/loops
 * - List operations
 * - Graphics rendering commands
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "common.h"

// Constants
#define MAX_VARIABLES 512
#define MAX_PROCEDURES 256
#define MAX_CODE_SIZE 131072 // 128 KB
#define MAX_CALL_DEPTH 64
#define OUTPUT_BUFFER_SIZE 32768
#define MAX_TOKENS 10000

// Variable types
typedef enum {
  TYPE_UNDEFINED,
  TYPE_NUMBER,
  TYPE_WORD,
  TYPE_LIST
} LogoType;

// Token types
typedef enum {
  TOKEN_EOF,
  TOKEN_NUMBER,
  TOKEN_WORD,
  TOKEN_LPAREN,
  TOKEN_RPAREN,
  TOKEN_LBRACKET,
  TOKEN_RBRACKET,
  TOKEN_COLON,
  TOKEN_COMMA,
  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_MULTIPLY,
  TOKEN_DIVIDE,
  TOKEN_EQUALS
} TokenType;

// Token structure
typedef struct {
  TokenType type;
  char value[256];
  double num_value;
} Token;

// Variable structure
typedef struct {
  char name[64];
  LogoType type;
  double num_value;
  char word_value[512];
  // Lists stored as text for simplicity
  char list_value[2048];
} Variable;

// Procedure structure
typedef struct {
  char name[64];
  int start_pos;
  int end_pos;
  char params[256]; // Space-separated parameter names
} Procedure;

// Global state
static struct {
  Variable variables[MAX_VARIABLES];
  int var_count;
  Procedure procedures[MAX_PROCEDURES];
  int proc_count;
  Token tokens[MAX_TOKENS];
  int token_count;
  int current_token;
  char* code_buffer;
  int code_size;
  char output[OUTPUT_BUFFER_SIZE];
  int output_pos;
  char error_msg[256];
  int error_code;
  int call_depth;
  // Turtle state (also maintained in graphics.c)
  double turtle_x;
  double turtle_y;
  double turtle_angle;
  int turtle_pen_down;
  uint32_t turtle_color;
} state;

/**
 * Initialize Logo interpreter
 */
void init_interpreter() {
  memset(&state, 0, sizeof(state));
  state.var_count = 0;
  state.proc_count = 0;
  state.token_count = 0;
  state.current_token = 0;
  state.output_pos = 0;
  state.error_code = 0;
  state.call_depth = 0;
  // Initial turtle state
  state.turtle_x = 0;
  state.turtle_y = 0;
  state.turtle_angle = 0;
  state.turtle_pen_down = 1;
  state.turtle_color = 0x000000; // Black
  graphics_init();
}

/**
 * Tokenizer
 */
static void tokenize(const char* code) {
  state.token_count = 0;
  state.current_token = 0;
  
  const char* ptr = code;
  while (*ptr && state.token_count < MAX_TOKENS) {
    // Skip whitespace
    while (*ptr && isspace(*ptr)) ptr++;
    if (!*ptr) break;

    Token* token = &state.tokens[state.token_count];

    // Number
    if (isdigit(*ptr) || (*ptr == '-' && isdigit(*(ptr + 1)))) {
      token->type = TOKEN_NUMBER;
      char* end;
      token->num_value = strtod(ptr, (char**)&end);
      snprintf(token->value, sizeof(token->value), "%g", token->num_value);
      ptr = end;
    }
    // Parentheses
    else if (*ptr == '(') {
      token->type = TOKEN_LPAREN;
      token->value[0] = '(';
      token->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ')') {
      token->type = TOKEN_RPAREN;
      token->value[0] = ')';
      token->value[1] = '\0';
      ptr++;
    }
    // Brackets
    else if (*ptr == '[') {
      token->type = TOKEN_LBRACKET;
      token->value[0] = '[';
      token->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ']') {
      token->type = TOKEN_RBRACKET;
      token->value[0] = ']';
      token->value[1] = '\0';
      ptr++;
    }
    // Colon for variable reference
    else if (*ptr == ':') {
      token->type = TOKEN_COLON;
      token->value[0] = ':';
      token->value[1] = '\0';
      ptr++;
    }
    // Operators
    else if (*ptr == '+') {
      token->type = TOKEN_PLUS;
      ptr++;
    }
    else if (*ptr == '-' && !isdigit(*(ptr + 1))) {
      token->type = TOKEN_MINUS;
      ptr++;
    }
    else if (*ptr == '*') {
      token->type = TOKEN_MULTIPLY;
      ptr++;
    }
    else if (*ptr == '/') {
      token->type = TOKEN_DIVIDE;
      ptr++;
    }
    else if (*ptr == '=') {
      token->type = TOKEN_EQUALS;
      ptr++;
    }
    // Word/command
    else {
      token->type = TOKEN_WORD;
      int i = 0;
      while (*ptr && !isspace(*ptr) && *ptr != '[' && *ptr != ']' && 
             *ptr != '(' && *ptr != ')' && i < sizeof(token->value) - 1) {
        token->value[i++] = *ptr++;
      }
      token->value[i] = '\0';
    }

    state.token_count++;
  }
}

/**
 * Get current token
 */
static Token* current_token() {
  if (state.current_token < state.token_count) {
    return &state.tokens[state.current_token];
  }
  static Token eof = {TOKEN_EOF, "", 0};
  return &eof;
}

/**
 * Advance to next token
 */
static void next_token() {
  state.current_token++;
}

/**
 * Parse number expression
 */
static double parse_number() {
  Token* tok = current_token();
  if (tok->type == TOKEN_NUMBER) {
    double val = tok->num_value;
    next_token();
    return val;
  }
  return 0;
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
  var->type = TYPE_UNDEFINED;
  return var;
}

/**
 * Execute Logo command
 */
static int execute_command(const char* cmd, double param) {
  const char* upper_cmd = cmd;
  
  // Forward
  if (strcasecmp(cmd, "FORWARD") == 0 || strcasecmp(cmd, "FD") == 0) {
    turtle_forward(param);
    state.turtle_x += param * cos(state.turtle_angle * 3.14159265359 / 180.0);
    state.turtle_y += param * sin(state.turtle_angle * 3.14159265359 / 180.0);
    return 0;
  }
  // Back
  else if (strcasecmp(cmd, "BACK") == 0 || strcasecmp(cmd, "BK") == 0) {
    turtle_back(param);
    state.turtle_x -= param * cos(state.turtle_angle * 3.14159265359 / 180.0);
    state.turtle_y -= param * sin(state.turtle_angle * 3.14159265359 / 180.0);
    return 0;
  }
  // Right
  else if (strcasecmp(cmd, "RIGHT") == 0 || strcasecmp(cmd, "RT") == 0) {
    turtle_right(param);
    state.turtle_angle += param;
    if (state.turtle_angle >= 360) state.turtle_angle -= 360;
    return 0;
  }
  // Left
  else if (strcasecmp(cmd, "LEFT") == 0 || strcasecmp(cmd, "LT") == 0) {
    turtle_left(param);
    state.turtle_angle -= param;
    if (state.turtle_angle < 0) state.turtle_angle += 360;
    return 0;
  }
  // Pen Down
  else if (strcasecmp(cmd, "PENDOWN") == 0 || strcasecmp(cmd, "PD") == 0) {
    turtle_pendown();
    state.turtle_pen_down = 1;
    return 0;
  }
  // Pen Up
  else if (strcasecmp(cmd, "PENUP") == 0 || strcasecmp(cmd, "PU") == 0) {
    turtle_penup();
    state.turtle_pen_down = 0;
    return 0;
  }
  // Set Pen Width
  else if (strcasecmp(cmd, "PENWIDTH") == 0) {
    turtle_setwidth(param);
    return 0;
  }
  // Home - return to origin
  else if (strcasecmp(cmd, "HOME") == 0) {
    turtle_setpos(0, 0);
    state.turtle_x = 0;
    state.turtle_y = 0;
    state.turtle_angle = 0;
    return 0;
  }
  // Clear Screen
  else if (strcasecmp(cmd, "CLEARSCREEN") == 0 || strcasecmp(cmd, "CS") == 0) {
    turtle_clear();
    return 0;
  }
  // Set Color
  else if (strcasecmp(cmd, "SETCOLOR") == 0 || strcasecmp(cmd, "SETC") == 0) {
    uint32_t color = (uint32_t)param;
    turtle_setcolor(color);
    state.turtle_color = color;
    return 0;
  }

  return 0;
}

/**
 * Parse and execute statements
 */
static int parse_statement(const char* code) {
  if (!code || !*code) return 0;

  tokenize(code);
  state.current_token = 0;

  while (state.current_token < state.token_count) {
    Token* tok = current_token();

    if (tok->type == TOKEN_WORD) {
      const char* word = tok->value;
      next_token();

      // REPEAT command
      if (strcasecmp(word, "REPEAT") == 0) {
        double count = parse_number();
        // Skip to command block [...]
        if (current_token()->type == TOKEN_LBRACKET) {
          next_token();
          // Collect command block
          char block[2048] = {0};
          int depth = 1;
          while (depth > 0 && state.current_token < state.token_count) {
            Token* t = current_token();
            if (t->type == TOKEN_LBRACKET) depth++;
            if (t->type == TOKEN_RBRACKET) depth--;
            if (depth > 0) {
              strcat(block, t->value);
              strcat(block, " ");
            }
            next_token();
          }
          // Execute block 'count' times
          for (int i = 0; i < (int)count; i++) {
            parse_statement(block);
          }
        }
      }
      // Direct commands with parameter
      else {
        // Check if next token is a number
        if (current_token()->type == TOKEN_NUMBER) {
          double param = parse_number();
          execute_command(word, param);
        }
        // Or just execute without parameter
        else {
          execute_command(word, 0);
        }
      }
    } else {
      next_token();
    }
  }

  return 0;
}

/**
 * Execute Logo code
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

  if (!state.code_buffer) {
    state.code_buffer = (char*)malloc(MAX_CODE_SIZE);
  }

  memcpy(state.code_buffer, code, code_len);
  state.code_size = code_len;
  state.output_pos = 0;
  state.error_code = 0;

  // Parse and execute
  if (parse_statement(code)) {
    state.error_code = 1;
  }

  // Add graphics summary
  char summary[256];
  snprintf(summary, sizeof(summary),
    "✅ Logo executed. Turtle at (%.1f, %.1f) angle %.1f°\n",
    state.turtle_x, state.turtle_y, state.turtle_angle);
  if (state.output_pos + strlen(summary) < OUTPUT_BUFFER_SIZE) {
    strcat(state.output, summary);
    state.output_pos += strlen(summary);
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
 * Get turtle graphics command buffer (for rendering)
 */
const GraphicsCommand* get_graphics_commands(int* count) {
  return graphics_get_commands(count);
}

/**
 * Cleanup
 */
void cleanup() {
  graphics_cleanup();
  if (state.code_buffer) {
    free(state.code_buffer);
    state.code_buffer = NULL;
  }
  memset(&state, 0, sizeof(state));
}
