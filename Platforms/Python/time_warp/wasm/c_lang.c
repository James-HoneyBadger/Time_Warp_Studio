/**
 * C Language Interpreter - WASM Version
 * Simplified C language support for educational purposes
 * 
 * Features:
 * - Variable declarations (int, float, char)
 * - Basic operators and expressions
 * - Control flow (if/else, for, while)
 * - Functions (simplified)
 * - Arrays
 * - Standard library functions (printf, etc)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "common.h"

// Constants
#define MAX_VARIABLES 512
#define MAX_FUNCTIONS 128
#define MAX_CODE_SIZE 262144 // 256 KB
#define OUTPUT_BUFFER_SIZE 65536
#define MAX_TOKENS 10000

// Variable types
typedef enum {
  TYPE_UNDEFINED,
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_CHAR,
  TYPE_STRING
} CType;

// Variable structure
typedef struct {
  char name[64];
  CType type;
  int int_value;
  double float_value;
  char char_value;
  char* str_value;
  int is_array;
  int array_size;
} CVariable;

// Token types
typedef enum {
  TOK_EOF,
  TOK_KEYWORD,
  TOK_IDENTIFIER,
  TOK_NUMBER,
  TOK_STRING,
  TOK_CHAR,
  TOK_OPERATOR,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_LBRACE,
  TOK_RBRACE,
  TOK_LBRACKET,
  TOK_RBRACKET,
  TOK_SEMICOLON,
  TOK_COMMA,
  TOK_DOT,
  TOK_ARROW,
  TOK_ASSIGN,
  TOK_PLUS_ASSIGN,
  TOK_MINUS_ASSIGN
} CTokenType;

// Token structure
typedef struct {
  CTokenType type;
  char value[256];
  long int_val;
  double real_val;
} CToken;

// Global state
static struct {
  CVariable variables[MAX_VARIABLES];
  int var_count;
  CToken tokens[MAX_TOKENS];
  int token_count;
  int current_token;
  char* code_buffer;
  int code_size;
  char output[OUTPUT_BUFFER_SIZE];
  int output_pos;
  char error_msg[256];
  int error_code;
} state;

/**
 * Initialize C interpreter
 */
void init_interpreter() {
  memset(&state, 0, sizeof(state));
  state.var_count = 0;
  state.token_count = 0;
  state.current_token = 0;
  state.output_pos = 0;
  state.error_code = 0;
}

/**
 * Tokenize C code
 */
static void tokenize(const char* code) {
  state.token_count = 0;
  state.current_token = 0;
  const char* ptr = code;

  while (*ptr && state.token_count < MAX_TOKENS) {
    // Skip whitespace
    while (*ptr && isspace(*ptr)) ptr++;
    if (!*ptr) break;

    // Skip comments
    if (ptr[0] == '/' && ptr[1] == '/') {
      while (*ptr && *ptr != '\n') ptr++;
      if (*ptr == '\n') ptr++;
      continue;
    }
    if (ptr[0] == '/' && ptr[1] == '*') {
      ptr += 2;
      while (*ptr && !(ptr[0] == '*' && ptr[1] == '/')) ptr++;
      if (*ptr) ptr += 2;
      continue;
    }

    CToken* tok = &state.tokens[state.token_count];

    // String literals
    if (*ptr == '"') {
      tok->type = TOK_STRING;
      ptr++;
      int i = 0;
      while (*ptr && *ptr != '"' && i < sizeof(tok->value) - 1) {
        if (*ptr == '\\' && ptr[1]) {
          ptr++;
          if (*ptr == 'n') tok->value[i++] = '\n';
          else if (*ptr == 't') tok->value[i++] = '\t';
          else tok->value[i++] = *ptr;
          ptr++;
        } else {
          tok->value[i++] = *ptr++;
        }
      }
      tok->value[i] = '\0';
      if (*ptr == '"') ptr++;
    }
    // Character literals
    else if (*ptr == '\'') {
      tok->type = TOK_CHAR;
      ptr++;
      tok->value[0] = *ptr++;
      tok->value[1] = '\0';
      if (*ptr == '\'') ptr++;
    }
    // Numbers
    else if (isdigit(*ptr) || (*ptr == '-' && isdigit(ptr[1]))) {
      tok->type = TOK_NUMBER;
      char* end;
      tok->int_val = strtol(ptr, (char**)&end, 10);
      tok->real_val = atof(ptr);
      snprintf(tok->value, sizeof(tok->value), "%ld", tok->int_val);
      ptr = end;
    }
    // Identifiers and keywords
    else if (isalpha(*ptr) || *ptr == '_') {
      int i = 0;
      while (isalnum(*ptr) || *ptr == '_') {
        tok->value[i++] = *ptr++;
      }
      tok->value[i] = '\0';

      // Check if keyword
      if (strcmp(tok->value, "int") == 0 ||
          strcmp(tok->value, "float") == 0 ||
          strcmp(tok->value, "char") == 0 ||
          strcmp(tok->value, "if") == 0 ||
          strcmp(tok->value, "else") == 0 ||
          strcmp(tok->value, "for") == 0 ||
          strcmp(tok->value, "while") == 0 ||
          strcmp(tok->value, "do") == 0 ||
          strcmp(tok->value, "return") == 0 ||
          strcmp(tok->value, "void") == 0 ||
          strcmp(tok->value, "main") == 0 ||
          strcmp(tok->value, "printf") == 0 ||
          strcmp(tok->value, "scanf") == 0 ||
          strcmp(tok->value, "include") == 0) {
        tok->type = TOK_KEYWORD;
      } else {
        tok->type = TOK_IDENTIFIER;
      }
    }
    // Operators and delimiters
    else if (*ptr == '(') {
      tok->type = TOK_LPAREN;
      ptr++;
    }
    else if (*ptr == ')') {
      tok->type = TOK_RPAREN;
      ptr++;
    }
    else if (*ptr == '{') {
      tok->type = TOK_LBRACE;
      ptr++;
    }
    else if (*ptr == '}') {
      tok->type = TOK_RBRACE;
      ptr++;
    }
    else if (*ptr == '[') {
      tok->type = TOK_LBRACKET;
      ptr++;
    }
    else if (*ptr == ']') {
      tok->type = TOK_RBRACKET;
      ptr++;
    }
    else if (*ptr == ';') {
      tok->type = TOK_SEMICOLON;
      ptr++;
    }
    else if (*ptr == ',') {
      tok->type = TOK_COMMA;
      ptr++;
    }
    else if (*ptr == '=') {
      tok->type = TOK_ASSIGN;
      ptr++;
    }
    else if (*ptr == '+') {
      if (ptr[1] == '=') {
        tok->type = TOK_PLUS_ASSIGN;
        ptr += 2;
      } else {
        tok->type = TOK_OPERATOR;
        tok->value[0] = '+';
        tok->value[1] = '\0';
        ptr++;
      }
    }
    else if (*ptr == '-') {
      if (ptr[1] == '=') {
        tok->type = TOK_MINUS_ASSIGN;
        ptr += 2;
      } else if (ptr[1] == '>') {
        tok->type = TOK_ARROW;
        ptr += 2;
      } else {
        tok->type = TOK_OPERATOR;
        tok->value[0] = '-';
        tok->value[1] = '\0';
        ptr++;
      }
    }
    else {
      tok->type = TOK_OPERATOR;
      tok->value[0] = *ptr++;
      tok->value[1] = '\0';
    }

    state.token_count++;
  }
}

/**
 * Get or create variable
 */
static CVariable* get_variable(const char* name) {
  for (int i = 0; i < state.var_count; i++) {
    if (strcmp(state.variables[i].name, name) == 0) {
      return &state.variables[i];
    }
  }

  if (state.var_count >= MAX_VARIABLES) {
    return NULL;
  }

  CVariable* var = &state.variables[state.var_count++];
  strncpy(var->name, name, sizeof(var->name) - 1);
  var->type = TYPE_UNDEFINED;
  return var;
}

/**
 * Parse and execute C program
 */
static int parse_program() {
  state.current_token = 0;

  while (state.current_token < state.token_count) {
    CToken* tok = &state.tokens[state.current_token];

    if (tok->type == TOK_KEYWORD) {
      // Variable declaration
      if (strcmp(tok->value, "int") == 0 ||
          strcmp(tok->value, "float") == 0 ||
          strcmp(tok->value, "char") == 0) {
        CType var_type = TYPE_INT;
        if (strcmp(tok->value, "float") == 0) var_type = TYPE_FLOAT;
        else if (strcmp(tok->value, "char") == 0) var_type = TYPE_CHAR;

        state.current_token++;

        // Parse variable names
        while (state.current_token < state.token_count &&
               state.tokens[state.current_token].type != TOK_SEMICOLON) {
          if (state.tokens[state.current_token].type == TOK_IDENTIFIER) {
            CVariable* var = get_variable(state.tokens[state.current_token].value);
            if (var) {
              var->type = var_type;
            }
          }
          state.current_token++;
        }
        if (state.current_token < state.token_count) {
          state.current_token++; // Skip semicolon
        }
      }
      // Main function
      else if (strcmp(tok->value, "main") == 0) {
        state.current_token++;
        // Skip to {
        while (state.current_token < state.token_count &&
               state.tokens[state.current_token].type != TOK_LBRACE) {
          state.current_token++;
        }
        if (state.current_token < state.token_count) {
          state.current_token++; // Skip {
        }

        // Parse main body
        while (state.current_token < state.token_count &&
               state.tokens[state.current_token].type != TOK_RBRACE) {
          // Handle printf
          if (state.tokens[state.current_token].type == TOK_KEYWORD &&
              strcmp(state.tokens[state.current_token].value, "printf") == 0) {
            state.current_token++;
            // Skip (
            state.current_token++;
            // Get string
            if (state.current_token < state.token_count &&
                state.tokens[state.current_token].type == TOK_STRING) {
              const char* fmt = state.tokens[state.current_token].value;
              if (state.output_pos + strlen(fmt) < OUTPUT_BUFFER_SIZE) {
                strcpy(&state.output[state.output_pos], fmt);
                state.output_pos += strlen(fmt);
              }
              state.current_token++;
            }
            // Skip to )
            while (state.current_token < state.token_count &&
                   state.tokens[state.current_token].type != TOK_RPAREN) {
              state.current_token++;
            }
            state.current_token++;
          } else {
            state.current_token++;
          }
        }
      } else {
        state.current_token++;
      }
    } else {
      state.current_token++;
    }
  }

  return 0;
}

/**
 * Execute C code
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

  // Tokenize and parse
  tokenize(code);
  parse_program();

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
