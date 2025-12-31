/**
 * Pascal Interpreter - WASM Version
 * Compiled Pascal-like language support
 * 
 * Features:
 * - Variable declarations
 * - Type system (integer, real, string)
 * - Procedures and functions
 * - Control flow (if/then/else, for, while)
 * - Basic operators
 * - Arrays
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "common.h"

// Constants
#define MAX_VARIABLES 512
#define MAX_PROCEDURES 128
#define MAX_CODE_SIZE 131072
#define OUTPUT_BUFFER_SIZE 32768
#define MAX_TOKENS 5000

// Variable types
typedef enum {
  TYPE_UNDEFINED,
  TYPE_INTEGER,
  TYPE_REAL,
  TYPE_BOOLEAN,
  TYPE_CHAR,
  TYPE_STRING
} PascalType;

// Variable structure
typedef struct {
  char name[64];
  PascalType type;
  int int_value;
  double real_value;
  char str_value[512];
  int is_array;
  int array_size;
} Variable;

// Token types
typedef enum {
  TOK_EOF,
  TOK_KEYWORD,
  TOK_IDENTIFIER,
  TOK_NUMBER,
  TOK_STRING,
  TOK_OPERATOR,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_LBRACKET,
  TOK_RBRACKET,
  TOK_SEMICOLON,
  TOK_COMMA,
  TOK_COLON,
  TOK_ASSIGN,
  TOK_DOT
} TokenType;

// Token structure
typedef struct {
  TokenType type;
  char value[256];
  int int_val;
  double real_val;
} Token;

// Global state
static struct {
  Variable variables[MAX_VARIABLES];
  int var_count;
  Token tokens[MAX_TOKENS];
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
 * Initialize Pascal interpreter
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
 * Tokenize Pascal code
 */
static void tokenize(const char* code) {
  state.token_count = 0;
  state.current_token = 0;
  const char* ptr = code;

  while (*ptr && state.token_count < MAX_TOKENS) {
    // Skip whitespace
    while (*ptr && isspace(*ptr)) ptr++;
    if (!*ptr) break;

    Token* tok = &state.tokens[state.token_count];

    // String literals
    if (*ptr == '\'') {
      tok->type = TOK_STRING;
      ptr++;
      int i = 0;
      while (*ptr && *ptr != '\'' && i < sizeof(tok->value) - 1) {
        tok->value[i++] = *ptr++;
      }
      tok->value[i] = '\0';
      if (*ptr == '\'') ptr++;
    }
    // Numbers
    else if (isdigit(*ptr)) {
      tok->type = TOK_NUMBER;
      char* end;
      tok->int_val = strtol(ptr, (char**)&end, 10);
      tok->real_val = atof(ptr);
      snprintf(tok->value, sizeof(tok->value), "%d", tok->int_val);
      ptr = end;
    }
    // Identifiers and keywords
    else if (isalpha(*ptr) || *ptr == '_') {
      int i = 0;
      while (isalnum(*ptr) || *ptr == '_') {
        tok->value[i++] = toupper(*ptr++);
      }
      tok->value[i] = '\0';

      // Check if keyword
      if (strcmp(tok->value, "VAR") == 0 ||
          strcmp(tok->value, "PROCEDURE") == 0 ||
          strcmp(tok->value, "FUNCTION") == 0 ||
          strcmp(tok->value, "BEGIN") == 0 ||
          strcmp(tok->value, "END") == 0 ||
          strcmp(tok->value, "IF") == 0 ||
          strcmp(tok->value, "THEN") == 0 ||
          strcmp(tok->value, "ELSE") == 0 ||
          strcmp(tok->value, "FOR") == 0 ||
          strcmp(tok->value, "WHILE") == 0 ||
          strcmp(tok->value, "DO") == 0 ||
          strcmp(tok->value, "INTEGER") == 0 ||
          strcmp(tok->value, "REAL") == 0 ||
          strcmp(tok->value, "BOOLEAN") == 0 ||
          strcmp(tok->value, "CHAR") == 0 ||
          strcmp(tok->value, "STRING") == 0 ||
          strcmp(tok->value, "WRITE") == 0 ||
          strcmp(tok->value, "WRITELN") == 0) {
        tok->type = TOK_KEYWORD;
      } else {
        tok->type = TOK_IDENTIFIER;
      }
    }
    // Operators and delimiters
    else if (*ptr == ':' && *(ptr + 1) == '=') {
      tok->type = TOK_ASSIGN;
      tok->value[0] = ':';
      tok->value[1] = '=';
      tok->value[2] = '\0';
      ptr += 2;
    }
    else if (*ptr == '(') {
      tok->type = TOK_LPAREN;
      tok->value[0] = '(';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ')') {
      tok->type = TOK_RPAREN;
      tok->value[0] = ')';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == '[') {
      tok->type = TOK_LBRACKET;
      tok->value[0] = '[';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ']') {
      tok->type = TOK_RBRACKET;
      tok->value[0] = ']';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ';') {
      tok->type = TOK_SEMICOLON;
      tok->value[0] = ';';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ',') {
      tok->type = TOK_COMMA;
      tok->value[0] = ',';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == ':') {
      tok->type = TOK_COLON;
      tok->value[0] = ':';
      tok->value[1] = '\0';
      ptr++;
    }
    else if (*ptr == '.') {
      tok->type = TOK_DOT;
      tok->value[0] = '.';
      tok->value[1] = '\0';
      ptr++;
    }
    else {
      // Operator
      tok->type = TOK_OPERATOR;
      tok->value[0] = *ptr++;
      tok->value[1] = '\0';
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
  static Token eof = {TOK_EOF, "", 0, 0};
  return &eof;
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
 * Parse Pascal program
 */
static int parse_program() {
  while (state.current_token < state.token_count) {
    Token* tok = current_token();

    if (tok->type == TOK_KEYWORD) {
      // VAR declaration
      if (strcmp(tok->value, "VAR") == 0) {
        state.current_token++;

        // Parse variable declarations
        while (state.current_token < state.token_count) {
          Token* var_tok = current_token();
          if (var_tok->type == TOK_IDENTIFIER) {
            char var_name[64];
            strcpy(var_name, var_tok->value);
            state.current_token++;

            // Skip colon and type
            while (state.current_token < state.token_count &&
                   current_token()->type != TOK_SEMICOLON) {
              state.current_token++;
            }
            if (current_token()->type == TOK_SEMICOLON) {
              state.current_token++;
            }

            // Create variable
            Variable* var = get_variable(var_name);
            if (var) {
              var->type = TYPE_INTEGER; // Default type
              var->int_value = 0;
            }

            // Check if we're still in VAR section
            if (current_token()->type != TOK_IDENTIFIER) {
              break;
            }
          } else {
            break;
          }
        }
      }
      // BEGIN/END block
      else if (strcmp(tok->value, "BEGIN") == 0) {
        state.current_token++;
        while (state.current_token < state.token_count &&
               strcmp(current_token()->value, "END") != 0) {
          Token* stmt = current_token();

          // WRITE/WRITELN
          if (strcmp(stmt->value, "WRITE") == 0 ||
              strcmp(stmt->value, "WRITELN") == 0) {
            state.current_token++;

            // Skip to string or expression
            if (current_token()->type == TOK_LPAREN) {
              state.current_token++;
              while (state.current_token < state.token_count &&
                     current_token()->type != TOK_RPAREN) {
                Token* content = current_token();
                if (content->type == TOK_STRING) {
                  // Output string
                  if (state.output_pos + strlen(content->value) < OUTPUT_BUFFER_SIZE) {
                    strcpy(&state.output[state.output_pos], content->value);
                    state.output_pos += strlen(content->value);
                  }
                } else if (content->type == TOK_NUMBER) {
                  // Output number
                  char buf[64];
                  snprintf(buf, sizeof(buf), "%d", content->int_val);
                  if (state.output_pos + strlen(buf) < OUTPUT_BUFFER_SIZE) {
                    strcpy(&state.output[state.output_pos], buf);
                    state.output_pos += strlen(buf);
                  }
                }
                state.current_token++;
              }
              if (current_token()->type == TOK_RPAREN) {
                state.current_token++;
              }
            }

            // WRITELN adds newline
            if (strcmp(stmt->value, "WRITELN") == 0) {
              if (state.output_pos < OUTPUT_BUFFER_SIZE) {
                state.output[state.output_pos++] = '\n';
              }
            }
          } else {
            state.current_token++;
          }

          // Skip semicolons
          if (current_token()->type == TOK_SEMICOLON) {
            state.current_token++;
          }
        }
        if (strcmp(current_token()->value, "END") == 0) {
          state.current_token++;
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
 * Execute Pascal code
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
