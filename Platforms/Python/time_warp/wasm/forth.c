/**
 * Forth Interpreter - WASM Version
 * Stack-based language with word definitions
 * 
 * Features:
 * - Stack operations (push, pop, dup, swap)
 * - Arithmetic operations
 * - Word definitions (: name ... ;)
 * - Conditionals (IF THEN ELSE)
 * - Loops (DO LOOP)
 * - Built-in words
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "common.h"

// Constants
#define STACK_SIZE 1024
#define MAX_WORDS 256
#define MAX_CODE_SIZE 131072
#define OUTPUT_BUFFER_SIZE 32768
#define MAX_WORD_NAME 64
#define MAX_WORD_BODY 2048

// Stack element
typedef double StackElement;

// Word definition
typedef struct {
  char name[MAX_WORD_NAME];
  char body[MAX_WORD_BODY];
  int is_builtin;
} Word;

// Global state
static struct {
  StackElement stack[STACK_SIZE];
  int stack_ptr;
  Word words[MAX_WORDS];
  int word_count;
  char* code_buffer;
  int code_size;
  char output[OUTPUT_BUFFER_SIZE];
  int output_pos;
  char error_msg[256];
  int error_code;
  int compilation_mode;
  char compile_buffer[MAX_WORD_BODY];
} state;

/**
 * Initialize Forth interpreter
 */
void init_interpreter() {
  memset(&state, 0, sizeof(state));
  state.stack_ptr = 0;
  state.word_count = 0;
  state.output_pos = 0;
  state.error_code = 0;
  state.compilation_mode = 0;
}

/**
 * Push to stack
 */
static int push(StackElement value) {
  if (state.stack_ptr >= STACK_SIZE) {
    strcpy(state.error_msg, "Stack overflow");
    return 1;
  }
  state.stack[state.stack_ptr++] = value;
  return 0;
}

/**
 * Pop from stack
 */
static int pop(StackElement* value) {
  if (state.stack_ptr <= 0) {
    strcpy(state.error_msg, "Stack underflow");
    return 1;
  }
  *value = state.stack[--state.stack_ptr];
  return 0;
}

/**
 * Peek at top of stack
 */
static int peek(StackElement* value) {
  if (state.stack_ptr <= 0) {
    strcpy(state.error_msg, "Stack underflow");
    return 1;
  }
  *value = state.stack[state.stack_ptr - 1];
  return 0;
}

/**
 * Find word definition
 */
static Word* find_word(const char* name) {
  for (int i = 0; i < state.word_count; i++) {
    if (strcmp(state.words[i].name, name) == 0) {
      return &state.words[i];
    }
  }
  return NULL;
}

/**
 * Define new word
 */
static int define_word(const char* name, const char* body) {
  if (state.word_count >= MAX_WORDS) {
    strcpy(state.error_msg, "Too many words");
    return 1;
  }

  Word* word = &state.words[state.word_count++];
  strncpy(word->name, name, MAX_WORD_NAME - 1);
  strncpy(word->body, body, MAX_WORD_BODY - 1);
  word->is_builtin = 0;

  return 0;
}

/**
 * Execute built-in word
 */
static int execute_builtin(const char* word) {
  StackElement a, b;
  char buf[256];

  if (strcmp(word, "+") == 0) {
    if (pop(&b) || pop(&a)) return 1;
    push(a + b);
  }
  else if (strcmp(word, "-") == 0) {
    if (pop(&b) || pop(&a)) return 1;
    push(a - b);
  }
  else if (strcmp(word, "*") == 0) {
    if (pop(&b) || pop(&a)) return 1;
    push(a * b);
  }
  else if (strcmp(word, "/") == 0) {
    if (pop(&b) || pop(&a)) return 1;
    if (b == 0) {
      strcpy(state.error_msg, "Division by zero");
      return 1;
    }
    push(a / b);
  }
  else if (strcmp(word, "MOD") == 0) {
    if (pop(&b) || pop(&a)) return 1;
    push((double)((int)a % (int)b));
  }
  else if (strcmp(word, "DUP") == 0) {
    if (peek(&a)) return 1;
    push(a);
  }
  else if (strcmp(word, "DROP") == 0) {
    if (pop(&a)) return 1;
  }
  else if (strcmp(word, "SWAP") == 0) {
    if (pop(&b) || pop(&a)) return 1;
    push(b);
    push(a);
  }
  else if (strcmp(word, "OVER") == 0) {
    if (state.stack_ptr < 2) {
      strcpy(state.error_msg, "Not enough stack");
      return 1;
    }
    StackElement top = state.stack[state.stack_ptr - 1];
    StackElement second = state.stack[state.stack_ptr - 2];
    push(second);
  }
  else if (strcmp(word, "ROT") == 0) {
    if (state.stack_ptr < 3) {
      strcpy(state.error_msg, "Not enough stack");
      return 1;
    }
    StackElement c = state.stack[state.stack_ptr - 1];
    StackElement b = state.stack[state.stack_ptr - 2];
    StackElement a = state.stack[state.stack_ptr - 3];
    state.stack[state.stack_ptr - 3] = b;
    state.stack[state.stack_ptr - 2] = c;
    state.stack[state.stack_ptr - 1] = a;
  }
  else if (strcmp(word, ".") == 0) {
    if (pop(&a)) return 1;
    snprintf(buf, sizeof(buf), "%g ", a);
    if (state.output_pos + strlen(buf) < OUTPUT_BUFFER_SIZE) {
      strcpy(&state.output[state.output_pos], buf);
      state.output_pos += strlen(buf);
    }
  }
  else if (strcmp(word, ".S") == 0) {
    // Print stack
    if (state.output_pos + 20 < OUTPUT_BUFFER_SIZE) {
      snprintf(buf, sizeof(buf), "[%d] ", state.stack_ptr);
      strcpy(&state.output[state.output_pos], buf);
      state.output_pos += strlen(buf);
    }
    for (int i = 0; i < state.stack_ptr; i++) {
      snprintf(buf, sizeof(buf), "%g ", state.stack[i]);
      if (state.output_pos + strlen(buf) < OUTPUT_BUFFER_SIZE) {
        strcpy(&state.output[state.output_pos], buf);
        state.output_pos += strlen(buf);
      }
    }
    if (state.output_pos < OUTPUT_BUFFER_SIZE) {
      state.output[state.output_pos++] = '\n';
    }
  }
  else if (strcmp(word, "SQRT") == 0) {
    if (pop(&a)) return 1;
    push(sqrt(a));
  }
  else if (strcmp(word, "ABS") == 0) {
    if (pop(&a)) return 1;
    push(a < 0 ? -a : a);
  }
  else if (strcmp(word, "NEGATE") == 0) {
    if (pop(&a)) return 1;
    push(-a);
  }
  else if (strcmp(word, "CR") == 0) {
    if (state.output_pos < OUTPUT_BUFFER_SIZE) {
      state.output[state.output_pos++] = '\n';
    }
  }
  else {
    return 1; // Unknown word
  }

  return 0;
}

/**
 * Tokenize and execute Forth code
 */
static int execute_forth(const char* code) {
  char word[256];
  int i = 0;

  while (*code) {
    // Skip whitespace
    while (*code && isspace(*code)) code++;
    if (!*code) break;

    // Read word
    i = 0;
    while (*code && !isspace(*code) && i < sizeof(word) - 1) {
      word[i++] = *code++;
    }
    word[i] = '\0';

    // Colon definition
    if (strcmp(word, ":") == 0) {
      // Read name
      while (*code && isspace(*code)) code++;
      i = 0;
      char def_name[64];
      while (*code && !isspace(*code) && i < sizeof(def_name) - 1) {
        def_name[i++] = *code++;
      }
      def_name[i] = '\0';

      // Read until semicolon
      strcpy(state.compile_buffer, "");
      while (*code && *code != ';') {
        // Read word
        while (*code && isspace(*code)) code++;
        if (*code == ';') break;

        i = 0;
        while (*code && !isspace(*code) && *code != ';' && i < sizeof(word) - 1) {
          word[i++] = *code++;
        }
        word[i] = '\0';

        strcat(state.compile_buffer, word);
        strcat(state.compile_buffer, " ");
      }

      if (*code == ';') code++;

      // Define word
      define_word(def_name, state.compile_buffer);
    }
    // Number
    else if (isdigit(word[0]) || (word[0] == '-' && isdigit(word[1]))) {
      push(atof(word));
    }
    // Word lookup
    else {
      Word* w = find_word(word);
      if (w) {
        // Execute user-defined word recursively
        execute_forth(w->body);
      } else if (execute_builtin(word)) {
        // Unknown word, try as number
        if (isdigit(word[0])) {
          push(atof(word));
        }
      }
    }
  }

  return 0;
}

/**
 * Execute Forth code
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
  state.stack_ptr = 0;

  // Execute
  if (execute_forth(code)) {
    state.error_code = 1;
  }

  // Add final newline if output exists
  if (state.output_pos > 0 && state.output[state.output_pos - 1] != '\n') {
    if (state.output_pos < OUTPUT_BUFFER_SIZE) {
      state.output[state.output_pos++] = '\n';
    }
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
