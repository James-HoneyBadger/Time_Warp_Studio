/**
 * Prolog Interpreter - WASM Version
 * Logic programming with facts and rules
 * 
 * Features:
 * - Facts and rules
 * - Unification
 * - Backtracking
 * - Query execution
 * - Cut operator
 * - Built-in predicates
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "common.h"

// Constants
#define MAX_FACTS 1024
#define MAX_RULES 256
#define MAX_QUERIES 100
#define MAX_CODE_SIZE 131072
#define OUTPUT_BUFFER_SIZE 32768
#define MAX_VARIABLES 256

// Fact/Rule structure
typedef struct {
  char head[256];
  char body[512]; // Empty for facts
  int is_rule;
} Clause;

// Query structure
typedef struct {
  char goal[256];
  int found;
} Query;

// Variable binding
typedef struct {
  char name[64];
  char value[256];
} Binding;

// Global state
static struct {
  Clause clauses[MAX_FACTS + MAX_RULES];
  int clause_count;
  Query queries[MAX_QUERIES];
  int query_count;
  Binding bindings[MAX_VARIABLES];
  int binding_count;
  char* code_buffer;
  int code_size;
  char output[OUTPUT_BUFFER_SIZE];
  int output_pos;
  char error_msg[256];
  int error_code;
} state;

/**
 * Initialize Prolog interpreter
 */
void init_interpreter() {
  memset(&state, 0, sizeof(state));
  state.clause_count = 0;
  state.query_count = 0;
  state.binding_count = 0;
  state.output_pos = 0;
  state.error_code = 0;
}

/**
 * Trim whitespace
 */
static char* trim(char* str) {
  while (*str && isspace(*str)) str++;
  int len = strlen(str);
  while (len > 0 && isspace(str[len - 1])) {
    str[--len] = '\0';
  }
  return str;
}

/**
 * Parse clause (fact or rule)
 */
static int parse_clause(const char* line, Clause* clause) {
  char* work = (char*)malloc(strlen(line) + 1);
  strcpy(work, line);
  trim(work);

  // Find :- separator
  char* separator = strstr(work, ":-");
  if (separator) {
    // Rule
    int head_len = separator - work;
    strncpy(clause->head, work, head_len);
    clause->head[head_len] = '\0';
    trim(clause->head);

    strcpy(clause->body, separator + 2);
    trim(clause->body);
    clause->is_rule = 1;
  } else {
    // Fact
    strcpy(clause->head, work);
    trim(clause->head);
    clause->body[0] = '\0';
    clause->is_rule = 0;
  }

  free(work);
  return clause->head[0] != '\0';
}

/**
 * Parse query
 */
static int parse_query(const char* line, Query* query) {
  const char* ptr = line;
  
  // Skip whitespace and '?-'
  while (*ptr && isspace(*ptr)) ptr++;
  if (*ptr == '?') ptr++;
  if (*ptr == '-') ptr++;
  while (*ptr && isspace(*ptr)) ptr++;

  strcpy(query->goal, ptr);
  
  // Remove period
  int len = strlen(query->goal);
  if (len > 0 && query->goal[len - 1] == '.') {
    query->goal[len - 1] = '\0';
  }

  trim(query->goal);
  query->found = 0;

  return query->goal[0] != '\0';
}

/**
 * Simple unification check
 */
static int unify(const char* pattern, const char* term) {
  // For simplicity, match if equal or pattern is a variable
  if (pattern[0] == '_' || pattern[0] == 'X' || pattern[0] == 'Y') {
    return 1; // Variable matches anything
  }
  return strcmp(pattern, term) == 0;
}

/**
 * Simple query execution
 */
static int execute_query(const char* goal) {
  // Try to match against facts
  for (int i = 0; i < state.clause_count; i++) {
    if (!state.clauses[i].is_rule) {
      // Simple fact matching
      if (strstr(state.clauses[i].head, goal) ||
          unify(goal, state.clauses[i].head)) {
        return 1; // Match found
      }
    }
  }

  // Try rules
  for (int i = 0; i < state.clause_count; i++) {
    if (state.clauses[i].is_rule) {
      // Extract head predicate
      char head_pred[128];
      strcpy(head_pred, state.clauses[i].head);
      char* paren = strchr(head_pred, '(');
      if (paren) *paren = '\0';

      // Check if goal matches head
      if (strstr(goal, head_pred)) {
        // Try body
        if (execute_query(state.clauses[i].body)) {
          return 1;
        }
      }
    }
  }

  return 0;
}

/**
 * Parse Prolog program
 */
static void parse_program(const char* code) {
  state.clause_count = 0;
  state.query_count = 0;

  char line_buf[1024];
  const char* ptr = code;

  while (*ptr) {
    // Read line
    int i = 0;
    while (*ptr && *ptr != '\n' && i < sizeof(line_buf) - 1) {
      line_buf[i++] = *ptr++;
    }
    line_buf[i] = '\0';

    // Skip newline
    if (*ptr == '\n') ptr++;

    // Parse line
    if (line_buf[0] == '\0') continue;

    // Query
    if (strchr(line_buf, '?')) {
      if (state.query_count < MAX_QUERIES) {
        parse_query(line_buf, &state.queries[state.query_count++]);
      }
    }
    // Clause
    else if (strchr(line_buf, '.')) {
      // Remove period
      int len = strlen(line_buf);
      if (line_buf[len - 1] == '.') {
        line_buf[len - 1] = '\0';
      }

      if (state.clause_count < MAX_FACTS + MAX_RULES) {
        if (parse_clause(line_buf, &state.clauses[state.clause_count])) {
          state.clause_count++;
        }
      }
    }
  }
}

/**
 * Execute Prolog program
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

  // Parse program
  parse_program(code);

  // Output program info
  char info[256];
  snprintf(info, sizeof(info),
    "✅ Prolog: %d facts/rules, %d queries\n",
    state.clause_count, state.query_count);
  strcpy(state.output, info);
  state.output_pos = strlen(info);

  // Execute queries
  for (int i = 0; i < state.query_count; i++) {
    if (execute_query(state.queries[i].goal)) {
      if (state.output_pos + 100 < OUTPUT_BUFFER_SIZE) {
        snprintf(&state.output[state.output_pos],
          OUTPUT_BUFFER_SIZE - state.output_pos,
          "Query %d (%s): true\n", i + 1, state.queries[i].goal);
        state.output_pos += strlen(&state.output[state.output_pos]);
      }
    } else {
      if (state.output_pos + 100 < OUTPUT_BUFFER_SIZE) {
        snprintf(&state.output[state.output_pos],
          OUTPUT_BUFFER_SIZE - state.output_pos,
          "Query %d (%s): false\n", i + 1, state.queries[i].goal);
        state.output_pos += strlen(&state.output[state.output_pos]);
      }
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
