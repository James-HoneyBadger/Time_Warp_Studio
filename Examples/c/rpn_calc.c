/*=================================================================
 * rpn_calc.c  — RPN Calculator with Stack, Variables & History
 * Demonstrates: stack struct, tokeniser, strtod, switch/case,
 * function pointers, string handling, and math operations.
 *
 * Usage:  ./rpn_calc [expression]
 *   Interactive: enter RPN expressions line by line.
 *   e.g.  "3 4 + 2 *"    → 14
 *         "5 1 2 + 4 * + 3 -"  → 14
 *         "pi 2 * sin"    → ~0 (sin(2π))
 *=================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>

/* ──────────────────────────────────────────────────────────────
 *  STACK
 * ──────────────────────────────────────────────────────────────*/

#define STACK_MAX 256

typedef struct {
    double data[STACK_MAX];
    int    top;   /* index of next free slot; 0 = empty */
} Stack;

static void  stk_init(Stack *s)             { s->top = 0; }
static int   stk_empty(const Stack *s)      { return s->top == 0; }
static int   stk_full(const Stack *s)       { return s->top == STACK_MAX; }
static size_t stk_size(const Stack *s)      { return (size_t)s->top; }

static int stk_push(Stack *s, double v) {
    if (stk_full(s)) { fprintf(stderr, "Stack overflow\n"); return -1; }
    s->data[s->top++] = v;
    return 0;
}

static int stk_pop(Stack *s, double *out) {
    if (stk_empty(s)) { fprintf(stderr, "Stack underflow\n"); return -1; }
    *out = s->data[--s->top];
    return 0;
}

static int stk_peek(Stack *s, double *out) {
    if (stk_empty(s)) { return -1; }
    *out = s->data[s->top - 1];
    return 0;
}

static void stk_print(const Stack *s) {
    printf("  Stack [%d]: ", s->top);
    for (int i = 0; i < s->top; i++) {
        if (i) printf(", ");
        printf("%.10g", s->data[i]);
    }
    printf("\n");
}

/* ──────────────────────────────────────────────────────────────
 *  VARIABLE STORE  (a=val, b=val, ...)
 * ──────────────────────────────────────────────────────────────*/

#define VAR_MAX 26

typedef struct {
    double vals[VAR_MAX];
    int    defined[VAR_MAX];
} VarStore;

static void var_init(VarStore *v) { memset(v, 0, sizeof *v); }

static int var_set(VarStore *v, char name, double val) {
    if (!isalpha(name)) return -1;
    int idx = tolower((unsigned char)name) - 'a';
    v->vals[idx]    = val;
    v->defined[idx] = 1;
    return 0;
}

static int var_get(VarStore *v, char name, double *out) {
    if (!isalpha(name)) return -1;
    int idx = tolower((unsigned char)name) - 'a';
    if (!v->defined[idx]) return -1;
    *out = v->vals[idx];
    return 0;
}

/* ──────────────────────────────────────────────────────────────
 *  HISTORY
 * ──────────────────────────────────────────────────────────────*/

#define HIST_MAX 50

typedef struct {
    char   exprs[HIST_MAX][256];
    double results[HIST_MAX];
    int    count;
} History;

static void hist_add(History *h, const char *expr, double result) {
    if (h->count < HIST_MAX) {
        strncpy(h->exprs[h->count], expr, 255);
        h->exprs[h->count][255] = '\0';
        h->results[h->count]    = result;
        h->count++;
    }
}

static void hist_print(const History *h, int n) {
    int start = h->count - n;
    if (start < 0) start = 0;
    for (int i = start; i < h->count; i++) {
        printf("  [%3d] %-30s = %.10g\n",
               i + 1, h->exprs[i], h->results[i]);
    }
}

/* ──────────────────────────────────────────────────────────────
 *  CONSTANTS (built-in named values)
 * ──────────────────────────────────────────────────────────────*/

typedef struct { const char *name; double value; } Constant;
static const Constant CONSTANTS[] = {
    {"pi",  M_PI},      {"e",   M_E},
    {"phi", 1.618033988749895},  /* golden ratio */
    {"tau", 2.0 * M_PI},
    {"sqrt2", M_SQRT2},
    {NULL,  0},
};

static int lookup_const(const char *token, double *out) {
    for (int i = 0; CONSTANTS[i].name; i++) {
        if (strcasecmp(token, CONSTANTS[i].name) == 0) {
            *out = CONSTANTS[i].value;
            return 1;
        }
    }
    return 0;
}

/* ──────────────────────────────────────────────────────────────
 *  UNARY OPERATIONS
 * ──────────────────────────────────────────────────────────────*/

typedef struct { const char *name; double (*fn)(double); } UnaryOp;
static const UnaryOp UNARY_OPS[] = {
    {"sin",  sin},   {"cos",  cos},   {"tan",  tan},
    {"asin", asin},  {"acos", acos},  {"atan", atan},
    {"sinh", sinh},  {"cosh", cosh},  {"tanh", tanh},
    {"exp",  exp},   {"log",  log},   {"log2", log2},   {"log10", log10},
    {"sqrt", sqrt},  {"cbrt", cbrt},
    {"ceil", ceil},  {"floor",floor}, {"round",round},  {"abs",   fabs},
    {"neg",  NULL},  /* handled specially */
    {NULL,   NULL},
};

static int try_unary(const char *token, Stack *s) {
    double a;
    if (!strcmp(token, "neg") || !strcmp(token, "chs")) {
        if (stk_pop(s, &a) < 0) return -1;
        return stk_push(s, -a);
    }
    if (!strcmp(token, "inv") || !strcmp(token, "1/x")) {
        if (stk_pop(s, &a) < 0) return -1;
        if (a == 0.0) { fprintf(stderr, "Division by zero\n"); return -1; }
        return stk_push(s, 1.0 / a);
    }
    if (!strcmp(token, "sq") || !strcmp(token, "sqr")) {
        if (stk_pop(s, &a) < 0) return -1;
        return stk_push(s, a * a);
    }
    if (!strcmp(token, "fact")) {  /* factorial — integers only */
        if (stk_pop(s, &a) < 0) return -1;
        long n = (long)a;
        if (n < 0 || n > 20) { fprintf(stderr, "Factorial out of range\n"); return -1; }
        double result = 1;
        for (long i = 2; i <= n; i++) result *= i;
        return stk_push(s, result);
    }
    for (int i = 0; UNARY_OPS[i].name; i++) {
        if (!strcmp(token, UNARY_OPS[i].name) && UNARY_OPS[i].fn) {
            if (stk_pop(s, &a) < 0) return -1;
            errno = 0;
            double r = UNARY_OPS[i].fn(a);
            if (errno) { perror(token); return -1; }
            return stk_push(s, r);
        }
    }
    return 1;  /* not a unary op */
}

/* ──────────────────────────────────────────────────────────────
 *  BINARY OPERATIONS
 * ──────────────────────────────────────────────────────────────*/

static int try_binary(const char *token, Stack *s) {
    double a, b;
    if (strlen(token) != 1 && strcmp(token, "**") &&
        strcmp(token, "mod") && strcmp(token, "pow") &&
        strcmp(token, "hypot") && strcmp(token, "atan2") &&
        strcmp(token, "max") && strcmp(token, "min") &&
        strcmp(token, "gcd")) {
        return 1;  /* not a known binary op name */
    }
    if (stk_pop(s, &b) < 0 || stk_pop(s, &a) < 0) return -1;

    double r = 0.0;
    if      (!strcmp(token, "+"))     r = a + b;
    else if (!strcmp(token, "-"))     r = a - b;
    else if (!strcmp(token, "*"))     r = a * b;
    else if (!strcmp(token, "/")) {
        if (b == 0.0) { fprintf(stderr, "Division by zero\n"); return -1; }
        r = a / b;
    }
    else if (!strcmp(token, "**") || !strcmp(token, "pow"))  r = pow(a, b);
    else if (!strcmp(token, "%") || !strcmp(token, "mod")) {
        if (b == 0.0) { fprintf(stderr, "Modulo by zero\n"); return -1; }
        r = fmod(a, b);
    }
    else if (!strcmp(token, "^"))     r = pow(a, b);
    else if (!strcmp(token, "hypot")) r = hypot(a, b);
    else if (!strcmp(token, "atan2")) r = atan2(a, b);
    else if (!strcmp(token, "max"))   r = (a > b) ? a : b;
    else if (!strcmp(token, "min"))   r = (a < b) ? a : b;
    else if (!strcmp(token, "gcd")) {
        long ia = (long)fabs(a), ib = (long)fabs(b);
        while (ib) { long t = ib; ib = ia % ib; ia = t; }
        r = (double)ia;
    }
    else { stk_push(s, a); stk_push(s, b); return 1; }  /* undo pop */

    return stk_push(s, r);
}

/* ──────────────────────────────────────────────────────────────
 *  STACK MANIPULATION TOKENS
 * ──────────────────────────────────────────────────────────────*/

static int try_stack_op(const char *token, Stack *s) {
    double a, b;
    if (!strcmp(token, "dup")) {
        if (stk_peek(s, &a) < 0) return -1;
        return stk_push(s, a);
    }
    if (!strcmp(token, "drop") || !strcmp(token, "pop")) {
        return stk_pop(s, &a);
    }
    if (!strcmp(token, "swap")) {
        if (stk_pop(s, &b) < 0 || stk_pop(s, &a) < 0) return -1;
        stk_push(s, b);
        return stk_push(s, a);
    }
    if (!strcmp(token, "over")) {
        if (s->top < 2) return -1;
        return stk_push(s, s->data[s->top - 2]);
    }
    if (!strcmp(token, "rot")) {   /* 3rd item to top */
        if (s->top < 3) return -1;
        double c = s->data[s->top - 3];
        s->data[s->top - 3] = s->data[s->top - 2];
        s->data[s->top - 2] = s->data[s->top - 1];
        s->data[s->top - 1] = c;
        return 0;
    }
    if (!strcmp(token, "clear") || !strcmp(token, "cls")) {
        stk_init(s);
        return 0;
    }
    return 1;
}

/* ──────────────────────────────────────────────────────────────
 *  MAIN EVALUATOR
 * ──────────────────────────────────────────────────────────────*/

typedef struct {
    Stack   stack;
    VarStore vars;
    History hist;
} Calc;

static void calc_init(Calc *c) {
    stk_init(&c->stack);
    var_init(&c->vars);
    memset(&c->hist, 0, sizeof c->hist);
}

/**
 * Evaluate one line of RPN expression.
 * Returns 0 on success, -1 on error.
 * If result != NULL and stack is non-empty, places the top value there.
 */
int calc_eval(Calc *c, const char *line, double *result) {
    char buf[1024];
    strncpy(buf, line, 1023); buf[1023] = '\0';

    char *token = strtok(buf, " \t\r\n,");
    while (token) {

        /* Skip comments */
        if (token[0] == '#') break;

        /* Variable assignment: "x=" stores top of stack in x */
        size_t tlen = strlen(token);
        if (tlen == 2 && isalpha(token[0]) && token[1] == '=') {
            double v;
            if (stk_peek(&c->stack, &v) == 0) {
                var_set(&c->vars, token[0], v);
                printf("  %c = %.10g\n", tolower((unsigned char)token[0]), v);
            }
            token = strtok(NULL, " \t\r\n,");
            continue;
        }

        /* Single-letter variable recall: "x" pushes stored value */
        if (tlen == 1 && isalpha(token[0])) {
            double v;
            if (var_get(&c->vars, token[0], &v) == 0) {
                stk_push(&c->stack, v);
                token = strtok(NULL, " \t\r\n,");
                continue;
            }
            /* fall through — could be a constant prefix or error */
        }

        /* Named constants */
        {
            double cv;
            if (lookup_const(token, &cv)) {
                stk_push(&c->stack, cv);
                token = strtok(NULL, " \t\r\n,");
                continue;
            }
        }

        /* Try parsing as a number */
        {
            char *endp;
            errno = 0;
            double v = strtod(token, &endp);
            if (endp != token && *endp == '\0' && errno == 0) {
                stk_push(&c->stack, v);
                token = strtok(NULL, " \t\r\n,");
                continue;
            }
        }

        /* Stack manipulation */
        {
            int r = try_stack_op(token, &c->stack);
            if (r == 0) { token = strtok(NULL, " \t\r\n,"); continue; }
            if (r <  0) return -1;
        }

        /* Unary ops */
        {
            int r = try_unary(token, &c->stack);
            if (r == 0) { token = strtok(NULL, " \t\r\n,"); continue; }
            if (r <  0) return -1;
        }

        /* Binary ops */
        {
            int r = try_binary(token, &c->stack);
            if (r == 0) { token = strtok(NULL, " \t\r\n,"); continue; }
            if (r <  0) return -1;
        }

        /* Special commands */
        if (!strcmp(token, "stack")) { stk_print(&c->stack); token = strtok(NULL," \t\r\n,"); continue; }
        if (!strcmp(token, "hist"))  { hist_print(&c->hist, 10); token = strtok(NULL," \t\r\n,"); continue; }

        fprintf(stderr, "Unknown token: %s\n", token);
        return -1;
    }

    double top;
    if (stk_peek(&c->stack, &top) == 0) {
        if (result) *result = top;
        return 0;
    }
    return 0;
}

/* ──────────────────────────────────────────────────────────────
 *  DEMO / MAIN
 * ──────────────────────────────────────────────────────────────*/

typedef struct { const char *expr; const char *note; } TestCase;
static const TestCase DEMOS[] = {
    {"3 4 +",             "3 + 4 = 7"},
    {"5 1 2 + 4 * + 3 -", "= 14 (classic RPN test)"},
    {"2 8 **",            "2^8 = 256"},
    {"9 sqrt",            "√9 = 3"},
    {"pi 4 /  sin",       "sin(π/4) ≈ 0.7071"},
    {"e e *",             "e² ≈ 7.389"},
    {"100 e log10",       "log10(100) push e (ignored), = 2"},
    {"3 4 hypot",         "hypot(3,4) = 5"},
    {"10 3 mod",          "10 mod 3 = 1"},
    {"5 fact",            "5! = 120"},
    {"12 8 gcd",          "gcd(12,8) = 4"},
    {"2 dup * 3 dup * +", "(2²) + (3²) = 13"},
    {"tau sqrt2 *",       "τ × √2 ≈ 8.886"},
    {NULL, NULL},
};

int main(int argc, char *argv[]) {
    Calc calc;
    calc_init(&calc);

    printf("╔══════════════════════════════════════════════════════════╗\n");
    printf("║         RPN CALCULATOR — C Demo                          ║\n");
    printf("╚══════════════════════════════════════════════════════════╝\n\n");

    if (argc > 1) {
        /* Evaluate all arguments concatenated */
        char expr[1024] = {0};
        for (int i = 1; i < argc; i++) {
            if (i > 1) strncat(expr, " ", sizeof expr - strlen(expr) - 1);
            strncat(expr, argv[i], sizeof expr - strlen(expr) - 1);
        }
        double result;
        if (calc_eval(&calc, expr, &result) == 0) {
            printf("Result: %.10g\n", result);
            hist_add(&calc.hist, expr, result);
            stk_print(&calc.stack);
        }
        return 0;
    }

    /* Run demo test cases */
    printf("── DEMO EXPRESSIONS ──────────────────────────────────────\n\n");
    for (int i = 0; DEMOS[i].expr; i++) {
        stk_init(&calc.stack);
        double result = 0.0;
        int rc = calc_eval(&calc, DEMOS[i].expr, &result);
        if (rc == 0) {
            printf("  %-35s → %-.10g\n", DEMOS[i].expr, result);
            hist_add(&calc.hist, DEMOS[i].expr, result);
        }
    }

    /* Variables demo */
    printf("\n── VARIABLE DEMO ─────────────────────────────────────────\n");
    stk_init(&calc.stack);
    calc_eval(&calc, "3 x=", NULL);   /* store 3 in x */
    calc_eval(&calc, "4 y=", NULL);   /* store 4 in y */
    calc_eval(&calc, "x y +", NULL);  /* use variables */
    double top;
    stk_peek(&calc.stack, &top);
    printf("  x=3, y=4 → x y + = %.g\n\n", top);

    /* Interactive mode */
    printf("── INTERACTIVE MODE ──────────────────────────────────────\n");
    printf("  Enter RPN expressions (q=quit, hist=history, stack=show stack):\n\n");

    char line[1024];
    while (1) {
        printf("rpn> ");
        fflush(stdout);
        if (!fgets(line, sizeof line, stdin)) break;
        line[strcspn(line, "\n")] = '\0';
        if (!strcmp(line, "q") || !strcmp(line, "quit")) break;
        if (!*line) continue;

        if (!strcmp(line, "hist")) { hist_print(&calc.hist, 10); continue; }
        if (!strcmp(line, "stack")) { stk_print(&calc.stack); continue; }

        double result;
        if (calc_eval(&calc, line, &result) == 0 && !stk_empty(&calc.stack)) {
            printf("  = %.10g\n", result);
            hist_add(&calc.hist, line, result);
        }
    }

    printf("\nFinal history:\n");
    hist_print(&calc.hist, calc.hist.count);
    printf("\nDone.\n");
    return 0;
}
