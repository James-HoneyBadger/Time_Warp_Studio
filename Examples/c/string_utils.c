/*
 * string_utils.c - String utility functions
 * Implements: length, copy, reverse, toupper/tolower,
 *             trim, split, startswith/endswith, count occurrences
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* ── string_length ─────────────────────────────────────────────────── */
size_t string_length(const char *s) {
    size_t n = 0;
    while (*s++) n++;
    return n;
}

/* ── string_reverse (in-place) ────────────────────────────────────── */
void string_reverse(char *s) {
    size_t len = string_length(s);
    for (size_t i = 0, j = len - 1; i < j; i++, j--) {
        char tmp = s[i]; s[i] = s[j]; s[j] = tmp;
    }
}

/* ── to_upper / to_lower ──────────────────────────────────────────── */
void str_toupper(char *s) { for (; *s; s++) *s = (char)toupper(*s); }
void str_tolower(char *s) { for (; *s; s++) *s = (char)tolower(*s); }

/* ── trim whitespace (in-place) ───────────────────────────────────── */
char *str_trim(char *s) {
    /* ltrim */
    while (isspace((unsigned char)*s)) s++;
    if (*s == '\0') return s;
    /* rtrim */
    char *end = s + string_length(s) - 1;
    while (end > s && isspace((unsigned char)*end)) end--;
    end[1] = '\0';
    return s;
}

/* ── starts_with / ends_with ──────────────────────────────────────── */
int starts_with(const char *s, const char *prefix) {
    while (*prefix)
        if (*s++ != *prefix++) return 0;
    return 1;
}

int ends_with(const char *s, const char *suffix) {
    size_t sl = string_length(s), pl = string_length(suffix);
    if (pl > sl) return 0;
    return strcmp(s + sl - pl, suffix) == 0;
}

/* ── count occurrences of char ────────────────────────────────────── */
int count_char(const char *s, char c) {
    int n = 0;
    while (*s) if (*s++ == c) n++;
    return n;
}

/* ── simple split by delimiter (max `max` tokens) ─────────────────── */
/* Returns number of tokens; caller must free each token and tokens[] */
int str_split(const char *s, char delim, char **tokens, int max) {
    int count = 0;
    const char *start = s;
    for (const char *p = s; ; p++) {
        if (*p == delim || *p == '\0') {
            size_t len = (size_t)(p - start);
            tokens[count] = (char *)malloc(len + 1);
            if (!tokens[count]) return -1;
            memcpy(tokens[count], start, len);
            tokens[count][len] = '\0';
            count++;
            if (count >= max || *p == '\0') break;
            start = p + 1;
        }
    }
    return count;
}

/* ── Main demo ────────────────────────────────────────────────────── */
int main(void) {
    printf("=== String Utilities Demo ===\n\n");

    /* Length */
    const char *s = "Hello, World!";
    printf("String: \"%s\"\n", s);
    printf("Length: %zu\n\n", string_length(s));

    /* Reverse */
    char buf[64];
    snprintf(buf, sizeof(buf), "%s", s);
    string_reverse(buf);
    printf("Reversed: \"%s\"\n\n", buf);

    /* Upper / Lower */
    snprintf(buf, sizeof(buf), "%s", s);
    str_toupper(buf);
    printf("Uppercase: \"%s\"\n", buf);
    snprintf(buf, sizeof(buf), "%s", s);
    str_tolower(buf);
    printf("Lowercase: \"%s\"\n\n", buf);

    /* Trim */
    char padded[] = "   lots of space   ";
    printf("Before trim: \"%s\"\n", padded);
    printf("After  trim: \"%s\"\n\n", str_trim(padded));

    /* starts_with / ends_with */
    printf("starts_with(\"%s\", \"Hello\"): %s\n", s,
           starts_with(s, "Hello") ? "true" : "false");
    printf("ends_with(\"%s\", \"World!\"): %s\n\n", s,
           ends_with(s, "World!") ? "true" : "false");

    /* Count */
    printf("Count of 'l' in \"%s\": %d\n\n", s, count_char(s, 'l'));

    /* Split */
    printf("=== Split by ',' ===\n");
    const char *csv = "red,green,blue,yellow,purple";
    char *tokens[16];
    int n = str_split(csv, ',', tokens, 16);
    printf("Input: \"%s\"\n", csv);
    for (int i = 0; i < n; i++) {
        printf("  token[%d] = \"%s\"\n", i, tokens[i]);
        free(tokens[i]);
    }

    return 0;
}
