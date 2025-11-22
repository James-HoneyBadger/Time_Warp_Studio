#include "interpreter.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <stdlib.h>

static void trim_leading(char **s)
{
    while (**s && isspace((unsigned char)**s))
        (*s)++;
}

static void rstrip(char *s)
{
    size_t n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1]))
    {
        s[--n] = '\0';
    }
}

static int starts_with_ci(const char *s, const char *prefix)
{
    while (*prefix && *s)
    {
        if (tolower((unsigned char)*s) != tolower((unsigned char)*prefix))
            return 0;
        s++;
        prefix++;
    }
    return *prefix == '\0';
}

static void safe_snprintf(char *out, size_t out_len, const char *fmt, ...)
{
    if (out_len == 0)
        return;
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(out, out_len, fmt, ap);
    va_end(ap);
    out[out_len - 1] = '\0';
}

int tw_execute_command(const char *line_in, char *out_buf, size_t out_buf_len)
{
    if (!line_in || !out_buf || out_buf_len == 0)
        return 1;

    // Work on a local copy to allow trimming and tokenizing
    char buf[1024];
    strncpy(buf, line_in, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';

    rstrip(buf);
    char *s = buf;
    trim_leading(&s);

    if (*s == '\0')
    {
        safe_snprintf(out_buf, out_buf_len, "");
        return 0;
    }

    // Pilot-like: T: and A:
    if (toupper((unsigned char)s[0]) == 'T' && s[1] == ':')
    {
        s += 2;
        trim_leading(&s);
        safe_snprintf(out_buf, out_buf_len, "ℹ️ %s", s);
        return 0;
    }
    if (toupper((unsigned char)s[0]) == 'A' && s[1] == ':')
    {
        s += 2;
        trim_leading(&s);
        safe_snprintf(out_buf, out_buf_len, "📝 %s", s);
        return 0;
    }

    // BASIC-like: PRINT or ECHO
    if (starts_with_ci(s, "PRINT ") || starts_with_ci(s, "ECHO "))
    {
        const char *msg = s + (toupper((unsigned char)s[0]) == 'P' ? 6 : 5);
        while (*msg && isspace((unsigned char)*msg))
            msg++;
        safe_snprintf(out_buf, out_buf_len, "✅ %s", msg);
        return 0;
    }
    if (strcasecmp(s, "PRINT") == 0 || strcasecmp(s, "ECHO") == 0)
    {
        safe_snprintf(out_buf, out_buf_len, "✅");
        return 0;
    }

    // Logo-like: FD/FORWARD, LEFT, RIGHT
    if (starts_with_ci(s, "FD ") || starts_with_ci(s, "FORWARD "))
    {
        const char *num = s + (toupper((unsigned char)s[0]) == 'F' && toupper((unsigned char)s[1]) == 'D' ? 3 : 8);
        while (*num && isspace((unsigned char)*num))
            num++;
        double v = atof(num);
        safe_snprintf(out_buf, out_buf_len, "🐢 FD %.2f", v);
        return 0;
    }
    if (starts_with_ci(s, "LEFT "))
    {
        const char *num = s + 5;
        while (*num && isspace((unsigned char)*num))
            num++;
        double v = atof(num);
        safe_snprintf(out_buf, out_buf_len, "🐢 LEFT %.2f", v);
        return 0;
    }
    if (starts_with_ci(s, "RIGHT "))
    {
        const char *num = s + 6;
        while (*num && isspace((unsigned char)*num))
            num++;
        double v = atof(num);
        safe_snprintf(out_buf, out_buf_len, "🐢 RIGHT %.2f", v);
        return 0;
    }

    // Quit/Exit (handled by caller in REPL; here we just acknowledge)
    if (strcasecmp(s, "QUIT") == 0 || strcasecmp(s, "EXIT") == 0)
    {
        safe_snprintf(out_buf, out_buf_len, "ℹ️ Bye");
        return 0;
    }

    // Unknown
    safe_snprintf(out_buf, out_buf_len, "❌ Unknown command");
    return 0;
}
