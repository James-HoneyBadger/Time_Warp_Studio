/*
 * Time Warp DOS - Full-Featured BASIC/PILOT/Logo Interpreter (C89)
 *
 * Features:
 * - Commands: PRINT, LET, INPUT, GOTO, GOSUB, RETURN, IF...THEN, FOR...NEXT,
 *             END, STOP, CLS, REM
 * - Integer variables A..Z, String variables A$..Z$
 * - Comparison operators: =, <, >, <=, >=, <>
 * - Interactive mode: LIST, NEW, RUN, SAVE, LOAD
 * - PILOT subset: L:, T:, A:, U:, J:, Y:, N: with label jumps, simple interpolation, and conditional branches
 * - Logo subset (text-mode): FORWARD/FD, RIGHT/RT, LEFT/LT, PENUP/PU, PENDOWN/PD, HOME, CLEAR, SHOW
 * - Line-numbered programs with automatic sorting
 *
 * Build with OpenWatcom:
 *   wcl -bt=dos -q -fe=TIMEWARP.EXE timewarp_dos.c
 *
 * Build with DJGPP:
 *   gcc -std=c89 -O2 -s -o TIMEWARP.EXE timewarp_dos.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINES 2048
#define MAX_LINE_LEN 256
#define MAX_STR_LEN 128
#define MAX_FOR_DEPTH 16
#define MAX_GOSUB_DEPTH 32

typedef struct
{
    int number;
    char text[MAX_LINE_LEN];
} Line;

typedef struct
{
    char var;
    int target;
    int step;
    int loop_line;
} ForStack;

static Line program[MAX_LINES];
static int line_count = 0;
static int vars[26];
static char str_vars[26][MAX_STR_LEN];
static ForStack for_stack[MAX_FOR_DEPTH];
static int for_depth = 0;
static int gosub_stack[MAX_GOSUB_DEPTH];
static int gosub_depth = 0;

/* PILOT labels */
#define MAX_LABELS 512
typedef struct
{
    char name[32];
    int index;
} PilotLabel;
static PilotLabel pilot_labels[MAX_LABELS];
static int pilot_label_count = 0;
static char pilot_last_answer[MAX_STR_LEN];
static int pilot_last_yes = 0; /* 1=yes, 0=no (or other) */

/* Logo (text-mode turtle) */
#define LOGO_W 60
#define LOGO_H 20
static char logo_canvas[LOGO_H][LOGO_W + 1];
static int canvas_w = LOGO_W;
static int canvas_h = LOGO_H;
static int logo_x = LOGO_W / 2;
static int logo_y = LOGO_H / 2;
static int logo_dir = 0; /* 0=E,1=SE,2=S,3=SW,4=W,5=NW,6=N,7=NE (45-degree steps) */
static int logo_pen = 1;
static int logo_dirty = 0;
static int logo_show_border = 1;
static int logo_show_turtle = 1;
static int logo_mark_start = 0; /* plot current position when pen down or after HOME */
static char logo_pen_char = '*';

static void trim(char *s)
{
    int i, j;
    for (i = 0; s[i] && isspace((unsigned char)s[i]); i++)
        ;
    if (i > 0)
        memmove(s, s + i, strlen(s + i) + 1);
    j = (int)strlen(s) - 1;
    while (j >= 0 && isspace((unsigned char)s[j]))
    {
        s[j] = '\0';
        j--;
    }
}

static void strtoupper(char *s)
{
    while (*s)
    {
        *s = (char)toupper((unsigned char)*s);
        s++;
    }
}

static int find_line_index(int number)
{
    int i;
    for (i = 0; i < line_count; i++)
    {
        if (program[i].number == number)
            return i;
    }
    return -1;
}

static const char *eptr;

static void skip_spaces(void)
{
    while (*eptr && isspace((unsigned char)*eptr))
        eptr++;
}

static int parse_number(void)
{
    int sign = 1;
    long val = 0;
    skip_spaces();
    if (*eptr == '+')
        eptr++;
    else if (*eptr == '-')
    {
        sign = -1;
        eptr++;
    }
    skip_spaces();
    if (!isdigit((unsigned char)*eptr))
        return 0;
    while (isdigit((unsigned char)*eptr))
    {
        val = val * 10 + (*eptr - '0');
        eptr++;
    }
    return (int)(sign * val);
}

static int parse_factor(void);
static int parse_term(void);
static int parse_expr(void);

static int parse_var_or_number(void)
{
    skip_spaces();
    if (isalpha((unsigned char)*eptr))
    {
        char c = (char)toupper((unsigned char)*eptr);
        int idx = c - 'A';
        eptr++;
        if (idx >= 0 && idx < 26)
            return vars[idx];
        return 0;
    }
    return parse_number();
}

static int parse_factor(void)
{
    int value;
    skip_spaces();
    if (*eptr == '(')
    {
        eptr++;
        value = parse_expr();
        skip_spaces();
        if (*eptr == ')')
            eptr++;
        return value;
    }
    return parse_var_or_number();
}

static int parse_term(void)
{
    int value = parse_factor();
    for (;;)
    {
        int op;
        skip_spaces();
        op = *eptr;
        if (op != '*' && op != '/')
            break;
        eptr++;
        {
            int rhs = parse_factor();
            if (op == '*')
                value = value * rhs;
            else if (op == '/')
            {
                if (rhs != 0)
                    value = value / rhs;
                else
                    value = 0;
            }
        }
    }
    return value;
}

static int parse_expr(void)
{
    int value = parse_term();
    for (;;)
    {
        int op;
        skip_spaces();
        op = *eptr;
        if (op != '+' && op != '-')
            break;
        eptr++;
        {
            int rhs = parse_term();
            if (op == '+')
                value = value + rhs;
            else
                value = value - rhs;
        }
    }
    return value;
}

static int eval_expr(const char *s)
{
    eptr = s;
    return parse_expr();
}

static int eval_comparison(const char *s)
{
    char buf[MAX_LINE_LEN];
    const char *op_ptr = NULL;
    int op_len = 0;
    int left, right;
    int i;

    strncpy(buf, s, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';

    for (i = 0; buf[i]; i++)
    {
        if (buf[i] == '<' && buf[i + 1] == '=')
        {
            op_ptr = buf + i;
            op_len = 2;
            break;
        }
        if (buf[i] == '>' && buf[i + 1] == '=')
        {
            op_ptr = buf + i;
            op_len = 2;
            break;
        }
        if (buf[i] == '<' && buf[i + 1] == '>')
        {
            op_ptr = buf + i;
            op_len = 2;
            break;
        }
        if (buf[i] == '=')
        {
            op_ptr = buf + i;
            op_len = 1;
            break;
        }
        if (buf[i] == '<')
        {
            op_ptr = buf + i;
            op_len = 1;
            break;
        }
        if (buf[i] == '>')
        {
            op_ptr = buf + i;
            op_len = 1;
            break;
        }
    }

    if (!op_ptr)
    {
        return eval_expr(s) != 0;
    }

    {
        char left_str[MAX_LINE_LEN];
        const char *right_str;
        int len_left = (int)(op_ptr - buf);
        if (len_left >= (int)sizeof(left_str))
            len_left = (int)sizeof(left_str) - 1;
        memcpy(left_str, buf, (size_t)len_left);
        left_str[len_left] = '\0';
        right_str = op_ptr + op_len;

        left = eval_expr(left_str);
        right = eval_expr(right_str);

        if (op_len == 2)
        {
            if (op_ptr[0] == '<' && op_ptr[1] == '=')
                return left <= right;
            if (op_ptr[0] == '>' && op_ptr[1] == '=')
                return left >= right;
            if (op_ptr[0] == '<' && op_ptr[1] == '>')
                return left != right;
        }
        else
        {
            if (op_ptr[0] == '=')
                return left == right;
            if (op_ptr[0] == '<')
                return left < right;
            if (op_ptr[0] == '>')
                return left > right;
        }
    }
    return 0;
}

static void parse_print(const char *p, int *no_newline)
{
    *no_newline = 0;
    while (*p && isspace((unsigned char)*p))
        p++;

    if (*p == '"')
    {
        const char *q = p + 1;
        while (*q && *q != '"')
            q++;
        if (*q == '"')
        {
            fwrite(p + 1, 1, (size_t)(q - (p + 1)), stdout);
            p = q + 1;
        }
        else
        {
            fputs(p, stdout);
            p += strlen(p);
        }
    }
    else if (isalpha((unsigned char)*p) && p[1] == '$')
    {
        char c = (char)toupper((unsigned char)*p);
        int idx = c - 'A';
        if (idx >= 0 && idx < 26)
        {
            fputs(str_vars[idx], stdout);
        }
        p += 2;
    }
    else
    {
        int v = eval_expr(p);
        printf("%d", v);
        p += strlen(p);
    }

    while (*p && isspace((unsigned char)*p))
        p++;
    if (*p == ';')
        *no_newline = 1;
}

static void parse_let(const char *p)
{
    char var = 0;
    int is_string = 0;

    while (*p && isspace((unsigned char)*p))
        p++;
    if (isalpha((unsigned char)*p))
    {
        var = (char)toupper((unsigned char)*p);
        p++;
        if (*p == '$')
        {
            is_string = 1;
            p++;
        }
    }
    while (*p && isspace((unsigned char)*p))
        p++;
    if (*p == '=')
        p++;
    while (*p && isspace((unsigned char)*p))
        p++;

    if (var >= 'A' && var <= 'Z')
    {
        int idx = var - 'A';
        if (is_string)
        {
            if (*p == '"')
            {
                const char *q = p + 1;
                int len;
                while (*q && *q != '"')
                    q++;
                len = (int)(q - (p + 1));
                if (len >= MAX_STR_LEN)
                    len = MAX_STR_LEN - 1;
                memcpy(str_vars[idx], p + 1, (size_t)len);
                str_vars[idx][len] = '\0';
            }
        }
        else
        {
            vars[idx] = eval_expr(p);
        }
    }
}

static void parse_input(const char *p)
{
    char var = 0;
    int is_string = 0;
    char buf[MAX_STR_LEN];

    while (*p && isspace((unsigned char)*p))
        p++;
    if (isalpha((unsigned char)*p))
    {
        var = (char)toupper((unsigned char)*p);
        p++;
        if (*p == '$')
        {
            is_string = 1;
            p++;
        }
    }

    printf("? %c%s = ", var ? var : 'X', is_string ? "$" : "");
    if (fgets(buf, sizeof(buf), stdin))
    {
        if (var >= 'A' && var <= 'Z')
        {
            int idx = var - 'A';
            if (is_string)
            {
                int len = (int)strlen(buf);
                if (len > 0 && buf[len - 1] == '\n')
                    buf[len - 1] = '\0';
                strncpy(str_vars[idx], buf, MAX_STR_LEN - 1);
                str_vars[idx][MAX_STR_LEN - 1] = '\0';
            }
            else
            {
                vars[idx] = atoi(buf);
            }
        }
    }
}

static int parse_if_then(const char *p, int *then_line)
{
    const char *then_ptr;
    char up[MAX_LINE_LEN];
    size_t i = 0;

    while (*p && i < sizeof(up) - 1)
    {
        up[i++] = (char)toupper((unsigned char)*p++);
    }
    up[i] = '\0';
    then_ptr = strstr(up, " THEN ");
    if (!then_ptr)
        return 0;

    {
        int cond;
        char left[MAX_LINE_LEN];
        int len_left = (int)(then_ptr - up);
        if (len_left < 0)
            len_left = 0;
        if ((size_t)len_left >= sizeof(left))
            len_left = (int)sizeof(left) - 1;
        memcpy(left, up, (size_t)len_left);
        left[len_left] = '\0';
        cond = eval_comparison(left);
        *then_line = atoi(then_ptr + 6);
        return cond;
    }
}

static void reset_vars(void)
{
    int i;
    for (i = 0; i < 26; i++)
    {
        vars[i] = 0;
        str_vars[i][0] = '\0';
    }
    for_depth = 0;
    gosub_depth = 0;
    pilot_last_answer[0] = '\0';
    pilot_last_yes = 0;
}

static int load_program(const char *path)
{
    FILE *f = fopen(path, "r");
    char buf[MAX_LINE_LEN];
    if (!f)
        return 0;
    line_count = 0;
    while (fgets(buf, sizeof(buf), f) != NULL)
    {
        char *p = buf;
        int num = 0;
        int has_num = 0;
        trim(p);
        if (*p == '\0')
            continue;
        if (isdigit((unsigned char)*p))
        {
            has_num = 1;
            while (isdigit((unsigned char)*p))
            {
                num = num * 10 + (*p - '0');
                p++;
            }
        }
        while (*p && isspace((unsigned char)*p))
            p++;
        if (!has_num)
            continue;
        if (line_count < MAX_LINES)
        {
            program[line_count].number = num;
            strncpy(program[line_count].text, p, MAX_LINE_LEN - 1);
            program[line_count].text[MAX_LINE_LEN - 1] = '\0';
            trim(program[line_count].text);
            line_count++;
        }
    }
    fclose(f);
    return line_count > 0;
}

static int save_program(const char *path)
{
    FILE *f = fopen(path, "w");
    int i;
    if (!f)
        return 0;
    for (i = 0; i < line_count; i++)
    {
        fprintf(f, "%d %s\n", program[i].number, program[i].text);
    }
    fclose(f);
    return 1;
}

static int cmp_line(const void *a, const void *b)
{
    const Line *la = (const Line *)a;
    const Line *lb = (const Line *)b;
    if (la->number < lb->number)
        return -1;
    if (la->number > lb->number)
        return 1;
    return 0;
}

static void sort_program(void)
{
    qsort(program, (size_t)line_count, sizeof(Line), cmp_line);
}

/* ---------------- PILOT helpers ---------------- */
static void pilot_collect_labels(void)
{
    int i;
    pilot_label_count = 0;
    for (i = 0; i < line_count && pilot_label_count < MAX_LABELS; i++)
    {
        char buf[MAX_LINE_LEN];
        char *p;
        int len;
        strncpy(buf, program[i].text, sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';
        trim(buf);
        if (buf[0] == '\0')
            continue;
        /* uppercase for detection */
        for (p = buf; *p; ++p)
            *p = (char)toupper((unsigned char)*p);
        if (buf[0] == 'L' && buf[1] == ':')
        {
            const char *name = buf + 2; /* use trimmed/uppercased buffer */
            /* skip spaces */
            while (*name && isspace((unsigned char)*name))
                name++;
            /* copy up to space */
            {
                char nm[32];
                int j = 0;
                while (name[j] && !isspace((unsigned char)name[j]) && j < (int)sizeof(nm) - 1)
                {
                    nm[j] = name[j];
                    j++;
                }
                nm[j] = '\0';
                strncpy(pilot_labels[pilot_label_count].name, nm, sizeof(pilot_labels[pilot_label_count].name) - 1);
                pilot_labels[pilot_label_count].name[sizeof(pilot_labels[pilot_label_count].name) - 1] = '\0';
                pilot_labels[pilot_label_count].index = i;
                pilot_label_count++;
            }
        }
    }
}

static int pilot_find_label(const char *name)
{
    int i;
    char up[32];
    int j = 0;
    while (name[j] && j < (int)sizeof(up) - 1)
    {
        up[j] = (char)toupper((unsigned char)name[j]);
        j++;
    }
    up[j] = '\0';
    for (i = 0; i < pilot_label_count; i++)
    {
        if (strcmp(pilot_labels[i].name, up) == 0)
            return pilot_labels[i].index;
    }
    return -1;
}

static void pilot_print_interpolated(const char *text)
{
    /* Replace *A* or *A$* with var values */
    const char *p = text;
    while (*p)
    {
        if (*p == '*')
        {
            const char *q = p + 1;
            char var = 0;
            int is_str = 0;
            if (*q && isalpha((unsigned char)*q))
            {
                var = (char)toupper((unsigned char)*q);
                q++;
            }
            if (*q == '$')
            {
                is_str = 1;
                q++;
            }
            if (*q == '*')
            {
                if (var >= 'A' && var <= 'Z')
                {
                    int idx = var - 'A';
                    if (is_str)
                        fputs(str_vars[idx], stdout);
                    else
                        printf("%d", vars[idx]);
                }
                p = q + 1; /* consume closing * */
                continue;
            }
        }
        /* default */
        fputc(*p, stdout);
        p++;
    }
}

/* ---------------- Logo helpers ---------------- */
static void logo_clear(void)
{
    int y;
    for (y = 0; y < LOGO_H; y++)
    {
        int x;
        for (x = 0; x < LOGO_W; x++)
            logo_canvas[y][x] = ' ';
        logo_canvas[y][LOGO_W] = '\0';
    }
    if (logo_x < 0)
        logo_x = 0;
    if (logo_x >= canvas_w)
        logo_x = canvas_w - 1;
    if (logo_y < 0)
        logo_y = 0;
    if (logo_y >= canvas_h)
        logo_y = canvas_h - 1;
    logo_dir = 0;
    logo_pen = 1;
    logo_dirty = 0;
}

static void logo_plot(int x, int y)
{
    if (x >= 0 && x < canvas_w && y >= 0 && y < canvas_h)
    {
        logo_canvas[y][x] = logo_pen_char;
        logo_dirty = 1;
    }
}

static void logo_home(void)
{
    logo_x = canvas_w / 2;
    logo_y = canvas_h / 2;
    if (logo_mark_start && logo_pen)
        logo_plot(logo_x, logo_y);
}

static void logo_turn_right(int deg)
{
    /* round to nearest 45 degrees for smoother UX */
    int steps = (deg >= 0) ? ((deg + 22) / 45) : -(((-deg + 22) / 45));
    /* normalize to [0,7] */
    logo_dir = (logo_dir + (steps % 8) + 8) % 8;
}

static void logo_turn_left(int deg)
{
    logo_turn_right(-deg);
}

static void logo_forward(int n)
{
    int i;
    int dx = 0, dy = 0;
    /* 8-direction unit steps */
    switch (logo_dir & 7)
    {
    case 0:
        dx = 1;
        dy = 0;
        break; /* E */
    case 1:
        dx = 1;
        dy = 1;
        break; /* SE */
    case 2:
        dx = 0;
        dy = 1;
        break; /* S */
    case 3:
        dx = -1;
        dy = 1;
        break; /* SW */
    case 4:
        dx = -1;
        dy = 0;
        break; /* W */
    case 5:
        dx = -1;
        dy = -1;
        break; /* NW */
    case 6:
        dx = 0;
        dy = -1;
        break; /* N */
    case 7:
        dx = 1;
        dy = -1;
        break; /* NE */
    }
    if (n < 0)
    {
        n = -n;
        dx = -dx;
        dy = -dy;
    }
    for (i = 0; i < n; i++)
    {
        int nx = logo_x + dx;
        int ny = logo_y + dy;
        /* clamp at edges: stop if next step would leave canvas */
        if (nx < 0 || nx >= canvas_w || ny < 0 || ny >= canvas_h)
        {
            /* plot boundary point if pen is down and point is valid */
            if (logo_pen)
            {
                int bx = logo_x;
                int by = logo_y;
                /* attempt to move to boundary if within one step */
                if (nx < 0)
                    bx = 0;
                else if (nx >= canvas_w)
                    bx = canvas_w - 1;
                else
                    bx = nx;
                if (ny < 0)
                    by = 0;
                else if (ny >= canvas_h)
                    by = canvas_h - 1;
                else
                    by = ny;
                logo_plot(bx, by);
            }
            break;
        }
        logo_x = nx;
        logo_y = ny;
        if (logo_pen)
            logo_plot(logo_x, logo_y);
    }
}

/* Bresenham line drawing (independent of pen up/down) */
static void logo_line(int x0, int y0, int x1, int y1)
{
    int dx = x1 - x0;
    int dy = y1 - y0;
    int sx = (dx < 0) ? -1 : 1;
    int sy = (dy < 0) ? -1 : 1;
    int err;
    dx = dx < 0 ? -dx : dx;
    dy = dy < 0 ? -dy : dy;

    if (dx > dy)
    {
        err = dx / 2;
        while (x0 != x1)
        {
            if (x0 >= 0 && x0 < canvas_w && y0 >= 0 && y0 < canvas_h)
            {
                logo_canvas[y0][x0] = logo_pen_char;
                logo_dirty = 1;
            }
            err -= dy;
            if (err < 0)
            {
                y0 += sy;
                err += dx;
            }
            x0 += sx;
        }
        if (x1 >= 0 && x1 < canvas_w && y1 >= 0 && y1 < canvas_h)
        {
            logo_canvas[y1][x1] = logo_pen_char;
            logo_dirty = 1;
        }
    }
    else
    {
        err = dy / 2;
        while (y0 != y1)
        {
            if (x0 >= 0 && x0 < canvas_w && y0 >= 0 && y0 < canvas_h)
            {
                logo_canvas[y0][x0] = logo_pen_char;
                logo_dirty = 1;
            }
            err -= dx;
            if (err < 0)
            {
                x0 += sx;
                err += dy;
            }
            y0 += sy;
        }
        if (x1 >= 0 && x1 < canvas_w && y1 >= 0 && y1 < canvas_h)
        {
            logo_canvas[y1][x1] = logo_pen_char;
            logo_dirty = 1;
        }
    }
}

/* Rectangle drawing: outline (filled=0) or filled (filled=1) */
static void logo_rect(int x, int y, int w, int h, int filled)
{
    int ix, iy;
    if (w < 0)
    {
        x = x + w + 1;
        w = -w;
    }
    if (h < 0)
    {
        y = y + h + 1;
        h = -h;
    }
    if (w <= 0 || h <= 0)
        return;

    if (filled)
    {
        for (iy = 0; iy < h; iy++)
        {
            int yy = y + iy;
            if (yy < 0 || yy >= canvas_h)
                continue;
            for (ix = 0; ix < w; ix++)
            {
                int xx = x + ix;
                if (xx >= 0 && xx < canvas_w)
                {
                    logo_canvas[yy][xx] = logo_pen_char;
                    logo_dirty = 1;
                }
            }
        }
    }
    else
    {
        /* top and bottom edges */
        for (ix = 0; ix < w; ix++)
        {
            int xx = x + ix;
            int yt = y;
            int yb = y + h - 1;
            if (yt >= 0 && yt < canvas_h && xx >= 0 && xx < canvas_w)
            {
                logo_canvas[yt][xx] = logo_pen_char;
                logo_dirty = 1;
            }
            if (yb >= 0 && yb < canvas_h && xx >= 0 && xx < canvas_w)
            {
                logo_canvas[yb][xx] = logo_pen_char;
                logo_dirty = 1;
            }
        }
        /* left and right edges */
        for (iy = 0; iy < h; iy++)
        {
            int yy = y + iy;
            int xl = x;
            int xr = x + w - 1;
            if (yy >= 0 && yy < canvas_h && xl >= 0 && xl < canvas_w)
            {
                logo_canvas[yy][xl] = logo_pen_char;
                logo_dirty = 1;
            }
            if (yy >= 0 && yy < canvas_h && xr >= 0 && xr < canvas_w)
            {
                logo_canvas[yy][xr] = logo_pen_char;
                logo_dirty = 1;
            }
        }
    }
}

/* Midpoint circle algorithm: outline or filled disc */
static void logo_circle(int xc, int yc, int r, int filled)
{
    int x = 0;
    int y = r;
    int d = 1 - r;

    /* helper to plot 8-way symmetry (and fill spans if needed) */
    while (y >= x)
    {
        /* Outline points */
        int pts[8][2];
        pts[0][0] = xc + x;
        pts[0][1] = yc + y;
        pts[1][0] = xc - x;
        pts[1][1] = yc + y;
        pts[2][0] = xc + x;
        pts[2][1] = yc - y;
        pts[3][0] = xc - x;
        pts[3][1] = yc - y;
        pts[4][0] = xc + y;
        pts[4][1] = yc + x;
        pts[5][0] = xc - y;
        pts[5][1] = yc + x;
        pts[6][0] = xc + y;
        pts[6][1] = yc - x;
        pts[7][0] = xc - y;
        pts[7][1] = yc - x;

        {
            int i;
            for (i = 0; i < 8; i++)
            {
                int px = pts[i][0];
                int py = pts[i][1];
                if (px >= 0 && px < canvas_w && py >= 0 && py < canvas_h)
                {
                    logo_canvas[py][px] = logo_pen_char;
                    logo_dirty = 1;
                }
            }
        }

        if (filled)
        {
            int xL, xR;
            /* draw horizontal spans between left/right pairs on each y */
            xL = xc - x;
            xR = xc + x;
            if (yc + y >= 0 && yc + y < canvas_h)
            {
                int xx;
                int yy = yc + y;
                if (xL < 0)
                    xL = 0;
                if (xR >= canvas_w)
                    xR = canvas_w - 1;
                for (xx = xL; xx <= xR; xx++)
                {
                    logo_canvas[yy][xx] = logo_pen_char;
                    logo_dirty = 1;
                }
            }
            if (yc - y >= 0 && yc - y < canvas_h)
            {
                int xx;
                int yy = yc - y;
                xL = xc - x;
                xR = xc + x;
                if (xL < 0)
                    xL = 0;
                if (xR >= canvas_w)
                    xR = canvas_w - 1;
                for (xx = xL; xx <= xR; xx++)
                {
                    logo_canvas[yy][xx] = logo_pen_char;
                    logo_dirty = 1;
                }
            }
            xL = xc - y;
            xR = xc + y;
            if (yc + x >= 0 && yc + x < canvas_h)
            {
                int xx;
                int yy = yc + x;
                if (xL < 0)
                    xL = 0;
                if (xR >= canvas_w)
                    xR = canvas_w - 1;
                for (xx = xL; xx <= xR; xx++)
                {
                    logo_canvas[yy][xx] = logo_pen_char;
                    logo_dirty = 1;
                }
            }
            if (yc - x >= 0 && yc - x < canvas_h)
            {
                int xx;
                int yy = yc - x;
                xL = xc - y;
                xR = xc + y;
                if (xL < 0)
                    xL = 0;
                if (xR >= canvas_w)
                    xR = canvas_w - 1;
                for (xx = xL; xx <= xR; xx++)
                {
                    logo_canvas[yy][xx] = logo_pen_char;
                    logo_dirty = 1;
                }
            }
        }

        x++;
        if (d < 0)
        {
            d += 2 * x + 1;
        }
        else
        {
            y--;
            d += 2 * (x - y) + 1;
        }
    }
}

static const char *logo_dir_name(void)
{
    switch (logo_dir & 7)
    {
    case 0:
        return "E";
    case 1:
        return "SE";
    case 2:
        return "S";
    case 3:
        return "SW";
    case 4:
        return "W";
    case 5:
        return "NW";
    case 6:
        return "N";
    case 7:
        return "NE";
    }
    return "?";
}

static void logo_show(void)
{
    int y;
    if (logo_show_border)
    {
        /* top border */
        int i;
        putchar('+');
        for (i = 0; i < canvas_w; i++)
            putchar('-');
        putchar('+');
        putchar('\n');
        for (y = 0; y < canvas_h; y++)
        {
            int x;
            putchar('|');
            for (x = 0; x < canvas_w; x++)
            {
                if (logo_show_turtle && x == logo_x && y == logo_y)
                    putchar('O');
                else
                    putchar(logo_canvas[y][x]);
            }
            putchar('|');
            putchar('\n');
        }
        /* bottom border */
        putchar('+');
        for (i = 0; i < canvas_w; i++)
            putchar('-');
        putchar('+');
        putchar('\n');
    }
    else
    {
        for (y = 0; y < canvas_h; y++)
        {
            int x;
            for (x = 0; x < canvas_w; x++)
            {
                if (logo_show_turtle && x == logo_x && y == logo_y)
                    putchar('O');
                else
                    putchar(logo_canvas[y][x]);
            }
            putchar('\n');
        }
    }
    /* status line */
    printf("Pos %d,%d  Dir %s  Pen %s\n", logo_x, logo_y, logo_dir_name(), logo_pen ? "DOWN" : "UP");
}

static void list_program(void)
{
    int i;
    for (i = 0; i < line_count; i++)
    {
        printf("%d %s\n", program[i].number, program[i].text);
    }
}

static void clear_screen(void)
{
#ifdef __DJGPP__
    printf("\033[2J\033[H");
#elif defined(__WATCOMC__)
    system("cls");
#else
    printf("\033[2J\033[H");
#endif
}

static int run_program(void)
{
    int pc = 0;
    reset_vars();
    /* Prepare PILOT labels and Logo canvas */
    pilot_collect_labels();
    logo_clear();
    /* Default display preferences per run */
    logo_show_border = 1;
    logo_show_turtle = 1;
    logo_mark_start = 0;
    canvas_w = LOGO_W;
    canvas_h = LOGO_H;

    while (pc >= 0 && pc < line_count)
    {
        char line[MAX_LINE_LEN];
        char kw[MAX_LINE_LEN];
        const char *p;
        int no_newline = 0;

        strncpy(line, program[pc].text, MAX_LINE_LEN - 1);
        line[MAX_LINE_LEN - 1] = '\0';
        strncpy(kw, line, sizeof(kw) - 1);
        kw[sizeof(kw) - 1] = '\0';
        strtoupper(kw);
        p = kw;

        /* Check for PILOT-style commands (e.g., T:, A:, U:, J:, L:) */
        {
            char pilotbuf[MAX_LINE_LEN];
            const char *s = program[pc].text;
            int i = 0;
            while (*s && isspace((unsigned char)*s))
                s++;
            /* uppercase copy */
            while (s[i] && i < (int)sizeof(pilotbuf) - 1)
            {
                pilotbuf[i] = (char)toupper((unsigned char)s[i]);
                i++;
            }
            pilotbuf[i] = '\0';
            if (pilotbuf[0] && pilotbuf[1] == ':')
            {
                char code = pilotbuf[0];
                const char *arg = program[pc].text + 2; /* original */
                while (*arg && isspace((unsigned char)*arg))
                    arg++;
                if (code == 'T')
                {
                    pilot_print_interpolated(arg);
                    putchar('\n');
                    pc++;
                    continue;
                }
                else if (code == 'A')
                {
                    /* Expect variable name like A or A$ */
                    char var = 0;
                    int is_str = 0;
                    char buf[MAX_STR_LEN];
                    if (*arg && isalpha((unsigned char)*arg))
                    {
                        var = (char)toupper((unsigned char)*arg);
                        arg++;
                    }
                    if (*arg == '$')
                    {
                        is_str = 1;
                        arg++;
                    }
                    if (var)
                    {
                        int idx = var - 'A';
                        printf("? %c%s = ", var, is_str ? "$" : "");
                        if (fgets(buf, sizeof(buf), stdin))
                        {
                            /* track last PILOT answer */
                            {
                                /* store raw (trimmed) answer */
                                int blen = (int)strlen(buf);
                                while (blen > 0 && (buf[blen - 1] == '\n' || buf[blen - 1] == '\r'))
                                {
                                    buf[--blen] = '\0';
                                }
                                strncpy(pilot_last_answer, buf, MAX_STR_LEN - 1);
                                pilot_last_answer[MAX_STR_LEN - 1] = '\0';
                                /* compute yes/no with broader recognition */
                                {
                                    const char *q = pilot_last_answer;
                                    char upbuf[MAX_STR_LEN];
                                    int i = 0;
                                    long ival;
                                    char *endptr;
                                    /* skip leading spaces */
                                    while (*q && isspace((unsigned char)*q))
                                        q++;
                                    /* uppercase copy */
                                    while (q[i] && i < (int)sizeof(upbuf) - 1)
                                    {
                                        upbuf[i] = (char)toupper((unsigned char)q[i]);
                                        i++;
                                    }
                                    upbuf[i] = '\0';
                                    /* trim trailing spaces */
                                    while (i > 0 && isspace((unsigned char)upbuf[i - 1]))
                                        upbuf[--i] = '\0';

                                    if (i == 0)
                                    {
                                        pilot_last_yes = 0;
                                    }
                                    else if (strcmp(upbuf, "Y") == 0 || strncmp(upbuf, "YES", 3) == 0 || strcmp(upbuf, "OK") == 0 || strcmp(upbuf, "OKAY") == 0 || strcmp(upbuf, "SURE") == 0 || strcmp(upbuf, "YEP") == 0 || strcmp(upbuf, "YEAH") == 0 || strcmp(upbuf, "AYE") == 0 || strcmp(upbuf, "AFFIRMATIVE") == 0)
                                    {
                                        pilot_last_yes = 1;
                                    }
                                    else if (strcmp(upbuf, "N") == 0 || strncmp(upbuf, "NO", 2) == 0 || strcmp(upbuf, "NOPE") == 0 || strcmp(upbuf, "NAH") == 0 || strcmp(upbuf, "NAY") == 0 || strcmp(upbuf, "NEGATIVE") == 0)
                                    {
                                        pilot_last_yes = 0;
                                    }
                                    else if (strcmp(upbuf, "T") == 0 || strncmp(upbuf, "TRUE", 4) == 0)
                                    {
                                        pilot_last_yes = 1;
                                    }
                                    else if (strcmp(upbuf, "F") == 0 || strncmp(upbuf, "FALSE", 5) == 0)
                                    {
                                        pilot_last_yes = 0;
                                    }
                                    else if (strncmp(upbuf, "ON", 2) == 0)
                                    {
                                        pilot_last_yes = 1;
                                    }
                                    else if (strncmp(upbuf, "OFF", 3) == 0)
                                    {
                                        pilot_last_yes = 0;
                                    }
                                    else if (upbuf[0] == '+' || upbuf[0] == '-' || isdigit((unsigned char)upbuf[0]))
                                    {
                                        ival = strtol(upbuf, &endptr, 10);
                                        pilot_last_yes = (ival != 0);
                                    }
                                    else
                                    {
                                        pilot_last_yes = 0;
                                    }
                                }
                            }
                            if (is_str)
                            {
                                /* buf is already trimmed of trailing newlines above */
                                strncpy(str_vars[idx], pilot_last_answer, MAX_STR_LEN - 1);
                                str_vars[idx][MAX_STR_LEN - 1] = '\0';
                            }
                            else
                            {
                                vars[idx] = atoi(pilot_last_answer);
                            }
                        }
                    }
                    pc++;
                    continue;
                }
                else if (code == 'Y' || code == 'N')
                {
                    int should_jump = 0;
                    if (code == 'Y')
                        should_jump = (pilot_last_yes != 0);
                    else
                        should_jump = (pilot_last_yes == 0);

                    if (should_jump)
                    {
                        /* Jump to label or line like J: */
                        const char *j = arg;
                        while (*j && isspace((unsigned char)*j))
                            j++;
                        if (isdigit((unsigned char)*j))
                        {
                            int target = atoi(j);
                            int idx = find_line_index(target);
                            if (idx >= 0)
                            {
                                pc = idx;
                                continue;
                            }
                        }
                        else
                        {
                            char nm[32];
                            int k = 0;
                            while (j[k] && !isspace((unsigned char)j[k]) && k < (int)sizeof(nm) - 1)
                            {
                                nm[k] = (char)toupper((unsigned char)j[k]);
                                k++;
                            }
                            nm[k] = '\0';
                            {
                                int idx = pilot_find_label(nm);
                                if (idx >= 0)
                                {
                                    pc = idx;
                                    continue;
                                }
                            }
                        }
                    }
                    /* if condition not met or jump target not found, fall through to next line */
                    pc++;
                    continue;
                }
                else if (code == 'U')
                {
                    /* Reuse LET parser */
                    parse_let(arg);
                    pc++;
                    continue;
                }
                else if (code == 'J')
                {
                    /* Jump to label or line */
                    const char *j = arg;
                    while (*j && isspace((unsigned char)*j))
                        j++;
                    if (isdigit((unsigned char)*j))
                    {
                        int target = atoi(j);
                        int idx = find_line_index(target);
                        if (idx >= 0)
                        {
                            pc = idx;
                            continue;
                        }
                    }
                    else
                    {
                        /* label */
                        char nm[32];
                        int k = 0;
                        while (j[k] && !isspace((unsigned char)j[k]) && k < (int)sizeof(nm) - 1)
                        {
                            nm[k] = (char)toupper((unsigned char)j[k]);
                            k++;
                        }
                        nm[k] = '\0';
                        {
                            int idx = pilot_find_label(nm);
                            if (idx >= 0)
                            {
                                pc = idx;
                                continue;
                            }
                        }
                    }
                    pc++;
                    continue;
                }
                else if (code == 'L')
                {
                    /* Label line: no-op at runtime */
                    pc++;
                    continue;
                }
            }
        }

        /* Check for Logo commands (FORWARD/FD, BACK/BK, RIGHT/RT, LEFT/LT, HEADING/HD, SETXY, CANVAS, PENCHAR, FILL, PU/PD, HOME, CLEAR, SHOW, BORDER, TURTLE, MARKSTART) */
        {
            char cmd[16];
            int wi = 0;
            const char *s = kw;
            while (*s && isspace((unsigned char)*s))
                s++;
            while (*s && !isspace((unsigned char)*s) && wi < (int)sizeof(cmd) - 1)
                cmd[wi++] = *s++;
            cmd[wi] = '\0';
            if (strcmp(cmd, "FORWARD") == 0 || strcmp(cmd, "FD") == 0)
            {
                int n = atoi(s);
                logo_forward(n);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "RIGHT") == 0 || strcmp(cmd, "RT") == 0)
            {
                int n = atoi(s);
                logo_turn_right(n);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "LEFT") == 0 || strcmp(cmd, "LT") == 0)
            {
                int n = atoi(s);
                logo_turn_left(n);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "BACK") == 0 || strcmp(cmd, "BK") == 0)
            {
                int n = atoi(s);
                logo_forward(-n);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "HEADING") == 0 || strcmp(cmd, "HD") == 0)
            {
                int deg = atoi(s);
                int norm = deg % 360;
                if (norm < 0)
                    norm += 360;
                logo_dir = ((norm + 22) / 45) % 8;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "SETXY") == 0)
            {
                int x, y;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y = atoi(s);
                if (x < 0)
                    x = 0;
                if (x >= canvas_w)
                    x = canvas_w - 1;
                if (y < 0)
                    y = 0;
                if (y >= canvas_h)
                    y = canvas_h - 1;
                logo_x = x;
                logo_y = y;
                if (logo_mark_start && logo_pen)
                    logo_plot(logo_x, logo_y);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "PENCHAR") == 0)
            {
                while (*s && isspace((unsigned char)*s))
                    s++;
                if (*s && *s != '\n' && *s != '\r')
                {
                    logo_pen_char = *s;
                }
                pc++;
                continue;
            }
            else if (strcmp(cmd, "FILL") == 0)
            {
                char ch = ' ';
                int yy, xx;
                while (*s && isspace((unsigned char)*s))
                    s++;
                if (*s && *s != '\n' && *s != '\r')
                    ch = *s;
                for (yy = 0; yy < canvas_h; yy++)
                {
                    for (xx = 0; xx < canvas_w; xx++)
                        logo_canvas[yy][xx] = ch;
                }
                logo_dirty = 1;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "CANVAS") == 0)
            {
                int w = 0, h = 0;
                /* parse two integers, separated by comma or spaces */
                while (*s && isspace((unsigned char)*s))
                    s++;
                w = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                h = atoi(s);
                if (w < 1)
                    w = 1;
                if (w > LOGO_W)
                    w = LOGO_W;
                if (h < 1)
                    h = 1;
                if (h > LOGO_H)
                    h = LOGO_H;
                canvas_w = w;
                canvas_h = h;
                if (logo_x >= canvas_w)
                    logo_x = canvas_w - 1;
                if (logo_y >= canvas_h)
                    logo_y = canvas_h - 1;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "LINE") == 0)
            {
                int x0, y0, x1, y1;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x0 = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y0 = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                x1 = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y1 = atoi(s);
                logo_line(x0, y0, x1, y1);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "RECT") == 0)
            {
                int x, y, w, h;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                w = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                h = atoi(s);
                logo_rect(x, y, w, h, 0);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "BOX") == 0)
            {
                int x, y, w, h;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                w = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                h = atoi(s);
                logo_rect(x, y, w, h, 1);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "CIRCLE") == 0)
            {
                int x, y, r;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                r = atoi(s);
                if (r < 0)
                    r = -r;
                logo_circle(x, y, r, 0);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "DISC") == 0)
            {
                int x, y, r;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                r = atoi(s);
                if (r < 0)
                    r = -r;
                logo_circle(x, y, r, 1);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "TEXT") == 0)
            {
                int x, y, off, cx;
                const char *s0;
                while (*s && isspace((unsigned char)*s))
                    s++;
                x = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                while (*s && (isspace((unsigned char)*s) || *s == ','))
                    s++;
                y = atoi(s);
                while (*s && (isdigit((unsigned char)*s) || *s == '+' || *s == '-'))
                    s++;
                off = (int)(s - kw);
                s0 = line + off;
                while (*s0 && isspace((unsigned char)*s0))
                    s0++;
                cx = x;
                if (*s0 == '"')
                {
                    const char *p2 = s0 + 1;
                    while (*p2 && *p2 != '"')
                    {
                        if (cx >= 0 && cx < canvas_w && y >= 0 && y < canvas_h)
                            logo_canvas[y][cx] = *p2;
                        cx++;
                        p2++;
                    }
                }
                else
                {
                    const char *p2 = s0;
                    while (*p2 && *p2 != '\r' && *p2 != '\n')
                    {
                        if (cx >= 0 && cx < canvas_w && y >= 0 && y < canvas_h)
                            logo_canvas[y][cx] = *p2;
                        cx++;
                        p2++;
                    }
                }
                logo_dirty = 1;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "DUMP") == 0)
            {
                char fname[128];
                int i2 = 0;
                FILE *f;
                while (*s && isspace((unsigned char)*s))
                    s++;
                while (*s && !isspace((unsigned char)*s) && i2 < (int)sizeof(fname) - 1)
                    fname[i2++] = *s++;
                fname[i2] = '\0';
                f = fopen(fname, "w");
                if (f)
                {
                    int ry;
                    for (ry = 0; ry < canvas_h; ry++)
                    {
                        fwrite(logo_canvas[ry], 1, (size_t)canvas_w, f);
                        fputc('\n', f);
                    }
                    fclose(f);
                }
                pc++;
                continue;
            }
            else if (strcmp(cmd, "BORDER") == 0)
            {
                while (*s && isspace((unsigned char)*s))
                    s++;
                if (strncmp(s, "ON", 2) == 0)
                    logo_show_border = 1;
                else if (strncmp(s, "OFF", 3) == 0)
                    logo_show_border = 0;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "TURTLE") == 0)
            {
                while (*s && isspace((unsigned char)*s))
                    s++;
                if (strncmp(s, "ON", 2) == 0)
                    logo_show_turtle = 1;
                else if (strncmp(s, "OFF", 3) == 0)
                    logo_show_turtle = 0;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "PENUP") == 0 || strcmp(cmd, "PU") == 0)
            {
                logo_pen = 0;
                pc++;
                continue;
            }
            else if (strcmp(cmd, "PENDOWN") == 0 || strcmp(cmd, "PD") == 0)
            {
                logo_pen = 1;
                if (logo_mark_start)
                    logo_plot(logo_x, logo_y);
                pc++;
                continue;
            }
            else if (strcmp(cmd, "HOME") == 0)
            {
                logo_home();
                pc++;
                continue;
            }
            else if (strcmp(cmd, "CLEAR") == 0)
            {
                logo_clear();
                pc++;
                continue;
            }
            else if (strcmp(cmd, "SHOW") == 0)
            {
                logo_show();
                pc++;
                continue;
            }
            else if (strcmp(cmd, "MARKSTART") == 0)
            {
                while (*s && isspace((unsigned char)*s))
                    s++;
                if (strncmp(s, "ON", 2) == 0)
                    logo_mark_start = 1;
                else if (strncmp(s, "OFF", 3) == 0)
                    logo_mark_start = 0;
                pc++;
                continue;
            }
        }

        {
            char word[16];
            int wi = 0;
            while (*p && isspace((unsigned char)*p))
                p++;
            while (*p && !isspace((unsigned char)*p) && wi < (int)sizeof(word) - 1)
                word[wi++] = *p++;
            word[wi] = '\0';

            if (strcmp(word, "REM") == 0)
            {
                pc++;
                continue;
            }
            else if (strcmp(word, "CLS") == 0)
            {
                clear_screen();
                pc++;
                continue;
            }
            else if (strcmp(word, "STOP") == 0 || strcmp(word, "END") == 0)
            {
                /* Auto-render Logo canvas if anything was drawn */
                if (logo_dirty)
                {
                    logo_show();
                }
                break;
            }
            else if (strcmp(word, "PRINT") == 0)
            {
                parse_print(program[pc].text + 5, &no_newline);
                if (!no_newline)
                    putchar('\n');
                pc++;
                continue;
            }
            else if (strcmp(word, "LET") == 0)
            {
                parse_let(program[pc].text + 3);
                pc++;
                continue;
            }
            else if (strcmp(word, "INPUT") == 0)
            {
                parse_input(program[pc].text + 5);
                pc++;
                continue;
            }
            else if (strcmp(word, "GOTO") == 0)
            {
                int target = atoi(program[pc].text + 4);
                int idx = find_line_index(target);
                if (idx >= 0)
                    pc = idx;
                else
                    pc++;
                continue;
            }
            else if (strcmp(word, "GOSUB") == 0)
            {
                int target = atoi(program[pc].text + 5);
                int idx = find_line_index(target);
                if (idx >= 0 && gosub_depth < MAX_GOSUB_DEPTH)
                {
                    gosub_stack[gosub_depth++] = pc + 1;
                    pc = idx;
                }
                else
                {
                    pc++;
                }
                continue;
            }
            else if (strcmp(word, "RETURN") == 0)
            {
                if (gosub_depth > 0)
                {
                    pc = gosub_stack[--gosub_depth];
                }
                else
                {
                    pc++;
                }
                continue;
            }
            else if (strcmp(word, "IF") == 0)
            {
                int then_line = -1;
                if (parse_if_then(program[pc].text + 2, &then_line))
                {
                    int idx = find_line_index(then_line);
                    if (idx >= 0)
                    {
                        pc = idx;
                        continue;
                    }
                }
                pc++;
                continue;
            }
            else if (strcmp(word, "FOR") == 0)
            {
                const char *orig = program[pc].text + 3;
                char var = 0;
                int start, end, step = 1;
                const char *to_ptr, *step_ptr;
                char up[MAX_LINE_LEN];
                int i_up = 0;

                while (*orig && i_up < (int)sizeof(up) - 1)
                {
                    up[i_up++] = (char)toupper((unsigned char)*orig++);
                }
                up[i_up] = '\0';

                orig = up;
                while (*orig && isspace((unsigned char)*orig))
                    orig++;
                if (isalpha((unsigned char)*orig))
                {
                    var = *orig;
                    orig++;
                }
                while (*orig && isspace((unsigned char)*orig))
                    orig++;
                if (*orig == '=')
                    orig++;

                to_ptr = strstr(orig, " TO ");
                if (to_ptr)
                {
                    char start_str[64];
                    int len = (int)(to_ptr - orig);
                    if (len >= (int)sizeof(start_str))
                        len = (int)sizeof(start_str) - 1;
                    memcpy(start_str, orig, (size_t)len);
                    start_str[len] = '\0';
                    start = eval_expr(start_str);

                    orig = to_ptr + 4;
                    step_ptr = strstr(orig, " STEP ");
                    if (step_ptr)
                    {
                        char end_str[64];
                        len = (int)(step_ptr - orig);
                        if (len >= (int)sizeof(end_str))
                            len = (int)sizeof(end_str) - 1;
                        memcpy(end_str, orig, (size_t)len);
                        end_str[len] = '\0';
                        end = eval_expr(end_str);
                        step = eval_expr(step_ptr + 6);
                    }
                    else
                    {
                        end = eval_expr(orig);
                    }

                    if (var >= 'A' && var <= 'Z' && for_depth < MAX_FOR_DEPTH)
                    {
                        int idx = var - 'A';
                        vars[idx] = start;
                        for_stack[for_depth].var = var;
                        for_stack[for_depth].target = end;
                        for_stack[for_depth].step = step;
                        for_stack[for_depth].loop_line = pc;
                        for_depth++;
                    }
                }
                pc++;
                continue;
            }
            else if (strcmp(word, "NEXT") == 0)
            {
                if (for_depth > 0)
                {
                    ForStack *fs = &for_stack[for_depth - 1];
                    int idx = fs->var - 'A';
                    vars[idx] += fs->step;

                    if ((fs->step > 0 && vars[idx] <= fs->target) ||
                        (fs->step < 0 && vars[idx] >= fs->target))
                    {
                        pc = fs->loop_line + 1;
                    }
                    else
                    {
                        for_depth--;
                        pc++;
                    }
                }
                else
                {
                    pc++;
                }
                continue;
            }
            else
            {
                pc++;
                continue;
            }
        }
    }
    return 0;
}

static void usage(const char *argv0)
{
    printf("Time Warp DOS v1.0 (C89)\n");
    printf("Usage: %s [program.spt]\n", argv0);
    printf("Without arguments: starts interactive mode\n");
    printf("With file: loads and runs the program\n\n");
    printf("BASIC: PRINT LET INPUT GOTO GOSUB RETURN IF...THEN FOR...NEXT END STOP CLS REM\n");
    printf("PILOT: L: T: A: U: J: Y: N:   (e.g., T:Hello *A*; A:A$; U: A = 5; J:START; Y:OK; N:RETRY)\n");
    printf("Logo:  FORWARD/FD n, BACK/BK n, RIGHT/RT n, LEFT/LT n (multiples of 45)\n");
    printf("       HEADING/HD deg, SETXY x,y, LINE x0,y0,x1,y1, TEXT x,y message,\n");
    printf("       RECT x,y,w,h, BOX x,y,w,h, CIRCLE x,y,r, DISC x,y,r,\n");
    printf("       CANVAS w,h, PENCHAR c, FILL [c], DUMP filename,\n");
    printf("       PENUP/PU, PENDOWN/PD, HOME, CLEAR, SHOW, BORDER ON|OFF, TURTLE ON|OFF, MARKSTART ON|OFF\n");
    printf("Interactive: LIST NEW RUN SAVE LOAD\n");
}

static void interactive_loop(void)
{
    char buf[MAX_LINE_LEN];
    printf("Time Warp DOS v1.0 - Interactive Mode\n");
    printf("Type HELP for commands, BYE to exit\n\n");

    while (1)
    {
        printf("> ");
        if (!fgets(buf, sizeof(buf), stdin))
            break;
        trim(buf);
        if (*buf == '\0')
            continue;

        {
            char cmd[16];
            int i = 0;
            const char *p = buf;
            while (*p && !isspace((unsigned char)*p) && i < (int)sizeof(cmd) - 1)
            {
                cmd[i++] = (char)toupper((unsigned char)*p++);
            }
            cmd[i] = '\0';

            if (strcmp(cmd, "BYE") == 0 || strcmp(cmd, "EXIT") == 0 || strcmp(cmd, "QUIT") == 0)
            {
                break;
            }
            else if (strcmp(cmd, "HELP") == 0)
            {
                printf("Interactive commands:\n");
                printf("  LIST - show program\n");
                printf("  NEW - clear program\n");
                printf("  RUN - execute program\n");
                printf("  SAVE filename - save program\n");
                printf("  LOAD filename - load program\n");
                printf("  BYE/EXIT/QUIT - exit\n");
                printf("  <number> <statement> - add/replace line\n\n");
            }
            else if (strcmp(cmd, "LIST") == 0)
            {
                list_program();
            }
            else if (strcmp(cmd, "NEW") == 0)
            {
                line_count = 0;
                reset_vars();
                printf("Program cleared.\n");
            }
            else if (strcmp(cmd, "RUN") == 0)
            {
                sort_program();
                run_program();
            }
            else if (strcmp(cmd, "SAVE") == 0)
            {
                while (*p && isspace((unsigned char)*p))
                    p++;
                if (*p)
                {
                    if (save_program(p))
                    {
                        printf("Saved to %s\n", p);
                    }
                    else
                    {
                        printf("Error saving file\n");
                    }
                }
                else
                {
                    printf("Usage: SAVE filename\n");
                }
            }
            else if (strcmp(cmd, "LOAD") == 0)
            {
                while (*p && isspace((unsigned char)*p))
                    p++;
                if (*p)
                {
                    line_count = 0;
                    if (load_program(p))
                    {
                        sort_program();
                        printf("Loaded %s (%d lines)\n", p, line_count);
                    }
                    else
                    {
                        printf("Error loading file\n");
                    }
                }
                else
                {
                    printf("Usage: LOAD filename\n");
                }
            }
            else if (isdigit((unsigned char)cmd[0]))
            {
                int num = atoi(buf);
                const char *text = buf;
                while (*text && isdigit((unsigned char)*text))
                    text++;
                while (*text && isspace((unsigned char)*text))
                    text++;

                if (*text)
                {
                    int idx = find_line_index(num);
                    if (idx >= 0)
                    {
                        strncpy(program[idx].text, text, MAX_LINE_LEN - 1);
                        program[idx].text[MAX_LINE_LEN - 1] = '\0';
                    }
                    else
                    {
                        if (line_count < MAX_LINES)
                        {
                            program[line_count].number = num;
                            strncpy(program[line_count].text, text, MAX_LINE_LEN - 1);
                            program[line_count].text[MAX_LINE_LEN - 1] = '\0';
                            line_count++;
                            sort_program();
                        }
                        else
                        {
                            printf("Program full\n");
                        }
                    }
                }
                else
                {
                    int idx = find_line_index(num);
                    if (idx >= 0)
                    {
                        int i;
                        for (i = idx; i < line_count - 1; i++)
                        {
                            program[i] = program[i + 1];
                        }
                        line_count--;
                    }
                }
            }
            else
            {
                printf("Unknown command: %s (type HELP)\n", cmd);
            }
        }
    }
}

int main(int argc, char **argv)
{
    if (argc > 1)
    {
        const char *path = argv[1];
        if (!load_program(path))
        {
            printf("Error: could not load program '%s'\n", path);
            usage(argv[0]);
            return 1;
        }
        sort_program();
        return run_program();
    }
    else
    {
        interactive_loop();
        return 0;
    }
}
