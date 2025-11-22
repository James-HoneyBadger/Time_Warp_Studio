/*
 * timewarp_amiga.c - Time Warp for Amiga (SAS/C or m68k-amigaos-gcc)
 *
 * Comprehensive BASIC/PILOT/Logo interpreter adapted from DOS version
 * with optional Intuition GUI for graphics rendering
 *
 * Build console version:
 *   m68k-amigaos-gcc -noixemul -o timewarp timewarp_amiga.c
 *
 * Build with Intuition GUI:
 *   m68k-amigaos-gcc -noixemul -DUSE_INTUITION -o timewarp_gui timewarp_amiga.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#ifdef USE_INTUITION
#include <intuition/intuition.h>
#include <graphics/gfx.h>
#include <graphics/rastport.h>
#include <proto/intuition.h>
#include <proto/graphics.h>
#include <proto/exec.h>

struct IntuitionBase *IntuitionBase = NULL;
struct GfxBase *GfxBase = NULL;
struct Window *tw_window = NULL;
struct RastPort *tw_rp = NULL;
#endif

#define MAX_LINES 2048
#define MAX_LINE_LEN 256
#define MAX_STR_LEN 128
#define MAX_FOR_DEPTH 16
#define MAX_GOSUB_DEPTH 32
#define MAX_LABELS 512

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

typedef struct
{
    char name[32];
    int index;
} Label;

static Line program[MAX_LINES];
static int line_count = 0;
static int vars[26];
static char str_vars[26][MAX_STR_LEN];
static ForStack for_stack[MAX_FOR_DEPTH];
static int for_depth = 0;
static int gosub_stack[MAX_GOSUB_DEPTH];
static int gosub_depth = 0;

/* PILOT */
static Label pilot_labels[MAX_LABELS];
static int pilot_label_count = 0;
static char pilot_last_answer[MAX_STR_LEN];
static int pilot_match_flag = 0;

/* Logo turtle state */
static int logo_x = 320;
static int logo_y = 240;
static int logo_angle = 0; /* 0-359 degrees */
static int logo_pen_down = 1;
static int logo_pen_color = 1; /* Amiga pen 1 (usually white) */

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
        *s = toupper(*s);
        s++;
    }
}

static int eval_expr(const char *expr);

static int is_var(char c)
{
    return (c >= 'A' && c <= 'Z');
}

static int get_var_value(char c)
{
    if (is_var(c))
        return vars[c - 'A'];
    return 0;
}

static void set_var(char c, int val)
{
    if (is_var(c))
        vars[c - 'A'] = val;
}

static int parse_int(const char **p)
{
    while (isspace(**p))
        (*p)++;
    if (isdigit(**p))
        return atoi(*p);
    if (is_var(**p))
        return get_var_value(*(*p)++);
    return 0;
}

static int eval_expr(const char *expr)
{
    char buf[256];
    strncpy(buf, expr, 255);
    buf[255] = 0;
    trim(buf);

    const char *p = buf;
    int left = parse_int(&p);

    while (*p)
    {
        while (isspace(*p))
            p++;
        if (!*p)
            break;

        char op = *p++;
        int right = parse_int(&p);

        switch (op)
        {
        case '+':
            left += right;
            break;
        case '-':
            left -= right;
            break;
        case '*':
            left *= right;
            break;
        case '/':
            if (right)
                left /= right;
            break;
        default:
            break;
        }
    }
    return left;
}

/* BASIC Commands */
static void cmd_print(const char *args)
{
    if (*args == '"')
    {
        const char *end = strchr(args + 1, '"');
        if (end)
        {
            int len = end - args - 1;
            printf("%.*s\n", len, args + 1);
        }
    }
    else
    {
        int val = eval_expr(args);
        printf("%d\n", val);
    }
}

static void cmd_let(const char *args)
{
    char var;
    if (sscanf(args, "%c", &var) == 1 && is_var(var))
    {
        const char *eq = strchr(args, '=');
        if (eq)
        {
            set_var(var, eval_expr(eq + 1));
        }
    }
}

static void cmd_input(const char *args)
{
    char var;
    if (sscanf(args, "%c", &var) == 1 && is_var(var))
    {
        printf("? ");
        fflush(stdout);
        char buf[128];
        if (fgets(buf, sizeof(buf), stdin))
        {
            set_var(var, atoi(buf));
        }
    }
}

/* Decide if loop should run at all */
int shouldEnter = (stepVal > 0) ? (startVal <= endVal) : (startVal >= endVal);
if (!shouldEnter)
{
    /* Skip forward to matching NEXT (with same var or first NEXT) */
    int found = 0;
    for (int si = pc + 1; si < line_count; si++)
    {
        char tmpLine[MAX_LINE_LEN];
        strncpy(tmpLine, program[si].text, MAX_LINE_LEN - 1);
        tmpLine[MAX_LINE_LEN - 1] = 0;
        trim(tmpLine);
        strtoupper(tmpLine);
        if (strncmp(tmpLine, "NEXT", 4) == 0)
        {
            char *pv = tmpLine + 4;
            while (*pv && isspace((unsigned char)*pv))
                pv++;
            char vch = *pv;
            if (vch == 0 || vch == var)
            {
                pc = si; /* pc++ at end will move past NEXT */
                found = 1;
                break;
            }
        }
    }
    if (!found)
    {
        /* No NEXT found: end program */
        pc = line_count; /* exit loop */
    }
}
else
{
    set_var(var, startVal);
    for_stack[for_depth].var = var;
    for_stack[for_depth].target = endVal;
    for_stack[for_depth].step = stepVal;
    for_stack[for_depth].loop_line = pc + 1;
    for_depth++;
    int newX = logo_x + (int)(distance * sin(rad));
    int newY = logo_y - (int)(distance * cos(rad));

    if (logo_pen_down)
    {
        SetAPen(tw_rp, logo_pen_color);
        Move(tw_rp, logo_x, logo_y);
        Draw(tw_rp, newX, newY);
    }

    logo_x = newX;
    logo_y = newY;
}
#else
printf("🐢 FORWARD %d\n", distance);
double rad = logo_angle * M_PI / 180.0;
logo_x += (int)(distance * sin(rad));
logo_y -= (int)(distance * cos(rad));
#endif
}

static void logo_right(int angle)
{
    logo_angle = (logo_angle + angle) % 360;
}

static void logo_left(int angle)
{
    logo_angle = (logo_angle - angle + 360) % 360;
}

static void logo_penup(void)
{
    logo_pen_down = 0;
}

static void logo_pendown(void)
{
    logo_pen_down = 1;
}

static void logo_home(void)
{
    logo_x = 320;
    logo_y = 240;
    logo_angle = 0;
}

static void logo_clearscreen(void)
{
#ifdef USE_INTUITION
    if (tw_window && tw_rp)
    {
        SetAPen(tw_rp, 0);
        RectFill(tw_rp, 0, 0, tw_window->Width - 1, tw_window->Height - 1);
        logo_home();
    }
#else
    printf("🐢 CLEARSCREEN\n");
    logo_home();
#endif
}

/* Execute BASIC program */
static void run_basic_program(void)
{
    int pc = 0;
    for_depth = 0;
    gosub_depth = 0;

    while (pc < line_count)
    {
        char *line = program[pc].text;
        trim(line);
        strtoupper(line);

        if (strncmp(line, "PRINT ", 6) == 0)
        {
            cmd_print(line + 6);
        }
        else if (strncmp(line, "FOR ", 4) == 0)
        {
            /* Syntax: FOR X = start TO end [STEP step] */
            if (for_depth < MAX_FOR_DEPTH)
            {
                char *p = line + 4;
                while (*p && isspace((unsigned char)*p))
                    p++;
                char var = *p;
                if (is_var(var))
                {
                    char *eq = strchr(p, '=');
                    if (eq)
                    {
                        /* Parse start */
                        int startVal = 0;
                        char tmp[128];
                        memset(tmp, 0, sizeof(tmp));
                        {
                            const char *from = eq + 1;
                            const char *toKw = strstr(from, "TO");
                            if (toKw)
                            {
                                int n = (int)(toKw - from);
                                if (n > (int)sizeof(tmp) - 1)
                                    n = (int)sizeof(tmp) - 1;
                                strncpy(tmp, from, n);
                                tmp[n] = 0;
                                startVal = eval_expr(tmp);
                                /* Parse end */
                                int endVal = 0;
                                int stepVal = 1;
                                const char *endFrom = toKw + 2;
                                const char *stepKw = strstr(endFrom, "STEP");
                                if (stepKw)
                                {
                                    int en = (int)(stepKw - endFrom);
                                    if (en > (int)sizeof(tmp) - 1)
                                        en = (int)sizeof(tmp) - 1;
                                    strncpy(tmp, endFrom, en);
                                    tmp[en] = 0;
                                    endVal = eval_expr(tmp);
                                    /* step */
                                    memset(tmp, 0, sizeof(tmp));
                                    strncpy(tmp, stepKw + 4, sizeof(tmp) - 1);
                                    stepVal = eval_expr(tmp);
                                    if (stepVal == 0)
                                        stepVal = 1;
                                }
                                else
                                {
                                    strncpy(tmp, endFrom, sizeof(tmp) - 1);
                                    tmp[sizeof(tmp) - 1] = 0;
                                    endVal = eval_expr(tmp);
                                }

                                /* Initialize and push frame */
                                set_var(var, startVal);
                                for_stack[for_depth].var = var;
                                for_stack[for_depth].target = endVal;
                                for_stack[for_depth].step = stepVal;
                                for_stack[for_depth].loop_line = pc + 1;
                                for_depth++;
                            }
                        }
                    }
                }
            }
        }
        else if (strncmp(line, "NEXT", 4) == 0)
        {
            /* Syntax: NEXT X */
            char *p = line + 4;
            while (*p && isspace((unsigned char)*p))
                p++;
            char var = *p;
            if (!is_var(var) && for_depth > 0)
            {
                var = for_stack[for_depth - 1].var; /* default to top */
            }
            if (is_var(var))
            {
                /* Find matching frame from top */
                int idx = -1;
                for (int i = for_depth - 1; i >= 0; i--)
                {
                    if (for_stack[i].var == var)
                    {
                        idx = i;
                        break;
                    }
                }
                if (idx >= 0)
                {
                    int cur = get_var_value(var);
                    int step = for_stack[idx].step;
                    int target = for_stack[idx].target;
                    cur += step;
                    set_var(var, cur);
                    int cont = 0;
                    if (step > 0)
                    {
                        if (cur <= target)
                            cont = 1;
                    }
                    else
                    {
                        if (cur >= target)
                            cont = 1;
                    }
                    if (cont)
                    {
                        pc = for_stack[idx].loop_line - 1; /* -1 because we'll pc++ at end */
                    }
                    else
                    {
                        if (idx == for_depth - 1)
                            for_depth--;
                    }
                }
            }
        }
        else if (strncmp(line, "LET ", 4) == 0)
        {
            cmd_let(line + 4);
        }
        else if (strncmp(line, "INPUT ", 6) == 0)
        {
            cmd_input(line + 6);
        }
        else if (strncmp(line, "GOTO ", 5) == 0)
        {
            int target = atoi(line + 5);
            for (int i = 0; i < line_count; i++)
            {
                if (program[i].number == target)
                {
                    pc = i - 1;
                    break;
                }
            }
        }
        else if (strncmp(line, "GOSUB ", 6) == 0)
        {
            if (gosub_depth < MAX_GOSUB_DEPTH)
            {
                gosub_stack[gosub_depth++] = pc;
                int target = atoi(line + 6);
                for (int i = 0; i < line_count; i++)
                {
                    if (program[i].number == target)
                    {
                        pc = i - 1;
                        break;
                    }
                }
            }
        }
        else if (strcmp(line, "RETURN") == 0)
        {
            if (gosub_depth > 0)
            {
                pc = gosub_stack[--gosub_depth];
            }
        }
        else if (strcmp(line, "END") == 0)
        {
            break;
        }

        pc++;
    }
}

/* Execute Logo program */
static void run_logo_program(const char *code)
{
    char buf[4096];
    strncpy(buf, code, 4095);
    buf[4095] = 0;

    char *tok = strtok(buf, " \n\t");
    while (tok)
    {
        strtoupper(tok);

        if (strcmp(tok, "FORWARD") == 0 || strcmp(tok, "FD") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                logo_forward(atoi(tok));
        }
        else if (strcmp(tok, "BACK") == 0 || strcmp(tok, "BK") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                logo_forward(-atoi(tok));
        }
        else if (strcmp(tok, "RIGHT") == 0 || strcmp(tok, "RT") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                logo_right(atoi(tok));
        }
        else if (strcmp(tok, "LEFT") == 0 || strcmp(tok, "LT") == 0)
        {
            tok = strtok(NULL, " \n\t");
            if (tok)
                logo_left(atoi(tok));
        }
        else if (strcmp(tok, "PENUP") == 0 || strcmp(tok, "PU") == 0)
        {
            logo_penup();
        }
        else if (strcmp(tok, "PENDOWN") == 0 || strcmp(tok, "PD") == 0)
        {
            logo_pendown();
        }
        else if (strcmp(tok, "HOME") == 0)
        {
            logo_home();
        }
        else if (strcmp(tok, "CS") == 0 || strcmp(tok, "CLEARSCREEN") == 0)
        {
            logo_clearscreen();
        }

        tok = strtok(NULL, " \n\t");
    }
}

/* Execute PILOT program */
static void run_pilot_program(const char *code)
{
    char **lines = malloc(MAX_LINES * sizeof(char *));
    int line_count = 0;

    char *text = strdup(code);
    char *line = strtok(text, "\n");
    while (line && line_count < MAX_LINES)
    {
        lines[line_count++] = strdup(line);
        line = strtok(NULL, "\n");
    }
    free(text);

    /* Collect labels */
    pilot_label_count = 0;
    for (int i = 0; i < line_count; i++)
    {
        char *ln = lines[i];
        trim(ln);
        if (*ln == '*')
        {
            ln++;
            sscanf(ln, "%31s", pilot_labels[pilot_label_count].name);
            pilot_labels[pilot_label_count].index = i;
            pilot_label_count++;
        }
    }

    /* Execute */
    int pc = 0;
    while (pc < line_count)
    {
        char *ln = lines[pc];
        trim(ln);

        if (*ln == 0 || *ln == '*')
        {
            pc++;
            continue;
        }

        char cmd = toupper(*ln);
        char *arg = ln + 1;
        if (*arg == ':')
            arg++;
        while (*arg == ' ')
            arg++;

        switch (cmd)
        {
        case 'T': /* Type */
            printf("%s\n", arg);
            break;

        case 'A': /* Accept */
            printf("? ");
            fflush(stdout);
            if (fgets(pilot_last_answer, sizeof(pilot_last_answer), stdin))
            {
                trim(pilot_last_answer);
            }
            break;

        case 'M': /* Match */
            pilot_match_flag = (strstr(pilot_last_answer, arg) != NULL);
            break;

        case 'Y': /* Yes (if match) */
            if (pilot_match_flag)
                printf("%s\n", arg);
            break;

        case 'N': /* No (if not match) */
            if (!pilot_match_flag)
                printf("%s\n", arg);
            break;

        case 'J':
        { /* Jump */
            for (int i = 0; i < pilot_label_count; i++)
            {
                if (strcmp(pilot_labels[i].name, arg) == 0)
                {
                    pc = pilot_labels[i].index - 1;
                    break;
                }
            }
            break;
        }

        case 'E': /* End */
            goto pilot_done;
        }

        pc++;
    }

pilot_done:
    for (int i = 0; i < line_count; i++)
        free(lines[i]);
    free(lines);
}

int main(int argc, char **argv)
{
#ifdef USE_INTUITION
    IntuitionBase = (struct IntuitionBase *)OpenLibrary("intuition.library", 0);
    GfxBase = (struct GfxBase *)OpenLibrary("graphics.library", 0);

    if (IntuitionBase && GfxBase)
    {
        struct NewWindow nw = {
            0, 0, 640, 480, 0, 1,
            IDCMP_CLOSEWINDOW,
            WFLG_CLOSEGADGET | WFLG_DRAGBAR | WFLG_DEPTHGADGET | WFLG_SIZEGADGET,
            NULL, NULL, "Time Warp IDE", NULL, NULL, 100, 100, 640, 480,
            WBENCHSCREEN};

        tw_window = OpenWindow(&nw);
        if (tw_window)
        {
            tw_rp = tw_window->RPort;
            SetAPen(tw_rp, 0);
            RectFill(tw_rp, 0, 0, 639, 479);
        }
    }
#endif

    printf("Time Warp IDE v1.0 - Amiga Edition\n");
    printf("Type BASIC, LOGO, or PILOT code, or QUIT to exit\n\n");

    char input[4096];
    while (1)
    {
        printf("> ");
        fflush(stdout);

        if (!fgets(input, sizeof(input), stdin))
            break;
        trim(input);
        strtoupper(input);

        if (strcmp(input, "QUIT") == 0 || strcmp(input, "EXIT") == 0)
        {
            break;
        }
        else if (strncmp(input, "FD ", 3) == 0 || strncmp(input, "FORWARD ", 8) == 0)
        {
            run_logo_program(input);
        }
        else if (strncmp(input, "T:", 2) == 0 || strncmp(input, "A:", 2) == 0)
        {
            run_pilot_program(input);
        }
        else if (strncmp(input, "PRINT ", 6) == 0)
        {
            cmd_print(input + 6);
        }
        else
        {
            printf("Unknown command\n");
        }
    }

#ifdef USE_INTUITION
    if (tw_window)
        CloseWindow(tw_window);
    if (GfxBase)
        CloseLibrary((struct Library *)GfxBase);
    if (IntuitionBase)
        CloseLibrary((struct Library *)IntuitionBase);
#endif

    return 0;
}
