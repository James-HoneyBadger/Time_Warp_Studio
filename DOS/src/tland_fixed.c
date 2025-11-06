/* Time Warp (DOS) - minimal interpreter + VGA turtle renderer
 * Build with DJGPP (DOSBox + DJGPP toolchain):
 *   gcc -O2 -ffast-math -s -o TLAND.EXE tland_fixed.c -lm
 */
#include <dos.h>
#include <dpmi.h>
#include <go32.h>
#include <sys/nearptr.h>
#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>

#define WIDTH 320
#define HEIGHT 200
#define PI 3.14159265358979323846

static int show_graphics = 1; /* set to 0 for headless save mode */
static char save_path[260] = {0};
static uint8_t *vmem = NULL; /* linear pointer to 0xA0000 (via nearptr) */

/* BIOS video */
static void set_video_mode(uint16_t ax)
{
    __dpmi_regs r;
    memset(&r, 0, sizeof(r));
    r.x.ax = ax;
    __dpmi_int(0x10, &r);
}
static void set_text_mode() { set_video_mode(0x0003); }
static void set_mode_13h() { set_video_mode(0x0013); }

/* pixel write */
static void pset(int x, int y, uint8_t color)
{
    if (!vmem)
        return;
    if (x < 0 || y < 0 || x >= WIDTH || y >= HEIGHT)
        return;
    vmem[y * WIDTH + x] = color;
}

/* very rough rgb->index mapping (3-3-2 bits) */
static uint8_t rgb_to_index(uint8_t r, uint8_t g, uint8_t b)
{
    return (uint8_t)(((r & 0xE0)) | ((g & 0xE0) >> 3) | ((b & 0xC0) >> 6));
}

/* line drawing */
static void line(int x0, int y0, int x1, int y1, uint8_t col)
{
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    int err = dx + dy, e2;
    for (;;)
    {
        pset(x0, y0, col);
        if (x0 == x1 && y0 == y1)
            break;
        e2 = 2 * err;
        if (e2 >= dy)
        {
            err += dy;
            x0 += sx;
        }
        if (e2 <= dx)
        {
            err += dx;
            y0 += sy;
        }
    }
}

/* turtle state */
static float turtle_x = WIDTH / 2.0f;
static float turtle_y = HEIGHT / 2.0f;
static float turtle_heading = 0.0f; /* deg */
static int turtle_pen_down = 1;
static uint8_t turtle_color_idx = 0xFF; /* white-ish */
static uint8_t turtle_r = 255, turtle_g = 255, turtle_b = 255;

static void turtle_cls()
{
    if (vmem)
        memset(vmem, 0, WIDTH * HEIGHT);
}

static void turtle_forward(float dist)
{
    float rad = turtle_heading * (float)PI / 180.0f;
    float nx = turtle_x + cosf(rad) * dist;
    float ny = turtle_y + sinf(rad) * dist;
    if (turtle_pen_down)
    {
        line((int)floorf(turtle_x), (int)floorf(turtle_y), (int)floorf(nx), (int)floorf(ny), turtle_color_idx);
    }
    turtle_x = nx;
    turtle_y = ny;
}
static void turtle_left(float deg) { turtle_heading -= deg; }
static void turtle_right(float deg) { turtle_heading += deg; }
static void turtle_pen_up() { turtle_pen_down = 0; }
static void turtle_pen_down_fn() { turtle_pen_down = 1; }
static void turtle_set_xy(float x, float y)
{
    turtle_x = x;
    turtle_y = y;
}
static void turtle_set_color(uint8_t r, uint8_t g, uint8_t b)
{
    turtle_r = r;
    turtle_g = g;
    turtle_b = b;
    turtle_color_idx = rgb_to_index(r, g, b);
}

/* BMP write (24-bit BGR, bottom-up, no palette) */
#pragma pack(push, 1)
typedef struct
{
    uint16_t bfType;
    uint32_t bfSize;
    uint16_t bfReserved1;
    uint16_t bfReserved2;
    uint32_t bfOffBits;
} BMPFILEHDR;
typedef struct
{
    uint32_t biSize;
    int32_t biWidth;
    int32_t biHeight;
    uint16_t biPlanes;
    uint16_t biBitCount;
    uint32_t biCompression;
    uint32_t biSizeImage;
    int32_t biXPelsPerMeter;
    int32_t biYPelsPerMeter;
    uint32_t biClrUsed;
    uint32_t biClrImportant;
} BMPINFOHDR;
#pragma pack(pop)

static void save_bmp_24(const char *path)
{
    FILE *f = fopen(path, "wb");
    if (!f)
        return;
    BMPFILEHDR fh;
    BMPINFOHDR ih;
    memset(&fh, 0, sizeof(fh));
    memset(&ih, 0, sizeof(ih));
    fh.bfType = 0x4D42; /* 'BM' */
    ih.biSize = sizeof(BMPINFOHDR);
    ih.biWidth = WIDTH;
    ih.biHeight = HEIGHT; /* bottom-up */
    ih.biPlanes = 1;
    ih.biBitCount = 24;
    ih.biCompression = 0;
    int rowbytes = WIDTH * 3;
    int pad = (4 - (rowbytes % 4)) % 4;
    ih.biSizeImage = (rowbytes + pad) * HEIGHT;
    fh.bfOffBits = sizeof(BMPFILEHDR) + sizeof(BMPINFOHDR);
    fh.bfSize = fh.bfOffBits + ih.biSizeImage;
    fwrite(&fh, sizeof(fh), 1, f);
    fwrite(&ih, sizeof(ih), 1, f);
    uint8_t *row = (uint8_t *)malloc(rowbytes);
    if (!row)
    {
        fclose(f);
        return;
    }
    for (int y = HEIGHT - 1; y >= 0; --y)
    {
        for (int x = 0; x < WIDTH; ++x)
        {
            uint8_t idx = vmem ? vmem[y * WIDTH + x] : 0;
            uint8_t r = (idx & 0xE0);
            uint8_t g = (idx & 0x1C) << 3;
            uint8_t b = (idx & 0x03) << 6;
            int off = x * 3;
            row[off + 0] = b;
            row[off + 1] = g;
            row[off + 2] = r;
        }
        fwrite(row, rowbytes, 1, f);
        for (int i = 0; i < pad; ++i)
            fputc(0, f);
    }
    free(row);
    fclose(f);
}

/* parsing helpers */
static void rtrim(char *s)
{
    int n = (int)strlen(s);
    while (n > 0 && (s[n - 1] == '\n' || s[n - 1] == '\r' || s[n - 1] == ' ' || s[n - 1] == '\t'))
    {
        s[--n] = '\0';
    }
}
static int starts_with(const char *s, const char *pfx)
{
    return strncmp(s, pfx, strlen(pfx)) == 0;
}

/* execute a single line (no REPEAT handling here) */
static void exec_line(const char *s)
{
    while (*s == ' ' || *s == '\t')
        ++s;
    if (*s == '\0' || *s == ';')
        return;
    if (starts_with(s, "PRINT "))
    {
        printf("%s\n", s + 6);
        return;
    }
    if (strcmp(s, "CLS") == 0)
    {
        turtle_cls();
        return;
    }
    if (starts_with(s, "FD "))
    {
        float d = (float)atof(s + 3);
        turtle_forward(d);
        return;
    }
    if (starts_with(s, "LT "))
    {
        float a = (float)atof(s + 3);
        turtle_left(a);
        return;
    }
    if (starts_with(s, "RT "))
    {
        float a = (float)atof(s + 3);
        turtle_right(a);
        return;
    }
    if (strcmp(s, "PU") == 0)
    {
        turtle_pen_up();
        return;
    }
    if (strcmp(s, "PD") == 0)
    {
        turtle_pen_down_fn();
        return;
    }
    if (starts_with(s, "SETXY "))
    {
        float x = 0, y = 0;
        sscanf(s + 6, "%f %f", &x, &y);
        turtle_set_xy(x, y);
        return;
    }
    if (starts_with(s, "COLOR "))
    {
        int r = 255, g = 255, b = 255;
        sscanf(s + 6, "%d %d %d", &r, &g, &b);
        if (r < 0)
            r = 0;
        if (r > 255)
            r = 255;
        if (g < 0)
            g = 0;
        if (g > 255)
            g = 255;
        if (b < 0)
            b = 0;
        if (b > 255)
            b = 255;
        turtle_set_color((uint8_t)r, (uint8_t)g, (uint8_t)b);
        return;
    }
    if (starts_with(s, "PEN "))
    { /* Allow PEN RGB(r,g,b) or PEN r g b */
        const char *p = s + 4;
        while (*p == ' ')
            ++p;
        if (starts_with(p, "RGB("))
        {
            int r = 255, g = 255, b = 255;
            sscanf(p, "RGB(%d,%d,%d)", &r, &g, &b);
            if (r < 0)
                r = 0;
            if (r > 255)
                r = 255;
            if (g < 0)
                g = 0;
            if (g > 255)
                g = 255;
            if (b < 0)
                b = 0;
            if (b > 255)
                b = 255;
            turtle_set_color((uint8_t)r, (uint8_t)g, (uint8_t)b);
        }
        else
        {
            int r = 255, g = 255, b = 255;
            sscanf(p, "%d %d %d", &r, &g, &b);
            if (r < 0)
                r = 0;
            if (r > 255)
                r = 255;
            if (g < 0)
                g = 0;
            if (g > 255)
                g = 255;
            if (b < 0)
                b = 0;
            if (b > 255)
                b = 255;
            turtle_set_color((uint8_t)r, (uint8_t)g, (uint8_t)b);
        }
        return;
    }
    /* Unknown lines are ignored for now */
}

typedef struct
{
    int count;
    int start;
} RepeatEntry;

static void run_script(FILE *fp)
{
    /* Load file into memory as lines */
    char **lines = NULL;
    int cap = 0, n = 0;
    char buf[256];
    while (fgets(buf, sizeof(buf), fp))
    {
        rtrim(buf);
        if (n >= cap)
        {
            cap = cap ? cap * 2 : 128;
            lines = (char **)realloc(lines, sizeof(char *) * cap);
        }
        lines[n] = (char *)malloc(strlen(buf) + 1);
        strcpy(lines[n], buf);
        ++n;
    }
    /* Simple interpreter with REPEAT/ENDREPEAT */
    RepeatEntry stack[16];
    int sp = 0; /* shallow stack */
    for (int i = 0; i < n; ++i)
    {
        const char *s = lines[i];
        const char *t = s;
        while (*t == ' ' || *t == '\t')
            ++t;
        if (starts_with(t, "REPEAT "))
        {
            int c = atoi(t + 7);
            if (c <= 0)
                continue;
            if (sp < (int)(sizeof(stack) / sizeof(stack[0])))
            {
                stack[sp].count = c;
                stack[sp].start = i + 1;
                ++sp;
            }
            continue;
        }
        if (strcmp(t, "ENDREPEAT") == 0)
        {
            if (sp > 0)
            {
                if (--stack[sp - 1].count > 0)
                {
                    i = stack[sp - 1].start - 1;
                }
                else
                {
                    --sp;
                }
            }
            continue;
        }
        exec_line(t);
    }
    for (int i = 0; i < n; ++i)
        free(lines[i]);
    free(lines);
}

int main(int argc, char **argv)
{
    const char *script = NULL;
    for (int i = 1; i < argc; ++i)
    {
        if (strcmp(argv[i], "--save") == 0 && i + 1 < argc)
        {
            strncpy(save_path, argv[i + 1], sizeof(save_path) - 1);
            show_graphics = 0;
            ++i;
        }
        else if (!script)
        {
            script = argv[i];
        }
    }
    if (!script)
    {
        printf("Time Warp (DOS)\nUsage: TLAND.EXE <script.tc> [--save out.bmp]\n");
        return 1;
    }
    FILE *fp = fopen(script, "rt");
    if (!fp)
    {
        printf("Cannot open %s\n", script);
        return 1;
    }

    /* Enter graphics regardless so VRAM exists for headless save */
    set_mode_13h();
    if (__djgpp_nearptr_enable() == 0)
    {
        printf("nearptr enable failed\n");
        fclose(fp);
        set_text_mode();
        return 1;
    }
    vmem = (uint8_t *)(__djgpp_conventional_base + 0xA0000);

    turtle_x = WIDTH / 2.0f;
    turtle_y = HEIGHT / 2.0f;
    turtle_heading = 0.0f;
    turtle_pen_down = 1;
    turtle_set_color(255, 255, 255);
    turtle_cls();
    run_script(fp);
    fclose(fp);

    if (save_path[0])
    {
        save_bmp_24(save_path);
    }

    if (show_graphics)
    {
        printf("\nPress any key...\n");
        while (!kbhit())
        {
        }
        getch();
    }
    __djgpp_nearptr_disable();
    set_text_mode();
    return 0;
}
