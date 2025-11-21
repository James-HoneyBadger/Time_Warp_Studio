#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include "interpreter.h"

static void run_single(int argc, char **argv)
{
    // Concatenate argv[1..] with spaces to form a command line
    size_t len = 0;
    for (int i = 1; i < argc; ++i)
        len += strlen(argv[i]) + 1;
    char *line = (char *)malloc(len + 1);
    if (!line)
        return;
    line[0] = '\0';
    for (int i = 1; i < argc; ++i)
    {
        strcat(line, argv[i]);
        if (i + 1 < argc)
            strcat(line, " ");
    }

    char out[1024];
    if (tw_execute_command(line, out, sizeof(out)) == 0)
    {
        if (out[0] != '\0')
            puts(out);
    }
    free(line);
}

static void repl(void)
{
    char line[1024];
    char out[1024];
    printf("> ");
    while (fgets(line, sizeof(line), stdin))
    {
        // Strip trailing newlines
        size_t n = strlen(line);
        while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r'))
            line[--n] = '\0';
        if (n == 0)
        {
            printf("> ");
            continue;
        }

        if (tw_execute_command(line, out, sizeof(out)) == 0)
        {
            if (out[0] != '\0')
                puts(out);
        }
        if (strcasecmp(line, "QUIT") == 0 || strcasecmp(line, "EXIT") == 0)
        {
            break;
        }
        printf("> ");
    }
}

static int run_file(const char *path)
{
    FILE *fp = fopen(path, "r");
    if (!fp)
    {
        fprintf(stderr, "❌ Could not open file: %s\n", path);
        return 1;
    }
    char line[1024];
    char out[1024];
    while (fgets(line, sizeof(line), fp))
    {
        // Strip trailing newlines
        size_t n = strlen(line);
        while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r'))
            line[--n] = '\0';
        if (n == 0)
            continue;

        if (tw_execute_command(line, out, sizeof(out)) == 0)
        {
            if (out[0] != '\0')
                puts(out);
        }
        if (strcasecmp(line, "QUIT") == 0 || strcasecmp(line, "EXIT") == 0)
        {
            break;
        }
    }
    fclose(fp);
    return 0;
}

int main(int argc, char **argv)
{
    if (argc > 1)
    {
        if ((strcmp(argv[1], "-f") == 0 || strcmp(argv[1], "--file") == 0) && argc > 2)
        {
            return run_file(argv[2]);
        }
        run_single(argc, argv);
        return 0;
    }
    repl();
    return 0;
}
