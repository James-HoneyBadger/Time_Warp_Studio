#ifndef TIME_WARP_AMIGA_INTERPRETER_H
#define TIME_WARP_AMIGA_INTERPRETER_H

#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif

    // Execute a single command line. Writes a NUL-terminated result into out_buf.
    // Returns 0 on success, non-zero on error. out_buf_len must be > 0.
    int tw_execute_command(const char *line, char *out_buf, size_t out_buf_len);

#ifdef __cplusplus
}
#endif

#endif // TIME_WARP_AMIGA_INTERPRETER_H
