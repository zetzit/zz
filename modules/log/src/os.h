#include <stdarg.h>
#include <stdio.h>


#if defined(__ANDROID__)
#include <android/log.h>

#define os_zz_log_error(mod, fmt) \
    va_list args; \
    va_start (args, fmt); \
    __android_log_vprint( ANDROID_LOG_ERROR, mod, fmt, args); \
    va_end (args);

#define os_zz_log_debug(mod, fmt) \
    va_list args; \
    va_start (args, fmt); \
    __android_log_vprint( ANDROID_LOG_DEBUG, mod, fmt, args); \
    va_end (args);

#define os_zz_log_info(mod, fmt) \
    va_list args; \
    va_start (args, fmt); \
    __android_log_vprint( ANDROID_LOG_INFO, mod, fmt, args); \
    va_end (args);

#define os_zz_log_warn(mod, fmt) \
    va_list args; \
    va_start (args, fmt); \
    __android_log_vprint( ANDROID_LOG_WARN, mod, fmt, args); \
    va_end (args);

#else

#define os_zz_log_error(mod, fmt) \
    fprintf(stderr, "[\x1B[31mERR\x1B[0m] %s ", mod); \
    va_list args; \
    va_start (args, fmt); \
    vfprintf( \
        stderr, \
        fmt, \
        args \
    ); \
    va_end (args); \
    fprintf(stderr, "\n"); \

#define os_zz_log_warn(mod, fmt) \
    fprintf(stderr, "[\x1B[33mWRN\x1B[0m] %s ", module); \
    va_list args; \
    va_start (args, fmt); \
    vfprintf( \
        stderr, \
        fmt, \
        args \
    ); \
    va_end (args); \
    fprintf(stderr, "\n"); \

#define os_zz_log_info(mod, fmt) \
    fprintf(stderr, "[\x1B[32mINF\x1B[0m] %s ", module); \
    va_list args; \
    va_start (args, fmt); \
    vfprintf( \
        stderr, \
        fmt, \
        args \
    ); \
    va_end (args); \
    fprintf(stderr, "\n"); \


#define os_zz_log_debug(mod, fmt) \
    fprintf(stderr, "[\x1B[36mDBG\x1B[0m] %s ", module); \
    va_list args; \
    va_start (args, fmt); \
    vfprintf( \
        stderr, \
        fmt, \
        args \
    ); \
    va_end (args); \
    fprintf(stderr, "\n"); \

#endif

