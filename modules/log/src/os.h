#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>


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

#elif defined (__XTENSA__)


    #define getenv(a) 0

    typedef enum {
        ESP_LOG_NONE,       /*!< No log output */
        ESP_LOG_ERROR,      /*!< Critical errors, software module can not recover on its own */
        ESP_LOG_WARN,       /*!< Error conditions from which recovery measures have been taken */
        ESP_LOG_INFO,       /*!< Information messages which describe normal flow of events */
        ESP_LOG_DEBUG,      /*!< Extra information which is not necessary for normal use (values, pointers, sizes, etc). */
        ESP_LOG_VERBOSE     /*!< Bigger chunks of debugging information, or frequent messages which can potentially flood the output. */
    } esp_log_level_t;

    void nop__esp_log_writev (esp_log_level_t level, const char* tag, const char* format, va_list args) {}
    void esp_log_writev () __attribute__ ((weak, alias ("nop__esp_log_writev")));


    #define os_zz_log_error(mod, fmt) \
        va_list args; \
        va_start (args, fmt); \
        esp_log_writev(ESP_LOG_ERROR, mod, fmt, args); \
        va_end (args);

    #define os_zz_log_debug(mod, fmt) \
        va_list args; \
        va_start (args, fmt); \
        esp_log_writev(ESP_LOG_DEBUG, mod, fmt, args); \
        va_end (args);

    #define os_zz_log_info(mod, fmt) \
        va_list args; \
        va_start (args, fmt); \
        esp_log_writev(ESP_LOG_INFO, mod, fmt, args); \
        va_end (args);

    #define os_zz_log_warn(mod, fmt) \
        va_list args; \
        va_start (args, fmt); \
        esp_log_writev(ESP_LOG_WARN, mod, fmt, args); \
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

