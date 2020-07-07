#if defined(__linux__)
    #define ZZ_OS_LINUX 1
    #include <unistd.h>
#else
    #define ZZ_OS_LINUX 0
#endif

#if defined(_WIN32)
    #define ZZ_OS_WINDOWS 1
    #include <windows.h>
#else
    #define ZZ_OS_WINDOWS 0
#endif

#if defined(__APPLE__)
    #define ZZ_OS_APPLE 1
#else
    #define ZZ_OS_APPLE 0
#endif
