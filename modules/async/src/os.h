#if defined(__linux__)
    #define ZZ_ASYNC_UNIX  1
    #define ZZ_ASYNC_URING 1
#elif defined(__APPLE__)
    #define ZZ_ASYNC_UNIX  1
    #define ZZ_ASYNC_POLL  1
#elif _WIN32
    #define ZZ_ASYNC_WIN32 1
#endif


#if ZZ_ASYNC_UNIX
    #include <errno.h>
    #include <fcntl.h>
    #include <stdint.h>
    #include <stdio.h>
    #include <string.h>
    #include <unistd.h>

    typedef struct {
        int         fd;
    } async_os_Io;
#endif

#if ZZ_ASYNC_URING
    #include <liburing.h>
    typedef struct {
        struct io_uring ring;
    } async_os_Driver;

    typedef struct {
        struct io_uring_sqe * sqe;
        struct io_uring_cqe * cqe;
    } async_os_Future;

#endif


#if ZZ_ASYNC_WIN32
    #define WIN32_LEAN_AND_MEAN 1
    #include <windows.h>
    #include <stdint.h>
    #include <stdbool.h>
    #include <winsock2.h>

    typedef struct {
        int         fd;
    } async_os_Io;

    typedef struct {
        HANDLE completion_port;

        bool     has_deadline;
        uint64_t deadline;
    } async_os_Driver;

    typedef struct {
    } async_os_Future;

#endif
