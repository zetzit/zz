#if defined(__linux__)
    #define ZZ_ASYNC_UNIX 1
    #define ZZ_ASYNC_POLL 1
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
    } Io;

#endif

#if ZZ_ASYNC_POLL

    #include <poll.h>
    #include <stdbool.h>

    typedef struct {
        size_t count;
        size_t tailsize;

        bool     has_deadline;
        uint64_t deadline;
    } Driver;

    typedef struct pollfd Tail;
#endif

