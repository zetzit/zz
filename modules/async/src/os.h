#if defined(__linux__)
#define ZZ_ASYNC_UNIX 1
#define ZZ_ASYNC_URING 1
#elif defined(__APPLE__)
#define ZZ_ASYNC_UNIX 1
#define ZZ_ASYNC_POLL 1
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

typedef struct
{
    int fd;
} async_os_Io;
#endif

#if ZZ_ASYNC_URING
#include <liburing.h>
typedef struct
{
    struct io_uring ring;
} async_os_Driver;

typedef struct
{
    struct io_uring_sqe *sqe;
    struct io_uring_cqe *cqe;
} async_os_Future;

#endif

#if ZZ_ASYNC_WIN32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdint.h>
#include <stdbool.h>
#include <winsock2.h>

typedef struct
{
    WSAOVERLAPPED ol;
    HANDLE file_handle;
} async_os_Io;

typedef struct
{
    HANDLE completion_port;
    OVERLAPPED_ENTRY completion_port_entries[100];
    DWORD entries_removed;

    bool has_deadline;
    uint64_t deadline;
} async_os_Driver;

typedef struct
{
    HANDLE completion_port;
    DWORD  number_of_bytes_transfered;
} async_os_Future;

static inline int open_async_file(async_os_Io *io, const char *filename)
{
    io->file_handle = CreateFileA(filename, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL);
    if (io->file_handle == NULL)
    {
        return -1;
    }
    return 0;
}

static inline int io_completion_port_init(async_os_Driver *driver)
{
    // Create the completion port used by this driver
    driver->completion_port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, (ULONG_PTR)NULL, 0);
    if (driver->completion_port == NULL)
    {
        return -1;
    }
    return 0;
}

static inline int io_completion_port_bind(async_os_Io *io, async_os_Future *future, async_os_Driver *driver, void *data)
{
    if (future->completion_port != driver->completion_port)
    {
        future->completion_port = CreateIoCompletionPort(io->file_handle, driver->completion_port, (ULONG_PTR)data, 0);
    }
    if (future->completion_port == NULL)
    {
        return -1;
    }
    return 0;
}

static inline int io_completion_port_read(async_os_Io *io, uint8_t *mem, size_t size)
{
    if (FALSE == ReadFile(io->file_handle, mem, size, NULL, &io->ol) && GetLastError() != ERROR_IO_PENDING)
    {
        return -1;
    }
    return 0;
}

static inline int io_completion_port_write(async_os_Io *io, const uint8_t *mem, size_t size)
{
    if (FALSE == WriteFile(io->file_handle, mem, size, NULL, &io->ol) && GetLastError() != ERROR_IO_PENDING)
    {
        return -1;
    }
    return 0;
}

static inline int io_completion_port_wait(async_os_Driver *driver)
{
    driver->entries_removed = 0;
    if (FALSE == GetQueuedCompletionStatusEx(driver->completion_port,
                                             driver->completion_port_entries, sizeof(driver->completion_port_entries) / sizeof(driver->completion_port_entries[0]),
                                             &driver->entries_removed,
                                             driver->has_deadline ? driver->deadline : INFINITE,
                                             FALSE))
    {
        return -1;
    }
    return 0;
}

static inline int io_completion_get_entry_count(async_os_Driver *driver)
{
    return driver->entries_removed;
}

static inline void *io_completion_get_entry_data(async_os_Driver *driver, const int i)
{
    if(driver->completion_port_entries[i].lpOverlapped == NULL)
        return NULL;
    return (void*)driver->completion_port_entries[i].lpCompletionKey;
}

static inline DWORD io_completion_get_entry_number_of_bytes_transfered(async_os_Driver *driver, const int i)
{
    return driver->completion_port_entries[i].dwNumberOfBytesTransferred;
}

#endif
