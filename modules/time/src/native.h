#include <stdint.h>

int _os_time_system(uint64_t *secs, uint64_t* nanos);

#if defined(__linux__) || defined(__APPLE__)
#include <time.h>
int _os_time_system(uint64_t *secs, uint64_t* nanos) {
    struct timespec tt;
    int r = clock_gettime(CLOCK_MONOTONIC, &tt);
    if (r != 0) {
        return r;
    }

    *secs  = tt.tv_sec;
    *nanos  = tt.tv_nsec;
    return 0;
}
#endif
