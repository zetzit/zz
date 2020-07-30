#if defined (_WIN32)
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <Ws2def.h>
#include <Ws2ipdef.h>
#endif

#if defined(__unix__) || defined(__APPLE__) || defined(__linux__)
#include <netinet/in.h>
#include <stdint.h>
#include <string.h>
#endif

#if defined(ESP_PLATFORM)
#include <netinet/in.h>
#include <stdint.h>
#include <string.h>
#include "lwip/err.h"
#include "lwip/sockets.h"
#include "lwip/sys.h"
#include <lwip/netdb.h>
#endif

typedef struct sockaddr_in  sockaddr_in4_t;
typedef struct sockaddr_in6 sockaddr_in6_t;
