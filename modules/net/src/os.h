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



typedef union {
    struct sockaddr_in  ipv4;
    struct sockaddr_in6 ipv6;
} zz_os_net_Address;

static inline void os_net_address_ipv4_set_port(zz_os_net_Address *self, short port_be16) {
    self->ipv4.sin_port = port_be16;
}

static inline void os_net_address_ipv6_set_port(zz_os_net_Address *self, short port_be16) {
    self->ipv6.sin6_port = port_be16;
}

static inline short os_net_address_ipv4_get_port(zz_os_net_Address const *self) {
    return self->ipv4.sin_port;
}

static inline short os_net_address_ipv6_get_port(zz_os_net_Address const *self) {
    return self->ipv6.sin6_port;
}

static inline void os_net_address_ipv4_set_ip(zz_os_net_Address *self, uint8_t const *ip) {
    self->ipv4.sin_family = AF_INET;
    memcpy(&(self->ipv4.sin_addr.s_addr), ip, 4);
}

static inline void os_net_address_ipv6_set_ip(zz_os_net_Address *self, uint8_t const *ip) {
    self->ipv6.sin6_family = AF_INET6;
    memcpy(&(self->ipv6.sin6_addr.s6_addr), ip, 16);
}

static inline uint8_t const* os_net_address_ipv6_get_ip(zz_os_net_Address const *self) {
    return self->ipv6.sin6_addr.s6_addr;
}

static inline uint8_t const* os_net_address_ipv4_get_ip(zz_os_net_Address const *self) {
    return (uint8_t const*)&(self->ipv4.sin_addr.s_addr);
}

