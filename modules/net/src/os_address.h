

#if defined(__unix__) || defined(__APPLE__)
#include <netinet/in.h>
#include <stdint.h>
#include <string.h>

typedef union {
    char             raw[32];
    struct sockaddr_in  ipv4;
    struct sockaddr_in6 ipv6;
} Address;

static inline void os_net_address_ipv4_set_port(uint8_t *raw, short port_be16) {
    Address *self = (Address*)raw;
    self->ipv4.sin_port = port_be16;
}

static inline void os_net_address_ipv6_set_port(uint8_t *raw, short port_be16) {
    Address *self = (Address*)raw;
    self->ipv6.sin6_port = port_be16;
}

static inline short os_net_address_ipv4_get_port(uint8_t const *raw) {
    Address const *self = (Address const*)raw;
    return self->ipv4.sin_port;
}

static inline short os_net_address_ipv6_get_port(uint8_t const *raw) {
    Address const *self = (Address const*)raw;
    return self->ipv6.sin6_port;
}

static inline void os_net_address_ipv4_set_ip(uint8_t *raw, uint8_t const *ip) {
    Address *self = (Address*)raw;
    self->ipv4.sin_family = AF_INET;
    memcpy(&(self->ipv4.sin_addr.s_addr), ip, 4);
}

static inline void os_net_address_ipv6_set_ip(uint8_t *raw, uint8_t const *ip) {
    Address *self = (Address*)raw;
    self->ipv6.sin6_family = AF_INET6;
    memcpy(&(self->ipv6.sin6_addr.s6_addr), ip, 16);
}

static inline uint8_t const* os_net_address_ipv6_get_ip(uint8_t const *raw) {
    Address const *self = (Address const*)raw;
    return self->ipv6.sin6_addr.s6_addr;
}

static inline uint8_t const* os_net_address_ipv4_get_ip(uint8_t const *raw) {
    Address const *self = (Address const*)raw;
    return (uint8_t const*)&(self->ipv4.sin_addr.s_addr);
}

#endif
