#include <netinet/in.h>
#include <stdint.h>
#include <string.h>


// --- address

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



// ---  udp
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

static inline void os_net_udp_bind(err_Err *e, size_t et, net_address_Address const* addr, io_Context *ctx)
{

    switch (addr->type) {
        case net_address_Type_Ipv6:
            ctx->fd = socket(AF_INET6, SOCK_DGRAM, IPPROTO_UDP);
            break;
        case net_address_Type_Ipv4:
            ctx->fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
            break;
        default:
            break;
    }

    if (ctx->fd < 0) {
        err_fail_with_errno(e, et, __FILE__, "os_net_udp_open", __LINE__, "socket");
        return;
    }

    int r = bind(ctx->fd, (struct  sockaddr*)(&addr->os), sizeof(struct sockaddr_in6));
    if (r != 0) {
        err_fail_with_errno(e, et, __FILE__, "os_net_udp_open", __LINE__, "bind");
    }
}

static inline void os_net_udp_close(io_Context ctx)
{
    close(ctx.fd);
}

static inline void os_net_udp_make_async(err_Err *e, size_t et, io_Context const *ctx) {
    int flags = fcntl(ctx->fd, F_GETFL, 0);
    if (flags == -1) {
        err_fail_with_errno(e, et, __FILE__, "os_net_udp_make_async", __LINE__, "fcntl");
    }
    flags = flags | O_NONBLOCK;

    flags = fcntl(ctx->fd, F_SETFL, flags);
    if (flags == -1) {
        err_fail_with_errno(e, et, __FILE__, "os_net_udp_make_async", __LINE__, "fcntl");
    }
}




static inline io_Result os_net_udp_recvfrom(
        err_Err *e, size_t et,
        io_Context const *ctx,
        char * mem,
        size_t memlen,
        net_address_Address *addr,
        size_t * l
)
{
    unsigned alen = sizeof(struct sockaddr_in6);

    int r = recvfrom(
        ctx->fd,
        mem,
        memlen,
        0,
        (struct sockaddr*)addr->os,
        &alen
    );

    if (((struct sockaddr*)addr->os)->sa_family  == AF_INET) {
        addr->type = net_address_Type_Ipv4;
    } else if (((struct sockaddr*)addr->os)->sa_family  == AF_INET6) {
        addr->type = net_address_Type_Ipv6;
    }

    if (r < 0) {
        if (errno == EAGAIN) {
            return io_Result_Later;
        }
        err_fail_with_errno(e, et, __FILE__, "os_net_udp_recvfrom", __LINE__, "recvfrom");
        return io_Result_Error;
    }

    *l = (size_t)r;

    return io_Result_Ready;
}


static inline size_t os_net_udp_sendto(
        err_Err *e, size_t et,
        io_Context const *ctx,
        char const * mem,
        size_t memlen,
        net_address_Address const *addr
)
{
    unsigned alen = sizeof(struct sockaddr_in6);

    int r = sendto(
        ctx->fd,
        mem,
        memlen,
        0,
        (struct sockaddr const*)addr->os,
        alen
    );

    if (r < 0) {
        err_fail_with_errno(e, et, __FILE__, "os_net_udp_sendto", __LINE__, "recvfrom");
        return 0;
    }

    return (size_t)r;
}

