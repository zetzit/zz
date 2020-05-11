#if !defined(BYTE_ORDER)
    #if defined(__BYTE_ORDER__)
        #define BYTE_ORDER __BYTE_ORDER__
    #elif defined(__BYTE_ORDER)
        #define BYTE_ORDER __BYTE_ORDER
    #else
        #error cannot find a definition of BYTE_ORDER
    #endif
#endif

#if !defined(LITTLE_ENDIAN)
    #if defined(__LITTLE_ENDIAN)
        #define LITTLE_ENDIAN __LITTLE_ENDIAN
    #elif defined(__ORDER_LITTLE_ENDIAN__)
        #define LITTLE_ENDIAN __ORDER_LITTLE_ENDIAN__
    #else
        #error cannot find a definition of LITTLE_ENDIAN
    #endif
#endif

#if !defined(BIG_ENDIAN)
    #if defined(__BIG_ENDIAN)
        #define BIG_ENDIAN __BIG_ENDIAN
    #elif defined(__ORDER_BIG_ENDIAN__)
        #define BIG_ENDIAN __ORDER_BIG_ENDIAN__
    #else
        #error cannot find a definition of BIG_ENDIAN
    #endif
#endif

#if BYTE_ORDER == LITTLE_ENDIAN
    #if BYTE_ORDER == BIG_ENDIAN
        #error "compiler claims byte order is both little and big endian"
    #endif
#else
    #if BYTE_ORDER == BIG_ENDIAN

    #else
        #error "compiler claims byte order is neither little nor big endian"
    #endif
#endif

inline static uint16_t bswap_16(uint16_t x)
{
    return x<<8 | x>>8;
}

inline static uint32_t bswap_32(uint32_t x)
{
    return x>>24 | ( (x >> 8) & 0xff00) | ((x<<8) & 0xff0000) | x<<24;
}

inline static uint64_t bswap_64(uint64_t x)
{
    return (bswap_32(x) + ((uint64_t)0)) <<32 | bswap_32(x>>32);
}
