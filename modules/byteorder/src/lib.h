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
        #error "compiler says byte order is both little and big endian"
    #endif
#else
    #if BYTE_ORDER == BIG_ENDIAN
        
    #else
        #error "compiler says byte order is neither little nor big endian"
    #endif
#endif

#if BYTE_ORDER == LITTLE_ENDIAN && BYTE_ORDER == BIG_ENDIAN
#endif
