/* Common binary protocol definitions */

#ifndef COMMON_STUBS_H
#define COMMON_STUBS_H

#define _BSD_SOURCE
#include <sys/types.h>

#include <string.h>
#include <arpa/inet.h>

#include <caml/config.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/signals.h>

#ifdef ARCH_INT64_TYPE
#include "int64_native.h"
#else
#include "int64_emul.h"
#endif


/* Endianness- and alignment-independent integer marshalling functions */

#define le8dec(x) (*x)

#ifndef le16dec  /* FreeBSD marshalling functions not available */

#ifdef OS_DARWIN
/* Darwin platform */
#include <libkern/OSByteOrder.h>

#define le16dec(ptr) OSReadLittleInt16(ptr, 0)
#define le32dec(ptr) OSReadLittleInt32(ptr, 0)
#define le64dec(ptr) OSReadLittleInt64(ptr, 0)

#define be16dec(ptr) OSReadBigInt16(ptr, 0)
#define be32dec(ptr) OSReadBigInt32(ptr, 0)
#define be64dec(ptr) OSReadBigInt64(ptr, 0)

#define le16enc(ptr, n) OSWriteLittleInt16(ptr, 0, n)
#define le32enc(ptr, n) OSWriteLittleInt32(ptr, 0, n)
#define le64enc(ptr, n) OSWriteLittleInt64(ptr, 0, n)

#define be16enc(ptr, n) OSWriteBigInt16(ptr, 0, n)
#define be32enc(ptr, n) OSWriteBigInt32(ptr, 0, n)
#define be64enc(ptr, n) OSWriteBigInt64(ptr, 0, n)

#else

/* Non-Darwin platform */
/* Emulate with memcpy and endianness conversion functions */

/* Define BSD conversion functions if undefined */
#ifndef le16toh

/* Define portable byte swapping if undefined */
/* WARNING: will not work on mixed endian machines! */
/* WARNING: these functions must be defined by the system in the above case */
#ifndef bswap_16
#define bswap_16(value) \
  (((uint16_t) ((value) & 0xff) << 8) | ((uint16_t) (value) >> 8))
#endif

#ifndef bswap_32
#define bswap_32(value) \
  (((uint32_t) bswap_16(((value) & 0xffff)) << 16) | \
    (uint32_t) bswap_16(((value) >> 16)))
#endif

#ifndef bswap_64
#define bswap_64(value) \
  (((uint64_t) bswap_32(((value) & 0xffffffff)) << 32) | \
    (uint64_t) bswap_32(((value) >> 32)))
#endif

#if __BYTE_ORDER == __LITTLE_ENDIAN
/* Little-endian architecture */
#define htobe16(x) bswap_16 (x)
#define htole16(x) (x)
#define be16toh(x) bswap_16 (x)
#define le16toh(x) (x)

#define htobe32(x) bswap_32 (x)
#define htole32(x) (x)
#define be32toh(x) bswap_32 (x)
#define le32toh(x) (x)

#define htobe64(x) bswap_64 (x)
#define htole64(x) (x)
#define be64toh(x) bswap_64 (x)
#define le64toh(x) (x)
#else
/* Big-endian architecture */
#define htobe16(x) (x)
#define htole16(x) bswap_16 (x)
#define be16toh(x) (x)
#define le16toh(x) bswap_16 (x)

#define htobe32(x) (x)
#define htole32(x) bswap_32 (x)
#define be32toh(x) (x)
#define le32toh(x) bswap_32 (x)

#define htobe64(x) (x)
#define htole64(x) bswap_64 (x)
#define be64toh(x) (x)
#define le64toh(x) bswap_64 (x)
#endif  /* byte order */

#endif  /* BSD conversion functions */

#define le16dec(ptr) \
  (__extension__ ({ uint16_t __n; memcpy(&__n, ptr, 2); le16toh(__n); }))
#define le32dec(ptr) \
  (__extension__ ({ uint32_t __n; memcpy(&__n, ptr, 4); le32toh(__n); }))
#define le64dec(ptr) \
  (__extension__ ({ uint64_t __n; memcpy(&__n, ptr, 8); le64toh(__n); }))

#define be16dec(ptr) \
  (__extension__ ({ uint16_t __n; memcpy(&__n, ptr, 2); be16toh(__n); }))
#define be32dec(ptr) \
  (__extension__ ({ uint32_t __n; memcpy(&__n, ptr, 4); be32toh(__n); }))
#define be64dec(ptr) \
  (__extension__ ({ uint64_t __n; memcpy(&__n, ptr, 8); be64toh(__n); }))

#define le16enc(ptr, n) \
  (__extension__ ({ uint16_t __n = htole16(n); memcpy(ptr, &__n, 2); }))
#define le32enc(ptr, n) \
  (__extension__ ({ uint32_t __n = htole32(n); memcpy(ptr, &__n, 4); }))
#define le64enc(ptr, n) \
  (__extension__ ({ uint64_t __n = htole64(n); memcpy(ptr, &__n, 8); }))

#define be16enc(ptr, n) \
  (__extension__ ({ uint16_t __n = htobe16(n); memcpy(ptr, &__n, 2); }))
#define be32enc(ptr, n) \
  (__extension__ ({ uint32_t __n = htobe32(n); memcpy(ptr, &__n, 4); }))
#define be64enc(ptr, n) \
  (__extension__ ({ uint64_t __n = htobe64(n); memcpy(ptr, &__n, 8); }))

#endif  /* OS_DARWIN */

#endif  /* FreeBSD marshalling functions */


/* Bin-prot integer codes */

#define CODE_NEG_INT8 (char) -1
#define CODE_INT16 (char) -2
#define CODE_INT32 (char) -3
#define CODE_INT64 (char) -4


/* Buffer short exception */

extern value *v_bin_prot_exc_Buffer_short;


/* Compiler pragmas and inlining */

/* Forget any previous definition of inlining, it may not be what we mean */
#ifdef inline
# undef inline
#endif

/* The semantics of "inline" in C99 is not what we intend so just drop it */
#if defined(__STDC__) && __STDC__ && \
    defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
# define inline
#endif

#if defined(__GNUC__) && __GNUC__ >= 3
# ifndef inline
#   define inline inline __attribute__ ((always_inline))
# endif
# ifndef __pure
#   define __pure __attribute__ ((pure))
# endif
# ifndef __const
#   define __const __attribute__ ((const))
# endif
# ifndef __malloc
#   define __malloc __attribute__ ((malloc))
# endif
# ifndef __unused
#   define __unused __attribute__ ((unused))
# endif
# ifndef __likely
#   define likely(x) __builtin_expect (!!(x), 1)
# endif
# ifndef __unlikely
#   define unlikely(x) __builtin_expect (!!(x), 0)
# endif
#else
  /* Non-GNU compilers should always ignore "inline" no matter the C-standard */
# ifndef inline
#   define inline
# endif
# ifndef __pure
#   define __pure
# endif
# ifndef  __const
#   define __const
# endif
# ifndef  __malloc
#   define __malloc
# endif
# ifndef  __unused
#   define __unused
# endif
# ifndef  __likely
#   define likely(x) (x)
# endif
# ifndef  __unlikely
#   define unlikely(x) (x)
# endif
#endif

#endif /* COMMON_STUBS_H */
