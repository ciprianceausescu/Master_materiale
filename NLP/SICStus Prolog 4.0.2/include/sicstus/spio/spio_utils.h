#ifndef SPIO_UTILS_H_INCLUDED
#define SPIO_UTILS_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include <stddef.h>             /* offsetof */
#include <string.h>             /* strcmp */


#define SPIO_SYMCAT_(A, B) A ## B
#define SPIO_SYMCAT(A, B) SPIO_SYMCAT_(A, B)


#define SPIO_WRONGENDIAN_ASSIGN16(TO, FROM) do{         \
   spio_t_uint16 tmp_from = (FROM);                     \
   (TO) = (((tmp_from & 0xFF) << 8) | (tmp_from >> 8)); \
}while(0)

#define SPIO_WRONGENDIAN_ASSIGN32(TO, FROM) do{ \
   spio_t_uint32 tmp_from = (FROM);             \
   spio_t_uint32 tmp_to;                        \
   tmp_to = (tmp_from & 0xFF);                  \
   tmp_to <<= 8;                                \
   tmp_from >>=8;                               \
   tmp_to |= (tmp_from & 0xFF);                 \
   tmp_to <<= 8;                                \
   tmp_from >>=8;                               \
   tmp_to |= (tmp_from & 0xFF);                 \
   tmp_to <<= 8;                                \
   tmp_from >>=8;                               \
   SPIO_ASSERT(tmp_from == (tmp_from & 0xFF));  \
   tmp_to |= (tmp_from);                        \
   (TO) = tmp_to;                               \
}while(0)

#define SPIO_RIGHTENDIAN_ASSIGN16(TO, FROM) do{ \
   spio_t_uint16 tmp_from = (FROM);             \
   (TO) = tmp_from;                             \
}while(0)

#define SPIO_RIGHTENDIAN_ASSIGN32(TO, FROM) do{ \
   spio_t_uint32 tmp_from = (FROM);             \
   (TO) = tmp_from;                             \
}while(0)


#ifdef SPIO_BIGENDIAN
#if SPIO_BIGENDIAN

#define SPIO_BIGENDIAN_ASSIGN16(TO, FROM) SPIO_RIGHTENDIAN_ASSIGN16((TO), (FROM))
#define SPIO_LITTLEENDIAN_ASSIGN16(TO, FROM) SPIO_WRONGENDIAN_ASSIGN16((TO),(FROM))

#define SPIO_BIGENDIAN_ASSIGN32(TO, FROM) SPIO_RIGHTENDIAN_ASSIGN32((TO), (FROM))
#define SPIO_LITTLEENDIAN_ASSIGN32(TO, FROM) SPIO_WRONGENDIAN_ASSIGN32((TO),(FROM))

#else  /* !SPIO_BIGENDIAN */

#define SPIO_BIGENDIAN_ASSIGN16(TO, FROM) SPIO_WRONGENDIAN_ASSIGN16((TO),(FROM))
#define SPIO_LITTLEENDIAN_ASSIGN16(TO, FROM) SPIO_RIGHTENDIAN_ASSIGN16((TO),(FROM))

#define SPIO_BIGENDIAN_ASSIGN32(TO, FROM) SPIO_WRONGENDIAN_ASSIGN32((TO),(FROM))
#define SPIO_LITTLEENDIAN_ASSIGN32(TO, FROM) SPIO_RIGHTENDIAN_ASSIGN32((TO),(FROM))

#endif  /* !SPIO_BIGENDIAN */
#endif  /* defined SPIO_BIGENDIAN */



#define SPIO_SET_MASK(VAR, BITMASK) ((VAR) |= (BITMASK))
#define SPIO_CLEAR_MASK(VAR, BITMASK) ((VAR) &= ~(BITMASK))
#define SPIO_MASK_IS_SET(VAR, BITMASK) (!!(((VAR) & (BITMASK)) == (BITMASK)))
#define SPIO_MASK_IS_CLEAR(VAR, BITMASK) (!!(((VAR) & (BITMASK)) == 0))
#define SPIO_BIT(N) (((spio_t_bits)0x1) << (N))
#define SPIO_NEXT_BIT(BITMASK) (BITMASK << 1)

#define SPIO_TRANSLATE_MASK(FROM_VAR, FROM_MASK, TO_VAR, TO_MASK) \
  do{ \
   if (SPIO_MASK_IS_SET((FROM_VAR), (FROM_MASK))) SPIO_SET_MASK((TO_VAR), (TO_MASK)); \
  } while (0)

#define SPIO_BITMASK8(B7,B6,B5,B4,B3,B2,B1,B0) (((B7)<<7)|((B6)<<6)|((B5)<<5)|((B4)<<4)|((B3)<<3)|((B2)<<2)|((B1)<<1)|((B0)<<0))


#define SPIO_FALSE 0
#define SPIO_TRUE (!SPIO_FALSE)

#define spio_streq(S1, S2) (strcmp((S1),(S2))==0)

#define SPIO_MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define SPIO_MAX(X,Y) ((X) > (Y) ? (X) : (Y))

#define SPIO_FIELD_OFFSET(TYPE, FIELD_NAME) offsetof(TYPE, FIELD_NAME)
/* given the address FIELD_ADDRESS of a struct field FIELD_NAME, return the address of the containing struct of type CONTAINING_TYPE. */
#define SPIO_CONTAINING_STRUCT(CONTAINING_TYPE, FIELD_NAME, FIELD_ADDRESS) \
  ((CONTAINING_TYPE*)(((char *)(FIELD_ADDRESS))-SPIO_FIELD_OFFSET(CONTAINING_TYPE, FIELD_NAME)))


#define SPIO_ROUND_DOWN(X, GRANULARITY) (((X)/(GRANULARITY)) * (GRANULARITY))
#define SPIO_ROUND_UP(X, GRANULARITY) SPIO_ROUND_DOWN((X)+((GRANULARITY)-1), (GRANULARITY))


extern spio_t_error_code spio_get_arg(spio_t_arglist args, char const *key, spio_t_variant_type type, void const **value);
extern spio_t_error_code spio_get_arg_variant(spio_t_arglist args, char const *key, spio_t_variant const **value);
extern spio_t_error_code spio_get_arg_string(spio_t_arglist args, char const *key, char const **value);

typedef spio_t_error_code spio_t_group_predicate(void *key, void *value, void *user_data);
/* split a vector of keys with associated values into a prefix satisfying a predicate */
spio_t_error_code spio_group_by_pred(spio_t_group_predicate *pred, void* keys[], void *values[], void *user_data, size_t *pnitems, spio_t_bits options);

extern spio_t_error_code spio_double_to_timespec(double x, spio_t_timespec *ptimespec);

#endif  /* SPIO_UTILS_H_INCLUDED */
