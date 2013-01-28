/* Stubs for writing basic values in the binary protocol */

#include "common_stubs.h"

/* Utility macros */

#define MK_ML_WRITER(NAME) \
  CAMLprim value ml_write_##NAME##_stub(value v_buf, value v_pos, value v_v) \
  { \
    struct caml_ba_array *buf = Caml_ba_array_val(v_buf); \
    char *start = buf->data; \
    long pos = Long_val(v_pos); \
    char *sptr = start + pos; \
    char *eptr = start + *buf->dim; \
    if (unlikely(pos < 0)) caml_array_bound_error(); \
    sptr = (char *) write_##NAME##_stub(sptr, eptr, v_v); \
    return Val_long(sptr - start); \
  }


/* Writing OCaml integers */

static inline void do_write_small_int(char *sptr, char n)
{
  *sptr = n;
}

static inline value write_small_int(char *sptr, char *eptr, char n)
{
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  do_write_small_int(sptr, n);
  return (value) (sptr + 1);
}

static inline void do_write_neg_int8(char *sptr, char n)
{
  *sptr++ = CODE_NEG_INT8;
  *(char *) sptr = n;
}

static inline value write_neg_int8(char *sptr, char *eptr, char n)
{
  char *next = sptr + 2;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  do_write_neg_int8(sptr, n);
  return (value) next;
}

static inline void do_write_int16(char *sptr, short n)
{
  *sptr++ = CODE_INT16;
  le16enc(sptr, n);
}

static inline value write_int16(char *sptr, char *eptr, short n)
{
  char *next = sptr + 3;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  do_write_int16(sptr, n);
  return (value) next;
}

static inline void do_write_int32(char *sptr, int n)
{
  *sptr++ = CODE_INT32;
  le32enc(sptr, n);
}

static inline value write_int32(char *sptr, char *eptr, int n)
{
  char *next = sptr + 5;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  do_write_int32(sptr, n);
  return (value) next;
}

#ifdef ARCH_SIXTYFOUR
static inline void do_write_int64(char *sptr, long n)
{
  *sptr++ = CODE_INT64;
  le64enc(sptr, n);
}

static inline value write_int64(char *sptr, char *eptr, long n)
{
  char *next = sptr + 9;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  do_write_int64(sptr, n);
  return (value) next;
}
#endif

static inline value write_int_nat0(char *sptr, char *eptr, unsigned long n)
{
  if (likely(n < 0x00000080ul)) return write_small_int(sptr, eptr, (char) n);
  if (likely(n < 0x00008000ul)) return write_int16(sptr, eptr, (short) n);
#ifdef ARCH_SIXTYFOUR
  if (unlikely(n >= 0x80000000ul)) return write_int64(sptr, eptr, (long) n);
#endif
  return write_int32(sptr, eptr, (int) n);
}

static inline value write_int_negative(char *sptr, char *eptr, long n)
{
  if (likely(n >= -0x00000080l)) return write_neg_int8(sptr, eptr, (char) n);
  if (likely(n >= -0x00008000l)) return write_int16(sptr, eptr, (short) n);
#ifdef ARCH_SIXTYFOUR
  if (unlikely(n < -0x80000000l)) return write_int64(sptr, eptr, (long) n);
#endif
  return write_int32(sptr, eptr, (int) n);
}

static inline value write_int(char *sptr, char *eptr, long n)
{
  /* Positive numbers (including zero) */
  if (likely(n >= 0)) return write_int_nat0(sptr, eptr, (unsigned long) n);
  /* Negative numbers */
  return write_int_negative(sptr, eptr, n);
}

CAMLprim value write_int_stub(char *sptr, char *eptr, value v_n)
{
  return write_int(sptr, eptr, Long_val(v_n));
}


/* Writing natural numbers (including zero) */

static inline value write_nat0(char *sptr, char *eptr, unsigned long n)
{
  if (likely(n < 0x00000080ul)) return write_small_int(sptr, eptr, (char) n);
  if (likely(n < 0x00010000ul)) return write_int16(sptr, eptr, (short) n);
#ifdef ARCH_SIXTYFOUR
  if (unlikely(n >= 0x100000000UL)) return write_int64(sptr, eptr, (long) n);
#endif
  return write_int32(sptr, eptr, (int) n);
}

CAMLprim value write_nat0_stub(char *sptr, char *eptr, value v_n)
{
  return write_nat0(sptr, eptr, (unsigned long) Long_val(v_n));
}


/* Writing 32bit integers */

CAMLprim value write_int32_stub(char *sptr, char *eptr, value v_n)
{
  return write_int(sptr, eptr, Int32_val(v_n));
}


/* Writing 64bit integers */

#ifndef ARCH_SIXTYFOUR
static inline value write_int64_type(char *sptr, char *eptr, int64 n)
{
  char *next = sptr + 9;
  int32 lower, upper;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr++ = CODE_INT64;
  I64_split(n, upper, lower);
  le32enc(sptr, lower);
  le32enc(sptr + 4, upper);
  return (value) next;
}

static inline value write_int64_type_nat0(char *sptr, char *eptr, int64 n)
{
  if (likely((I64_compare(n, I64_literal(0, 0x00000080l)) < 0)))
    return write_small_int(sptr, eptr, (char) I64_to_int32(n));
  if (likely(I64_compare(n, I64_literal(0, 0x00008000l)) < 0))
    return write_int16(sptr, eptr, (short) I64_to_int32(n));
  if (unlikely(I64_compare(n, I64_literal(0, 0x80000000l)) >= 0))
    return write_int64_type(sptr, eptr, n);
  return write_int32(sptr, eptr, (int) I64_to_int32(n));
}

static inline value write_int64_type_negative(char *sptr, char *eptr, int64 n)
{
  if (likely(I64_compare(n, I64_literal(0xFFFFFFFF, -0x00000080l)) >= 0))
    return write_neg_int8(sptr, eptr, (char) I64_to_int32(n));
  if (likely(I64_compare(n, I64_literal(0xFFFFFFFF, -0x00008000l)) >= 0))
    return write_int16(sptr, eptr, (short) I64_to_int32(n));
  if (unlikely(I64_compare(n, I64_literal(0xFFFFFFFF, -0x80000000l)) < 0))
    return write_int64_type(sptr, eptr, n);
  return write_int32(sptr, eptr, I64_to_int32(n));
}
#endif

CAMLprim value write_int64_stub(char *sptr, char *eptr, value v_n)
{
  int64 n = Int64_val(v_n);
#ifdef ARCH_SIXTYFOUR
  return write_int(sptr, eptr, n);
#else
  if (likely(! I64_is_negative(n))) return write_int64_type_nat0(sptr, eptr, n);
  return write_int64_type_negative(sptr, eptr, n);
#endif
}


/* Writing nativeints */

CAMLprim value write_nativeint_stub(char *sptr, char *eptr, value v_n)
{
  long n = Nativeint_val(v_n);
  return write_int(sptr, eptr, n);
}


/* Writing booleans and characters */

CAMLprim value write_small_int_stub(char *sptr, char *eptr, value v_n)
{
  return write_small_int(sptr, eptr, (char) Int_val(v_n));
}


/* Writing strings */

CAMLprim value write_string_stub(char *sptr, char *eptr, value v_str)
{
  char *str = String_val(v_str);
  unsigned long len = caml_string_length(v_str);
  char *next, *dst;
  if (likely(len < 0x00000014ul)) {
    /* Speedup for copying small strings */
    dst = sptr + 1;
    next = dst + len;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    *sptr = (char) len;
    if (likely(len-- > 0))
      do dst[len] = str[len]; while (likely(len-- != 0));
    return (value) next;
  }
  if (likely(len < 0x00000080ul)) {
    dst = sptr + 1;
    next = dst + len;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    *sptr = (char) len;
    memcpy(dst, str, (size_t) len);
    return (value) next;
  }
  if (likely(len < 0x00010000ul)) {
    dst = sptr + 3;
    next = dst + len;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int16(sptr, (short) len);
    memcpy(dst, str, (size_t) len);
    return (value) next;
  }
#ifdef ARCH_SIXTYFOUR
  if (unlikely(len >= 0x100000000UL)) {
    dst = sptr + 9;
    next = dst + len;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int64(sptr, len);
    memcpy(dst, str, (size_t) len);
    return (value) next;
  }
#endif
  else {
    dst = sptr + 5;
    next = dst + len;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int32(sptr, (unsigned int) len);
    memcpy(dst, str, (size_t) len);
    return (value) next;
  }
}


/* Writing floats and float arrays */

CAMLprim inline value write_float_stub(char *sptr, char *eptr, value v_n)
{
  char *next = sptr + sizeof(double);
  double n = Double_val(v_n);
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  memcpy(sptr, &n, sizeof(double));
  return (value) next;
}

MK_ML_WRITER(float)

CAMLprim inline value write_float_array_stub(char *sptr, char *eptr, value v_ar)
{
  unsigned long wlen = Wosize_val(v_ar);
  double *src = (double *) v_ar;
  unsigned long len = wlen / Double_wosize;
  unsigned long tot_size = len * sizeof(double);
  char *next, *dst;
  if (likely(len < 0x00000080ul)) {
    dst = (char *) sptr + 1;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    *(char *) sptr = (char) len;
    memcpy(dst, src, (size_t) tot_size);
    return (value) next;
  }
  if (likely(len < 0x00010000ul)) {
    dst = (char *) sptr + 3;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int16(sptr, (short) len);
    memcpy(dst, src, (size_t) tot_size);
    return (value) next;
  }
#ifdef ARCH_SIXTYFOUR
  if (likely(len >= 0x100000000UL)) {
    dst = (char *) sptr + 9;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int64(sptr, len);
    memcpy(dst, src, (size_t) tot_size);
    return (value) next;
  }
#endif
  else {
    dst = (char *) sptr + 5;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int32(sptr, (int) len);
    memcpy(dst, src, (size_t) tot_size);
    return (value) next;
  }
}

MK_ML_WRITER(float_array)


/* Writing polymorphic variants */

CAMLprim inline value write_variant_tag_stub(
  char *sptr, char *eptr, value v_tag)
{
  char *next = sptr + 4;
  int tag;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  tag = (int) (Is_block(v_tag) ? Field(v_tag, 0) : v_tag);
  le32enc(sptr, tag);
  return (value) next;
}

MK_ML_WRITER(variant_tag)


/* Writing raw strings */

CAMLprim inline value write_raw_string_stub(
  char *sptr, char *eptr, value v_str, value v_pos, value v_len)
{
  size_t pos = (size_t) Long_val(v_pos), len = (size_t) Long_val(v_len);
  char *next = sptr + len;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  memcpy(sptr, String_val(v_str) + pos, len);
  return (value) next;
}


/* Writing bigarrays */

static inline value write_area(
  value v,
  void *sptr, char *eptr, void *src, unsigned long len, size_t tot_size)
{
  char *next, *dst;
  if (likely(len < 0x00000080ul)) {
    dst = ((char *) sptr) + 1;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    *(char *) sptr = (char) len;
    memcpy(dst, src, tot_size);
    return (value) next;
  }
  if (likely(len < 0x00010000ul)) {
    dst = (char *) sptr + 3;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    do_write_int16(sptr, (short) len);
    memcpy(dst, src, tot_size);
    return (value) next;
  }
#ifdef ARCH_SIXTYFOUR
  if (unlikely(len >= 0x100000000UL)) {
    dst = (char *) sptr + 9;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    Begin_roots1(v);
    caml_enter_blocking_section();
      do_write_int64(sptr, len);
      memcpy(dst, src, tot_size);
    caml_leave_blocking_section();
    End_roots();
    return (value) next;
  }
#endif
  else {
    dst = (char *) sptr + 5;
    next = dst + tot_size;
    if (unlikely(next > eptr))
      caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    Begin_roots1(v);
    caml_enter_blocking_section();
      do_write_int32(sptr, (int) len);
      memcpy(dst, src, tot_size);
    caml_leave_blocking_section();
    End_roots();
    return (value) next;
  }
}

#define MK_BA1_WRITER(NAME, TYPE) \
  CAMLprim inline value \
  write_##NAME##_stub(char *sptr, char *eptr, value v_v) \
  { \
    struct caml_ba_array *vec = Caml_ba_array_val(v_v); \
    unsigned long len = (unsigned long) *vec->dim; \
    return \
      write_area( \
        v_v, sptr, eptr, vec->data, len, (size_t) len * sizeof(TYPE)); \
  } \
  \
  MK_ML_WRITER(NAME)

MK_BA1_WRITER(bigstring, char)

#define MK_VEC_MAT_WRITERS(NAME, TYPE) \
  MK_BA1_WRITER(NAME##_vec, TYPE) \
  \
  CAMLprim inline value \
  write_##NAME##_mat_stub(char *sptr, char *eptr, value v_m) \
  { \
    struct caml_ba_array *mat = Caml_ba_array_val(v_m); \
    unsigned long dim1 = (unsigned long) mat->dim[0]; \
    unsigned long dim2 = (unsigned long) mat->dim[1]; \
    unsigned long size = dim1 * dim2; \
    sptr = (char *) write_nat0(sptr, eptr, dim1); \
    return \
      write_area( \
        v_m, sptr, eptr, mat->data, dim2, (size_t) size * sizeof(TYPE)); \
  } \
  MK_ML_WRITER(NAME##_mat)

MK_VEC_MAT_WRITERS(float32, float)
MK_VEC_MAT_WRITERS(float64, double)


/* Writing bits */

CAMLprim value write_int_8bit_stub(char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 1;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *(char *) sptr = (char) Int_val(v_n);
  return (value) next;
}
MK_ML_WRITER(int_8bit)

CAMLprim value write_int_16bit_stub(char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 2;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  le16enc(sptr, Int_val(v_n));
  return (value) next;
}
MK_ML_WRITER(int_16bit)

CAMLprim value write_int_32bit_stub(char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 4;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  le32enc(sptr, Int_val(v_n));
  return (value) next;
}
MK_ML_WRITER(int_32bit)

CAMLprim value write_int_64bit_stub(char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 8;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  le64enc(sptr, Long_val(v_n));
#else
  {
    long n = Long_val(v_n);
    long tmp = (n < 0) ? 0xFFFFFFFFl : 0l;
    le32enc(sptr, n);
    memcpy(sptr + 4, &tmp, sizeof(long));
  }
#endif
  return (value) next;
}
MK_ML_WRITER(int_64bit)

CAMLprim inline value write_int64_bits_stub(char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 8;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  le64enc(sptr, Int64_val(v_n));
#else
  {
    int64 n = Int64_val(v_n);
    unsigned int lower, upper;
    I64_split(n, upper, lower);
    le32enc(sptr, lower);
    le32enc(sptr + 4, upper);
  }
#endif
  return (value) next;
}
MK_ML_WRITER(int64_bits)

CAMLprim inline value write_network16_int_stub(
  char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 2;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  be16enc(sptr, Int_val(v_n));
  return (value) next;
}
MK_ML_WRITER(network16_int)

CAMLprim inline value write_network32_int_stub(
  char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 4;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  be32enc(sptr, Int_val(v_n));
  return (value) next;
}
MK_ML_WRITER(network32_int)

CAMLprim inline value write_network32_int32_stub(
  char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 4;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  be32enc(sptr, Int32_val(v_n));
  return (value) next;
}
MK_ML_WRITER(network32_int32)

CAMLprim inline value write_network64_int_stub(
  char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 8;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  be64enc(sptr, Long_val(v_n));
#else /* 32bit */
  {
    unsigned int tmp = 0;
    memcpy(sptr, &tmp, 4);
    be32enc(sptr + 4, Long_val(v_n));
  }
#endif
  return (value) next;
}
MK_ML_WRITER(network64_int)

CAMLprim inline value write_network64_int64_stub(
  char *sptr, char *eptr, value v_n)
{
  char *next = sptr + 8;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  be64enc(sptr, Int64_val(v_n));
#else /* 32bit */
  {
    int64 n = Int64_val(v_n);
    uint32_t lower, upper;
    I64_split(n, upper, lower);
    be32enc(sptr, upper);
    be32enc(sptr + 4, lower);
  }
#endif
  return (value) next;
}
MK_ML_WRITER(network64_int64)
