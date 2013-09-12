/* Stubs for reading basic values in the binary protocol */

#include "common_stubs.h"

/* Initialisation */

static value *v_exc_Error = NULL;
static value *v_exc_Read_error = NULL;

CAMLprim value bin_prot_unsafe_read_c_init_stub(value __unused v_unit)
{
  v_exc_Error = caml_named_value("Bin_prot.Unsafe_read_c.Error");
  return Val_unit;
}

CAMLprim value bin_prot_read_ml_init_stub(value __unused v_unit)
{
  v_exc_Read_error = caml_named_value("Bin_prot.Common.Read_error");
  return Val_unit;
}

/* Raising errors */

/* Constant values come from the order of variants in common.ml */
#define READ_ERROR_NEG_INT8 0
#define READ_ERROR_INT_CODE 1
#define READ_ERROR_INT_OVERFLOW 2
#define READ_ERROR_NAT0_CODE 3
#define READ_ERROR_NAT0_OVERFLOW 4
#define READ_ERROR_INT32_CODE 5
#define READ_ERROR_INT64_CODE 6
#define READ_ERROR_NATIVEINT_CODE 7
#define READ_ERROR_UNIT_CODE 8
#define READ_ERROR_BOOL_CODE 9
#define READ_ERROR_OPTION_CODE 10
#define READ_ERROR_STRING_TOO_LONG 11
#define READ_ERROR_VARIANT_TAG 12
#define READ_ERROR_ARRAY_TOO_LONG 13

static inline value raise_Error(int loc) Noreturn;

static inline value raise_Error(int loc)
{
  caml_raise_with_arg(*v_exc_Error, Val_int(loc));
}

static inline void raise_Read_error(int loc, unsigned long pos) Noreturn;

static inline void raise_Read_error(int loc, unsigned long pos)
{
  value v_exc = caml_alloc_small(3, 0);
  Field(v_exc, 0) = *v_exc_Read_error;
  Field(v_exc, 1) = Val_int(loc);
  Field(v_exc, 2) = Val_long(pos);
  caml_raise(v_exc);
}


/* Utility macros */

#define MK_GEN_SAFE_READ(NAME, SIZE, TYPE, LEN, CHECK) \
  static inline TYPE safe_read_##NAME##SIZE(char **sptr_ptr, char *eptr) \
  { \
    char *sptr = *sptr_ptr; \
    char *next = sptr + LEN; \
    TYPE n; \
    if (unlikely(next > eptr)) \
      caml_raise_constant(*v_bin_prot_exc_Buffer_short); \
    n = le##SIZE##dec(sptr); \
    CHECK \
    *sptr_ptr = next; \
    return n; \
  }

#define MK_SAFE_READ(SIZE, TYPE, LEN, CHECK) \
  MK_GEN_SAFE_READ(int, SIZE, TYPE, LEN, CHECK)

#define MK_GEN_SAFE_NAT0_READ(PREF, SIZE, TYPE, LEN, CHECK) \
  MK_GEN_SAFE_READ(PREF##nat0_, SIZE, unsigned TYPE, LEN, CHECK)

#define MK_SAFE_NAT0_READ(SIZE, TYPE, LEN, CHECK) \
  MK_GEN_SAFE_READ(nat0_, SIZE, unsigned TYPE, LEN, CHECK)

#define MK_ML_READER(NAME) \
  CAMLprim value ml_read_##NAME##_stub(value v_buf, value v_pos_ref) \
  { \
    CAMLparam2(v_buf, v_pos_ref); \
      struct caml_ba_array *buf = Caml_ba_array_val(v_buf); \
      char *start = buf->data; \
      long pos = Long_val(Field(v_pos_ref, 0)); \
      char *sptr = start + pos; \
      char **sptr_ptr = &sptr; \
      char *eptr = start + *buf->dim; \
      value v_res; \
      if (unlikely(pos < 0)) caml_array_bound_error(); \
      v_res = read_##NAME##_stub(sptr_ptr, eptr); \
      Field(v_pos_ref, 0) = Val_long(sptr - start); \
    CAMLreturn(v_res); \
  }


/* Reading OCaml integers */

MK_GEN_SAFE_READ(neg_int, 8, char, 1,
  if (unlikely(n >= 0)) {
    *sptr_ptr = sptr - 1;
    raise_Error(READ_ERROR_NEG_INT8);
  })

MK_SAFE_READ(16, short, 2, {})

#ifdef ARCH_SIXTYFOUR
  MK_SAFE_READ(32, int, 4, {})
#else
  MK_GEN_SAFE_READ(int, 32, int, 4,
    if (unlikely(n < -0x40000000l || n > 0x3FFFFFFFl)) {
      *sptr_ptr = sptr - 1;
      raise_Error(READ_ERROR_INT_OVERFLOW);
    })
  MK_GEN_SAFE_READ(nocheck_int, 32, int, 4, {})
#endif

#ifdef ARCH_SIXTYFOUR
  MK_SAFE_READ(64, long, 8,
    if (unlikely(n < -0x4000000000000000L || n > 0x3FFFFFFFFFFFFFFFL)) {
      *sptr_ptr = sptr - 1;
      raise_Error(READ_ERROR_INT_OVERFLOW);
    })
  MK_GEN_SAFE_READ(nocheck_int, 64, long, 8, {})
#endif

static inline long read_int(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  int code;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  code = *sptr;
  *sptr_ptr = sptr + 1;
  if (likely(code >= 0)) return code;
  if (likely(code == CODE_INT16)) return safe_read_int16(sptr_ptr, eptr);
  if (likely(code == CODE_NEG_INT8)) return safe_read_neg_int8(sptr_ptr, eptr);
  if (likely(code == CODE_INT32)) return safe_read_int32(sptr_ptr, eptr);
#ifdef ARCH_SIXTYFOUR
  if (likely(code == CODE_INT64)) return safe_read_int64(sptr_ptr, eptr);
#endif
  *sptr_ptr = sptr;
  raise_Error(READ_ERROR_INT_CODE);
}

CAMLprim value read_int_stub(char **sptr_ptr, char *eptr)
{
  return Val_long(read_int(sptr_ptr, eptr));
}


/* Non-negative OCaml integers */

MK_SAFE_NAT0_READ(16, short, 2, {})

#ifdef ARCH_SIXTYFOUR
  MK_SAFE_NAT0_READ(32, int, 4, {})
#else
  MK_SAFE_NAT0_READ(32, int, 4,
    if (unlikely(n > 0x3FFFFFFFl)) {
      *sptr_ptr = sptr - 1;
      raise_Error(READ_ERROR_NAT0_OVERFLOW);
    })
  MK_GEN_SAFE_NAT0_READ(nocheck, 32, int, 4, {})
#endif

#ifdef ARCH_SIXTYFOUR
  MK_SAFE_NAT0_READ(64, long, 8,
    if (unlikely(n > 0x3FFFFFFFFFFFFFFFL)) {
      *sptr_ptr = sptr - 1;
      raise_Error(READ_ERROR_NAT0_OVERFLOW);
    })
#endif

static inline unsigned long read_nat0(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  int code;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  code = *sptr;
  *sptr_ptr = sptr + 1;
  if (likely(code >= 0)) return code;
  if (likely(code == CODE_INT16)) return safe_read_nat0_16(sptr_ptr, eptr);
  if (likely(code == CODE_INT32)) return safe_read_nat0_32(sptr_ptr, eptr);
#ifdef ARCH_SIXTYFOUR
  if (likely(code == CODE_INT64)) return safe_read_nat0_64(sptr_ptr, eptr);
#endif
  *sptr_ptr = sptr;
  raise_Error(READ_ERROR_NAT0_CODE);
}

CAMLprim value read_nat0_stub(char **sptr_ptr, char *eptr)
{
  return Val_long(read_nat0(sptr_ptr, eptr));
}


/* Reading 32bit integers */

static inline long read_int32(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  int code;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  code = *sptr;
  *sptr_ptr = sptr + 1;
  if (likely(code >= 0)) return code;
  if (likely(code == CODE_INT16)) return safe_read_int16(sptr_ptr, eptr);
  if (likely(code == CODE_NEG_INT8)) return safe_read_neg_int8(sptr_ptr, eptr);
  if (likely(code == CODE_INT32))
#ifdef ARCH_SIXTYFOUR
    return safe_read_int32(sptr_ptr, eptr);
#else
    return safe_read_nocheck_int32(sptr_ptr, eptr);
#endif
  *sptr_ptr = sptr;
  raise_Error(READ_ERROR_INT32_CODE);
}

CAMLprim value read_int32_stub(char **sptr_ptr, char *eptr)
{
  return caml_copy_int32(read_int32(sptr_ptr, eptr));
}


/* Reading 64bit integers */

static inline int64 read_int64(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  int code;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  code = *sptr;
  *sptr_ptr = sptr + 1;
#ifdef ARCH_SIXTYFOUR
  if (likely(code >= 0))
    return code;
  if (likely(code == CODE_INT16)) return safe_read_int16(sptr_ptr, eptr);
  if (likely(code == CODE_NEG_INT8)) return safe_read_neg_int8(sptr_ptr, eptr);
  if (likely(code == CODE_INT32)) return safe_read_int32(sptr_ptr, eptr);
  if (likely(code == CODE_INT64)) return safe_read_nocheck_int64(sptr_ptr, eptr);
#else
  if (likely(code >= 0))
    return I64_literal(0, code);
  if (likely(code == CODE_INT16))
    return I64_of_int32(safe_read_int16(sptr_ptr, eptr));
  if (likely(code == CODE_NEG_INT8))
    return I64_literal(0xFFFFFFFF, safe_read_neg_int8(sptr_ptr, eptr));
  if (likely(code == CODE_INT32))
    return I64_of_int32(safe_read_nocheck_int32(sptr_ptr, eptr));
  if (likely(code == CODE_INT64)) {
    unsigned int l = safe_read_nocheck_int32(sptr_ptr, eptr);
    return I64_literal(safe_read_nocheck_int32(sptr_ptr, eptr), l);
  }
#endif
  *sptr_ptr = sptr;
  raise_Error(READ_ERROR_INT64_CODE);
}

CAMLprim value read_int64_stub(char **sptr_ptr, char *eptr)
{
  return caml_copy_int64(read_int64(sptr_ptr, eptr));
}


/* Reading nativeints */

static inline long read_nativeint(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  int code;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  code = *sptr;
  *sptr_ptr = sptr + 1;
  if (likely(code >= 0)) return code;
  if (likely(code == CODE_INT16)) return safe_read_int16(sptr_ptr, eptr);
  if (likely(code == CODE_NEG_INT8)) return safe_read_neg_int8(sptr_ptr, eptr);
#ifdef ARCH_SIXTYFOUR
  if (likely(code == CODE_INT32)) return safe_read_int32(sptr_ptr, eptr);
  if (likely(code == CODE_INT64)) return safe_read_nocheck_int64(sptr_ptr, eptr);
#else
  if (likely(code == CODE_INT32)) return safe_read_nocheck_int32(sptr_ptr, eptr);
#endif
  *sptr_ptr = sptr;
  raise_Error(READ_ERROR_NATIVEINT_CODE);
}

CAMLprim value read_nativeint_stub(char **sptr_ptr, char *eptr)
{
  return caml_copy_nativeint(read_nativeint(sptr_ptr, eptr));
}


/* Reading unit value */

CAMLprim value read_unit_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  int res;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  res = *sptr;
  if (res == 0) { *sptr_ptr = ++sptr; return Val_unit; }
  raise_Error(READ_ERROR_UNIT_CODE);
}


/* Reading booleans and options */

#define READ_ZERO_OR_ONE(NAME, CODE) \
  CAMLprim value read_##NAME##_stub(char **sptr_ptr, char *eptr) \
  { \
    char *sptr = *sptr_ptr; \
    int res; \
    if (unlikely(sptr >= eptr)) \
      caml_raise_constant(*v_bin_prot_exc_Buffer_short); \
    res = *sptr; \
    if (res == 0) { *sptr_ptr = ++sptr; return Val_int(0); } \
    if (res == 1) { *sptr_ptr = ++sptr; return Val_int(1); } \
    raise_Error(CODE); \
  }

READ_ZERO_OR_ONE(bool, READ_ERROR_BOOL_CODE)
READ_ZERO_OR_ONE(option_bool, READ_ERROR_OPTION_CODE)


/* Reading characters */

CAMLprim value read_char_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  unsigned char res;
  if (unlikely(sptr >= eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  res = (unsigned char) *sptr;
  *sptr_ptr = ++sptr;
  return Val_int(res);
}
MK_ML_READER(char)


/* Reading strings */

CAMLprim value read_string_stub(char **sptr_ptr, char *eptr)
{
  value v_res;
  char *start = *sptr_ptr;
  unsigned long len = read_nat0(sptr_ptr, eptr);
  char *sptr = *sptr_ptr;
  char *next = sptr + len;
  if (unlikely(len > Bsize_wsize(Max_wosize) - 1)) {
    *sptr_ptr = start;
    raise_Error(READ_ERROR_STRING_TOO_LONG);
  }
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
  v_res = caml_alloc_string(len);
  memcpy(String_val(v_res), sptr, len);
  return v_res;
}


/* Reading floats and float arrays */

CAMLprim inline value read_float_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + sizeof(double);
  double n;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
  memcpy(&n, sptr, sizeof(double));
  return caml_copy_double(n);
}

MK_ML_READER(float)

CAMLprim value read_float_array_stub(char **sptr_ptr, char *eptr)
{
  char *start = *sptr_ptr;
  unsigned long len = read_nat0(sptr_ptr, eptr);
  unsigned long tot_size;
  unsigned long wsize;
  char *sptr;
  char *next;
  value v_res;
  if (unlikely(len == 0)) return Atom(0);
  wsize = len * Double_wosize;
  if (unlikely(wsize > Max_wosize)) {
    *sptr_ptr = start;
    raise_Error(READ_ERROR_ARRAY_TOO_LONG);
  }
  sptr = *sptr_ptr;
  tot_size = len * sizeof(double);
  next = sptr + tot_size;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
  v_res = caml_alloc(wsize, Double_array_tag);
  memcpy((double *) v_res, sptr, tot_size);
  return v_res;
}

CAMLprim value ml_read_float_array_stub(value v_buf, value v_pos_ref)
{
  CAMLparam2(v_buf, v_pos_ref);
    struct caml_ba_array *buf = Caml_ba_array_val(v_buf);
    char *start = buf->data;
    long pos = Long_val(Field(v_pos_ref, 0));
    char *sptr = start + pos;
    char **sptr_ptr = &sptr;
    char *eptr = start + *buf->dim;
    value v_res;
    unsigned long len;
    unsigned long tot_size;
    unsigned long wsize;
    char *next;
    if (unlikely(pos < 0)) caml_array_bound_error();
    len = read_nat0(sptr_ptr, eptr);
    if (unlikely(len == 0)) {
      Field(v_pos_ref, 0) = Val_long(sptr - start);
      CAMLreturn(Atom(0));
    }
    wsize = len * Double_wosize;
    if (unlikely(wsize > Max_wosize))
      raise_Read_error(READ_ERROR_ARRAY_TOO_LONG, pos);
    tot_size = len * sizeof(double);
    next = sptr + tot_size;
    if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
    v_res = caml_alloc(wsize, Double_array_tag);
    memcpy((double *) v_res, sptr, tot_size);
    Field(v_pos_ref, 0) = Val_long(next - start);
  CAMLreturn(v_res);
}


/* Reading polymorphic variants */

CAMLprim value read_variant_tag_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + 4;
  int n;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  n = le32dec(sptr);
  if (likely(Is_long(n))) {
    *sptr_ptr = next;
    return (value) n;
  }
  raise_Error(READ_ERROR_VARIANT_TAG);
}

CAMLprim value ml_read_variant_tag_stub(value v_buf, value v_pos_ref)
{
  struct caml_ba_array *buf = Caml_ba_array_val(v_buf);
  char *start = buf->data;
  long pos = Long_val(Field(v_pos_ref, 0));
  char *sptr = start + pos;
  unsigned long next_pos = (unsigned long) pos + 4;
  int n;
  if (unlikely(pos < 0)) caml_array_bound_error();
  if (unlikely(next_pos > (unsigned long) *buf->dim))
    caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  n = le32dec(sptr);
  if (likely(Is_long(n))) {
    Field(v_pos_ref, 0) = Val_long(next_pos);
    return (value) n;
  }
  else raise_Read_error(READ_ERROR_VARIANT_TAG, pos);
}


/* Reading raw strings */

CAMLprim inline value read_raw_string_stub(
  char **sptr_ptr, char *eptr, value v_str, value v_pos, value v_len)
{
  size_t pos = (size_t) Long_val(v_pos), len = (size_t) Long_val(v_len);
  char *sptr = *sptr_ptr;
  char *next = sptr + len;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
  memcpy(String_val(v_str) + pos, sptr, len);
  return Val_unit;
}


/* Reading bigarrays */

#define MK_BA1_READER(NAME, TYPE, TFLAG, TLAYOUT) \
  CAMLprim inline value read_##NAME##_stub(char **sptr_ptr, char *eptr) \
  { \
    unsigned long len = read_nat0(sptr_ptr, eptr); \
    unsigned long tot_size = len * sizeof(TYPE); \
    char *sptr = *sptr_ptr; \
    char *next = sptr + tot_size; \
    intnat dim; \
    value v_res; \
    if (unlikely(next > eptr)) \
      caml_raise_constant(*v_bin_prot_exc_Buffer_short); \
    dim = len; \
    v_res = \
      caml_ba_alloc( \
        CAML_BA_##TFLAG | CAML_BA_##TLAYOUT##_LAYOUT, 1, NULL, &dim); \
    *sptr_ptr = next; \
    if (unlikely(tot_size > 65536)) { \
      Begin_roots1(v_res); \
      caml_enter_blocking_section(); \
        memcpy((TYPE *) Caml_ba_data_val(v_res), sptr, tot_size); \
      caml_leave_blocking_section(); \
      End_roots(); \
    } else memcpy((TYPE *) Caml_ba_data_val(v_res), sptr, tot_size); \
    return v_res; \
  } \
  \
  MK_ML_READER(NAME)

MK_BA1_READER(bigstring, char, UINT8, C)

#define MK_VEC_MAT_READERS(NAME, TYPE, TFLAG) \
  MK_BA1_READER(NAME##_vec, TYPE, TFLAG, FORTRAN) \
  \
  CAMLprim inline value read_##NAME##_mat_stub(char **sptr_ptr, char *eptr) \
  { \
    unsigned long dim1 = read_nat0(sptr_ptr, eptr); \
    unsigned long dim2 = read_nat0(sptr_ptr, eptr); \
    unsigned long size = dim1 * dim2; \
    unsigned long tot_size = size * sizeof(TYPE); \
    char *sptr = *sptr_ptr; \
    char *next = sptr + tot_size; \
    intnat dims[2]; \
    value v_res; \
    if (unlikely(next > eptr)) \
      caml_raise_constant(*v_bin_prot_exc_Buffer_short); \
    dims[0] = dim1; \
    dims[1] = dim2; \
    v_res = \
      caml_ba_alloc( \
        CAML_BA_##TFLAG | CAML_BA_FORTRAN_LAYOUT, 2, NULL, dims); \
    *sptr_ptr = next; \
    if (unlikely(tot_size > 65536)) { \
      Begin_roots1(v_res); \
      caml_enter_blocking_section(); \
        memcpy((TYPE *) Caml_ba_data_val(v_res), sptr, tot_size); \
      caml_leave_blocking_section(); \
      End_roots(); \
    } else memcpy((TYPE *) Caml_ba_data_val(v_res), sptr, tot_size); \
    return v_res; \
  } \
  \
  MK_ML_READER(NAME##_mat)

MK_VEC_MAT_READERS(float32, float, FLOAT32)
MK_VEC_MAT_READERS(float64, double, FLOAT64)


/* Reading bits */

CAMLprim value read_int_16bit_stub(char **sptr_ptr, char *eptr)
{
  unsigned short res = safe_read_nat0_16(sptr_ptr, eptr);
  return Val_int(res);
}
MK_ML_READER(int_16bit)

CAMLprim value read_int_32bit_stub(char **sptr_ptr, char *eptr)
{
  unsigned int res = safe_read_nat0_32(sptr_ptr, eptr);
  return Val_int(res);
}
MK_ML_READER(int_32bit)

CAMLprim inline value read_int_64bit_stub(char **sptr_ptr, char *eptr)
{
  long n;
#ifndef ARCH_SIXTYFOUR
  long upper;
#endif
  char *sptr = *sptr_ptr;
  char *next = sptr + 8;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  n = le64dec(sptr);
  if (unlikely(n < -0x4000000000000000L || n > 0x3FFFFFFFFFFFFFFFL))
    raise_Error(READ_ERROR_INT_OVERFLOW);
#else
  n = le32dec(sptr);
  memcpy(&upper, sptr + 4, 4);
  if (upper == 0l) {
    if ((unsigned long) n > 0x3FFFFFFFl) raise_Error(READ_ERROR_INT_OVERFLOW);
  } else if (upper == -1) {
    if (n < -0x40000000l) raise_Error(READ_ERROR_INT_OVERFLOW);
  }
  else raise_Error(READ_ERROR_INT_OVERFLOW);
#endif
  *sptr_ptr = next;
  return Val_long(n);
}
MK_ML_READER(int_64bit)

CAMLprim inline value read_int64_bits_stub(char **sptr_ptr, char *eptr)
{
  int64 n;
  value v_res;
  char *sptr = *sptr_ptr;
  char *next = sptr + 8;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  n = le64dec(sptr);
#else
  n = I64_literal(le32dec(sptr + 4), le32dec(sptr));
#endif
  v_res = caml_copy_int64(n);
  *sptr_ptr = next;
  return (value) v_res;
}
MK_ML_READER(int64_bits)

CAMLprim inline value read_network16_int_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + 2;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
  return (value) Val_int(be16dec(sptr));
}
MK_ML_READER(network16_int)

CAMLprim inline value read_network32_int_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + 4;
  int n;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  n = (int) be32dec(sptr);
#ifdef ARCH_SIXTYFOUR
  *sptr_ptr = next;
  return (value) Val_int((uint32_t) n);
#else
  if (unlikely(n < -0x40000000l || n > 0x3FFFFFFFl))
    raise_Error(READ_ERROR_INT_OVERFLOW);
  *sptr_ptr = next;
  return (value) Val_int(n);
#endif
}
MK_ML_READER(network32_int)

CAMLprim inline value read_network32_int32_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + 4;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
  return (value) caml_copy_int32(be32dec(sptr));
}
MK_ML_READER(network32_int32)

CAMLprim inline value read_network64_int_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + 8;
  long n;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
#ifdef ARCH_SIXTYFOUR
  n = (long) be64dec(sptr);
  if (unlikely(n < -0x4000000000000000L || n > 0x3FFFFFFFFFFFFFFFL))
    raise_Error(READ_ERROR_INT_OVERFLOW);
#else /* 32bit */
  /* Read the upper 32 bits first.  They must all be zero, otherwise we
     consider this an overflow.  On 32bit platforms the integer must
     fit completely into one word. */
  memcpy(&n, sptr, 4);
  if (n != 0) raise_Error(READ_ERROR_INT_OVERFLOW);
  n = be32dec(sptr + 4);
  if (unlikely(n < -0x40000000l || n > 0x3FFFFFFFl))
    raise_Error(READ_ERROR_INT_OVERFLOW);
#endif
  *sptr_ptr = next;
  return (value) Val_long(n);
}
MK_ML_READER(network64_int)

CAMLprim inline value read_network64_int64_stub(char **sptr_ptr, char *eptr)
{
  char *sptr = *sptr_ptr;
  char *next = sptr + 8;
  int64 n;
  if (unlikely(next > eptr)) caml_raise_constant(*v_bin_prot_exc_Buffer_short);
  *sptr_ptr = next;
#ifdef ARCH_SIXTYFOUR
  n = (long) be64dec(sptr);
#else /* 32bit */
  n = I64_literal(be32dec(sptr), be32dec(sptr + 4));
#endif
  return (value) caml_copy_int64(n);
}
MK_ML_READER(network64_int64)
