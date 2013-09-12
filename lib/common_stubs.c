/* Stubs for common binary protocol functions */

#include "common_stubs.h"

value *v_bin_prot_exc_Buffer_short;

CAMLprim value bin_prot_common_init_stub(value __unused v_unit)
{
  v_bin_prot_exc_Buffer_short =
    caml_named_value("Bin_prot.Common.Buffer_short");
  return Val_unit;
}


/* Utility definitions */

CAMLprim inline __pure value get_buf_ptr_stub(value v_buf, value v_pos)
{
  char *sptr = Caml_ba_data_val(v_buf);
  char *eptr = sptr + Long_val(v_pos);
  return (value) eptr;
}

CAMLprim inline __attribute__ ((const)) value
get_buf_pos_stub(value v_start, value v_cur)
{
  return Val_long((char *) v_cur - (char *) v_start);
}

CAMLprim value get_safe_buf_pos_stub(
  value __unused v_buf, value v_start, value v_cur)
{
  return get_buf_pos_stub(v_start, v_cur);
}

CAMLprim value shift_sptr_stub(char *sptr, value v_n)
{
  return (value) (sptr + Long_val(v_n));
}

CAMLprim value get_eptr_from_sptr_ptr(char **sptr_ptr, value v_pos)
{
  return (value) (*sptr_ptr + Long_val(v_pos));
}

CAMLprim __malloc char ** alloc_sptr_ptr_stub(value v_buf, value v_pos)
{
  char **sptr_ptr = caml_stat_alloc(sizeof(char *));
  *sptr_ptr = (char *) get_buf_ptr_stub(v_buf, v_pos);
  return sptr_ptr;
}

CAMLprim value dealloc_sptr_ptr_stub(value v_buf, char /*@only@*/ **sptr_ptr)
{
  unsigned long pos = (*sptr_ptr) - (char *) Caml_ba_data_val(v_buf);
  free((char **) sptr_ptr);
  return Val_long(pos);
}

CAMLprim value set_sptr_ptr_stub(char **sptr_ptr, value v_buf, value v_pos)
{
  *sptr_ptr = (char *) Caml_ba_data_val(v_buf) + Long_val(v_pos);
  return Val_unit;
}

CAMLprim __pure value get_sptr_ptr_stub(char **sptr_ptr, value v_buf)
{
  return Val_long(*sptr_ptr - (char *) Caml_ba_data_val(v_buf));
}

CAMLprim __pure value get_sptr_ptr_sptr_stub(char **sptr_ptr)
{
  return (value) *sptr_ptr;
}

CAMLprim value set_sptr_ptr_sptr_stub(char **sptr_ptr, char *sptr)
{
  *sptr_ptr = sptr;
  return Val_unit;
}

CAMLprim value get_ptr_string_stub(char *sptr, char *eptr)
{
  unsigned long len = eptr - sptr;
  value v_str = caml_alloc_string((mlsize_t) len);
  memcpy(String_val(v_str), sptr, (size_t) len);
  return v_str;
}


/* Blitting strings to buffers */

static inline __pure char * get_buf(value v_buf, value v_pos)
{
  return (char *) Caml_ba_data_val(v_buf) + Long_val(v_pos);
}

CAMLprim value bin_prot_blit_string_buf_stub(
  value v_src_pos, value v_str, value v_dst_pos, value v_buf, value v_len)
{
  char *str = String_val(v_str) + Long_val(v_src_pos);
  char *buf = get_buf(v_buf, v_dst_pos);
  memcpy(buf, str, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value bin_prot_blit_buf_string_stub(
  value v_src_pos, value v_buf, value v_dst_pos, value v_str, value v_len)
{
  char *buf = get_buf(v_buf, v_src_pos);
  char *str = String_val(v_str) + Long_val(v_dst_pos);
  memcpy(str, buf, (size_t) Long_val(v_len));
  return Val_unit;
}

CAMLprim value bin_prot_blit_buf_stub(
  value v_src_pos, value v_src, value v_dst_pos, value v_dst, value v_len)
{
  struct caml_ba_array *ba_src = Caml_ba_array_val(v_src);
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_dst);
  char *src = (char *) ba_src->data + Long_val(v_src_pos);
  char *dst = (char *) ba_dst->data + Long_val(v_dst_pos);
  size_t len = (size_t) Long_val(v_len);
  if
    (
      unlikely(len > 65536)
      || unlikely(((ba_src->flags & CAML_BA_MAPPED_FILE) != 0))
      || unlikely(((ba_dst->flags & CAML_BA_MAPPED_FILE) != 0))
    )
  /* use [memmove] rather than [memcpy] because src and dst may overlap */
  {
    Begin_roots2(v_src, v_dst);
    caml_enter_blocking_section();
      memmove(dst, src, len);
    caml_leave_blocking_section();
    End_roots();
  }
  else memmove(dst, src, len);
  return Val_unit;
}
