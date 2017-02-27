#include <caml/mlvalues.h>
#include <caml/bigarray.h>

CAMLprim value bin_prot_get_float_offset(value buf, value pos)
{
  return (value)((char *)Caml_ba_data_val(buf) + Long_val(pos));
}
