(**
   This module defines default converters for the types defined in the OCaml
   standard library.
*)
include Size

let bin_unit = Type_class.bin_unit
let bin_writer_unit = Type_class.bin_writer_unit
let bin_write_unit_ = Unsafe_write_c.bin_write_unit
let bin_write_unit = Write_ml.bin_write_unit
let bin_reader_unit = Type_class.bin_reader_unit
let bin_read_unit = Read_ml.bin_read_unit
let bin_read_unit_ = Unsafe_read_c.bin_read_unit
let bin_read_unit__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "unit"

let bin_bool = Type_class.bin_bool
let bin_writer_bool = Type_class.bin_writer_bool
let bin_write_bool_ = Unsafe_write_c.bin_write_bool
let bin_write_bool = Write_ml.bin_write_bool
let bin_reader_bool = Type_class.bin_reader_bool
let bin_read_bool = Read_ml.bin_read_bool
let bin_read_bool_ = Unsafe_read_c.bin_read_bool
let bin_read_bool__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "bool"

let bin_string = Type_class.bin_string
let bin_writer_string = Type_class.bin_writer_string
let bin_write_string_ = Unsafe_write_c.bin_write_string
let bin_write_string = Write_ml.bin_write_string
let bin_reader_string = Type_class.bin_reader_string
let bin_read_string = Read_ml.bin_read_string
let bin_read_string_ = Unsafe_read_c.bin_read_string
let bin_read_string__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "string"

let bin_char = Type_class.bin_char
let bin_writer_char = Type_class.bin_writer_char
let bin_write_char_ = Unsafe_write_c.bin_write_char
let bin_write_char = Write_ml.bin_write_char
let bin_reader_char = Type_class.bin_reader_char
let bin_read_char = Read_ml.bin_read_char
let bin_read_char_ = Unsafe_read_c.bin_read_char
let bin_read_char__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "char"

let bin_int = Type_class.bin_int
let bin_writer_int = Type_class.bin_writer_int
let bin_write_int_ = Unsafe_write_c.bin_write_int
let bin_write_int = Write_ml.bin_write_int
let bin_reader_int = Type_class.bin_reader_int
let bin_read_int = Read_ml.bin_read_int
let bin_read_int_ = Unsafe_read_c.bin_read_int
let bin_read_int__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "int"

let bin_float = Type_class.bin_float
let bin_writer_float = Type_class.bin_writer_float
let bin_write_float_ = Unsafe_write_c.bin_write_float
let bin_write_float = Write_ml.bin_write_float
let bin_reader_float = Type_class.bin_reader_float
let bin_read_float = Read_ml.bin_read_float
let bin_read_float_ = Unsafe_read_c.bin_read_float
let bin_read_float__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float"

let bin_int32 = Type_class.bin_int32
let bin_writer_int32 = Type_class.bin_writer_int32
let bin_write_int32_ = Unsafe_write_c.bin_write_int32
let bin_write_int32 = Write_ml.bin_write_int32
let bin_reader_int32 = Type_class.bin_reader_int32
let bin_read_int32 = Read_ml.bin_read_int32
let bin_read_int32_ = Unsafe_read_c.bin_read_int32
let bin_read_int32__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "int32"

let bin_int64 = Type_class.bin_int64
let bin_writer_int64 = Type_class.bin_writer_int64
let bin_write_int64_ = Unsafe_write_c.bin_write_int64
let bin_write_int64 = Write_ml.bin_write_int64
let bin_reader_int64 = Type_class.bin_reader_int64
let bin_read_int64 = Read_ml.bin_read_int64
let bin_read_int64_ = Unsafe_read_c.bin_read_int64
let bin_read_int64__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "int64"

let bin_nativeint = Type_class.bin_nativeint
let bin_writer_nativeint = Type_class.bin_writer_nativeint
let bin_write_nativeint_ = Unsafe_write_c.bin_write_nativeint
let bin_write_nativeint = Write_ml.bin_write_nativeint
let bin_reader_nativeint = Type_class.bin_reader_nativeint
let bin_read_nativeint = Read_ml.bin_read_nativeint
let bin_read_nativeint_ = Unsafe_read_c.bin_read_nativeint
let bin_read_nativeint__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "nativeint"

let bin_ref = Type_class.bin_ref
let bin_writer_ref = Type_class.bin_writer_ref
let bin_write_ref_ = Unsafe_write_c.bin_write_ref
let bin_write_ref = Write_ml.bin_write_ref
let bin_reader_ref = Type_class.bin_reader_ref
let bin_read_ref = Read_ml.bin_read_ref
let bin_read_ref_ = Unsafe_read_c.bin_read_ref
let bin_read_ref__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "ref"

let bin_lazy_t = Type_class.bin_lazy
let bin_writer_lazy_t = Type_class.bin_writer_lazy
let bin_write_lazy_t_ = Unsafe_write_c.bin_write_lazy
let bin_write_lazy_t = Write_ml.bin_write_lazy
let bin_reader_lazy_t = Type_class.bin_reader_lazy
let bin_read_lazy_t = Read_ml.bin_read_lazy
let bin_read_lazy_t_ = Unsafe_read_c.bin_read_lazy
let bin_read_lazy_t__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "lazy"

let bin_lazy = Type_class.bin_lazy
let bin_writer_lazy = Type_class.bin_writer_lazy
let bin_write_lazy_ = Unsafe_write_c.bin_write_lazy
let bin_write_lazy = Write_ml.bin_write_lazy
let bin_reader_lazy = Type_class.bin_reader_lazy
let bin_read_lazy = Read_ml.bin_read_lazy
let bin_read_lazy_ = Unsafe_read_c.bin_read_lazy
let bin_read_lazy__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "lazy"

let bin_option = Type_class.bin_option
let bin_writer_option = Type_class.bin_writer_option
let bin_write_option_ = Unsafe_write_c.bin_write_option
let bin_write_option = Write_ml.bin_write_option
let bin_reader_option = Type_class.bin_reader_option
let bin_read_option = Read_ml.bin_read_option
let bin_read_option_ = Unsafe_read_c.bin_read_option
let bin_read_option__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "option"

let bin_list = Type_class.bin_list
let bin_writer_list = Type_class.bin_writer_list
let bin_write_list_ = Unsafe_write_c.bin_write_list
let bin_write_list = Write_ml.bin_write_list
let bin_reader_list = Type_class.bin_reader_list
let bin_read_list = Read_ml.bin_read_list
let bin_read_list_ = Unsafe_read_c.bin_read_list
let bin_read_list__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "list"

let bin_array = Type_class.bin_array
let bin_writer_array = Type_class.bin_writer_array
let bin_write_array_ = Unsafe_write_c.bin_write_array
let bin_write_array = Write_ml.bin_write_array
let bin_reader_array = Type_class.bin_reader_array
let bin_read_array = Read_ml.bin_read_array
let bin_read_array_ = Unsafe_read_c.bin_read_array
let bin_read_array__ _f _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "array"

let bin_hashtbl = Type_class.bin_hashtbl
let bin_writer_hashtbl = Type_class.bin_writer_hashtbl
let bin_write_hashtbl_ = Unsafe_write_c.bin_write_hashtbl
let bin_write_hashtbl = Write_ml.bin_write_hashtbl
let bin_reader_hashtbl = Type_class.bin_reader_hashtbl
let bin_read_hashtbl = Read_ml.bin_read_hashtbl
let bin_read_hashtbl_ = Unsafe_read_c.bin_read_hashtbl
let bin_read_hashtbl__ _f _g _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "hashtbl"

let bin_bigstring = Type_class.bin_bigstring
let bin_writer_bigstring = Type_class.bin_writer_bigstring
let bin_write_bigstring_ = Unsafe_write_c.bin_write_bigstring
let bin_write_bigstring = Write_ml.bin_write_bigstring
let bin_reader_bigstring = Type_class.bin_reader_bigstring
let bin_read_bigstring = Read_ml.bin_read_bigstring
let bin_read_bigstring_ = Unsafe_read_c.bin_read_bigstring
let bin_read_bigstring__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "bigstring"

let bin_mat = Type_class.bin_mat
let bin_writer_mat = Type_class.bin_writer_mat
let bin_write_mat_ = Unsafe_write_c.bin_write_mat
let bin_write_mat = Write_ml.bin_write_mat
let bin_reader_mat = Type_class.bin_reader_mat
let bin_read_mat = Read_ml.bin_read_mat
let bin_read_mat_ = Unsafe_read_c.bin_read_mat
let bin_read_mat__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "mat"

let bin_float32_mat = Type_class.bin_float32_mat
let bin_writer_float32_mat = Type_class.bin_writer_float32_mat
let bin_write_float32_mat_ = Unsafe_write_c.bin_write_float32_mat
let bin_write_float32_mat = Write_ml.bin_write_float32_mat
let bin_reader_float32_mat = Type_class.bin_reader_float32_mat
let bin_read_float32_mat = Read_ml.bin_read_float32_mat
let bin_read_float32_mat_ = Unsafe_read_c.bin_read_float32_mat
let bin_read_float32_mat__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float32_mat"

let bin_float64_mat = Type_class.bin_float64_mat
let bin_writer_float64_mat = Type_class.bin_writer_float64_mat
let bin_write_float64_mat_ = Unsafe_write_c.bin_write_float64_mat
let bin_write_float64_mat = Write_ml.bin_write_float64_mat
let bin_reader_float64_mat = Type_class.bin_reader_float64_mat
let bin_read_float64_mat = Read_ml.bin_read_float64_mat
let bin_read_float64_mat_ = Unsafe_read_c.bin_read_float64_mat
let bin_read_float64_mat__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float64_mat"

let bin_vec = Type_class.bin_vec
let bin_writer_vec = Type_class.bin_writer_vec
let bin_write_vec_ = Unsafe_write_c.bin_write_vec
let bin_write_vec = Write_ml.bin_write_vec
let bin_reader_vec = Type_class.bin_reader_vec
let bin_read_vec = Read_ml.bin_read_vec
let bin_read_vec_ = Unsafe_read_c.bin_read_vec
let bin_read_vec__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "vec"

let bin_float32_vec = Type_class.bin_float32_vec
let bin_writer_float32_vec = Type_class.bin_writer_float32_vec
let bin_write_float32_vec_ = Unsafe_write_c.bin_write_float32_vec
let bin_write_float32_vec = Write_ml.bin_write_float32_vec
let bin_reader_float32_vec = Type_class.bin_reader_float32_vec
let bin_read_float32_vec = Read_ml.bin_read_float32_vec
let bin_read_float32_vec_ = Unsafe_read_c.bin_read_float32_vec
let bin_read_float32_vec__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float32_vec"

let bin_float64_vec = Type_class.bin_float64_vec
let bin_writer_float64_vec = Type_class.bin_writer_float64_vec
let bin_write_float64_vec_ = Unsafe_write_c.bin_write_float64_vec
let bin_write_float64_vec = Write_ml.bin_write_float64_vec
let bin_reader_float64_vec = Type_class.bin_reader_float64_vec
let bin_read_float64_vec = Read_ml.bin_read_float64_vec
let bin_read_float64_vec_ = Unsafe_read_c.bin_read_float64_vec
let bin_read_float64_vec__ _sptr_ptr _eptr _vint =
  Unsafe_read_c.raise_variant_wrong_type "float64_vec"
