include module type of struct
  include Bin_shape
end

val bin_shape_unit : t
val bin_shape_bool : t
val bin_shape_string : t
val bin_shape_bytes : t
val bin_shape_char : t
val bin_shape_float : t
val bin_shape_int : t
val bin_shape_int32 : t
val bin_shape_int63 : t
val bin_shape_int64 : t
val bin_shape_nativeint : t
val bin_shape_nat0 : t
val bin_shape_digest : t
val bin_shape_float32_vec : t
val bin_shape_float64_vec : t
val bin_shape_vec : t
val bin_shape_float32_mat : t
val bin_shape_float64_mat : t
val bin_shape_mat : t
val bin_shape_bigstring : t
val bin_shape_floatarray : t
val bin_shape_variant_int : t
val bin_shape_int_8bit : t
val bin_shape_int_16bit : t
val bin_shape_int_32bit : t
val bin_shape_int_64bit : t
val bin_shape_int64_bits : t
val bin_shape_network16_int : t
val bin_shape_network32_int : t
val bin_shape_network32_int32 : t
val bin_shape_network64_int : t
val bin_shape_network64_int64 : t
val bin_shape_ref : t -> t
val bin_shape_option : t -> t
val bin_shape_list : t -> t
val bin_shape_array : t -> t
val bin_shape_hashtbl : t -> t -> t
val bin_shape_lazy : t -> t
val bin_shape_pair : t -> t -> t
val bin_shape_triple : t -> t -> t -> t
