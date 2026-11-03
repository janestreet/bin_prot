open! Base

module Definitions = struct
  type 'a sizer = 'a -> int
  type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
  type ('a, 'b, 'c) sizer2 = 'a sizer -> ('b, 'c) sizer1
  type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> ('b, 'c, 'd) sizer2
end

module type Size = sig
  (** Compute size of values in the binary protocol. *)

  open Common

  include module type of struct
    include Definitions
  end

  val bin_size_unit : unit sizer
  val bin_size_bool : bool sizer
  val bin_size_string : string sizer
  val bin_size_bytes : bytes sizer
  val bin_size_char : char sizer
  val bin_size_int : int sizer
  val bin_size_float : float sizer
  val bin_size_int32 : int32 sizer
  val bin_size_int64 : int64 sizer
  val bin_size_nativeint : nativeint sizer
  val bin_size_nat0 : Nat0.t sizer
  val bin_size_ref : 'a. ('a, 'a ref) sizer1
  val bin_size_lazy_t : ('a, 'a lazy_t) sizer1
  val bin_size_lazy : ('a, 'a lazy_t) sizer1
  val bin_size_option : 'a. ('a, 'a option) sizer1
  val bin_size_or_null : ('a, 'a or_null) sizer1
  val bin_size_pair : ('a, 'b, 'a * 'b) sizer2
  val bin_size_triple : ('a, 'b, 'c, 'a * 'b * 'c) sizer3
  val bin_size_list : 'a. ('a, 'a list) sizer1
  val bin_size_array : 'a. ('a, 'a array) sizer1
  val bin_size_iarray : 'a. ('a, 'a iarray) sizer1

  val bin_size_bigarray1
    :  kind:('a, 'k) Stdlib.Bigarray.kind
    -> ('a, 'k, _) Stdlib.Bigarray.Array1.t sizer

  val bin_size_bigarray2
    :  kind:('a, 'k) Stdlib.Bigarray.kind
    -> ('a, 'k, _) Stdlib.Bigarray.Array2.t sizer

  val bin_size_genarray
    :  kind:('a, 'k) Stdlib.Bigarray.kind
    -> ('a, 'k, _) Stdlib.Bigarray.Genarray.t sizer

  val bin_size_float32_vec : vec32 sizer
  val bin_size_float64_vec : vec64 sizer
  val bin_size_vec : vec sizer
  val bin_size_float32_mat : mat32 sizer
  val bin_size_float64_mat : mat64 sizer
  val bin_size_mat : mat sizer
  val bin_size_bigstring : buf sizer
  val bin_size_floatarray : floatarray sizer
  val bin_size_variant_int : int sizer
  val bin_size_int_8bit : int sizer
  val bin_size_int_16bit : int sizer
  val bin_size_int_32bit : int sizer
  val bin_size_int_64bit : int sizer
  val bin_size_int32_bits : int32 sizer
  val bin_size_int64_bits : int64 sizer
  val bin_size_network16_int : int sizer
  val bin_size_network32_int : int sizer
  val bin_size_network32_int32 : int32 sizer
  val bin_size_network64_int : int sizer
  val bin_size_network64_int64 : int64 sizer
  val bin_size_md5 : Md5_lib.t sizer

  (* Provide the maximum sizes for fields which do not depend upon an array/vector/matrix
     length, choosing the size required for the largest architecture. This allows for the
     most conservative estimation of space required. *)
  module Maximum : sig
    val bin_size_unit : int
    val bin_size_bool : int
    val bin_size_char : int
    val bin_size_md5 : int
    val bin_size_int : int
    val bin_size_float : int
    val bin_size_int32 : int
    val bin_size_int64 : int
    val bin_size_nativeint : int
    val bin_size_nat0 : int
    val bin_size_variant_int : int
    val bin_size_int_8bit : int
    val bin_size_int_16bit : int
    val bin_size_int_32bit : int
    val bin_size_int_64bit : int
    val bin_size_int32_bits : int
    val bin_size_int64_bits : int
    val bin_size_network16_int : int
    val bin_size_network32_int : int
    val bin_size_network32_int32 : int
    val bin_size_network64_int : int
    val bin_size_network64_int64 : int
  end

  (* Provide absolute minimum sizes for fields, choosing [0] for the lengths of any
     arrays/vectors/matrices. *)
  module Minimum : sig
    val bin_size_unit : int
    val bin_size_bool : int
    val bin_size_string : int
    val bin_size_bytes : int
    val bin_size_char : int
    val bin_size_md5 : int
    val bin_size_int : int
    val bin_size_float : int
    val bin_size_int32 : int
    val bin_size_int64 : int
    val bin_size_nativeint : int
    val bin_size_nat0 : int
    val bin_size_ref : int
    val bin_size_lazy_t : int
    val bin_size_option : int
    val bin_size_or_null : int
    val bin_size_pair : int
    val bin_size_triple : int
    val bin_size_list : int
    val bin_size_array : int
    val bin_size_iarray : int
    val bin_size_float32_vec : int
    val bin_size_float64_vec : int
    val bin_size_vec : int
    val bin_size_float32_mat : int
    val bin_size_float64_mat : int
    val bin_size_mat : int
    val bin_size_bigstring : int
    val bin_size_floatarray : int
    val bin_size_float_array : int
    val bin_size_variant_int : int
    val bin_size_int_8bit : int
    val bin_size_int_16bit : int
    val bin_size_int_32bit : int
    val bin_size_int_64bit : int
    val bin_size_int32_bits : int
    val bin_size_int64_bits : int
    val bin_size_network16_int : int
    val bin_size_network32_int : int
    val bin_size_network32_int32 : int
    val bin_size_network64_int : int
    val bin_size_network64_int64 : int
  end
end
