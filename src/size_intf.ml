open! Base

module%template Definitions = struct
  [@@@mode.default m = (global, local)]

  type 'a sizer = 'a -> int
  type ('a, 'b) sizer1 = ('a sizer[@mode m]) -> ('b sizer[@mode m])
  type ('a, 'b, 'c) sizer2 = ('a sizer[@mode m]) -> (('b, 'c) sizer1[@mode m])
  type ('a, 'b, 'c, 'd) sizer3 = ('a sizer[@mode m]) -> (('b, 'c, 'd) sizer2[@mode m])
end

module type Size = sig
  (** Compute size of values in the binary protocol. *)

  open Common

  include module type of struct
    include Definitions
  end

  [%%template:
  [@@@mode.default m = (global, local)]

  val bin_size_unit : (unit sizer[@mode m])
  val bin_size_bool : (bool sizer[@mode m])
  val bin_size_string : (string sizer[@mode m])
  val bin_size_bytes : (bytes sizer[@mode m])
  val bin_size_char : (char sizer[@mode m])
  val bin_size_int : (int sizer[@mode m])
  val bin_size_float : (float sizer[@mode m])
  val bin_size_int32 : (int32 sizer[@mode m])
  val bin_size_int64 : (int64 sizer[@mode m])
  val bin_size_nativeint : (nativeint sizer[@mode m])
  val bin_size_nat0 : (Nat0.t sizer[@mode m])
  val bin_size_ref : 'a. (('a, 'a ref) sizer1[@mode m])
  val bin_size_lazy_t : (('a, 'a lazy_t) sizer1[@mode m])
  val bin_size_lazy : (('a, 'a lazy_t) sizer1[@mode m])
  val bin_size_option : 'a. (('a, 'a option) sizer1[@mode m])
  val bin_size_pair : (('a, 'b, 'a * 'b) sizer2[@mode m])
  val bin_size_triple : (('a, 'b, 'c, 'a * 'b * 'c) sizer3[@mode m])
  val bin_size_list : 'a. (('a, 'a list) sizer1[@mode m])
  val bin_size_array : (('a, 'a array) sizer1[@mode m])
  val bin_size_iarray : (('a, 'a iarray) sizer1[@mode m])
  val bin_size_float32_vec : (vec32 sizer[@mode m])
  val bin_size_float64_vec : (vec64 sizer[@mode m])
  val bin_size_vec : (vec sizer[@mode m])
  val bin_size_float32_mat : (mat32 sizer[@mode m])
  val bin_size_float64_mat : (mat64 sizer[@mode m])
  val bin_size_mat : (mat sizer[@mode m])
  val bin_size_bigstring : (buf sizer[@mode m])
  val bin_size_floatarray : (floatarray sizer[@mode m])
  val bin_size_variant_int : (int sizer[@mode m])
  val bin_size_int_8bit : (int sizer[@mode m])
  val bin_size_int_16bit : (int sizer[@mode m])
  val bin_size_int_32bit : (int sizer[@mode m])
  val bin_size_int_64bit : (int sizer[@mode m])
  val bin_size_int32_bits : (int32 sizer[@mode m])
  val bin_size_int64_bits : (int64 sizer[@mode m])
  val bin_size_network16_int : (int sizer[@mode m])
  val bin_size_network32_int : (int sizer[@mode m])
  val bin_size_network32_int32 : (int32 sizer[@mode m])
  val bin_size_network64_int : (int sizer[@mode m])
  val bin_size_network64_int64 : (int64 sizer[@mode m])
  val bin_size_md5 : (Md5_lib.t sizer[@mode m])]

  (* Provide the maximum sizes for fields which do not depend upon an array/vector/matrix
     length, choosing the size required for the largest architecture.  This allows for the
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
