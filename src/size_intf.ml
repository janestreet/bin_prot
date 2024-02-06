module Definitions = struct
  type 'a sizer = 'a -> int
  type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
  type ('a, 'b, 'c) sizer2 = 'a sizer -> ('b, 'c) sizer1
  type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> ('b, 'c, 'd) sizer2
  type 'a sizer_local = 'a -> int
  type ('a, 'b) sizer_local1 = 'a sizer_local -> 'b sizer_local
  type ('a, 'b, 'c) sizer_local2 = 'a sizer_local -> ('b, 'c) sizer_local1
  type ('a, 'b, 'c, 'd) sizer_local3 = 'a sizer_local -> ('b, 'c, 'd) sizer_local2
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
  val bin_size_ref : ('a, 'a ref) sizer1
  val bin_size_lazy_t : ('a, 'a lazy_t) sizer1
  val bin_size_lazy : ('a, 'a lazy_t) sizer1
  val bin_size_option : ('a, 'a option) sizer1
  val bin_size_pair : ('a, 'b, 'a * 'b) sizer2
  val bin_size_triple : ('a, 'b, 'c, 'a * 'b * 'c) sizer3
  val bin_size_list : ('a, 'a list) sizer1
  val bin_size_array : ('a, 'a array) sizer1
  val bin_size_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t) sizer2
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
  val bin_size_int64_bits : int64 sizer
  val bin_size_network16_int : int sizer
  val bin_size_network32_int : int sizer
  val bin_size_network32_int32 : int32 sizer
  val bin_size_network64_int : int sizer
  val bin_size_network64_int64 : int64 sizer
  val bin_size_md5 : Md5_lib.t sizer

  (* Local versions *)

  val bin_size_unit__local : unit sizer_local
  val bin_size_bool__local : bool sizer_local
  val bin_size_string__local : string sizer_local
  val bin_size_bytes__local : bytes sizer_local
  val bin_size_char__local : char sizer_local
  val bin_size_int__local : int sizer_local
  val bin_size_float__local : float sizer_local
  val bin_size_int32__local : int32 sizer_local
  val bin_size_int64__local : int64 sizer_local
  val bin_size_nativeint__local : nativeint sizer_local
  val bin_size_nat0__local : Nat0.t sizer_local
  val bin_size_ref__local : ('a, 'a ref) sizer_local1
  val bin_size_lazy_t__local : ('a, 'a lazy_t) sizer_local1
  val bin_size_lazy__local : ('a, 'a lazy_t) sizer_local1
  val bin_size_option__local : ('a, 'a option) sizer_local1
  val bin_size_pair__local : ('a, 'b, 'a * 'b) sizer_local2
  val bin_size_triple__local : ('a, 'b, 'c, 'a * 'b * 'c) sizer_local3
  val bin_size_list__local : ('a, 'a list) sizer_local1
  val bin_size_array__local : ('a, 'a array) sizer_local1
  val bin_size_float32_vec__local : vec32 sizer_local
  val bin_size_float64_vec__local : vec64 sizer_local
  val bin_size_vec__local : vec sizer_local
  val bin_size_float32_mat__local : mat32 sizer_local
  val bin_size_float64_mat__local : mat64 sizer_local
  val bin_size_mat__local : mat sizer_local
  val bin_size_bigstring__local : buf sizer_local
  val bin_size_floatarray__local : floatarray sizer_local
  val bin_size_variant_int__local : int sizer_local
  val bin_size_int_8bit__local : int sizer_local
  val bin_size_int_16bit__local : int sizer_local
  val bin_size_int_32bit__local : int sizer_local
  val bin_size_int_64bit__local : int sizer_local
  val bin_size_int64_bits__local : int64 sizer_local
  val bin_size_network16_int__local : int sizer_local
  val bin_size_network32_int__local : int sizer_local
  val bin_size_network32_int32__local : int32 sizer_local
  val bin_size_network64_int__local : int sizer_local
  val bin_size_network64_int64__local : int64 sizer_local
  val bin_size_md5__local : Md5_lib.t sizer_local

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
    val bin_size_hashtbl : int
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
    val bin_size_int64_bits : int
    val bin_size_network16_int : int
    val bin_size_network32_int : int
    val bin_size_network32_int32 : int
    val bin_size_network64_int : int
    val bin_size_network64_int64 : int
  end
end
