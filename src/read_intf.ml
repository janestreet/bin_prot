module Definitions = struct
  open Common

  (** Type of reader functions for the binary protocol.  They take a
      buffer and a reference to a read position, and return the unmarshalled
      value.  The next buffer position after reading in the value will be
      stored in the position reference. *)
  type ('a, 'ctx) reader = ctx:'ctx -> buf -> pos_ref:pos_ref -> 'a

  type ('a, 'b, 'ctx) reader1 = (('a, 'ctx) reader[@mode m]) -> (('b, 'ctx) reader[@mode m])
  type ('a, 'b, 'c, 'ctx) reader2 = (('a, 'ctx) reader[@mode m]) -> (('b, 'c, 'ctx) reader1[@mode m])
  type ('a, 'b, 'c, 'd, 'ctx) reader3 = (('a, 'ctx) reader[@mode m]) -> (('b, 'c, 'd, 'ctx) reader2[@mode m])
end

module type Read = sig
  (** Reading values from the binary protocol using (mostly) OCaml. *)

  open Common

  include module type of struct
    include Definitions
  end

  val bin_read_unit : (unit, 'ctx) reader
  val bin_read_bool : (bool, 'ctx) reader
  val bin_read_string : (string, 'ctx) reader
  val bin_read_bytes : (bytes, 'ctx) reader
  val bin_read_char : (char, 'ctx) reader
  val bin_read_int : (int, 'ctx) reader
  val bin_read_nat0 : (Nat0.t, 'ctx) reader
  val bin_read_float : (float, 'ctx) reader
  val bin_read_int32 : (int32, 'ctx) reader
  val bin_read_int64 : (int64, 'ctx) reader
  val bin_read_nativeint : (nativeint, 'ctx) reader
  val bin_read_ref : ('a, 'a ref, 'ctx) reader1
  val bin_read_lazy : ('a, 'a lazy_t, 'ctx) reader1
  val bin_read_option : ('a, 'a option, 'ctx) reader1
  val bin_read_pair : ('a, 'b, 'a * 'b, 'ctx) reader2
  val bin_read_triple : ('a, 'b, 'c, 'a * 'b * 'c, 'ctx) reader3
  val bin_read_list : ('a, 'a list, 'ctx) reader1
  val bin_read_array : ('a, 'a array, 'ctx) reader1
  val bin_read_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t, 'ctx) reader2
  val bin_read_float32_vec : (vec32, 'ctx) reader
  val bin_read_float64_vec : (vec64, 'ctx) reader
  val bin_read_vec : (vec, 'ctx) reader
  val bin_read_float32_mat : (mat32, 'ctx) reader
  val bin_read_float64_mat : (mat64, 'ctx) reader
  val bin_read_mat : (mat, 'ctx) reader
  val bin_read_bigstring : (buf, 'ctx) reader
  val bin_read_floatarray : (floatarray, 'ctx) reader
  val bin_read_variant_int : (int, 'ctx) reader
  val bin_read_int_8bit : (int, 'ctx) reader
  val bin_read_int_16bit : (int, 'ctx) reader
  val bin_read_int_32bit : (int, 'ctx) reader
  val bin_read_int_64bit : (int, 'ctx) reader
  val bin_read_int64_bits : (int64, 'ctx) reader
  val bin_read_network16_int : (int, 'ctx) reader
  val bin_read_network32_int : (int, 'ctx) reader
  val bin_read_network32_int32 : (int32, 'ctx) reader
  val bin_read_network64_int : (int, 'ctx) reader
  val bin_read_network64_int64 : (int64, 'ctx) reader
  val bin_read_md5 : (Md5_lib.t, 'ctx) reader

  (** Fail early if the list is larger than [max_len]. *)
  val bin_read_list_with_max_len : max_len:int -> ('a, 'a list, 'ctx) reader1
end
