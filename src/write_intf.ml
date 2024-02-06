module Definitions = struct
  open Common

  (** Type of writer functions for the binary protocol. They take a buffer,
      a write position and a value, and return the next position after
      writing out the value. *)
  type 'a writer = buf -> pos:pos -> 'a -> pos

  type ('a, 'b) writer1 = 'a writer -> 'b writer
  type ('a, 'b, 'c) writer2 = 'a writer -> ('b, 'c) writer1
  type ('a, 'b, 'c, 'd) writer3 = 'a writer -> ('b, 'c, 'd) writer2
  type 'a writer_local = buf -> pos:pos -> 'a -> pos
  type ('a, 'b) writer_local1 = 'a writer_local -> 'b writer_local
  type ('a, 'b, 'c) writer_local2 = 'a writer_local -> ('b, 'c) writer_local1
  type ('a, 'b, 'c, 'd) writer_local3 = 'a writer_local -> ('b, 'c, 'd) writer_local2
end

module type Write = sig
  (** Writing values to the binary protocol using (mostly) OCaml. *)

  open Common

  include module type of struct
    include Definitions
  end

  val bin_write_unit : unit writer
  val bin_write_bool : bool writer
  val bin_write_string : string writer
  val bin_write_bytes : bytes writer
  val bin_write_char : char writer
  val bin_write_int : int writer
  val bin_write_nat0 : Nat0.t writer
  val bin_write_float : float writer
  val bin_write_int32 : int32 writer
  val bin_write_int64 : int64 writer
  val bin_write_nativeint : nativeint writer
  val bin_write_ref : ('a, 'a ref) writer1
  val bin_write_lazy : ('a, 'a lazy_t) writer1
  val bin_write_option : ('a, 'a option) writer1
  val bin_write_pair : ('a, 'b, 'a * 'b) writer2
  val bin_write_triple : ('a, 'b, 'c, 'a * 'b * 'c) writer3
  val bin_write_list : ('a, 'a list) writer1
  val bin_write_array : ('a, 'a array) writer1
  val bin_write_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t) writer2
  val bin_write_float32_vec : vec32 writer
  val bin_write_float64_vec : vec64 writer
  val bin_write_vec : vec writer
  val bin_write_float32_mat : mat32 writer
  val bin_write_float64_mat : mat64 writer
  val bin_write_mat : mat writer
  val bin_write_bigstring : buf writer
  val bin_write_floatarray : floatarray writer
  val bin_write_md5 : Md5_lib.t writer

  (** [bin_write_variant_int] writes out the exact little-endian bit
      representation of the variant tag of the given value (= 32 bits). *)
  val bin_write_variant_int : int writer

  (** [bin_write_int_8bit] writes out the exact little-endian bit representation
      of the given [int] value using the lower 8 bits. *)
  val bin_write_int_8bit : int writer

  (** [bin_write_int_16bit] writes out the exact little-endian bit representation
      of the given [int] value using the lower 16 bits. *)
  val bin_write_int_16bit : int writer

  (** [bin_write_int_32bit] writes out the exact little-endian bit representation
      of the given [int] value using the lower 32 bits. *)
  val bin_write_int_32bit : int writer

  (** [bin_write_int_64bit] writes out the exact little-endian bit representation
      of the given [int] value using all 64 bits.  On 32bit platforms negative
      numbers will be sign-extended in the 64bit representation. *)
  val bin_write_int_64bit : int writer

  (** [bin_write_int64_bits] writes out the exact little-endian bit
      representation of the given [int64] value. *)
  val bin_write_int64_bits : int64 writer

  (** [bin_write_network16_int] writes out an integer in 16bit network
      byte order (= big-endian). *)
  val bin_write_network16_int : int writer

  (** [bin_write_network32_int] writes out an integer in 32bit network
      byte order (= big-endian). *)
  val bin_write_network32_int : int writer

  (** [bin_write_network32_int32] writes out a 32bit integer in 32bit
      network byte order (= big-endian). *)
  val bin_write_network32_int32 : int32 writer

  (** [bin_write_network64_int] writes out an integer in 64bit network
      byte order (= big-endian). *)
  val bin_write_network64_int : int writer

  (** [bin_write_network64_int64] writes out a 64bit integer in 64bit
      network byte order (= big-endian). *)
  val bin_write_network64_int64 : int64 writer

  (* Local versions *)

  val bin_write_unit__local : unit writer_local
  val bin_write_bool__local : bool writer_local
  val bin_write_string__local : string writer_local
  val bin_write_bytes__local : bytes writer_local
  val bin_write_char__local : char writer_local
  val bin_write_int__local : int writer_local
  val bin_write_nat0__local : Nat0.t writer_local
  val bin_write_float__local : float writer_local
  val bin_write_int32__local : int32 writer_local
  val bin_write_int64__local : int64 writer_local
  val bin_write_nativeint__local : nativeint writer_local
  val bin_write_ref__local : ('a, 'a ref) writer_local1
  val bin_write_lazy__local : ('a, 'a lazy_t) writer_local1
  val bin_write_option__local : ('a, 'a option) writer_local1
  val bin_write_pair__local : ('a, 'b, 'a * 'b) writer_local2
  val bin_write_triple__local : ('a, 'b, 'c, 'a * 'b * 'c) writer_local3
  val bin_write_list__local : ('a, 'a list) writer_local1
  val bin_write_array__local : ('a, 'a array) writer_local1
  val bin_write_float32_vec__local : vec32 writer_local
  val bin_write_float64_vec__local : vec64 writer_local
  val bin_write_vec__local : vec writer_local
  val bin_write_float32_mat__local : mat32 writer_local
  val bin_write_float64_mat__local : mat64 writer_local
  val bin_write_mat__local : mat writer_local
  val bin_write_bigstring__local : buf writer_local
  val bin_write_floatarray__local : floatarray writer_local
  val bin_write_md5__local : Md5_lib.t writer_local

  (** [bin_write_variant_int__local] writes out the exact little-endian bit
      representation of the variant tag of the given value (= 32 bits). *)
  val bin_write_variant_int__local : int writer_local

  (** [bin_write_int_8bit__local] writes out the exact little-endian bit representation
      of the given [int] value using the lower 8 bits. *)
  val bin_write_int_8bit__local : int writer_local

  (** [bin_write_int_16bit__local] writes out the exact little-endian bit representation
      of the given [int] value using the lower 16 bits. *)
  val bin_write_int_16bit__local : int writer_local

  (** [bin_write_int_32bit__local] writes out the exact little-endian bit representation
      of the given [int] value using the lower 32 bits. *)
  val bin_write_int_32bit__local : int writer_local

  (** [bin_write_int_64bit__local] writes out the exact little-endian bit representation
      of the given [int] value using all 64 bits.  On 32bit platforms negative
      numbers will be sign-extended in the 64bit representation. *)
  val bin_write_int_64bit__local : int writer_local

  (** [bin_write_int64_bits__local] writes out the exact little-endian bit
      representation of the given [int64] value. *)
  val bin_write_int64_bits__local : int64 writer_local

  (** [bin_write_network16_int__local] writes out an integer in 16bit network
      byte order (= big-endian). *)
  val bin_write_network16_int__local : int writer_local

  (** [bin_write_network32_int__local] writes out an integer in 32bit network
      byte order (= big-endian). *)
  val bin_write_network32_int__local : int writer_local

  (** [bin_write_network32_int32__local] writes out a 32bit integer in 32bit
      network byte order (= big-endian). *)
  val bin_write_network32_int32__local : int32 writer_local

  (** [bin_write_network64_int__local] writes out an integer in 64bit network
      byte order (= big-endian). *)
  val bin_write_network64_int__local : int writer_local

  (** [bin_write_network64_int64__local] writes out a 64bit integer in 64bit
      network byte order (= big-endian). *)
  val bin_write_network64_int64__local : int64 writer_local
end
