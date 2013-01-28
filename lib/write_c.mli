(** Wrapping unsafe C-style writers to safe ML-style ones. *)

open Common
open Unsafe_common
open Write_ml

(** {2 Generic functions for easy creation of wrappers} *)

val unsafe_get_init : buf -> pos : pos -> sptr * sptr * eptr
(** [unsafe_get_init buf ~pos] @return the triple [(start, sptr, eptr)]
    where [start] is the pointer to the start of buffer [buf], [sptr]
    the pointer to the position [pos] in [buf], and [eptr] the pointer to
    the end of the buffer.  NOTE: you must make sure that [buf] remains
    unreclaimed as long as any of the three pointers is accessible! *)

val make : 'a Unsafe_write_c.writer -> 'a Write_ml.writer
(** [make c_writer] takes an unsafe C-style writer [c_writer].
    @return a safe ML-style writer. *)

val make1 : ('a, 'b) Unsafe_write_c.writer1 -> ('a, 'b) Write_ml.writer1
(** [make1 mk_c_writer ml_el_writer] takes a higher-order C-style writer
    [mk_c_writer] and an ML-writer [ml_el_writer] that operates on the
    same type as the argument of the C-style writer.  @return ML-style
    writer for the higher-order type. *)

val make2 : ('a, 'b, 'c) Unsafe_write_c.writer2 -> ('a, 'b, 'c) Write_ml.writer2
(** [make2 mk_c_writer ml_el1_writer ml_el2_writer] like {!make1} but
    operates on unsafe C-style write functions for types with two type
    parameters. *)

val make3 :
  ('a, 'b, 'c, 'd) Unsafe_write_c.writer3 -> ('a, 'b, 'c, 'd) Write_ml.writer3
(** [make3 mk_c_writer ml_el1_writer ml_el2_writer ml_el3_writer] like
    {!make1} but operates on unsafe C-style write functions for types
    with three type parameters. *)

val unmake :
  'a Write_ml.writer -> buf -> start : sptr -> 'a Unsafe_write_c.writer
(** [unmake ml_writer buf ~start] takes an ML-style writer [ml_writer], a
    buffer, and the pointer [start] to the start of the buffer.  This
    function can be used to wrap higher-order type conversion functions
    and, together with {!unsafe_get_init}, is used in e.g. {!make1},
    {!make2} and {!make3} for that purpose.  @return an unsafe C-style
    writer. *)


(** {2 Unsafe C-style writers for basic types wrapped as ML-style writers} *)

val bin_write_unit : unit writer
val bin_write_bool : bool writer
val bin_write_string : string writer
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
val bin_write_float_array : float array writer
val bin_write_variant_tag : [> ] writer
val bin_write_array_no_length : ('a, 'a array) writer1
val bin_write_int_64bit : int writer
val bin_write_int64_bits : int64 writer
val bin_write_network16_int : int writer
val bin_write_network32_int : int writer
val bin_write_network32_int32 : int32 writer
val bin_write_network64_int : int writer
val bin_write_network64_int64 : int64 writer
