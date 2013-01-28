(** Wrapping unsafe C-style readers to safe ML-style ones. *)

open Common
open Unsafe_common
open Read_ml

(** {2 Generic functions for easy creation of wrappers} *)

val make : 'a Unsafe_read_c.reader -> 'a Read_ml.reader
(** [make c_reader] takes an unsafe C-style reader [c_reader].
    @return a safe ML-style reader. *)

val make1 : ('a, 'b) Unsafe_read_c.reader1 -> ('a, 'b) Read_ml.reader1
(** [make1 mk_c_reader ml_el_reader] takes a higher-order C-style reader
    [mk_c_reader] and an ML-reader [ml_el_reader] that operates on the
    same type as the argument of the C-style reader.  @return ML-style
    reader for the higher-order type. *)

val make2 : ('a, 'b, 'c) Unsafe_read_c.reader2 -> ('a, 'b, 'c) Read_ml.reader2
(** [make2 mk_c_reader ml_el1_reader ml_el2_reader] like {!make1} but
    operates on unsafe C-style write functions for types with two type
    parameters. *)

val make3 :
  ('a, 'b, 'c, 'd) Unsafe_read_c.reader3 -> ('a, 'b, 'c, 'd) Read_ml.reader3
(** [make3 mk_c_reader ml_el1_reader ml_el2_reader ml_el3_reader] like
    {!make1} but operates on unsafe C-style write functions for types
    with three type parameters. *)

val unmake : 'b Read_ml.reader -> buf -> 'b Unsafe_read_c.reader
(** [unmake ml_reader buf] takes an ML-style reader [ml_reader] and a
    buffer.  This function can be used to wrap higher-order type
    conversion functions and, together with {!Unsafe_common.get_sptr_ptr},
    {Unsafe_common.!set_sptr_ptr} and {!handle_error}, is used in
    e.g. {!make1}, {!make2} and {!make3} for that purpose.  @return an
    unsafe C-style reader. *)

val handle_error : buf -> sptr_ptr -> ReadError.t -> 'a
(** [handle_error buf sptr_ptr err] deallocates [sptr_ptr] for buffer
    [buf].  @raise ReadError with the appropriate location information
    and [err] then. *)

val handle_exc : buf -> sptr_ptr -> exn -> 'a
(** [handle_exc buf sptr_ptr exc] deallocates [sptr_ptr] for buffer
    [buf].  raise exc then. *)

val at_end : buf -> sptr_ptr -> pos_ref -> 'a -> 'a
(** [at_end buf sptr_ptr pos_ref el] deallocates [sptr_ptr] for buffer
    [buf], sets [pos_ref] to the new position, then returns [el]. *)


(** {2 Unsafe C-style readers for basic types wrapped as ML-style readers} *)

val bin_read_unit : unit reader
val bin_read_bool : bool reader
val bin_read_string : string reader
val bin_read_char : char reader
val bin_read_int : int reader
val bin_read_nat0 : Nat0.t reader
val bin_read_float : float reader
val bin_read_int32 : int32 reader
val bin_read_int64 : int64 reader
val bin_read_nativeint : nativeint reader
val bin_read_ref : ('a, 'a ref) reader1
val bin_read_lazy : ('a, 'a lazy_t) reader1
val bin_read_option : ('a, 'a option) reader1
val bin_read_pair : ('a, 'b, 'a * 'b) reader2
val bin_read_triple : ('a, 'b, 'c, 'a * 'b * 'c) reader3
val bin_read_list : ('a, 'a list) reader1
val bin_read_array : ('a, 'a array) reader1
val bin_read_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t) reader2
val bin_read_float32_vec : vec32 reader
val bin_read_float64_vec : vec64 reader
val bin_read_vec : vec reader
val bin_read_float32_mat : mat32 reader
val bin_read_float64_mat : mat64 reader
val bin_read_mat : mat reader
val bin_read_bigstring : buf reader
val bin_read_float_array : float array reader
val bin_read_variant_int : int reader
val bin_read_variant_tag : [> ] reader
val bin_read_int_64bit : int reader
val bin_read_int64_bits : int64 reader
val bin_read_network16_int : int reader
val bin_read_network32_int : int reader
val bin_read_network32_int32 : int32 reader
val bin_read_network64_int : int reader
val bin_read_network64_int64 : int64 reader
