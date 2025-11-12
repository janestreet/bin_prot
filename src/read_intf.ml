open! Base

module Definitions = struct
  open Common

  [%%template
  [@@@mode.default m = (global, local)]

  (** Type of reader functions for the binary protocol. They take a buffer and a reference
      to a read position, and return the unmarshalled value. The next buffer position
      after reading in the value will be stored in the position reference. *)
  type ('a : any) reader = buf @ local -> pos_ref:pos_ref @ local -> 'a @ m

  type ('a : any, 'b : any) reader1 = ('a reader[@mode m]) -> ('b reader[@mode m])

  type ('a : any, 'b : any, 'c : any) reader2 =
    ('a reader[@mode m]) -> (('b, 'c) reader1[@mode m])

  type ('a : any, 'b : any, 'c : any, 'd : any) reader3 =
    ('a reader[@mode m]) -> (('b, 'c, 'd) reader2[@mode m])]

  (** Type of reader functions for polymorphic variant readers, after reading their tag.
      Used for definitions such as [__bin_read_t__]. The [int] argument is a numerical
      representation of the variant tag, such as [`a]. *)
  type ('a : any) vtag_reader = buf @ local -> pos_ref:pos_ref @ local -> int -> 'a

  type ('a : any, 'b : any) vtag_reader1 = 'a reader -> 'b vtag_reader
  type ('a : any, 'b : any, 'c : any) vtag_reader2 = 'a reader -> ('b, 'c) vtag_reader1

  type ('a : any, 'b : any, 'c : any, 'd : any) vtag_reader3 =
    'a reader -> ('b, 'c, 'd) vtag_reader2
end

module type Read = sig @@ portable
  (** Reading values from the binary protocol using (mostly) OCaml. *)

  open Common

  include module type of struct
    include Definitions
  end

  [%%template:
  [@@@mode m = (global, local)]

  type ('a : any) global_reader := ('a reader[@mode global])
  type ('a : any) reader := ('a reader[@mode m])
  type ('a : any, 'b : any) reader1 := (('a, 'b) reader1[@mode m])
  type ('a : any, 'b : any, 'c : any) reader2 := (('a, 'b, 'c) reader2[@mode m])

  type ('a : any, 'b : any, 'c : any, 'd : any) reader3 :=
    (('a, 'b, 'c, 'd) reader3[@mode m])

  [@@@mode.default m]

  val bin_read_unit : unit reader [@@zero_alloc arity 2]
  val bin_read_bool : bool reader [@@zero_alloc arity 2]
  val bin_read_string : string reader [@@zero_alloc_if_local m opt arity 2]
  val bin_read_bytes : bytes reader [@@zero_alloc_if_local m opt arity 2]
  val bin_read_char : char reader [@@zero_alloc arity 2]
  val bin_read_int : int reader [@@zero_alloc arity 2]
  val bin_read_nat0 : Nat0.t reader [@@zero_alloc opt arity 2]
  val bin_read_float : float reader [@@zero_alloc_if_local m arity 2]
  val bin_read_int32 : int32 reader [@@zero_alloc_if_local m arity 2]
  val bin_read_int64 : int64 reader [@@zero_alloc_if_local m arity 2]
  val bin_read_nativeint : nativeint reader [@@zero_alloc_if_local m opt arity 2]

  (* Note: since the contents of a [ref] must always be global, this takes a global reader
     rather than a local one *)
  val bin_read_ref : ('a : value_or_null). 'a global_reader -> 'a ref reader
  val bin_read_option : ('a : value_or_null). ('a, 'a option) reader1
  val bin_read_pair : ('a, 'b, 'a * 'b) reader2
  val bin_read_triple : ('a, 'b, 'c, 'a * 'b * 'c) reader3
  val bin_read_list : ('a : value_or_null). ('a, 'a list) reader1

  (* Note: since the contents of an [array] must always be global, this takes a global
     reader rather than a local one *)
  val bin_read_array : 'a global_reader -> 'a array reader
  val bin_read_iarray : 'a reader -> 'a iarray reader
  val bin_read_float32_vec : vec32 reader
  val bin_read_float64_vec : vec64 reader
  val bin_read_vec : vec reader
  val bin_read_float32_mat : mat32 reader
  val bin_read_float64_mat : mat64 reader
  val bin_read_mat : mat reader
  val bin_read_bigstring : buf reader
  val bin_read_variant_int : int reader [@@zero_alloc arity 2]
  val bin_read_int_8bit : int reader [@@zero_alloc arity 2]
  val bin_read_int_16bit : int reader [@@zero_alloc arity 2]
  val bin_read_int_32bit : int reader [@@zero_alloc arity 2]
  val bin_read_int_64bit : int reader [@@zero_alloc arity 2]
  val bin_read_int32_bits : int32 reader [@@zero_alloc_if_local m arity 2]
  val bin_read_int64_bits : int64 reader [@@zero_alloc_if_local m arity 2]
  val bin_read_network16_int : int reader [@@zero_alloc arity 2]
  val bin_read_network32_int : int reader [@@zero_alloc arity 2]
  val bin_read_network32_int32 : int32 reader [@@zero_alloc_if_local m arity 2]
  val bin_read_network64_int : int reader [@@zero_alloc arity 2]
  val bin_read_network64_int64 : int64 reader [@@zero_alloc_if_local m arity 2]
  val bin_read_md5 : Md5_lib.t reader [@@zero_alloc_if_local m opt arity 2]

  (** Fail early if the list is larger than [max_len]. *)
  val bin_read_list_with_max_len : max_len:int -> ('a, 'a list) reader1]

  val bin_read_lazy : ('a, 'a lazy_t) reader1

  val bin_read_bigarray1
    :  kind:('a, 'k) Stdlib.Bigarray.kind
    -> layout:'layout Stdlib.Bigarray.layout
    -> ('a, 'k, 'layout) Stdlib.Bigarray.Array1.t reader

  val bin_read_bigarray2
    :  kind:('a, 'k) Stdlib.Bigarray.kind
    -> layout:'layout Stdlib.Bigarray.layout
    -> ('a, 'k, 'layout) Stdlib.Bigarray.Array2.t reader

  val bin_read_floatarray : floatarray reader
end
