module Definitions = struct
  open Common

  (** Type of reader functions for the binary protocol.  They take a
      buffer and a reference to a read position, and return the unmarshalled
      value.  The next buffer position after reading in the value will be
      stored in the position reference. *)
  type 'a reader = buf -> pos_ref:pos_ref -> 'a

  type ('a, 'b) reader1 = 'a reader -> 'b reader
  type ('a, 'b, 'c) reader2 = 'a reader -> ('b, 'c) reader1
  type ('a, 'b, 'c, 'd) reader3 = 'a reader -> ('b, 'c, 'd) reader2

  (** Type of reader functions for polymorphic variant readers, after reading their tag.
      Used for definitions such as [__bin_read_t__]. The [int] argument is a numerical
      representation of the variant tag, such as [`a]. *)
  type 'a vtag_reader = buf -> pos_ref:pos_ref -> int -> 'a

  type ('a, 'b) vtag_reader1 = 'a reader -> 'b vtag_reader
  type ('a, 'b, 'c) vtag_reader2 = 'a reader -> ('b, 'c) vtag_reader1
  type ('a, 'b, 'c, 'd) vtag_reader3 = 'a reader -> ('b, 'c, 'd) vtag_reader2
end

module type Read = sig
  (** Reading values from the binary protocol using (mostly) OCaml. *)

  open Common

  include module type of struct
    include Definitions
  end

  val bin_read_unit : unit reader
  val bin_read_bool : bool reader
  val bin_read_string : string reader
  val bin_read_bytes : bytes reader
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
  val bin_read_floatarray : floatarray reader
  val bin_read_variant_int : int reader
  val bin_read_int_8bit : int reader
  val bin_read_int_16bit : int reader
  val bin_read_int_32bit : int reader
  val bin_read_int_64bit : int reader
  val bin_read_int64_bits : int64 reader
  val bin_read_network16_int : int reader
  val bin_read_network32_int : int reader
  val bin_read_network32_int32 : int32 reader
  val bin_read_network64_int : int reader
  val bin_read_network64_int64 : int64 reader
  val bin_read_md5 : Md5_lib.t reader

  (** Fail early if the list is larger than [max_len]. *)
  val bin_read_list_with_max_len : max_len:int -> ('a, 'a list) reader1
end
