module Definitions = struct
  type 'a writer =
    { size : 'a Size.sizer
    ; write : 'a Write.writer
    }

  type ('a, 'ctx) reader =
    { read : ('a, 'ctx) Read.reader
    ; vtag_read : (int -> 'a, 'ctx) Read.reader
    }

  type ('a, 'ctx) t =
    { shape : Shape.t
    ; writer : 'a writer
    ; reader : ('a, 'ctx) reader
    }

  type 'a writer0 = 'a writer
  type ('a, 'ctx) reader0 = ('a, 'ctx) reader
  type ('a, 'ctx) t0 = ('a, 'ctx) t

  module S1 = struct
    type ('a, 'b) writer = 'a writer0 -> 'b writer0
    type ('a, 'b, 'ctx) reader = ('a, 'ctx) reader0 -> ('b, 'ctx) reader0
    type ('a, 'b, 'ctx) t = ('a, 'ctx) t0 -> ('b, 'ctx) t0
  end

  module S2 = struct
    type ('a, 'b, 'c) writer = 'a writer0 -> ('b, 'c) S1.writer
    type ('a, 'b, 'c, 'ctx) reader = ('a, 'ctx) reader0 -> ('b, 'c, 'ctx) S1.reader
    type ('a, 'b, 'c, 'ctx) t = ('a, 'ctx) t0 -> ('b, 'c, 'ctx) S1.t
  end

  module S3 = struct
    type ('a, 'b, 'c, 'd) writer = 'a writer0 -> ('b, 'c, 'd) S2.writer
    type ('a, 'b, 'c, 'd, 'ctx) reader = ('a, 'ctx) reader0 -> ('b, 'c, 'd, 'ctx) S2.reader
    type ('a, 'b, 'c, 'd, 'ctx) t = ('a, 'ctx) t0 -> ('b, 'c, 'd, 'ctx) S2.t
  end
end

module type Type_class = sig
  (** Sizers, writers, and readers in records *)

  open Common

  include module type of struct
    include Definitions
  end

  (*$ open Bin_prot_cinaps.Sig *)
  (*$ mk_base "unit" *)
  val bin_writer_unit : unit writer
  val bin_reader_unit : (unit, 'ctx) reader
  val bin_shape_unit : Shape.t
  val bin_unit : (unit, 'ctx) t

  (*$ mk_base "bool" *)
  val bin_writer_bool : bool writer
  val bin_reader_bool : (bool, 'ctx) reader
  val bin_shape_bool : Shape.t
  val bin_bool : (bool, 'ctx) t

  (*$ mk_base "string" *)
  val bin_writer_string : string writer
  val bin_reader_string : (string, 'ctx) reader
  val bin_shape_string : Shape.t
  val bin_string : (string, 'ctx) t

  (*$ mk_base "bytes" *)
  val bin_writer_bytes : bytes writer
  val bin_reader_bytes : (bytes, 'ctx) reader
  val bin_shape_bytes : Shape.t
  val bin_bytes : (bytes, 'ctx) t

  (*$ mk_base "char" *)
  val bin_writer_char : char writer
  val bin_reader_char : (char, 'ctx) reader
  val bin_shape_char : Shape.t
  val bin_char : (char, 'ctx) t

  (*$ mk_base "int" *)
  val bin_writer_int : int writer
  val bin_reader_int : (int, 'ctx) reader
  val bin_shape_int : Shape.t
  val bin_int : (int, 'ctx) t

  (*$ mk_base "float" *)
  val bin_writer_float : float writer
  val bin_reader_float : (float, 'ctx) reader
  val bin_shape_float : Shape.t
  val bin_float : (float, 'ctx) t

  (*$ mk_base "int32" *)
  val bin_writer_int32 : int32 writer
  val bin_reader_int32 : (int32, 'ctx) reader
  val bin_shape_int32 : Shape.t
  val bin_int32 : (int32, 'ctx) t

  (*$ mk_base "int64" *)
  val bin_writer_int64 : int64 writer
  val bin_reader_int64 : (int64, 'ctx) reader
  val bin_shape_int64 : Shape.t
  val bin_int64 : (int64, 'ctx) t

  (*$ mk_base "nativeint" *)
  val bin_writer_nativeint : nativeint writer
  val bin_reader_nativeint : (nativeint, 'ctx) reader
  val bin_shape_nativeint : Shape.t
  val bin_nativeint : (nativeint, 'ctx) t

  (*$ mk_base_tp "nat0" "Nat0.t" *)
  val bin_writer_nat0 : Nat0.t writer
  val bin_reader_nat0 : (Nat0.t, 'ctx) reader
  val bin_shape_nat0 : Shape.t
  val bin_nat0 : (Nat0.t, 'ctx) t

  (*$ mk_base1 "ref" *)
  val bin_writer_ref : ('a, 'a ref) S1.writer
  val bin_reader_ref : ('a, 'a ref, 'ctx) S1.reader
  val bin_shape_ref : Shape.t -> Shape.t
  val bin_ref : ('a, 'a ref, 'ctx) S1.t

  (*$ mk_base1_tp "lazy" "lazy_t" *)
  val bin_writer_lazy : ('a, 'a lazy_t) S1.writer
  val bin_reader_lazy : ('a, 'a lazy_t, 'ctx) S1.reader
  val bin_shape_lazy : Shape.t -> Shape.t
  val bin_lazy : ('a, 'a lazy_t, 'ctx) S1.t

  (*$ mk_base1 "option" *)
  val bin_writer_option : ('a, 'a option) S1.writer
  val bin_reader_option : ('a, 'a option, 'ctx) S1.reader
  val bin_shape_option : Shape.t -> Shape.t
  val bin_option : ('a, 'a option, 'ctx) S1.t
  (*$*)

  val bin_writer_pair : ('a, 'b, 'a * 'b) S2.writer
  val bin_reader_pair : ('a, 'b, 'a * 'b, 'ctx) S2.reader
  val bin_pair : ('a, 'b, 'a * 'b, 'ctx) S2.t
  val bin_writer_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.writer
  val bin_reader_triple : ('a, 'b, 'c, 'a * 'b * 'c, 'ctx) S3.reader
  val bin_triple : ('a, 'b, 'c, 'a * 'b * 'c, 'ctx) S3.t

  (*$ mk_base1 "list" *)
  val bin_writer_list : ('a, 'a list) S1.writer
  val bin_reader_list : ('a, 'a list, 'ctx) S1.reader
  val bin_shape_list : Shape.t -> Shape.t
  val bin_list : ('a, 'a list, 'ctx) S1.t

  (*$ mk_base1 "array" *)
  val bin_writer_array : ('a, 'a array) S1.writer
  val bin_reader_array : ('a, 'a array, 'ctx) S1.reader
  val bin_shape_array : Shape.t -> Shape.t
  val bin_array : ('a, 'a array, 'ctx) S1.t

  (*$ mk_base2_tp "hashtbl" "Hashtbl.t" *)
  val bin_writer_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t) S2.writer
  val bin_reader_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t, 'ctx) S2.reader
  val bin_shape_hashtbl : Shape.t -> Shape.t -> Shape.t
  val bin_hashtbl : ('a, 'b, ('a, 'b) Hashtbl.t, 'ctx) S2.t

  (*$ mk_base_tp "float32_vec" "vec32" *)
  val bin_writer_float32_vec : vec32 writer
  val bin_reader_float32_vec : (vec32, 'ctx) reader
  val bin_shape_float32_vec : Shape.t
  val bin_float32_vec : (vec32, 'ctx) t

  (*$ mk_base_tp "float64_vec" "vec64" *)
  val bin_writer_float64_vec : vec64 writer
  val bin_reader_float64_vec : (vec64, 'ctx) reader
  val bin_shape_float64_vec : Shape.t
  val bin_float64_vec : (vec64, 'ctx) t

  (*$ mk_base "vec" *)
  val bin_writer_vec : vec writer
  val bin_reader_vec : (vec, 'ctx) reader
  val bin_shape_vec : Shape.t
  val bin_vec : (vec, 'ctx) t

  (*$ mk_base_tp "float32_mat" "mat32" *)
  val bin_writer_float32_mat : mat32 writer
  val bin_reader_float32_mat : (mat32, 'ctx) reader
  val bin_shape_float32_mat : Shape.t
  val bin_float32_mat : (mat32, 'ctx) t

  (*$ mk_base_tp "float64_mat" "mat64" *)
  val bin_writer_float64_mat : mat64 writer
  val bin_reader_float64_mat : (mat64, 'ctx) reader
  val bin_shape_float64_mat : Shape.t
  val bin_float64_mat : (mat64, 'ctx) t

  (*$ mk_base "mat" *)
  val bin_writer_mat : mat writer
  val bin_reader_mat : (mat, 'ctx) reader
  val bin_shape_mat : Shape.t
  val bin_mat : (mat, 'ctx) t

  (*$ mk_base_tp "bigstring" "buf" *)
  val bin_writer_bigstring : buf writer
  val bin_reader_bigstring : (buf, 'ctx) reader
  val bin_shape_bigstring : Shape.t
  val bin_bigstring : (buf, 'ctx) t
  (*$*)

  (*$ mk_base "floatarray" *)
  val bin_writer_floatarray : floatarray writer
  val bin_reader_floatarray : (floatarray, 'ctx) reader
  val bin_shape_floatarray : Shape.t
  val bin_floatarray : (floatarray, 'ctx) t
  (*$*)

  val bin_writer_variant_int : int writer
  val bin_reader_variant_int : (int, 'ctx) reader
  val bin_variant_int : (int, 'ctx) t

  (*$ mk_base_tp "int_8bit" "int" *)
  val bin_writer_int_8bit : int writer
  val bin_reader_int_8bit : (int, 'ctx) reader
  val bin_shape_int_8bit : Shape.t
  val bin_int_8bit : (int, 'ctx) t

  (*$ mk_base_tp "int_16bit" "int" *)
  val bin_writer_int_16bit : int writer
  val bin_reader_int_16bit : (int, 'ctx) reader
  val bin_shape_int_16bit : Shape.t
  val bin_int_16bit : (int, 'ctx) t

  (*$ mk_base_tp "int_32bit" "int" *)
  val bin_writer_int_32bit : int writer
  val bin_reader_int_32bit : (int, 'ctx) reader
  val bin_shape_int_32bit : Shape.t
  val bin_int_32bit : (int, 'ctx) t

  (*$ mk_base_tp "int_64bit" "int" *)
  val bin_writer_int_64bit : int writer
  val bin_reader_int_64bit : (int, 'ctx) reader
  val bin_shape_int_64bit : Shape.t
  val bin_int_64bit : (int, 'ctx) t

  (*$ mk_base_tp "int64_bits" "int64" *)
  val bin_writer_int64_bits : int64 writer
  val bin_reader_int64_bits : (int64, 'ctx) reader
  val bin_shape_int64_bits : Shape.t
  val bin_int64_bits : (int64, 'ctx) t

  (*$ mk_base_tp "network16_int" "int" *)
  val bin_writer_network16_int : int writer
  val bin_reader_network16_int : (int, 'ctx) reader
  val bin_shape_network16_int : Shape.t
  val bin_network16_int : (int, 'ctx) t

  (*$ mk_base_tp "network32_int" "int" *)
  val bin_writer_network32_int : int writer
  val bin_reader_network32_int : (int, 'ctx) reader
  val bin_shape_network32_int : Shape.t
  val bin_network32_int : (int, 'ctx) t

  (*$ mk_base_tp "network32_int32" "int32" *)
  val bin_writer_network32_int32 : int32 writer
  val bin_reader_network32_int32 : (int32, 'ctx) reader
  val bin_shape_network32_int32 : Shape.t
  val bin_network32_int32 : (int32, 'ctx) t

  (*$ mk_base_tp "network64_int" "int" *)
  val bin_writer_network64_int : int writer
  val bin_reader_network64_int : (int, 'ctx) reader
  val bin_shape_network64_int : Shape.t
  val bin_network64_int : (int, 'ctx) t

  (*$ mk_base_tp "network64_int64" "int64" *)
  val bin_writer_network64_int64 : int64 writer
  val bin_reader_network64_int64 : (int64, 'ctx) reader
  val bin_shape_network64_int64 : Shape.t
  val bin_network64_int64 : (int64, 'ctx) t
  (*$*)

  (** Conversion of binable types *)

  val cnv_writer : ('a -> 'b) -> 'b writer -> 'a writer
  val cnv_reader : ('b -> 'a) -> ('b, 'ctx) reader -> ('a, 'ctx) reader
  val cnv : (Shape.t -> Shape.t) -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'ctx) t -> ('a, 'ctx) t
end
