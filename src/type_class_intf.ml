open! Base

module Definitions = struct
  type ('a : any) writer =
    { size : 'a Size.sizer
    ; write : 'a Write.writer
    }

  type ('a : any) reader =
    { read : 'a Read.reader
    ; vtag_read : 'a Read.vtag_reader
    }

  type ('a : any) t =
    { shape : Shape.t
    ; writer : 'a writer
    ; reader : 'a reader
    }

  type ('a : any) writer0 = 'a writer
  type ('a : any) reader0 = 'a reader
  type ('a : any) t0 = 'a t

  module S1 = struct
    type ('a : any, 'b : any) writer = 'a writer0 -> 'b writer0
    type ('a : any, 'b : any) reader = 'a reader0 -> 'b reader0
    type ('a : any, 'b : any) t = 'a t0 -> 'b t0
  end

  module S2 = struct
    type ('a : any, 'b : any, 'c : any) writer = 'a writer0 -> ('b, 'c) S1.writer
    type ('a : any, 'b : any, 'c : any) reader = 'a reader0 -> ('b, 'c) S1.reader
    type ('a : any, 'b : any, 'c : any) t = 'a t0 -> ('b, 'c) S1.t
  end

  module S3 = struct
    type ('a : any, 'b : any, 'c : any, 'd : any) writer =
      'a writer0 -> ('b, 'c, 'd) S2.writer

    type ('a : any, 'b : any, 'c : any, 'd : any) reader =
      'a reader0 -> ('b, 'c, 'd) S2.reader

    type ('a : any, 'b : any, 'c : any, 'd : any) t = 'a t0 -> ('b, 'c, 'd) S2.t
  end
end

module type Type_class = sig @@ portable
  (** Sizers, writers, and readers in records *)

  open Common

  include module type of struct
    include Definitions
  end

  (*$ open Bin_prot_cinaps.Sig *)
  (*$ mk_base "unit" *)
  val bin_writer_unit : unit writer
  val bin_reader_unit : unit reader
  val bin_shape_unit : Shape.t
  val bin_unit : unit t

  (*$ mk_base "bool" *)
  val bin_writer_bool : bool writer
  val bin_reader_bool : bool reader
  val bin_shape_bool : Shape.t
  val bin_bool : bool t

  (*$ mk_base "string" *)
  val bin_writer_string : string writer
  val bin_reader_string : string reader
  val bin_shape_string : Shape.t
  val bin_string : string t

  (*$ mk_base "bytes" *)
  val bin_writer_bytes : bytes writer
  val bin_reader_bytes : bytes reader
  val bin_shape_bytes : Shape.t
  val bin_bytes : bytes t

  (*$ mk_base "char" *)
  val bin_writer_char : char writer
  val bin_reader_char : char reader
  val bin_shape_char : Shape.t
  val bin_char : char t

  (*$ mk_base "int" *)
  val bin_writer_int : int writer
  val bin_reader_int : int reader
  val bin_shape_int : Shape.t
  val bin_int : int t

  (*$ mk_base "float" *)
  val bin_writer_float : float writer
  val bin_reader_float : float reader
  val bin_shape_float : Shape.t
  val bin_float : float t

  (*$ mk_base "int32" *)
  val bin_writer_int32 : int32 writer
  val bin_reader_int32 : int32 reader
  val bin_shape_int32 : Shape.t
  val bin_int32 : int32 t

  (*$ mk_base "int64" *)
  val bin_writer_int64 : int64 writer
  val bin_reader_int64 : int64 reader
  val bin_shape_int64 : Shape.t
  val bin_int64 : int64 t

  (*$ mk_base "nativeint" *)
  val bin_writer_nativeint : nativeint writer
  val bin_reader_nativeint : nativeint reader
  val bin_shape_nativeint : Shape.t
  val bin_nativeint : nativeint t

  (*$ mk_base_tp "nat0" "Nat0.t" *)
  val bin_writer_nat0 : Nat0.t writer
  val bin_reader_nat0 : Nat0.t reader
  val bin_shape_nat0 : Shape.t
  val bin_nat0 : Nat0.t t

  (*$ mk_base1 ~layout:"value_or_null" "ref" *)
  val bin_writer_ref : ('a : value_or_null). ('a, 'a ref) S1.writer
  val bin_reader_ref : ('a : value_or_null). ('a, 'a ref) S1.reader
  val bin_shape_ref : Shape.t -> Shape.t
  val bin_ref : ('a : value_or_null). ('a, 'a ref) S1.t

  (*$ mk_base1_tp "lazy" "lazy_t" *)
  val bin_writer_lazy : ('a, 'a lazy_t) S1.writer
  val bin_reader_lazy : ('a, 'a lazy_t) S1.reader
  val bin_shape_lazy : Shape.t -> Shape.t
  val bin_lazy : ('a, 'a lazy_t) S1.t

  (*$ mk_base1 ~layout:"value_or_null" "option" *)
  val bin_writer_option : ('a : value_or_null). ('a, 'a option) S1.writer
  val bin_reader_option : ('a : value_or_null). ('a, 'a option) S1.reader
  val bin_shape_option : Shape.t -> Shape.t
  val bin_option : ('a : value_or_null). ('a, 'a option) S1.t

  (*$ mk_base1 "or_null" *)
  val bin_writer_or_null : ('a, 'a or_null) S1.writer
  val bin_reader_or_null : ('a, 'a or_null) S1.reader
  val bin_shape_or_null : Shape.t -> Shape.t
  val bin_or_null : ('a, 'a or_null) S1.t
  (*$*)

  val bin_writer_pair : ('a, 'b, 'a * 'b) S2.writer
  val bin_reader_pair : ('a, 'b, 'a * 'b) S2.reader
  val bin_pair : ('a, 'b, 'a * 'b) S2.t
  val bin_writer_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.writer
  val bin_reader_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.reader
  val bin_triple : ('a, 'b, 'c, 'a * 'b * 'c) S3.t

  (*$ mk_base1 ~layout:"value_or_null" "list" *)
  val bin_writer_list : ('a : value_or_null). ('a, 'a list) S1.writer
  val bin_reader_list : ('a : value_or_null). ('a, 'a list) S1.reader
  val bin_shape_list : Shape.t -> Shape.t
  val bin_list : ('a : value_or_null). ('a, 'a list) S1.t

  (*$ mk_base1 ~layout:"value_or_null mod separable" "array" *)
  val bin_writer_array : ('a : value_or_null mod separable). ('a, 'a array) S1.writer
  val bin_reader_array : ('a : value_or_null mod separable). ('a, 'a array) S1.reader
  val bin_shape_array : Shape.t -> Shape.t
  val bin_array : ('a : value_or_null mod separable). ('a, 'a array) S1.t

  (*$ mk_base1 ~layout:"value_or_null mod separable" "iarray" *)
  val bin_writer_iarray : ('a : value_or_null mod separable). ('a, 'a iarray) S1.writer
  val bin_reader_iarray : ('a : value_or_null mod separable). ('a, 'a iarray) S1.reader
  val bin_shape_iarray : Shape.t -> Shape.t
  val bin_iarray : ('a : value_or_null mod separable). ('a, 'a iarray) S1.t

  (*$ mk_base_tp "float32_vec" "vec32" *)
  val bin_writer_float32_vec : vec32 writer
  val bin_reader_float32_vec : vec32 reader
  val bin_shape_float32_vec : Shape.t
  val bin_float32_vec : vec32 t

  (*$ mk_base_tp "float64_vec" "vec64" *)
  val bin_writer_float64_vec : vec64 writer
  val bin_reader_float64_vec : vec64 reader
  val bin_shape_float64_vec : Shape.t
  val bin_float64_vec : vec64 t

  (*$ mk_base "vec" *)
  val bin_writer_vec : vec writer
  val bin_reader_vec : vec reader
  val bin_shape_vec : Shape.t
  val bin_vec : vec t

  (*$ mk_base_tp "float32_mat" "mat32" *)
  val bin_writer_float32_mat : mat32 writer
  val bin_reader_float32_mat : mat32 reader
  val bin_shape_float32_mat : Shape.t
  val bin_float32_mat : mat32 t

  (*$ mk_base_tp "float64_mat" "mat64" *)
  val bin_writer_float64_mat : mat64 writer
  val bin_reader_float64_mat : mat64 reader
  val bin_shape_float64_mat : Shape.t
  val bin_float64_mat : mat64 t

  (*$ mk_base "mat" *)
  val bin_writer_mat : mat writer
  val bin_reader_mat : mat reader
  val bin_shape_mat : Shape.t
  val bin_mat : mat t

  (*$ mk_base_tp "bigstring" "buf" *)
  val bin_writer_bigstring : buf writer
  val bin_reader_bigstring : buf reader
  val bin_shape_bigstring : Shape.t
  val bin_bigstring : buf t
  (*$*)

  (*$ mk_base "floatarray" *)
  val bin_writer_floatarray : floatarray writer
  val bin_reader_floatarray : floatarray reader
  val bin_shape_floatarray : Shape.t
  val bin_floatarray : floatarray t
  (*$*)

  val bin_writer_variant_int : int writer
  val bin_reader_variant_int : int reader
  val bin_variant_int : int t

  (*$ mk_base_tp "int_8bit" "int" *)
  val bin_writer_int_8bit : int writer
  val bin_reader_int_8bit : int reader
  val bin_shape_int_8bit : Shape.t
  val bin_int_8bit : int t

  (*$ mk_base_tp "int_16bit" "int" *)
  val bin_writer_int_16bit : int writer
  val bin_reader_int_16bit : int reader
  val bin_shape_int_16bit : Shape.t
  val bin_int_16bit : int t

  (*$ mk_base_tp "int_32bit" "int" *)
  val bin_writer_int_32bit : int writer
  val bin_reader_int_32bit : int reader
  val bin_shape_int_32bit : Shape.t
  val bin_int_32bit : int t

  (*$ mk_base_tp "int_64bit" "int" *)
  val bin_writer_int_64bit : int writer
  val bin_reader_int_64bit : int reader
  val bin_shape_int_64bit : Shape.t
  val bin_int_64bit : int t

  (*$ mk_base_tp "int32_bits" "int32" *)
  val bin_writer_int32_bits : int32 writer
  val bin_reader_int32_bits : int32 reader
  val bin_shape_int32_bits : Shape.t
  val bin_int32_bits : int32 t

  (*$ mk_base_tp "int64_bits" "int64" *)
  val bin_writer_int64_bits : int64 writer
  val bin_reader_int64_bits : int64 reader
  val bin_shape_int64_bits : Shape.t
  val bin_int64_bits : int64 t

  (*$ mk_base_tp "network16_int" "int" *)
  val bin_writer_network16_int : int writer
  val bin_reader_network16_int : int reader
  val bin_shape_network16_int : Shape.t
  val bin_network16_int : int t

  (*$ mk_base_tp "network32_int" "int" *)
  val bin_writer_network32_int : int writer
  val bin_reader_network32_int : int reader
  val bin_shape_network32_int : Shape.t
  val bin_network32_int : int t

  (*$ mk_base_tp "network32_int32" "int32" *)
  val bin_writer_network32_int32 : int32 writer
  val bin_reader_network32_int32 : int32 reader
  val bin_shape_network32_int32 : Shape.t
  val bin_network32_int32 : int32 t

  (*$ mk_base_tp "network64_int" "int" *)
  val bin_writer_network64_int : int writer
  val bin_reader_network64_int : int reader
  val bin_shape_network64_int : Shape.t
  val bin_network64_int : int t

  (*$ mk_base_tp "network64_int64" "int64" *)
  val bin_writer_network64_int64 : int64 writer
  val bin_reader_network64_int64 : int64 reader
  val bin_shape_network64_int64 : Shape.t
  val bin_network64_int64 : int64 t
  (*$*)

  (** Conversion of binable types *)

  val cnv_writer : ('a -> 'b) -> 'b writer -> 'a writer
  val cnv_reader : ('b -> 'a) -> 'b reader -> 'a reader
  val cnv : (Shape.t -> Shape.t) -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t
end
