open! Base
open Common

(** sizers *)

include Size_intf.Size

(** {2 Monomorphic types} *)

(** shapes *)

val bin_shape_bigstring : Bin_shape.t
val bin_shape_bool : Bin_shape.t
val bin_shape_bytes : Bin_shape.t
val bin_shape_char : Bin_shape.t
val bin_shape_float : Bin_shape.t
val bin_shape_float32_mat : Bin_shape.t
val bin_shape_float32_vec : Bin_shape.t
val bin_shape_float64_mat : Bin_shape.t
val bin_shape_float64_vec : Bin_shape.t
val bin_shape_floatarray : Bin_shape.t
val bin_shape_int : Bin_shape.t
val bin_shape_int32 : Bin_shape.t
val bin_shape_int64 : Bin_shape.t
val bin_shape_mat : Bin_shape.t
val bin_shape_nativeint : Bin_shape.t
val bin_shape_string : Bin_shape.t
val bin_shape_unit : Bin_shape.t
val bin_shape_vec : Bin_shape.t

(** readers *)

val bin_read_bigstring : buf Read.reader
val bin_read_bool : bool Read.reader
val bin_read_bytes : bytes Read.reader
val bin_read_char : char Read.reader
val bin_read_float : float Read.reader
val bin_read_float32_mat : mat32 Read.reader
val bin_read_float32_vec : vec32 Read.reader
val bin_read_float64_mat : mat64 Read.reader
val bin_read_float64_vec : vec64 Read.reader
val bin_read_floatarray : floatarray Read.reader
val bin_read_int : int Read.reader
val bin_read_int32 : int32 Read.reader
val bin_read_int64 : int64 Read.reader
val bin_read_mat : mat Read.reader
val bin_read_nativeint : nativeint Read.reader
val bin_read_string : string Read.reader
val bin_read_unit : unit Read.reader
val bin_read_vec : vec Read.reader

(** vtag readers *)

val __bin_read_bigstring__ : buf Read.vtag_reader
val __bin_read_bool__ : bool Read.vtag_reader
val __bin_read_bytes__ : bytes Read.vtag_reader
val __bin_read_char__ : char Read.vtag_reader
val __bin_read_float32_mat__ : mat32 Read.vtag_reader
val __bin_read_float32_vec__ : vec32 Read.vtag_reader
val __bin_read_float64_mat__ : mat64 Read.vtag_reader
val __bin_read_float64_vec__ : vec64 Read.vtag_reader
val __bin_read_float__ : float Read.vtag_reader
val __bin_read_floatarray__ : floatarray Read.vtag_reader
val __bin_read_int32__ : int32 Read.vtag_reader
val __bin_read_int64__ : int64 Read.vtag_reader
val __bin_read_int__ : int Read.vtag_reader
val __bin_read_mat__ : mat Read.vtag_reader
val __bin_read_nativeint__ : nativeint Read.vtag_reader
val __bin_read_string__ : string Read.vtag_reader
val __bin_read_unit__ : unit Read.vtag_reader
val __bin_read_vec__ : vec Read.vtag_reader

(** writers **)

[%%template:
[@@@mode m = (global, local)]

type 'a writer := ('a Write.writer[@mode m])

[@@@mode.default m]

val bin_write_bigstring : buf writer
val bin_write_bool : bool writer
val bin_write_bytes : bytes writer
val bin_write_char : char writer
val bin_write_float : float writer
val bin_write_float32_mat : mat32 writer
val bin_write_float32_vec : vec32 writer
val bin_write_float64_mat : mat64 writer
val bin_write_float64_vec : vec64 writer
val bin_write_floatarray : floatarray writer
val bin_write_int : int writer
val bin_write_int32 : int32 writer
val bin_write_int64 : int64 writer
val bin_write_mat : mat writer
val bin_write_nativeint : nativeint writer
val bin_write_string : string writer
val bin_write_unit : unit writer
val bin_write_vec : vec writer

(* end [%%template] *)]

(** type class readers *)

val bin_reader_bigstring : buf Type_class.reader
val bin_reader_bool : bool Type_class.reader
val bin_reader_bytes : bytes Type_class.reader
val bin_reader_char : char Type_class.reader
val bin_reader_float : float Type_class.reader
val bin_reader_float32_mat : mat32 Type_class.reader
val bin_reader_float32_vec : vec32 Type_class.reader
val bin_reader_float64_mat : mat64 Type_class.reader
val bin_reader_float64_vec : vec64 Type_class.reader
val bin_reader_floatarray : floatarray Type_class.reader
val bin_reader_int : int Type_class.reader
val bin_reader_int32 : int32 Type_class.reader
val bin_reader_int64 : int64 Type_class.reader
val bin_reader_mat : mat Type_class.reader
val bin_reader_nativeint : nativeint Type_class.reader
val bin_reader_string : string Type_class.reader
val bin_reader_unit : unit Type_class.reader
val bin_reader_vec : vec Type_class.reader

(** type class writers **)

val bin_writer_bigstring : buf Type_class.writer
val bin_writer_bool : bool Type_class.writer
val bin_writer_bytes : bytes Type_class.writer
val bin_writer_char : char Type_class.writer
val bin_writer_float : float Type_class.writer
val bin_writer_float32_mat : mat32 Type_class.writer
val bin_writer_float32_vec : vec32 Type_class.writer
val bin_writer_float64_mat : mat64 Type_class.writer
val bin_writer_float64_vec : vec64 Type_class.writer
val bin_writer_floatarray : floatarray Type_class.writer
val bin_writer_int : int Type_class.writer
val bin_writer_int32 : int32 Type_class.writer
val bin_writer_int64 : int64 Type_class.writer
val bin_writer_mat : mat Type_class.writer
val bin_writer_nativeint : nativeint Type_class.writer
val bin_writer_string : string Type_class.writer
val bin_writer_unit : unit Type_class.writer
val bin_writer_vec : vec Type_class.writer

(** type classes *)

val bin_bigstring : buf Type_class.t
val bin_bool : bool Type_class.t
val bin_bytes : bytes Type_class.t
val bin_char : char Type_class.t
val bin_float : float Type_class.t
val bin_float32_mat : mat32 Type_class.t
val bin_float32_vec : vec32 Type_class.t
val bin_float64_mat : mat64 Type_class.t
val bin_float64_vec : vec64 Type_class.t
val bin_floatarray : floatarray Type_class.t
val bin_int : int Type_class.t
val bin_int32 : int32 Type_class.t
val bin_int64 : int64 Type_class.t
val bin_mat : mat Type_class.t
val bin_nativeint : nativeint Type_class.t
val bin_string : string Type_class.t
val bin_unit : unit Type_class.t
val bin_vec : vec Type_class.t

(** {2 polymorphic types} *)

(** shapes *)

val bin_shape_array : Bin_shape.t -> Bin_shape.t
val bin_shape_iarray : Bin_shape.t -> Bin_shape.t
val bin_shape_lazy : Bin_shape.t -> Bin_shape.t
val bin_shape_lazy_t : Bin_shape.t -> Bin_shape.t
val bin_shape_list : Bin_shape.t -> Bin_shape.t
val bin_shape_option : Bin_shape.t -> Bin_shape.t
val bin_shape_ref : Bin_shape.t -> Bin_shape.t

(** readers *)

val bin_read_array : ('a, 'a array) Read.reader1
val bin_read_iarray : ('a, 'a iarray) Read.reader1
val bin_read_lazy : ('a, 'a lazy_t) Read.reader1
val bin_read_lazy_t : ('a, 'a lazy_t) Read.reader1
val bin_read_list : 'a. ('a, 'a list) Read.reader1
val bin_read_option : 'a. ('a, 'a option) Read.reader1
val bin_read_ref : 'a. ('a, 'a ref) Read.reader1

(** vtag readers *)

val __bin_read_array__ : ('a, 'a array) Read.vtag_reader1
val __bin_read_iarray__ : ('a, 'a iarray) Read.vtag_reader1
val __bin_read_lazy__ : ('a, 'a lazy_t) Read.vtag_reader1
val __bin_read_lazy_t__ : ('a, 'a lazy_t) Read.vtag_reader1
val __bin_read_list__ : 'a. ('a, 'a list) Read.vtag_reader1
val __bin_read_option__ : 'a. ('a, 'a option) Read.vtag_reader1
val __bin_read_ref__ : 'a. ('a, 'a ref) Read.vtag_reader1

(** writers **)

[%%template:
[@@@mode m = (global, local)]

type ('a, 'b) writer1 := (('a, 'b) Write.writer1[@mode m])

[@@@mode.default m]

val bin_write_array : ('a, 'a array) writer1
val bin_write_iarray : ('a, 'a iarray) writer1
val bin_write_lazy : ('a, 'a lazy_t) writer1
val bin_write_lazy_t : ('a, 'a lazy_t) writer1
val bin_write_list : 'a. ('a, 'a list) writer1
val bin_write_option : 'a. ('a, 'a option) writer1
val bin_write_ref : 'a. ('a, 'a ref) writer1

(* end [%%template] *)]

(** type class readers *)

val bin_reader_array : ('a, 'a array) Type_class.S1.reader
val bin_reader_iarray : ('a, 'a iarray) Type_class.S1.reader
val bin_reader_lazy : ('a, 'a lazy_t) Type_class.S1.reader
val bin_reader_lazy_t : ('a, 'a lazy_t) Type_class.S1.reader
val bin_reader_list : 'a. ('a, 'a list) Type_class.S1.reader
val bin_reader_option : 'a. ('a, 'a option) Type_class.S1.reader
val bin_reader_ref : 'a. ('a, 'a ref) Type_class.S1.reader

(** type class writers **)

val bin_writer_array : ('a, 'a array) Type_class.S1.writer
val bin_writer_iarray : ('a, 'a iarray) Type_class.S1.writer
val bin_writer_lazy : ('a, 'a lazy_t) Type_class.S1.writer
val bin_writer_lazy_t : ('a, 'a lazy_t) Type_class.S1.writer
val bin_writer_list : 'a. ('a, 'a list) Type_class.S1.writer
val bin_writer_option : 'a. ('a, 'a option) Type_class.S1.writer
val bin_writer_ref : 'a. ('a, 'a ref) Type_class.S1.writer

(** type classes *)

val bin_array : ('a, 'a array) Type_class.S1.t
val bin_iarray : ('a, 'a iarray) Type_class.S1.t
val bin_lazy : ('a, 'a lazy_t) Type_class.S1.t
val bin_lazy_t : ('a, 'a lazy_t) Type_class.S1.t
val bin_list : 'a. ('a, 'a list) Type_class.S1.t
val bin_option : 'a. ('a, 'a option) Type_class.S1.t
val bin_ref : 'a. ('a, 'a ref) Type_class.S1.t
