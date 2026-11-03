open! Base
open Bin_prot
open Bin_prot.Std

[%%template
  type ('a : k mod separable) array = 'a array
  [@@kind k = (base_non_value, value_or_null mod external64)]]

let lengths = [ 0; 1; 16; 256; 4096; 32768 ]
let ignore_int (_ : int) = ()

let reverse_bits_int x =
  let result = ref 0 in
  for i = 0 to Stdlib.Sys.word_size - 2 do
    result := Stdlib.((!result lsl 1) lor ((x lsr i) land 1))
  done;
  !result
;;

let reverse_bits_int32 x =
  let result = ref 0l in
  for i = 0 to 31 do
    result
    := Stdlib.Int32.logor
         (Stdlib.Int32.shift_left !result 1)
         (Stdlib.Int32.logand (Stdlib.Int32.shift_right_logical x i) 1l)
  done;
  !result
;;

let reverse_bits_int64 x =
  let result = ref 0L in
  for i = 0 to 63 do
    result
    := Stdlib.Int64.logor
         (Stdlib.Int64.shift_left !result 1)
         (Stdlib.Int64.logand (Stdlib.Int64.shift_right_logical x i) 1L)
  done;
  !result
;;

let reverse_bits_nativeint x =
  let result = ref 0n in
  for i = 0 to Stdlib.Sys.word_size - 1 do
    result
    := Stdlib.Nativeint.logor
         (Stdlib.Nativeint.shift_left !result 1)
         (Stdlib.Nativeint.logand (Stdlib.Nativeint.shift_right_logical x i) 1n)
  done;
  !result
;;

module type Array_binable = sig
  type t

  val example : int -> t
  val bin_size_t : t Size.sizer
  val bin_write_t : t Write.writer
  val bin_read_t : t Read.reader
end

module Bench (M : Array_binable) = struct
  let%bench_fun ("size" [@indexed len = lengths]) =
    let t = M.example len in
    fun () -> ignore_int (M.bin_size_t t)
  ;;

  let%bench_fun ("write" [@indexed len = lengths]) =
    let t = M.example len in
    let buf = Common.create_buf (M.bin_size_t t) in
    fun () -> ignore_int (M.bin_write_t buf ~pos:0 t)
  ;;

  let%bench_fun ("read" [@indexed len = lengths]) =
    let t = M.example len in
    let buf = Common.create_buf (M.bin_size_t t) in
    let end_pos = M.bin_write_t buf ~pos:0 t in
    assert (end_pos = M.bin_size_t t);
    let pos_ref = ref 0 in
    fun () ->
      pos_ref := 0;
      Sys.opaque_identity (M.bin_read_t buf ~pos_ref)
  ;;
end

module Reversed_int = struct
  type t = int

  let reverse_on_wire = reverse_bits_int
  let bin_shape_t = bin_shape_int
  let bin_size_t t = bin_size_int (reverse_on_wire t)
  let bin_write_t buf ~pos t = bin_write_int buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = bin_read_int buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    __bin_read_int__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Reversed_float = struct
  type t = float

  let reverse_on_wire t =
    t |> Int64.bits_of_float |> reverse_bits_int64 |> Int64.float_of_bits
  ;;

  let bin_shape_t = bin_shape_float
  let bin_size_t t = bin_size_float (reverse_on_wire t)
  let bin_write_t buf ~pos t = bin_write_float buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = bin_read_float buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    __bin_read_float__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Reversed_bits32 = struct
  type t : bits32 = Int32_u.t

  let reverse_on_wire t = Int32_u.of_int32 (reverse_bits_int32 (Int32_u.to_int32 t))
  let bin_shape_t = Int32_u.bin_shape_t
  let bin_size_t t = Int32_u.bin_size_t (reverse_on_wire t)
  let bin_write_t buf ~pos t = Int32_u.bin_write_t buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = Int32_u.bin_read_t buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    Int32_u.__bin_read_t__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Reversed_bits64 = struct
  type t : bits64 = Int64_u.t

  let reverse_on_wire t = Int64_u.of_int64 (reverse_bits_int64 (Int64_u.to_int64 t))
  let bin_shape_t = Int64_u.bin_shape_t
  let bin_size_t t = Int64_u.bin_size_t (reverse_on_wire t)
  let bin_write_t buf ~pos t = Int64_u.bin_write_t buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = Int64_u.bin_read_t buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    Int64_u.__bin_read_t__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Reversed_word = struct
  type t : word = Nativeint_u.t

  let reverse_on_wire t =
    Nativeint_u.of_nativeint_exn (reverse_bits_nativeint (Nativeint_u.to_nativeint_exn t))
  ;;

  let bin_shape_t = Nativeint_u.bin_shape_t
  let bin_size_t t = Nativeint_u.bin_size_t (reverse_on_wire t)
  let bin_write_t buf ~pos t = Nativeint_u.bin_write_t buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = Nativeint_u.bin_read_t buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    Nativeint_u.__bin_read_t__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Reversed_float32 = struct
  type t : float32 = Float32_u.t

  let reverse_on_wire t =
    Float32_u.to_bits t
    |> Int32_u.to_int32
    |> reverse_bits_int32
    |> Int32_u.of_int32
    |> Float32_u.of_bits
  ;;

  let bin_shape_t = Float32_u.bin_shape_t
  let bin_size_t t = Float32_u.bin_size_t (reverse_on_wire t)
  let bin_write_t buf ~pos t = Float32_u.bin_write_t buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = Float32_u.bin_read_t buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    Float32_u.__bin_read_t__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Reversed_float64 = struct
  type t : float64 = Float_u.t

  let reverse_on_wire t =
    Float_u.to_bits t
    |> Int64_u.to_int64
    |> reverse_bits_int64
    |> Int64_u.of_int64
    |> Float_u.of_bits
  ;;

  let bin_shape_t = Float_u.bin_shape_t
  let bin_size_t t = Float_u.bin_size_t (reverse_on_wire t)
  let bin_write_t buf ~pos t = Float_u.bin_write_t buf ~pos (reverse_on_wire t)
  let bin_read_t buf ~pos_ref = Float_u.bin_read_t buf ~pos_ref |> reverse_on_wire

  let __bin_read_t__ buf ~pos_ref variant_int =
    Float_u.__bin_read_t__ buf ~pos_ref variant_int |> reverse_on_wire
  ;;
end

module Canonical_int_array = struct
  type t = int array [@@deriving bin_io]

  let example len = Array.init len ~f:Fn.id
end

module Custom_int_array = struct
  type t = Reversed_int.t array [@@deriving bin_io]

  let example len = Array.init len ~f:Fn.id
end

(* Same elements as the [int_array] modules above, but using the
   [value_or_null mod external64] instances of the array bin_io functions. *)
module Canonical_int_external64_array = struct
  type t = (int array[@kind value_or_null mod external64]) [@@deriving bin_io]

  let example len = Array.init len ~f:Fn.id
end

module Custom_int_external64_array = struct
  type t = (Reversed_int.t array[@kind value_or_null mod external64]) [@@deriving bin_io]

  let example len = Array.init len ~f:Fn.id
end

module Canonical_float_array = struct
  type t = float array [@@deriving bin_io]

  let example len = Array.init len ~f:Float.of_int
end

module Custom_float_array = struct
  type t = Reversed_float.t array [@@deriving bin_io]

  let example len = Array.init len ~f:Float.of_int
end

module Canonical_bits32_array = struct
  type t = (Int32_u.t array[@kind bits32]) [@@deriving bin_io]

  let example len = (Array.init [@kind bits32]) len ~f:Int32_u.of_int_exn
end

module Custom_bits32_array = struct
  type t = (Reversed_bits32.t array[@kind bits32]) [@@deriving bin_io]

  let example len = (Array.init [@kind bits32]) len ~f:Int32_u.of_int_exn
end

module Canonical_bits64_array = struct
  type t = (Int64_u.t array[@kind bits64]) [@@deriving bin_io]

  let example len = (Array.init [@kind bits64]) len ~f:Int64_u.of_int
end

module Custom_bits64_array = struct
  type t = (Reversed_bits64.t array[@kind bits64]) [@@deriving bin_io]

  let example len = (Array.init [@kind bits64]) len ~f:Int64_u.of_int
end

module Canonical_word_array = struct
  type t = (Nativeint_u.t array[@kind word]) [@@deriving bin_io]

  let example len = (Array.init [@kind word]) len ~f:Nativeint_u.of_int
end

module Custom_word_array = struct
  type t = (Reversed_word.t array[@kind word]) [@@deriving bin_io]

  let example len = (Array.init [@kind word]) len ~f:Nativeint_u.of_int
end

module Canonical_float32_array = struct
  type t = (Float32_u.t array[@kind float32]) [@@deriving bin_io]

  let example len = (Array.init [@kind float32]) len ~f:Float32_u.of_int
end

module Custom_float32_array = struct
  type t = (Reversed_float32.t array[@kind float32]) [@@deriving bin_io]

  let example len = (Array.init [@kind float32]) len ~f:Float32_u.of_int
end

module Canonical_float64_array = struct
  type t = (Float_u.t array[@kind float64]) [@@deriving bin_io]

  let example len = (Array.init [@kind float64]) len ~f:Float_u.of_int
end

module Custom_float64_array = struct
  type t = (Reversed_float64.t array[@kind float64]) [@@deriving bin_io]

  let example len = (Array.init [@kind float64]) len ~f:Float_u.of_int
end

module%bench [@name "array int canonical"] _ = Bench (Canonical_int_array)
module%bench [@name "array int custom"] _ = Bench (Custom_int_array)

module%bench [@name "array int external64 canonical"] _ =
  Bench (Canonical_int_external64_array)

module%bench [@name "array int external64 custom"] _ = Bench (Custom_int_external64_array)
module%bench [@name "array float canonical"] _ = Bench (Canonical_float_array)
module%bench [@name "array float custom"] _ = Bench (Custom_float_array)
module%bench [@name "array bits32 canonical"] _ = Bench (Canonical_bits32_array)
module%bench [@name "array bits32 custom"] _ = Bench (Custom_bits32_array)
module%bench [@name "array bits64 canonical"] _ = Bench (Canonical_bits64_array)
module%bench [@name "array bits64 custom"] _ = Bench (Custom_bits64_array)
module%bench [@name "array word canonical"] _ = Bench (Canonical_word_array)
module%bench [@name "array word custom"] _ = Bench (Custom_word_array)
module%bench [@name "array float32 canonical"] _ = Bench (Canonical_float32_array)
module%bench [@name "array float32 custom"] _ = Bench (Custom_float32_array)
module%bench [@name "array float64 canonical"] _ = Bench (Canonical_float64_array)
module%bench [@name "array float64 custom"] _ = Bench (Custom_float64_array)
