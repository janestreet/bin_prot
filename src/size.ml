(* Size: compute size of values in the binary protocol. *)

let arch_sixtyfour = Sys.word_size = 64

open Common
include Size_intf.Definitions

module Maximum = struct
  let bin_size_unit = 1
  let bin_size_bool = 1
  let bin_size_char = 1
  let bin_size_md5 = 16
  let bin_size_int_nat0 = if arch_sixtyfour then 9 else 5
  let bin_size_int_negative = if arch_sixtyfour then 9 else 5
  let bin_size_int = max bin_size_int_nat0 bin_size_int_negative
  let bin_size_float = 8
  let bin_size_int32 = 5
  let bin_size_int64 = 9
  let bin_size_nativeint = bin_size_int
  let bin_size_nat0 = bin_size_int_nat0
  let bin_size_variant_int = 4
  let bin_size_int_8bit = 1
  let bin_size_int_16bit = 2
  let bin_size_int_32bit = 4
  let bin_size_int_64bit = 8
  let bin_size_int64_bits = 8
  let bin_size_network16_int = 2
  let bin_size_network32_int = 4
  let bin_size_network32_int32 = 4
  let bin_size_network64_int = 8
  let bin_size_network64_int64 = 8
end

module Minimum = struct
  let bin_size_unit = Maximum.bin_size_unit
  let bin_size_bool = Maximum.bin_size_bool
  let bin_size_char = Maximum.bin_size_char
  let bin_size_md5 = 16
  let bin_size_int_nat0 = 1
  let bin_size_int_negative = 2
  let bin_size_int = min bin_size_int_nat0 bin_size_int_negative
  let bin_size_float = Maximum.bin_size_float
  let bin_size_int32 = bin_size_int
  let bin_size_int64 = bin_size_int
  let bin_size_nativeint = bin_size_int
  let bin_size_nat0 = 1
  let bin_size_ref = 1
  let bin_size_lazy_t = 1
  let bin_size_option = 1
  let bin_size_pair = 1 + 1
  let bin_size_triple = 1 + 1 + 1
  let bin_size_len = bin_size_nat0
  let bin_size_list = bin_size_len
  let bin_size_array = bin_size_len
  let bin_size_hashtbl = bin_size_len
  let bin_size_string = bin_size_len
  let bin_size_bytes = bin_size_len
  let bin_size_vec = bin_size_len
  let bin_size_float32_vec = bin_size_vec
  let bin_size_float64_vec = bin_size_vec
  let bin_size_mat = bin_size_len + bin_size_len
  let bin_size_float32_mat = bin_size_mat
  let bin_size_float64_mat = bin_size_mat
  let bin_size_bigstring = bin_size_len
  let bin_size_floatarray = bin_size_len
  let bin_size_float_array = bin_size_len
  let bin_size_variant_int = Maximum.bin_size_variant_int
  let bin_size_int_8bit = Maximum.bin_size_int_8bit
  let bin_size_int_16bit = Maximum.bin_size_int_16bit
  let bin_size_int_32bit = Maximum.bin_size_int_32bit
  let bin_size_int_64bit = Maximum.bin_size_int_64bit
  let bin_size_int64_bits = Maximum.bin_size_int64_bits
  let bin_size_network16_int = Maximum.bin_size_network16_int
  let bin_size_network32_int = Maximum.bin_size_network32_int
  let bin_size_network32_int32 = Maximum.bin_size_network32_int32
  let bin_size_network64_int = Maximum.bin_size_network64_int
  let bin_size_network64_int64 = Maximum.bin_size_network64_int64
end

let bin_size_unit () = 1
let bin_size_bool _ = 1

let bin_size_int_nat0 n =
  if n < 0x00000080
  then 1
  else if n < 0x00008000
  then 3
  else if arch_sixtyfour && n >= (* 0x80000000 *) 1 lsl 31
  then 9
  else 5
;;

let bin_size_int_negative n =
  if n >= -0x00000080
  then 2
  else if n >= -0x00008000
  then 3
  else if arch_sixtyfour && n < (* -0x80000000 *) -(1 lsl 31)
  then 9
  else 5
;;

let bin_size_char _ = 1
let bin_size_int n = if n >= 0 then bin_size_int_nat0 n else bin_size_int_negative n

let bin_size_nat0 nat0 =
  let n = (nat0 : Nat0.t :> int) in
  if n < 0x00000080
  then 1
  else if n < 0x00010000
  then 3
  else if arch_sixtyfour && n >= (* 0x100000000 *) 1 lsl 32
  then 9
  else 5
;;

let bin_size_string_or_bytes len =
  let plen = Nat0.unsafe_of_int len in
  let size_len = bin_size_nat0 plen in
  size_len + len
;;

let bin_size_string str = bin_size_string_or_bytes (Base.String.length str)
let bin_size_bytes str = bin_size_string_or_bytes (Base.Bytes.length str)
let bin_size_md5 _ = 16

let bin_size_float f =
  (* If we just ignore the argument the compiler will still require it to exist and be
     boxed. This means that if for instance we call this for a field of a float record,
     the compiler will allocate the float for nothing.

     With this line the compiler really ignores the float. *)
  ignore (truncate f);
  8
;;

let bin_size_int32 =
  if arch_sixtyfour
  then fun n -> bin_size_int (Int32.to_int n)
  else
    fun n ->
    if n >= 0x00008000l || n < -0x00008000l then 5 else bin_size_int (Int32.to_int n)
;;

let bin_size_int64 =
  if arch_sixtyfour
  then
    fun n ->
    if n >= 0x80000000L || n < -0x80000000L then 9 else bin_size_int (Int64.to_int n)
  else
    fun n ->
    if n >= 0x80000000L || n < -0x80000000L
    then 9
    else bin_size_int32 (Base.Int64.to_int32_trunc n) [@nontail]
;;

let bin_size_nativeint =
  if arch_sixtyfour
  then fun n -> bin_size_int64 (Base.Int64.of_nativeint n) [@nontail]
  else fun n -> bin_size_int32 (Base.Nativeint.to_int32_trunc n) [@nontail]
;;

let bin_size_ref bin_size_el r = bin_size_el !r
let bin_size_lazy_t bin_size_el lv = bin_size_el (Base.Lazy.force lv)
let bin_size_lazy = bin_size_lazy_t

let bin_size_option bin_size_el = function
  | None -> 1
  | Some v -> 1 + bin_size_el v
;;

let bin_size_pair bin_size_a bin_size_b (a, b) = bin_size_a a + bin_size_b b

let bin_size_triple bin_size_a bin_size_b bin_size_c (a, b, c) =
  bin_size_a a + bin_size_b b + bin_size_c c
;;

let bin_size_list =
  let rec loop ~bin_size_el ~size_acc ~len_acc lst =
    match lst with
    | [] -> size_acc + bin_size_nat0 (Nat0.unsafe_of_int len_acc)
    | hd :: tl ->
      loop ~bin_size_el ~size_acc:(size_acc + bin_size_el hd) ~len_acc:(len_acc + 1) tl
  in
  fun bin_size_el lst -> loop ~bin_size_el ~size_acc:0 ~len_acc:0 lst
;;

let bin_size_len len =
  let plen = Nat0.unsafe_of_int len in
  bin_size_nat0 plen
;;

external float_array_length : Float.Array.t -> int = "%floatarray_length"

let bin_size_floatarray ar =
  let len = float_array_length ar in
  bin_size_len len + (8 * len)
;;

let bin_size_float_array ar =
  let len = Base.Array.length ar in
  bin_size_len len + (8 * len)
;;

let bin_size_array_loop bin_size_el ar ~total_len ~n =
  let total_len_ref = ref total_len in
  for i = 0 to n - 1 do
    let el = Base.Array.unsafe_get ar i in
    total_len_ref := !total_len_ref + bin_size_el el
  done;
  !total_len_ref
;;

let bin_size_array (type a) bin_size_el ar =
  let module Obj = Base.Exported_for_specific_uses.Obj_local in
  if (Obj.magic (bin_size_el : a sizer) : float sizer) == (bin_size_float :> float sizer)
  then bin_size_float_array (Obj.magic (ar : a array) : float array) [@nontail]
  else (
    let n = Base.Array.length ar in
    let total_len = bin_size_len n in
    bin_size_array_loop bin_size_el ar ~total_len ~n)
;;

let bin_size_hashtbl bin_size_key bin_size_val htbl =
  let cnt_ref = ref 0 in
  let coll_htbl k v total_len =
    incr cnt_ref;
    total_len + bin_size_key k + bin_size_val v
  in
  let len = Hashtbl.length htbl in
  let total_len = Hashtbl.fold coll_htbl htbl (bin_size_len len) in
  if !cnt_ref <> len then raise_concurrent_modification "bin_size_hashtbl";
  total_len
;;

external array1_dim : ('a, 'b, 'c) Stdlib.Bigarray.Array1.t -> int = "%caml_ba_dim_1"
external array2_dim1 : ('a, 'b, 'c) Stdlib.Bigarray.Array2.t -> int = "%caml_ba_dim_1"
external array2_dim2 : ('a, 'b, 'c) Stdlib.Bigarray.Array2.t -> int = "%caml_ba_dim_2"

let bin_size_gen_vec vec multiplier =
  let len = array1_dim vec in
  bin_size_len len + (multiplier * len)
;;

let bin_size_float32_vec vec = bin_size_gen_vec vec 4
let bin_size_float64_vec vec = bin_size_gen_vec vec 8
let bin_size_vec = bin_size_float64_vec

let bin_size_gen_mat mat multiplier =
  let dim1 = array2_dim1 mat in
  let dim2 = array2_dim2 mat in
  let size = dim1 * dim2 in
  bin_size_len dim1 + bin_size_len dim2 + (multiplier * size)
;;

let bin_size_float32_mat mat = bin_size_gen_mat mat 4
let bin_size_float64_mat mat = bin_size_gen_mat mat 8
let bin_size_mat = bin_size_float64_mat
let bin_size_bigstring buf = bin_size_gen_vec buf 1
let bin_size_variant_int _ = 4
let bin_size_int_8bit _ = 1
let bin_size_int_16bit _ = 2
let bin_size_int_32bit _ = 4
let bin_size_int_64bit _ = 8
let bin_size_int64_bits _ = 8
let bin_size_network16_int _ = 2
let bin_size_network32_int _ = 4
let bin_size_network32_int32 _ = 4
let bin_size_network64_int _ = 8
let bin_size_network64_int64 _ = 8

(* Local versions *)

let bin_size_unit__local = bin_size_unit
let bin_size_bool__local = bin_size_bool
let bin_size_string__local = bin_size_string
let bin_size_bytes__local = bin_size_bytes
let bin_size_char__local = bin_size_char
let bin_size_int__local = bin_size_int
let bin_size_float__local = bin_size_float
let bin_size_int32__local = bin_size_int32
let bin_size_int64__local = bin_size_int64
let bin_size_nativeint__local = bin_size_nativeint
let bin_size_nat0__local = bin_size_nat0
let bin_size_ref__local = bin_size_ref
let bin_size_lazy_t__local = bin_size_lazy_t
let bin_size_lazy__local = bin_size_lazy

let bin_size_option__local bin_size_el = function
  | None -> 1
  | Some v -> 1 + bin_size_el v
;;

let bin_size_pair__local bin_size_a bin_size_b (a, b) = bin_size_a a + bin_size_b b

let bin_size_triple__local bin_size_a bin_size_b bin_size_c (a, b, c) =
  bin_size_a a + bin_size_b b + bin_size_c c
;;

let bin_size_list__local =
  let rec loop ~bin_size_el ~size_acc ~len_acc lst =
    match lst with
    | [] -> size_acc + bin_size_nat0 (Nat0.unsafe_of_int len_acc)
    | hd :: tl ->
      loop ~bin_size_el ~size_acc:(size_acc + bin_size_el hd) ~len_acc:(len_acc + 1) tl
  in
  fun bin_size_el lst -> loop ~bin_size_el ~size_acc:0 ~len_acc:0 lst
;;

let bin_size_array__local = bin_size_array
let bin_size_float32_vec__local = bin_size_float32_vec
let bin_size_float64_vec__local = bin_size_float64_vec
let bin_size_vec__local = bin_size_vec
let bin_size_float32_mat__local = bin_size_float32_mat
let bin_size_float64_mat__local = bin_size_float64_mat
let bin_size_mat__local = bin_size_mat
let bin_size_bigstring__local = bin_size_bigstring
let bin_size_floatarray__local = bin_size_floatarray
let bin_size_variant_int__local = bin_size_variant_int
let bin_size_int_8bit__local = bin_size_int_8bit
let bin_size_int_16bit__local = bin_size_int_16bit
let bin_size_int_32bit__local = bin_size_int_32bit
let bin_size_int_64bit__local = bin_size_int_64bit
let bin_size_int64_bits__local = bin_size_int64_bits
let bin_size_network16_int__local = bin_size_network16_int
let bin_size_network32_int__local = bin_size_network32_int
let bin_size_network32_int32__local = bin_size_network32_int32
let bin_size_network64_int__local = bin_size_network64_int
let bin_size_network64_int64__local = bin_size_network64_int64
let bin_size_md5__local = bin_size_md5
