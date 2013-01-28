(* Size: compute size of values in the binary protocol. *)

open Bigarray

open Common

type 'a sizer = 'a -> int
type ('a, 'b) sizer1 = 'a sizer -> 'b sizer
type ('a, 'b, 'c) sizer2 = 'a sizer -> ('b, 'c) sizer1
type ('a, 'b, 'c, 'd) sizer3 = 'a sizer -> ('b, 'c, 'd) sizer2

let bin_size_unit () = 1

let bin_size_bool _ = 1

let bin_size_int_nat0 n =
  if      n  < 0x00000080 then 1
  else if n  < 0x00008000 then 3
#ifdef ARCH_SIXTYFOUR
  else if n >= 0x80000000 then 9
#endif
  else 5

let bin_size_int_negative n =
  if      n >= -0x00000080 then 2
  else if n >= -0x00008000 then 3
#ifdef ARCH_SIXTYFOUR
  else if n  < -0x80000000 then 9
#endif
  else 5

let bin_size_char _ = 1

let bin_size_int n =
  if n >= 0 then bin_size_int_nat0 n
  else bin_size_int_negative n

let bin_size_nat0 nat0 =
  let n = (nat0 : Nat0.t :> int) in
  if      n <   0x00000080 then 1
  else if n <   0x00010000 then 3
#ifdef ARCH_SIXTYFOUR
  else if n >= 0x100000000 then 9
#endif
  else 5

let bin_size_string str =
  let len = String.length str in
  let plen = Nat0.unsafe_of_int len in
  let size_len = bin_size_nat0 plen in
  size_len + len

let bin_size_float _ = 8

#ifdef ARCH_SIXTYFOUR
let bin_size_int32 n = bin_size_int (Int32.to_int n)
#else
let bin_size_int32 n =
  if n >= 0x00008000l || n < -0x00008000l then 5
  else bin_size_int (Int32.to_int n)
#endif

#ifdef ARCH_SIXTYFOUR
let bin_size_int64 n =
  if n >= 0x80000000L || n < -0x80000000L then 9
  else bin_size_int (Int64.to_int n)
#else
let bin_size_int64 n =
  if n >= 0x80000000L || n < -0x80000000L then 9
  else bin_size_int32 (Int64.to_int32 n)
#endif

let bin_size_nativeint n =
#ifdef ARCH_SIXTYFOUR
  bin_size_int64 (Int64.of_nativeint n)
#else
  bin_size_int32 (Nativeint.to_int32 n)
#endif

let bin_size_ref bin_size_el r = bin_size_el !r
let bin_size_lazy bin_size_el lv = bin_size_el (Lazy.force lv)

let bin_size_option bin_size_el = function
  | None -> 1
  | Some v -> 1 + bin_size_el v

let bin_size_pair bin_size_a bin_size_b (a, b) = bin_size_a a + bin_size_b b

let bin_size_triple bin_size_a bin_size_b bin_size_c (a, b, c) =
  bin_size_a a + bin_size_b b + bin_size_c c

let bin_size_list bin_size_el lst =
  let rec loop len = function
    | [] -> len
    | h :: t -> loop (len + bin_size_el h) t
  in
  let len = Nat0.unsafe_of_int (List.length lst) in
  let size_len = bin_size_nat0 len in
  loop size_len lst

let bin_size_len len =
  let plen = Nat0.unsafe_of_int len in
  bin_size_nat0 plen

let bin_size_array_loop bin_size_el ar ~total_len ~n =
  let total_len_ref = ref total_len in
  for i = 0 to n - 1 do
    let el = Array.unsafe_get ar i in
    total_len_ref := !total_len_ref + bin_size_el el
  done;
  !total_len_ref

let bin_size_array bin_size_el ar =
  let n = Array.length ar in
  let total_len = bin_size_len n in
  bin_size_array_loop bin_size_el ar ~total_len ~n

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

let bin_size_gen_vec vec multiplier =
  let len = Array1.dim vec in
  bin_size_len len + multiplier * len

let bin_size_float32_vec vec = bin_size_gen_vec vec 4
let bin_size_float64_vec vec = bin_size_gen_vec vec 8
let bin_size_vec = bin_size_float64_vec

let bin_size_gen_mat mat multiplier =
  let dim1 = Array2.dim1 mat in
  let dim2 = Array2.dim2 mat in
  let size = dim1 * dim2 in
  bin_size_len dim1 + bin_size_len dim2 + multiplier * size

let bin_size_float32_mat mat = bin_size_gen_mat mat 4
let bin_size_float64_mat mat = bin_size_gen_mat mat 8
let bin_size_mat = bin_size_float64_mat

let bin_size_bigstring buf = bin_size_gen_vec buf 1

let bin_size_float_array ar =
  let len = Array.length ar in
  bin_size_len len + 8 * len

let bin_size_variant_tag _ = 4

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

let bin_size_array_no_length bin_size_el ar =
  bin_size_array_loop bin_size_el ar ~total_len:0 ~n:(Array.length ar)
