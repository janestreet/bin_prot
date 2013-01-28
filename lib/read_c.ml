(* Read_c: wrapping unsafe C-style readers to safe ML-style ones. *)

open Bigarray

open Common
open Unsafe_common
open Unsafe_read_c

let handle_error buf sptr_ptr read_err =
  let err_pos = dealloc_sptr_ptr buf sptr_ptr in
  let err_pos =
    match read_err with
    | ReadError.Variant _ -> err_pos - 4
    | _ -> err_pos
  in
  raise_read_error read_err err_pos

let handle_exc buf sptr_ptr exc =
  ignore (dealloc_sptr_ptr buf sptr_ptr);
  raise exc

let at_end buf sptr_ptr pos_ref el =
  let pos = dealloc_sptr_ptr buf sptr_ptr in
  pos_ref := pos;
  el

let make read_c buf ~pos_ref =
  let sptr_ptr, eptr = get_read_init buf ~pos_ref in
  let el =
    try read_c sptr_ptr eptr with
    | Error read_err -> handle_error buf sptr_ptr read_err
    | exc -> handle_exc buf sptr_ptr exc
  in
  at_end buf sptr_ptr pos_ref el

let unmake read_ml buf sptr_ptr _eptr =
  let start_pos = get_sptr_ptr sptr_ptr buf in
  let pos_ref = ref start_pos in
  let el = read_ml buf ~pos_ref in
  set_sptr_ptr sptr_ptr buf ~pos:!pos_ref;
  el

let make1 read_c read_ml_el buf ~pos_ref =
  let sptr_ptr, eptr = get_read_init buf ~pos_ref in
  let read_c_el = unmake read_ml_el buf in
  let el =
    try read_c read_c_el sptr_ptr eptr with
    | Error read_err -> handle_error buf sptr_ptr read_err
    | exc -> handle_exc buf sptr_ptr exc
  in
  at_end buf sptr_ptr pos_ref el

let make2 read_c read_ml_el1 read_ml_el2 buf ~pos_ref =
  let sptr_ptr, eptr = get_read_init buf ~pos_ref in
  let read_c_el1 = unmake read_ml_el1 buf in
  let read_c_el2 = unmake read_ml_el2 buf in
  let el =
    try read_c read_c_el1 read_c_el2 sptr_ptr eptr with
    | Error read_err -> handle_error buf sptr_ptr read_err
    | exc -> handle_exc buf sptr_ptr exc
  in
  at_end buf sptr_ptr pos_ref el

let make3 read_c read_ml_el1 read_ml_el2 read_ml_el3 buf ~pos_ref =
  let sptr_ptr, eptr = get_read_init buf ~pos_ref in
  let read_c_el1 = unmake read_ml_el1 buf in
  let read_c_el2 = unmake read_ml_el2 buf in
  let read_c_el3 = unmake read_ml_el3 buf in
  let el =
    try read_c read_c_el1 read_c_el2 read_c_el3 sptr_ptr eptr with
    | Error read_err -> handle_error buf sptr_ptr read_err
    | exc -> handle_exc buf sptr_ptr exc
  in
  at_end buf sptr_ptr pos_ref el

let bin_read_unit = make Unsafe_read_c.bin_read_unit
let bin_read_bool = make Unsafe_read_c.bin_read_bool
let bin_read_string = make Unsafe_read_c.bin_read_string
let bin_read_char = make Unsafe_read_c.bin_read_char
let bin_read_int = make Unsafe_read_c.bin_read_int
let bin_read_float = make Unsafe_read_c.bin_read_float
let bin_read_int32 = make Unsafe_read_c.bin_read_int32
let bin_read_int64 = make Unsafe_read_c.bin_read_int64
let bin_read_nativeint = make Unsafe_read_c.bin_read_nativeint
let bin_read_nat0 = make Unsafe_read_c.bin_read_nat0
let bin_read_ref mlw = make1 Unsafe_read_c.bin_read_ref mlw
let bin_read_lazy mlw = make1 Unsafe_read_c.bin_read_lazy mlw
let bin_read_option mlw = make1 Unsafe_read_c.bin_read_option mlw
let bin_read_pair mlw = make2 Unsafe_read_c.bin_read_pair mlw
let bin_read_triple mlw = make3 Unsafe_read_c.bin_read_triple mlw
let bin_read_list mlw = make1 Unsafe_read_c.bin_read_list mlw
let bin_read_array mlw = make1 Unsafe_read_c.bin_read_array mlw
let bin_read_hashtbl mlw = make2 Unsafe_read_c.bin_read_hashtbl mlw
let bin_read_float32_vec = make Unsafe_read_c.bin_read_float32_vec
let bin_read_float64_vec = make Unsafe_read_c.bin_read_float64_vec
let bin_read_vec = make Unsafe_read_c.bin_read_vec
let bin_read_float32_mat = make Unsafe_read_c.bin_read_float32_mat
let bin_read_float64_mat = make Unsafe_read_c.bin_read_float64_mat
let bin_read_mat = make Unsafe_read_c.bin_read_mat
let bin_read_bigstring = make Unsafe_read_c.bin_read_bigstring
let bin_read_float_array = make Unsafe_read_c.bin_read_float_array
let bin_read_variant_int el = make Unsafe_read_c.bin_read_variant_int el
let bin_read_variant_tag el = make Unsafe_read_c.bin_read_variant_tag el
let bin_read_int_64bit = make Unsafe_read_c.bin_read_int_64bit
let bin_read_int64_bits = make Unsafe_read_c.bin_read_int64_bits
let bin_read_network16_int = make Unsafe_read_c.bin_read_network16_int
let bin_read_network32_int = make Unsafe_read_c.bin_read_network32_int
let bin_read_network32_int32 = make Unsafe_read_c.bin_read_network32_int32
let bin_read_network64_int = make Unsafe_read_c.bin_read_network64_int
let bin_read_network64_int64 = make Unsafe_read_c.bin_read_network64_int64
