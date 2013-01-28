(* Write_c: wrapping unsafe C-style writers to safe ML-style ones. *)

open Bigarray

open Common
open Unsafe_common

let unsafe_get_init buf ~pos =
  if pos < 0 then array_bound_error ()
  else
    let buf_len = Array1.dim buf in
    if pos > buf_len then raise Buffer_short
    else
      let start = get_sptr buf ~pos:0 in
      let sptr = get_sptr buf ~pos in
      let eptr = get_eptr buf ~pos:buf_len in
      start, sptr, eptr

let make write_c buf ~pos el =
  let start, sptr, eptr = unsafe_get_init buf ~pos in
  let cur = write_c sptr eptr el in
  get_safe_buf_pos buf ~start ~cur

let unmake write_ml buf ~start sptr _eptr el =
  let start_pos = get_buf_pos ~start ~cur:sptr in
  let pos = write_ml buf ~pos:start_pos el in
  get_sptr buf ~pos

let make1 write_c write_ml_el buf ~pos el =
  let start, sptr, eptr = unsafe_get_init buf ~pos in
  let write_c_el = unmake write_ml_el buf ~start in
  let cur = write_c write_c_el sptr eptr el in
  get_safe_buf_pos buf ~start ~cur

let make2 write_c write_ml_el1 write_ml_el2 buf ~pos el =
  let start, sptr, eptr = unsafe_get_init buf ~pos in
  let write_c_el1 = unmake write_ml_el1 buf ~start in
  let write_c_el2 = unmake write_ml_el2 buf ~start in
  let cur = write_c write_c_el1 write_c_el2 sptr eptr el in
  get_safe_buf_pos buf ~start ~cur

let make3 write_c write_ml_el1 write_ml_el2 write_ml_el3 buf ~pos el =
  let start, sptr, eptr = unsafe_get_init buf ~pos in
  let write_c_el1 = unmake write_ml_el1 buf ~start in
  let write_c_el2 = unmake write_ml_el2 buf ~start in
  let write_c_el3 = unmake write_ml_el3 buf ~start in
  let cur = write_c write_c_el1 write_c_el2 write_c_el3 sptr eptr el in
  get_safe_buf_pos buf ~start ~cur

let bin_write_unit = make Unsafe_write_c.bin_write_unit
let bin_write_bool = make Unsafe_write_c.bin_write_bool
let bin_write_string = make Unsafe_write_c.bin_write_string
let bin_write_char = make Unsafe_write_c.bin_write_char
let bin_write_int = make Unsafe_write_c.bin_write_int
let bin_write_float = make Unsafe_write_c.bin_write_float
let bin_write_int32 = make Unsafe_write_c.bin_write_int32
let bin_write_int64 = make Unsafe_write_c.bin_write_int64
let bin_write_nativeint = make Unsafe_write_c.bin_write_nativeint
let bin_write_nat0 = make Unsafe_write_c.bin_write_nat0
let bin_write_ref mlw = make1 Unsafe_write_c.bin_write_ref mlw
let bin_write_lazy mlw = make1 Unsafe_write_c.bin_write_lazy mlw
let bin_write_option mlw = make1 Unsafe_write_c.bin_write_option mlw
let bin_write_pair mlw = make2 Unsafe_write_c.bin_write_pair mlw
let bin_write_triple mlw = make3 Unsafe_write_c.bin_write_triple mlw
let bin_write_list mlw = make1 Unsafe_write_c.bin_write_list mlw
let bin_write_array mlw = make1 Unsafe_write_c.bin_write_array mlw
let bin_write_hashtbl mlw = make2 Unsafe_write_c.bin_write_hashtbl mlw
let bin_write_float32_vec = make Unsafe_write_c.bin_write_float32_vec
let bin_write_float64_vec = make Unsafe_write_c.bin_write_float64_vec
let bin_write_vec = make Unsafe_write_c.bin_write_vec
let bin_write_float32_mat = make Unsafe_write_c.bin_write_float32_mat
let bin_write_float64_mat = make Unsafe_write_c.bin_write_float64_mat
let bin_write_mat = make Unsafe_write_c.bin_write_mat
let bin_write_bigstring = make Unsafe_write_c.bin_write_bigstring
let bin_write_float_array = make Unsafe_write_c.bin_write_float_array
let bin_write_variant_tag el = make Unsafe_write_c.bin_write_variant_tag el

let bin_write_array_no_length mlw =
  make1 Unsafe_write_c.bin_write_array_no_length mlw

let bin_write_int_64bit = make Unsafe_write_c.bin_write_int_64bit
let bin_write_int64_bits = make Unsafe_write_c.bin_write_int64_bits
let bin_write_network16_int = make Unsafe_write_c.bin_write_network16_int
let bin_write_network32_int = make Unsafe_write_c.bin_write_network32_int
let bin_write_network32_int32 = make Unsafe_write_c.bin_write_network32_int32
let bin_write_network64_int = make Unsafe_write_c.bin_write_network64_int
let bin_write_network64_int64 = make Unsafe_write_c.bin_write_network64_int64
