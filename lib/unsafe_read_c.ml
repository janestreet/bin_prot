(* Unsafe_read_c: reading values from the binary protocol using unsafe C. *)

open Bigarray
open Common
open Unsafe_common

exception Error of ReadError.t

let raise_variant_wrong_type type_name =
  raise (Error (ReadError.Variant_wrong_type type_name))

external init : unit -> unit = "bin_prot_unsafe_read_c_init_stub"

let () =
  let err = Error ReadError.Neg_int8 in
  Callback.register_exception "Bin_prot.Unsafe_read_c.Error" err;
  init ()

type 'a reader = sptr_ptr -> eptr -> 'a
type ('a, 'b) reader1 = 'a reader -> 'b reader
type ('a, 'b, 'c) reader2 = 'a reader -> ('b, 'c) reader1
type ('a, 'b, 'c, 'd) reader3 = 'a reader -> ('b, 'c, 'd) reader2

external bin_read_unit : sptr_ptr -> eptr -> unit = "read_unit_stub"
external bin_read_bool : sptr_ptr -> eptr -> bool = "read_bool_stub"
external bin_read_option_bool : sptr_ptr -> eptr -> bool = "read_option_bool_stub"
external bin_read_string : sptr_ptr -> eptr -> string = "read_string_stub"
external bin_read_char : sptr_ptr -> eptr -> char = "read_char_stub"
external bin_read_int : sptr_ptr -> eptr -> int = "read_int_stub"
external bin_read_float : sptr_ptr -> eptr -> float = "read_float_stub"
external bin_read_int32 : sptr_ptr -> eptr -> int32 = "read_int32_stub"
external bin_read_int64 : sptr_ptr -> eptr -> int64 = "read_int64_stub"

external bin_read_nativeint :
  sptr_ptr -> eptr -> nativeint = "read_nativeint_stub"

external bin_read_nat0 : sptr_ptr -> eptr -> Nat0.t = "read_nat0_stub"

let bin_read_ref bin_read_el sptr_ptr eptr =
  let el = bin_read_el sptr_ptr eptr in
  ref el

let bin_read_lazy bin_read_el sptr_ptr eptr =
  let el = bin_read_el sptr_ptr eptr in
  Lazy.lazy_from_val el

let bin_read_option bin_read_el sptr_ptr eptr =
  if bin_read_option_bool sptr_ptr eptr then
    let el = bin_read_el sptr_ptr eptr in
    Some el
  else None

let bin_read_pair bin_read_a bin_read_b sptr_ptr eptr =
  let a = bin_read_a sptr_ptr eptr in
  let b = bin_read_b sptr_ptr eptr in
  a, b

let bin_read_triple bin_read_a bin_read_b bin_read_c sptr_ptr eptr =
  let a = bin_read_a sptr_ptr eptr in
  let b = bin_read_b sptr_ptr eptr in
  let c = bin_read_c sptr_ptr eptr in
  a, b, c

let rec bin_read_n_rev_list bin_read_el sptr_ptr eptr len =
  let rec loop n acc =
    if n = 0 then acc
    else loop (n - 1) (bin_read_el sptr_ptr eptr :: acc)
  in
  loop len []

let bin_read_list bin_read_el sptr_ptr eptr =
  let len = (bin_read_nat0 sptr_ptr eptr :> int) in
  let rev_lst = bin_read_n_rev_list bin_read_el sptr_ptr eptr len in
  List.rev rev_lst

#ifndef ARCH_SIXTYFOUR
let dummy_float_buf = create_buf 8
let () = ignore (Write_ml.bin_write_float dummy_float_buf ~pos:0 3.1)
let dummy_float_buf_eptr = get_eptr dummy_float_buf ~pos:0
let max_array_length_2 = Sys.max_array_length / 2
#endif

let set_and_raise sptr_ptr sptr err =
  set_sptr_ptr_sptr sptr_ptr sptr;
  raise (Error err)

let bin_read_array bin_read_el sptr_ptr eptr =
  let sptr = get_sptr_ptr_sptr sptr_ptr in
  let len = (bin_read_nat0 sptr_ptr eptr :> int) in
  if len = 0 then [||]
  else (
#ifdef ARCH_SIXTYFOUR
    if len > Sys.max_array_length then
      set_and_raise sptr_ptr sptr ReadError.Array_too_long;
#else
    if len > max_array_length_2 then (
      let dummy_sptr_ptr = alloc_sptr_ptr dummy_float_buf ~pos:0 in
      let maybe_float =
        try
          let el = bin_read_el dummy_sptr_ptr dummy_float_buf_eptr in
          Some el
        with _ -> None
      in
      ignore (dealloc_sptr_ptr dummy_float_buf dummy_sptr_ptr);
      match maybe_float with
      | None ->
          if len > Sys.max_array_length then
            set_and_raise sptr_ptr sptr ReadError.Array_too_long
      | Some el ->
          if
            Obj.tag (Obj.repr el) = Obj.double_tag ||
            len > Sys.max_array_length
          then
            set_and_raise sptr_ptr sptr ReadError.Array_too_long
    );
#endif
    let first = bin_read_el sptr_ptr eptr in
    let res = Array.create len first in
    for i = 1 to len - 1 do
      let el = bin_read_el sptr_ptr eptr in
      Array.unsafe_set res i el
    done;
    res)

let bin_read_hashtbl bin_read_key bin_read_val sptr_ptr eptr =
  let len = (bin_read_nat0 sptr_ptr eptr :> int) in
  let read_kv_pair = bin_read_pair bin_read_key bin_read_val in
  let els = bin_read_n_rev_list read_kv_pair sptr_ptr eptr len in
  copy_htbl_list (Hashtbl.create len) els

external bin_read_float32_vec :
  sptr_ptr -> eptr -> vec32 = "read_float32_vec_stub"

external bin_read_float64_vec :
  sptr_ptr -> eptr -> vec64 = "read_float64_vec_stub"

external bin_read_vec : sptr_ptr -> eptr -> vec = "read_float64_vec_stub"

external bin_read_float32_mat :
  sptr_ptr -> eptr -> mat32 = "read_float32_mat_stub"

external bin_read_float64_mat :
  sptr_ptr -> eptr -> mat64 = "read_float64_mat_stub"

external bin_read_mat : sptr_ptr -> eptr -> mat = "read_float64_mat_stub"

external bin_read_bigstring : sptr_ptr -> eptr -> buf = "read_bigstring_stub"

external bin_read_float_array :
  sptr_ptr -> eptr -> float array = "read_float_array_stub"

external bin_read_variant_int :
  sptr_ptr -> eptr -> int = "read_variant_tag_stub"

external bin_read_variant_tag :
  sptr_ptr -> eptr -> [> ] = "read_variant_tag_stub"

external bin_read_raw_string :
  sptr_ptr -> eptr -> string -> pos : int -> len : int -> unit
  = "read_raw_string_stub"

let bin_read_raw_string sptr_ptr eptr str ~pos ~len =
  if pos < 0 then
    failwith "Bin_prot.unsafe_read_c.bin_read_raw_string: pos < 0"
  else if len < 0 then
    failwith "Bin_prot.unsafe_read_c.bin_read_raw_string: len < 0"
  else if pos + len > String.length str then
    failwith "Bin_prot.unsafe_read_c.bin_read_raw_string: pos + len > str_len"
  else bin_read_raw_string sptr_ptr eptr str ~pos ~len

external bin_read_int_8bit : sptr_ptr -> eptr -> int = "read_char_stub"
external bin_read_int_16bit : sptr_ptr -> eptr -> int = "read_int_16bit_stub"
external bin_read_int_32bit : sptr_ptr -> eptr -> int = "read_int_32bit_stub"
external bin_read_int_64bit : sptr_ptr -> eptr -> int = "read_int_64bit_stub"

external bin_read_int64_bits :
  sptr_ptr -> eptr -> int64 = "read_int64_bits_stub"

external bin_read_network16_int :
  sptr_ptr -> eptr -> int = "read_network16_int_stub"

external bin_read_network32_int :
  sptr_ptr -> eptr -> int = "read_network32_int_stub"

external bin_read_network32_int32 :
  sptr_ptr -> eptr -> int32 = "read_network32_int32_stub"

external bin_read_network64_int :
  sptr_ptr -> eptr -> int = "read_network64_int_stub"

external bin_read_network64_int64 :
  sptr_ptr -> eptr -> int64 = "read_network64_int64_stub"
