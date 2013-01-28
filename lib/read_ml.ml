(* Read_ml: reading values from the binary protocol using (mostly) OCaml. *)

#include "int_codes.mlh"

open Bigarray

open Common

external init : unit -> unit = "bin_prot_read_ml_init_stub"

let () =
  let read_error = Read_error (ReadError.Neg_int8, 0) in
  Callback.register_exception "Bin_prot.Common.Read_error" read_error;
  init ()

type 'a reader = buf -> pos_ref : pos_ref -> 'a
type ('a, 'b) reader1 = 'a reader -> 'b reader
type ('a, 'b, 'c) reader2 = 'a reader -> ('b, 'c) reader1
type ('a, 'b, 'c, 'd) reader3 = 'a reader -> ('b, 'c, 'd) reader2

let rewrite_c_error f buf ~pos_ref =
  try f buf ~pos_ref
  with Unsafe_read_c.Error read_err -> raise_read_error read_err !pos_ref

let bin_read_unit (buf : buf) ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  if buf.{pos} = '\000' then pos_ref := pos + 1
  else raise_read_error ReadError.Unit_code pos

let bin_read_bool (buf : buf) ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  match buf.{pos} with
  | '\000' ->
      pos_ref := pos + 1;
      false
  | '\001' ->
      pos_ref := pos + 1;
      true
  | _ -> raise_read_error ReadError.Bool_code pos

let get_signed_code (buf : buf) i =
  let c = Char.code buf.{i} in
  if c >= 128 then c - 256
  else c

let safe_bin_read_neg_int8 (buf : buf) ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  let n = get_signed_code buf pos in
  if n >= 0 then raise_read_error ReadError.Neg_int8 (pos - 1);
  pos_ref := pos + 1;
  n

let do_bin_read_int16 (buf : buf) pos =
  let n = Char.code buf.{pos} in
  n + get_signed_code buf (pos + 1) lsl 8

let safe_bin_read_int16 buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;  (* Can be above next line (no errors possible with 16bit).
                       This should improve the generated code. *)
  do_bin_read_int16 buf pos

let check_overflow ~last ~pos =
  (* NOTE: OCaml integers are ordinary machine integers shifted to
     the left to gain one bit for tagging purposes (this bottommost
     bit is set for integers and unset for structured blocks).
     Negative numbers are handled the same way, and they are, as usual,
     in two-complement representation.  Therefore the topmost byte in
     the memory representation of the OCaml-integer has lost one bit,
     limiting the range of positive integers, whose topmost bit must
     always be unset, from 0x00 to 0x3f instead of from 0x00 to 0x7f.
     For negative integers this means that the topmost byte, whose
     topmost bit is always set in that case, does not range from 0xff
     down to 0x80, but only from 0xff to 0xc0 (note the reversal of the
     direction due to two-complement representation!).  Thus any topmost
     byte greater than 0x3f and lower than 0xc0 is illegal. *)
  if last > 0x3f && last < 0xc0 then
    raise_read_error ReadError.Int_overflow (pos - 1)

let do_bin_read_int32 (buf : buf) pos =
#ifdef ARCH_SIXTYFOUR
  let last = get_signed_code buf (pos + 3) in
#else
  let last = Char.code buf.{pos + 3} in
  check_overflow ~last ~pos;
#endif
  let n = last lsl 24 in
  let n = n + Char.code buf.{pos + 2} lsl 16 in
  let n = n + Char.code buf.{pos + 1} lsl 8 in
  n + Char.code buf.{pos}

let safe_bin_read_int32 buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 4 in
  check_next buf next;
  let n = do_bin_read_int32 buf pos in
  pos_ref := next;
  n

#ifdef ARCH_SIXTYFOUR
let do_bin_read_int64 (buf : buf) pos =
  let last = Char.code buf.{pos + 7} in
  check_overflow ~last ~pos;
  let n = last lsl 56 in
  let n = n + Char.code buf.{pos + 6} lsl 48 in
  let n = n + Char.code buf.{pos + 5} lsl 40 in
  let n = n + Char.code buf.{pos + 4} lsl 32 in
  let n = n + Char.code buf.{pos + 3} lsl 24 in
  let n = n + Char.code buf.{pos + 2} lsl 16 in
  let n = n + Char.code buf.{pos + 1} lsl 8 in
  n + Char.code buf.{pos}

let safe_bin_read_int64 buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 8 in
  check_next buf next;
  let n = do_bin_read_int64 buf pos in
  pos_ref := next;
  n
#endif

let do_bin_read_nat0_16 (buf : buf) pos =
  let n = Char.code buf.{pos} in
  n + Char.code buf.{pos + 1} lsl 8

let safe_bin_read_nat0_16 buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;  (* Can be above next line (no errors possible with 16bit).
                       This should improve the generated code. *)
  let n = do_bin_read_nat0_16 buf pos in
  Nat0.unsafe_of_int n

let check_pos_overflow ~last ~pos =
  (* NOTE: see {!check_overflow} for meaning of constant *)
  if last > 0x3f then raise_read_error ReadError.Nat0_overflow (pos - 1)

let do_bin_read_nat0_32 (buf : buf) pos =
  let last = Char.code buf.{pos + 3} in
#ifndef ARCH_SIXTYFOUR
  check_pos_overflow ~last ~pos;
#endif
  let n = last lsl 24 in
  let n = n + Char.code buf.{pos + 2} lsl 16 in
  let n = n + Char.code buf.{pos + 1} lsl 8 in
  n + Char.code buf.{pos}

let safe_bin_read_nat0_32 buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 4 in
  check_next buf next;
  let n = do_bin_read_nat0_32 buf pos in
  pos_ref := next;
  Nat0.unsafe_of_int n

#ifdef ARCH_SIXTYFOUR
let do_bin_read_nat0_64 (buf : buf) pos =
  let last = Char.code buf.{pos + 7} in
  check_pos_overflow ~last ~pos;
  let n = last lsl 56 in
  let n = n + Char.code buf.{pos + 6} lsl 48 in
  let n = n + Char.code buf.{pos + 5} lsl 40 in
  let n = n + Char.code buf.{pos + 4} lsl 32 in
  let n = n + Char.code buf.{pos + 3} lsl 24 in
  let n = n + Char.code buf.{pos + 2} lsl 16 in
  let n = n + Char.code buf.{pos + 1} lsl 8 in
  n + Char.code buf.{pos}

let safe_bin_read_nat0_64 buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 8 in
  check_next buf next;
  let n = do_bin_read_nat0_64 buf pos in
  pos_ref := next;
  Nat0.unsafe_of_int n
#endif

let bin_read_nat0 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  let code = get_signed_code buf pos in
  pos_ref := pos + 1;
  if code >= 0 then Nat0.unsafe_of_int code
  else if code = code_int16 then safe_bin_read_nat0_16 buf ~pos_ref
  else if code = code_int32 then safe_bin_read_nat0_32 buf ~pos_ref
#ifdef ARCH_SIXTYFOUR
  else if code = code_int64 then safe_bin_read_nat0_64 buf ~pos_ref
#endif
  else (
    pos_ref := pos;
    raise_read_error ReadError.Nat0_code pos)

let bin_read_string buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > Sys.max_string_length then
    raise_read_error ReadError.String_too_long start_pos;
  let pos = !pos_ref in
  let next = pos + len in
  check_next buf next;
  pos_ref := next;
  let str = String.create len in
  unsafe_blit_buf_string ~src_pos:pos buf ~dst_pos:0 str ~len;
  str

let bin_read_char (buf : buf) ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  pos_ref := pos + 1;
  buf.{pos}

let bin_read_int buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  let code = get_signed_code buf pos in
  pos_ref := pos + 1;
  if code >= 0 then code
  else if code = code_int16 then safe_bin_read_int16 buf ~pos_ref
  else if code = code_neg_int8 then safe_bin_read_neg_int8 buf ~pos_ref
  else if code = code_int32 then safe_bin_read_int32 buf ~pos_ref
#ifdef ARCH_SIXTYFOUR
  else if code = code_int64 then safe_bin_read_int64 buf ~pos_ref
#endif
  else (
    pos_ref := pos;
    raise_read_error ReadError.Int_code pos)

external bin_read_float :
  buf -> pos_ref : int ref -> float = "ml_read_float_stub"

let bin_read_float buf ~pos_ref = rewrite_c_error bin_read_float buf ~pos_ref

let read_int32_aux buf ~pos_ref =
#ifdef ARCH_SIXTYFOUR
  let n = safe_bin_read_int32 buf ~pos_ref in
  Int32.of_int n
#else
  let pos = !pos_ref in
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = Char.code buf.{pos} in
  let n = n + Char.code buf.{pos + 1} lsl 8 in
  let n = n + Char.code buf.{pos + 2} lsl 16 in
  let n32 = Int32.of_int (Char.code buf.{pos + 3}) in
  Int32.add (Int32.of_int n) (Int32.shift_left n32 24)
#endif

let bin_read_int32 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  let code = get_signed_code buf pos in
  pos_ref := pos + 1;
  if code >= 0 then Int32.of_int code
  else if code = code_int16 then
    let n = safe_bin_read_int16 buf ~pos_ref in
    Int32.of_int n
  else if code = code_neg_int8 then
    let n = safe_bin_read_neg_int8 buf ~pos_ref in
    Int32.of_int n
  else if code = code_int32 then read_int32_aux buf ~pos_ref
  else (
    pos_ref := pos;
    raise_read_error ReadError.Int32_code pos)

let read_int64_aux buf ~pos_ref =
  let pos = !pos_ref in
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = Char.code buf.{pos} in
  let n = n + Char.code buf.{pos + 1} lsl 8 in
  let n = n + Char.code buf.{pos + 2} lsl 16 in
#ifdef ARCH_SIXTYFOUR
  let n = n + Char.code buf.{pos + 3} lsl 24 in
  let n = n + Char.code buf.{pos + 4} lsl 32 in
  let n = n + Char.code buf.{pos + 5} lsl 40 in
  let n = n + Char.code buf.{pos + 6} lsl 48 in
  let n64 = Int64.of_int (Char.code buf.{pos + 7}) in
  Int64.add (Int64.of_int n) (Int64.shift_left n64 56)
#else
  let n64 = Int64.of_int (Char.code buf.{pos + 3}) in
  let n64_1 = Int64.add (Int64.of_int n) (Int64.shift_left n64 24) in
  let n = Char.code buf.{pos + 4} in
  let n = n + Char.code buf.{pos + 5} lsl 8 in
  let n = n + Char.code buf.{pos + 6} lsl 16 in
  let n64 = Int64.of_int (Char.code buf.{pos + 7}) in
  let n64_2 = Int64.add (Int64.of_int n) (Int64.shift_left n64 24) in
  Int64.add n64_1 (Int64.shift_left n64_2 32)
#endif

let bin_read_int64 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  let code = get_signed_code buf pos in
  pos_ref := pos + 1;
  if code >= 0 then Int64.of_int code
  else if code = code_int16 then
    let n = safe_bin_read_int16 buf ~pos_ref in
    Int64.of_int n
  else if code = code_neg_int8 then
    let n = safe_bin_read_neg_int8 buf ~pos_ref in
    Int64.of_int n
  else if code = code_int32 then
    let n = read_int32_aux buf ~pos_ref in
    Int64.of_int32 n
  else if code = code_int64 then read_int64_aux buf ~pos_ref
  else (
    pos_ref := pos;
    raise_read_error ReadError.Int64_code pos)

let bin_read_nativeint buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  let code = get_signed_code buf pos in
  pos_ref := pos + 1;
  if code >= 0 then Nativeint.of_int code
  else if code = code_int16 then
    let n = safe_bin_read_int16 buf ~pos_ref in
    Nativeint.of_int n
  else if code = code_neg_int8 then
    let n = safe_bin_read_neg_int8 buf ~pos_ref in
    Nativeint.of_int n
  else if code = code_int32 then
    let n = read_int32_aux buf ~pos_ref in
    Nativeint.of_int32 n
#ifdef ARCH_SIXTYFOUR
  else if code = code_int64 then
    let n = read_int64_aux buf ~pos_ref in
    Int64.to_nativeint n
#endif
  else (
    pos_ref := pos;
    raise_read_error ReadError.Nativeint_code pos)

let bin_read_ref bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  ref el

let bin_read_lazy bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  Lazy.lazy_from_val el

let bin_read_option bin_read_el buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  match buf.{pos} with
  | '\000' ->
      pos_ref := pos + 1;
      None
  | '\001' ->
      pos_ref := pos + 1;
      let el = bin_read_el buf ~pos_ref in
      Some el
  | _ -> raise_read_error ReadError.Option_code pos

let bin_read_pair bin_read_a bin_read_b buf ~pos_ref =
  let a = bin_read_a buf ~pos_ref in
  let b = bin_read_b buf ~pos_ref in
  a, b

let bin_read_triple bin_read_a bin_read_b bin_read_c buf ~pos_ref =
  let a = bin_read_a buf ~pos_ref in
  let b = bin_read_b buf ~pos_ref in
  let c = bin_read_c buf ~pos_ref in
  a, b, c

let rec bin_read_n_rev_list bin_read_el buf ~pos_ref len =
  let rec loop n acc =
    if n = 0 then acc
    else loop (n - 1) (bin_read_el buf ~pos_ref :: acc)
  in
  loop len []

let bin_read_list bin_read_el buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let rev_lst = bin_read_n_rev_list bin_read_el buf ~pos_ref len in
  List.rev rev_lst

#ifndef ARCH_SIXTYFOUR
let dummy_float_buf = create_buf 8
let () = ignore (Write_ml.bin_write_float dummy_float_buf ~pos:0 3.1)
let max_array_length_2 = Sys.max_array_length / 2
#endif

let bin_read_array bin_read_el buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len = 0 then [||]
  else (
#ifdef ARCH_SIXTYFOUR
    if len > Sys.max_array_length then
      raise_read_error ReadError.Array_too_long start_pos;
#else
    if len > max_array_length_2 then (
      let maybe_float =
        try
          let el = bin_read_el dummy_float_buf ~pos_ref:(ref 0) in
          Some el
        with _ -> None
      in
      match maybe_float with
      | None ->
          if len > Sys.max_array_length then
            raise_read_error ReadError.Array_too_long start_pos
      | Some el ->
          if
            Obj.tag (Obj.repr el) = Obj.double_tag ||
            len > Sys.max_array_length
          then raise_read_error ReadError.Array_too_long start_pos
    );
#endif
    let first = bin_read_el buf ~pos_ref in
    let res = Array.create len first in
    for i = 1 to len - 1 do
      let el = bin_read_el buf ~pos_ref in
      Array.unsafe_set res i el
    done;
    res)

let bin_read_hashtbl bin_read_key bin_read_val buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let htbl = Hashtbl.create len in
  let read_kv_pair = bin_read_pair bin_read_key bin_read_val in
  let els = bin_read_n_rev_list read_kv_pair buf ~pos_ref len in
  copy_htbl_list htbl els

external bin_read_float32_vec :
  buf -> pos_ref : pos_ref -> vec32 = "ml_read_float32_vec_stub"

let bin_read_float32_vec buf ~pos_ref =
  rewrite_c_error bin_read_float32_vec buf ~pos_ref

external bin_read_float64_vec :
  buf -> pos_ref : pos_ref -> vec64 = "ml_read_float64_vec_stub"

let bin_read_float64_vec buf ~pos_ref =
  rewrite_c_error bin_read_float64_vec buf ~pos_ref

external bin_read_vec :
  buf -> pos_ref : pos_ref -> vec = "ml_read_float64_vec_stub"

let bin_read_vec buf ~pos_ref = rewrite_c_error bin_read_vec buf ~pos_ref

external bin_read_float32_mat :
  buf -> pos_ref : pos_ref -> mat32 = "ml_read_float32_mat_stub"

let bin_read_float32_mat buf ~pos_ref =
  rewrite_c_error bin_read_float32_mat buf ~pos_ref

external bin_read_float64_mat :
  buf -> pos_ref : pos_ref -> mat64 = "ml_read_float64_mat_stub"

let bin_read_float64_mat buf ~pos_ref =
  rewrite_c_error bin_read_float64_mat buf ~pos_ref

external bin_read_mat :
  buf -> pos_ref : pos_ref -> mat = "ml_read_float64_mat_stub"

let bin_read_mat buf ~pos_ref = rewrite_c_error bin_read_mat buf ~pos_ref

external bin_read_bigstring :
  buf -> pos_ref : pos_ref -> buf = "ml_read_bigstring_stub"

let bin_read_bigstring buf ~pos_ref =
  rewrite_c_error bin_read_bigstring buf ~pos_ref

external bin_read_float_array :
  buf -> pos_ref : pos_ref -> float array = "ml_read_float_array_stub"

let bin_read_float_array buf ~pos_ref =
  rewrite_c_error bin_read_float_array buf ~pos_ref

external bin_read_variant_int :
  buf -> pos_ref : pos_ref -> int = "ml_read_variant_tag_stub"

let bin_read_variant_int buf ~pos_ref =
  rewrite_c_error bin_read_variant_int buf ~pos_ref

external bin_read_variant_tag :
  buf -> pos_ref : pos_ref -> [> ] = "ml_read_variant_tag_stub"

let bin_read_variant_tag buf ~pos_ref =
  rewrite_c_error bin_read_variant_tag buf ~pos_ref

external bin_read_int_8bit :
  buf -> pos_ref : pos_ref -> int = "ml_read_char_stub"

let bin_read_int_8bit buf ~pos_ref =
  rewrite_c_error bin_read_int_8bit buf ~pos_ref

external bin_read_int_16bit :
  buf -> pos_ref : pos_ref -> int = "ml_read_int_16bit_stub"

let bin_read_int_16bit buf ~pos_ref =
  rewrite_c_error bin_read_int_16bit buf ~pos_ref

external bin_read_int_32bit :
  buf -> pos_ref : pos_ref -> int = "ml_read_int_32bit_stub"

let bin_read_int_32bit buf ~pos_ref =
  rewrite_c_error bin_read_int_32bit buf ~pos_ref

external bin_read_int_64bit :
  buf -> pos_ref : pos_ref -> int = "ml_read_int_64bit_stub"

let bin_read_int_64bit buf ~pos_ref =
  rewrite_c_error bin_read_int_64bit buf ~pos_ref

external bin_read_int64_bits :
  buf -> pos_ref : pos_ref -> int64 = "ml_read_int64_bits_stub"

let bin_read_int64_bits buf ~pos_ref =
  rewrite_c_error bin_read_int64_bits buf ~pos_ref

external bin_read_network16_int :
  buf -> pos_ref : pos_ref -> int = "ml_read_network16_int_stub"

let bin_read_network16_int buf ~pos_ref =
  rewrite_c_error bin_read_network16_int buf ~pos_ref

external bin_read_network32_int :
  buf -> pos_ref : pos_ref -> int = "ml_read_network32_int_stub"

let bin_read_network32_int buf ~pos_ref =
  rewrite_c_error bin_read_network32_int buf ~pos_ref

external bin_read_network32_int32 :
  buf -> pos_ref : pos_ref -> int32 = "ml_read_network32_int32_stub"

let bin_read_network32_int32 buf ~pos_ref =
  rewrite_c_error bin_read_network32_int32 buf ~pos_ref

external bin_read_network64_int :
  buf -> pos_ref : pos_ref -> int = "ml_read_network64_int_stub"

let bin_read_network64_int buf ~pos_ref =
  rewrite_c_error bin_read_network64_int buf ~pos_ref

external bin_read_network64_int64 :
  buf -> pos_ref : pos_ref -> int64 = "ml_read_network64_int64_stub"

let bin_read_network64_int64 buf ~pos_ref =
  rewrite_c_error bin_read_network64_int64 buf ~pos_ref
