(* Unsafe_write_c: writing values to the binary protocol using unsafe C. *)

open Bigarray

open Common
open Unsafe_common

type 'a writer = sptr -> eptr -> 'a -> sptr
type ('a, 'b) writer1 = 'a writer -> 'b writer
type ('a, 'b, 'c) writer2 = 'a writer -> ('b, 'c) writer1
type ('a, 'b, 'c, 'd) writer3 = 'a writer -> ('b, 'c, 'd) writer2

external bin_write_unit : sptr -> eptr -> unit -> sptr = "write_small_int_stub"
external bin_write_bool : sptr -> eptr -> bool -> sptr = "write_small_int_stub"
external bin_write_string : sptr -> eptr -> string -> sptr = "write_string_stub"
external bin_write_char : sptr -> eptr -> char -> sptr = "write_small_int_stub"
external bin_write_int : sptr -> eptr -> int -> sptr = "write_int_stub"
external bin_write_float : sptr -> eptr -> float -> sptr = "write_float_stub"
external bin_write_int32 : sptr -> eptr -> int32 -> sptr = "write_int32_stub"
external bin_write_int64 : sptr -> eptr -> int64 -> sptr = "write_int64_stub"

external bin_write_nativeint :
  sptr -> eptr -> nativeint -> sptr = "write_nativeint_stub"

external bin_write_nat0 :
  sptr -> eptr -> Nat0.t -> sptr = "write_nat0_stub"

let bin_write_ref bin_write_el sptr eptr r = bin_write_el sptr eptr !r

let bin_write_lazy bin_write_el sptr eptr lv =
  let v = Lazy.force lv in
  bin_write_el sptr eptr v

let bin_write_option bin_write_el sptr eptr = function
  | None -> bin_write_bool sptr eptr false
  | Some v ->
      let new_sptr = bin_write_bool sptr eptr true in
      bin_write_el new_sptr eptr v

let bin_write_pair bin_write_a bin_write_b sptr eptr (a, b) =
  let new_sptr = bin_write_a sptr eptr a in
  bin_write_b new_sptr eptr b

let bin_write_triple bin_write_a bin_write_b bin_write_c sptr eptr (a, b, c) =
  let new_sptr1 = bin_write_a sptr eptr a in
  let new_sptr2 = bin_write_b new_sptr1 eptr b in
  bin_write_c new_sptr2 eptr c

let bin_write_list bin_write_el sptr eptr lst =
  let rec loop els_sptr = function
    | [] -> els_sptr
    | h :: t ->
        let new_els_sptr = bin_write_el els_sptr eptr h in
        loop new_els_sptr t
  in
  let plen = Nat0.unsafe_of_int (List.length lst) in
  let els_sptr = bin_write_nat0 sptr eptr plen in
  loop els_sptr lst

let bin_write_array_loop bin_write_el sptr eptr ar n =
  let els_sptr_ref = ref sptr in
  for i = 0 to n - 1 do
    let el = Array.unsafe_get ar i in
    let new_els_sptr = bin_write_el !els_sptr_ref eptr el in
    els_sptr_ref := new_els_sptr
  done;
  !els_sptr_ref

let bin_write_array bin_write_el sptr eptr ar =
  let n = Array.length ar in
  let pn = Nat0.unsafe_of_int n in
  let els_sptr = bin_write_nat0 sptr eptr pn in
  bin_write_array_loop bin_write_el els_sptr eptr ar n

let bin_write_hashtbl bin_write_key bin_write_val sptr eptr htbl =
  let len = Hashtbl.length htbl in
  let plen = Nat0.unsafe_of_int len in
  let els_sptr = bin_write_nat0 sptr eptr plen in
  let cnt_ref = ref 0 in
  let coll_htbl k v els_sptr =
    incr cnt_ref;
    let new_els_sptr = bin_write_key els_sptr eptr k in
    bin_write_val new_els_sptr eptr v
  in
  let res_sptr = Hashtbl.fold coll_htbl htbl els_sptr in
  if !cnt_ref <> len then raise_concurrent_modification "bin_write_hashtbl";
  res_sptr

external bin_write_float32_vec :
  sptr -> eptr -> vec32 -> sptr = "write_float32_vec_stub"

external bin_write_float64_vec :
  sptr -> eptr -> vec64 -> sptr = "write_float64_vec_stub"

external bin_write_vec : sptr -> eptr -> vec -> sptr = "write_float64_vec_stub"

external bin_write_float32_mat :
  sptr -> eptr -> mat32 -> sptr = "write_float32_mat_stub"

external bin_write_float64_mat :
  sptr -> eptr -> mat64 -> sptr = "write_float64_mat_stub"

external bin_write_mat : sptr -> eptr -> mat -> sptr = "write_float64_mat_stub"

external bin_write_bigstring :
  sptr -> eptr -> buf -> sptr = "write_bigstring_stub"

external bin_write_float_array :
  sptr -> eptr -> float array -> sptr = "write_float_array_stub"

external bin_write_variant_tag :
  sptr -> eptr -> [> ] -> sptr = "write_variant_tag_stub"

external bin_write_raw_string :
  sptr -> eptr -> string -> pos : int -> len : int -> sptr
  = "write_raw_string_stub"

let bin_write_raw_string sptr eptr str ~pos ~len =
  if pos < 0 then
    failwith "Bin_prot.unsafe_write_c.bin_write_raw_string: pos < 0"
  else if len < 0 then
    failwith "Bin_prot.unsafe_write_c.bin_write_raw_string: len < 0"
  else if pos + len > String.length str then
    failwith "Bin_prot.unsafe_write_c.bin_write_raw_string: pos + len > str_len"
  else bin_write_raw_string sptr eptr str ~pos ~len

external bin_write_int_8bit :
  sptr -> eptr -> int -> sptr = "write_int_8bit_stub"

external bin_write_int_16bit :
  sptr -> eptr -> int -> sptr = "write_int_16bit_stub"

external bin_write_int_32bit :
  sptr -> eptr -> int -> sptr = "write_int_32bit_stub"

external bin_write_int_64bit :
  sptr -> eptr -> int -> sptr = "write_int_64bit_stub"

external bin_write_int64_bits :
  sptr -> eptr -> int64 -> sptr = "write_int64_bits_stub"

external bin_write_network16_int :
  sptr -> eptr -> int -> sptr = "write_network16_int_stub"

external bin_write_network32_int :
  sptr -> eptr -> int -> sptr = "write_network32_int_stub"

external bin_write_network32_int32 :
  sptr -> eptr -> int32 -> sptr = "write_network32_int32_stub"

external bin_write_network64_int :
  sptr -> eptr -> int -> sptr = "write_network64_int_stub"

external bin_write_network64_int64 :
  sptr -> eptr -> int64 -> sptr = "write_network64_int64_stub"

let bin_write_array_no_length bin_write_el sptr eptr ar =
  bin_write_array_loop bin_write_el sptr eptr ar (Array.length ar)
