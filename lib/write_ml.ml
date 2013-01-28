(* Write_ml: writing values to the binary protocol using (mostly) OCaml. *)

#include "int_codes.mlh"

open Bigarray

open Common

type 'a writer = buf -> pos : pos -> 'a -> pos
type ('a, 'b) writer1 = 'a writer -> 'b writer
type ('a, 'b, 'c) writer2 = 'a writer -> ('b, 'c) writer1
type ('a, 'b, 'c, 'd) writer3 = 'a writer -> ('b, 'c, 'd) writer2

let bin_write_unit buf ~pos () =
  check_pos buf pos;
  buf.{pos} <- '\000';
  pos + 1

let bin_write_bool (buf : buf) ~pos b =
  check_pos buf pos;
  buf.{pos} <- if b then '\001' else '\000';
  pos + 1

let all_bin_write_small_int (buf : buf) ~pos n =
  check_pos buf pos;
  buf.{pos} <- Char.unsafe_chr n;
  pos + 1

let all_bin_write_neg_int8 (buf : buf) ~pos n =
  let next = pos + 2 in
  check_next buf next;
  buf.{pos} <- Char.unsafe_chr code_neg_int8;
  buf.{pos + 1} <- Char.unsafe_chr n;
  next

let all_bin_write_int16 (buf : buf) ~pos n =
  let next = pos + 3 in
  check_next buf next;
  buf.{pos} <- Char.unsafe_chr code_int16;
  buf.{pos + 1} <- Char.unsafe_chr n;
  buf.{pos + 2} <- Char.unsafe_chr (n asr 8);
  next

let all_bin_write_int32 (buf : buf) ~pos n =
  let next = pos + 5 in
  check_next buf next;
  buf.{pos} <- Char.unsafe_chr code_int32;
  buf.{pos + 1} <- Char.unsafe_chr n;
  buf.{pos + 2} <- Char.unsafe_chr (n asr 8);
  buf.{pos + 3} <- Char.unsafe_chr (n asr 16);
  buf.{pos + 4} <- Char.unsafe_chr (n asr 24);
  next

#ifdef ARCH_SIXTYFOUR
let all_bin_write_int64 (buf : buf) ~pos n =
  let next = pos + 9 in
  check_next buf next;
  buf.{pos} <- Char.unsafe_chr code_int64;
  buf.{pos + 1} <- Char.unsafe_chr n;
  buf.{pos + 2} <- Char.unsafe_chr (n asr 8);
  buf.{pos + 3} <- Char.unsafe_chr (n asr 16);
  buf.{pos + 4} <- Char.unsafe_chr (n asr 24);
  buf.{pos + 5} <- Char.unsafe_chr (n asr 32);
  buf.{pos + 6} <- Char.unsafe_chr (n asr 40);
  buf.{pos + 7} <- Char.unsafe_chr (n asr 48);
  buf.{pos + 8} <- Char.unsafe_chr (n asr 56);
  next
#endif

let bin_write_int_nat0 buf ~pos n =
  if      n  < 0x00000080 then all_bin_write_small_int buf ~pos n
  else if n  < 0x00008000 then all_bin_write_int16 buf ~pos n
#ifdef ARCH_SIXTYFOUR
  else if n >= 0x80000000 then all_bin_write_int64 buf ~pos n
#endif
  else all_bin_write_int32 buf ~pos n

let bin_write_int_negative buf ~pos n =
  if      n >= -0x00000080 then all_bin_write_neg_int8 buf ~pos n
  else if n >= -0x00008000 then all_bin_write_int16 buf ~pos n
#ifdef ARCH_SIXTYFOUR
  else if n  < -0x80000000 then all_bin_write_int64 buf ~pos n
#endif
  else all_bin_write_int32 buf ~pos n

let bin_write_char (buf : buf) ~pos c =
  check_pos buf pos;
  buf.{pos} <- c;
  pos + 1

let bin_write_int buf ~pos n =
  if n >= 0 then bin_write_int_nat0 buf ~pos n
  else bin_write_int_negative buf ~pos n

let bin_write_nat0 buf ~pos nat0 =
  let n = (nat0 : Nat0.t :> int) in
  if      n <   0x00000080 then all_bin_write_small_int buf ~pos n
  else if n <   0x00010000 then all_bin_write_int16 buf ~pos n
#ifdef ARCH_SIXTYFOUR
  else if n >= 0x100000000 then all_bin_write_int64 buf ~pos n
#endif
  else all_bin_write_int32 buf ~pos n

let bin_write_string buf ~pos str =
  let len = String.length str in
  let plen = Nat0.unsafe_of_int len in
  let new_pos = bin_write_nat0 buf ~pos plen in
  let next = new_pos + len in
  check_next buf next;
  (* TODO: optimize for small strings *)
  unsafe_blit_string_buf ~src_pos:0 str ~dst_pos:new_pos buf ~len;
  next

external bin_write_float :
  buf -> pos : int -> float -> int = "ml_write_float_stub"

#ifdef ARCH_SIXTYFOUR
let bin_write_int32 buf ~pos n = bin_write_int buf ~pos (Int32.to_int n)
#else
let bin_write_int32 buf ~pos n =
  if n >= 0x00008000l || n < -0x00008000l then
    let next = pos + 5 in
    check_next buf next;
    buf.{pos} <- Char.unsafe_chr code_int32;
    let n_int = Int32.to_int n in
    buf.{pos + 1} <- Char.unsafe_chr n_int;
    buf.{pos + 2} <- Char.unsafe_chr (n_int asr 8);
    buf.{pos + 3} <- Char.unsafe_chr (n_int asr 16);
    buf.{pos + 4} <- Char.unsafe_chr (Int32.to_int (Int32.shift_right n 24));
    next
  else bin_write_int buf ~pos (Int32.to_int n)
#endif

#ifdef ARCH_SIXTYFOUR
let bin_write_int64 buf ~pos n =
  if n >= 0x80000000L || n < -0x80000000L then
    let next = pos + 9 in
    check_next buf next;
    buf.{pos} <- Char.unsafe_chr code_int64;
    let n_int = Int64.to_int n in
    buf.{pos + 1} <- Char.unsafe_chr n_int;
    buf.{pos + 2} <- Char.unsafe_chr (n_int asr 8);
    buf.{pos + 3} <- Char.unsafe_chr (n_int asr 16);
    buf.{pos + 4} <- Char.unsafe_chr (n_int asr 24);
    buf.{pos + 5} <- Char.unsafe_chr (n_int asr 32);
    buf.{pos + 6} <- Char.unsafe_chr (n_int asr 40);
    buf.{pos + 7} <- Char.unsafe_chr (n_int asr 48);
    buf.{pos + 8} <- Char.unsafe_chr (Int64.to_int (Int64.shift_right n 56));
    next
  else bin_write_int buf ~pos (Int64.to_int n)
#else
let bin_write_int64 buf ~pos n =
  if n >= 0x80000000L || n < -0x80000000L then
    let next = pos + 9 in
    check_next buf next;
    buf.{pos} <- Char.unsafe_chr code_int64;
    let n1_int = Int64.to_int n in
    buf.{pos + 1} <- Char.unsafe_chr n1_int;
    buf.{pos + 2} <- Char.unsafe_chr (n1_int asr 8);
    buf.{pos + 3} <- Char.unsafe_chr (n1_int asr 16);
    let n2_int = Int64.to_int (Int64.shift_right n 24) in
    buf.{pos + 4} <- Char.unsafe_chr n2_int;
    buf.{pos + 5} <- Char.unsafe_chr (n2_int asr 8);
    buf.{pos + 6} <- Char.unsafe_chr (n2_int asr 16);
    let n3_int = Int64.to_int (Int64.shift_right n 48) in
    buf.{pos + 7} <- Char.unsafe_chr n3_int;
    buf.{pos + 8} <- Char.unsafe_chr (n3_int asr 8);
    next
  else bin_write_int32 buf ~pos (Int64.to_int32 n)
#endif

let bin_write_nativeint buf ~pos n =
#ifdef ARCH_SIXTYFOUR
  bin_write_int64 buf ~pos (Int64.of_nativeint n)
#else
  bin_write_int32 buf ~pos (Nativeint.to_int32 n)
#endif

let bin_write_ref bin_write_el buf ~pos r = bin_write_el buf ~pos !r

let bin_write_lazy bin_write_el buf ~pos lv =
  let v = Lazy.force lv in
  bin_write_el buf ~pos v

let bin_write_option bin_write_el buf ~pos = function
  | None -> bin_write_bool buf ~pos false
  | Some v ->
      let next = bin_write_bool buf ~pos true in
      bin_write_el buf ~pos:next v

let bin_write_pair bin_write_a bin_write_b buf ~pos (a, b) =
  let next = bin_write_a buf ~pos a in
  bin_write_b buf ~pos:next b

let bin_write_triple bin_write_a bin_write_b bin_write_c buf ~pos (a, b, c) =
  let next1 = bin_write_a buf ~pos a in
  let next2 = bin_write_b buf ~pos:next1 b in
  bin_write_c buf ~pos:next2 c

let bin_write_list bin_write_el buf ~pos lst =
  let rec loop els_pos = function
    | [] -> els_pos
    | h :: t ->
        let new_els_pos = bin_write_el buf ~pos:els_pos h in
        loop new_els_pos t
  in
  let len = Nat0.unsafe_of_int (List.length lst) in
  let els_pos = bin_write_nat0 buf ~pos len in
  loop els_pos lst

let bin_write_array_loop bin_write_el buf ~els_pos ~n ar =
  let els_pos_ref = ref els_pos in
  for i = 0 to n - 1 do
    els_pos_ref := bin_write_el buf ~pos:!els_pos_ref (Array.unsafe_get ar i)
  done;
  !els_pos_ref

let bin_write_array bin_write_el buf ~pos ar =
  let n = Array.length ar in
  let pn = Nat0.unsafe_of_int n in
  let els_pos = bin_write_nat0 buf ~pos pn in
  bin_write_array_loop bin_write_el buf ~els_pos ~n ar

let bin_write_hashtbl bin_write_key bin_write_val buf ~pos htbl =
  let len = Hashtbl.length htbl in
  let plen = Nat0.unsafe_of_int len in
  let els_pos = bin_write_nat0 buf ~pos plen in
  let cnt_ref = ref 0 in
  let coll_htbl k v els_pos =
    incr cnt_ref;
    let new_els_pos = bin_write_key buf ~pos:els_pos k in
    bin_write_val buf ~pos:new_els_pos v
  in
  let res_pos = Hashtbl.fold coll_htbl htbl els_pos in
  if !cnt_ref <> len then raise_concurrent_modification "bin_write_hashtbl";
  res_pos

external bin_write_float32_vec :
  buf -> pos : int -> vec32 -> int = "ml_write_float32_vec_stub"

external bin_write_float64_vec :
  buf -> pos : int -> vec64 -> int = "ml_write_float64_vec_stub"

external bin_write_vec :
  buf -> pos : int -> vec -> int = "ml_write_float64_vec_stub"

external bin_write_float32_mat :
  buf -> pos : int -> mat32 -> int = "ml_write_float32_mat_stub"

external bin_write_float64_mat :
  buf -> pos : int -> mat64 -> int = "ml_write_float64_mat_stub"

external bin_write_mat :
  buf -> pos : int -> mat -> int = "ml_write_float64_mat_stub"

external bin_write_bigstring :
  buf -> pos : int -> buf -> int = "ml_write_bigstring_stub"

external bin_write_float_array :
  buf -> pos : int -> float array -> int = "ml_write_float_array_stub"

external bin_write_variant_tag :
  buf -> pos : int -> [> ] -> int = "ml_write_variant_tag_stub"

external bin_write_int_8bit :
  buf -> pos : int -> int -> int = "ml_write_int_8bit_stub"

external bin_write_int_16bit :
  buf -> pos : int -> int -> int = "ml_write_int_16bit_stub"

external bin_write_int_32bit :
  buf -> pos : int -> int -> int = "ml_write_int_32bit_stub"

external bin_write_int_64bit :
  buf -> pos : int -> int -> int = "ml_write_int_64bit_stub"

external bin_write_int64_bits :
  buf -> pos : int -> int64 -> int = "ml_write_int64_bits_stub"

external bin_write_network16_int :
  buf -> pos : int -> int -> int = "ml_write_network16_int_stub"

external bin_write_network32_int :
  buf -> pos : int -> int -> int = "ml_write_network32_int_stub"

external bin_write_network32_int32 :
  buf -> pos : int -> int32 -> int = "ml_write_network32_int32_stub"

external bin_write_network64_int :
  buf -> pos : int -> int -> int = "ml_write_network64_int_stub"

external bin_write_network64_int64 :
  buf -> pos : int -> int64 -> int = "ml_write_network64_int64_stub"

let bin_write_array_no_length bin_write_el buf ~pos ar =
  bin_write_array_loop bin_write_el buf ~els_pos:pos ~n:(Array.length ar) ar
