(* Read_ml: reading values from the binary protocol using (mostly) OCaml. *)

#include "int_codes.mlh"

open Bigarray
open Common

type 'a reader = buf -> pos_ref : pos_ref -> 'a
type ('a, 'b) reader1 = 'a reader -> 'b reader
type ('a, 'b, 'c) reader2 = 'a reader -> ('b, 'c) reader1
type ('a, 'b, 'c, 'd) reader3 = 'a reader -> ('b, 'c, 'd) reader2

external unsafe_get : buf -> int -> char = "%caml_ba_unsafe_ref_1";;
external unsafe_get8 : buf -> int -> int = "%caml_ba_unsafe_ref_1";;

let unsafe_get8_signed buf pos =
  let c = unsafe_get8 buf pos in
  if c >= 128 then c - 256 else c
;;

(* We use cpp macros to inline basic functions.  We don't rely on OCaml inlining because
   it doesn't remove the boxing when not necessary.  For example, with this code:

   {[
     Int64.to_int (f x y)
   ]}

   a temporary int64 will still be allocated, even if [f] is inlined.
*)

#ifdef ARCH_SIXTYFOUR

#define SAFE_INT_OF_INT32(pos, x) (Int32.to_int (x))

#define SAFE_INT_OF_INT64(pos, x) \
  (if (x) >= -0x4000_0000_0000_0000L && (x) < 0x4000_0000_0000_0000L then \
     Int64.to_int (x) \
   else \
     raise_read_error ReadError.Int_overflow (pos))

#define SAFE_NATIVEINT_OF_INT64(pos, x) (Int64.to_nativeint (x))

#else

#define SAFE_INT_OF_INT32(pos, x) \
  (if (x) >= -0x4000_0000l && (x) < 0x4000_0000l then \
     Int32.to_int (x) \
   else \
     raise_read_error ReadError.Int_overflow (pos))

#define SAFE_INT_OF_INT64(pos, x) \
  (if (x) >= -0x0000_0000_4000_0000L && (x) < 0x0000_0000_4000_0000L then \
     Int64.to_int (x) \
   else \
     raise_read_error ReadError.Int_overflow (pos))

#define SAFE_NATIVEINT_OF_INT64(pos, x) \
  (if (x) >= -0x0000_0000_8000_0000L && (x) < 0x0000_0000_8000_0000L then \
     Int64.to_nativeint (x) \
   else \
     raise_read_error ReadError.Int_overflow (pos))

#endif

#ifdef HAVE_FAST_BA_ACCESS

external unsafe_get16 : buf -> int -> int = "%caml_bigstring_get16u";;
external unsafe_get32 : buf -> int -> int32 = "%caml_bigstring_get32u";;
external unsafe_get64 : buf -> int -> int64 = "%caml_bigstring_get64u";;

external bswap16 : int -> int = "%bswap16";;
external bswap32 : int32 -> int32 = "%bswap_int32";;
external bswap64 : int64 -> int64 = "%bswap_int64";;

#ifdef ARCH_BIG_ENDIAN

#define UNSAFE_GET16BE_UNSIGNED(buf, pos) (unsafe_get16 (buf) (pos))
#define UNSAFE_GET32BE(buf, pos) (unsafe_get32 (buf) (pos))
#define UNSAFE_GET64BE(buf, pos) (unsafe_get64 (buf) (pos))

#define UNSAFE_GET16LE_UNSIGNED(buf, pos) (bswap16 (unsafe_get16 (buf) (pos)))
#define UNSAFE_GET32LE(buf, pos) (bswap32 (unsafe_get32 (buf) (pos)))
#define UNSAFE_GET64LE(buf, pos) (bswap64 (unsafe_get64 (buf) (pos)))

#else

#define UNSAFE_GET16LE_UNSIGNED(buf, pos) (unsafe_get16 (buf) (pos))
#define UNSAFE_GET32LE(buf, pos) (unsafe_get32 (buf) (pos))
#define UNSAFE_GET64LE(buf, pos) (unsafe_get64 (buf) (pos))

#define UNSAFE_GET16BE_UNSIGNED(buf, pos) (bswap16 (unsafe_get16 (buf) (pos)))
#define UNSAFE_GET32BE(buf, pos) (bswap32 (unsafe_get32 (buf) (pos)))
#define UNSAFE_GET64BE(buf, pos) (bswap64 (unsafe_get64 (buf) (pos)))

#endif

#define UNSAFE_GET16LE_SIGNED(buf, pos) \
  (let x = UNSAFE_GET16LE_UNSIGNED(buf, pos) in \
   if x > 32767 then \
     x - 65536 \
   else \
     x)

#define UNSAFE_GET16BE_SIGNED(buf, pos) \
  (let x = UNSAFE_GET16BE_UNSIGNED(buf, pos) in \
   if x > 32767 then \
     x - 65536 \
   else \
     x)

#else

#define UNSAFE_GET8_AS_INT32(buf, pos) (Int32.of_int (unsafe_get8 (buf) (pos)))
#define UNSAFE_GET8_AS_INT64(buf, pos) (Int64.of_int (unsafe_get8 (buf) (pos)))

#define UNSAFE_GET16LE_UNSIGNED(buf, pos) \
  (let n = unsafe_get8 (buf) (pos) in \
   n lor (unsafe_get8 (buf) (pos + 1) lsl 8))

#define UNSAFE_GET16LE_SIGNED(buf, pos) \
  (let n = unsafe_get8 (buf) (pos) in \
   n lor (unsafe_get8_signed (buf) (pos + 1) lsl 8))

#define UNSAFE_GET32LE(buf, pos) \
  (let n = UNSAFE_GET8_AS_INT32(buf, pos) in \
   let n = Int32.logor n (Int32.shift_left (UNSAFE_GET8_AS_INT32 (buf, pos + 1)) 8) in \
   let n = Int32.logor n (Int32.shift_left (UNSAFE_GET8_AS_INT32 (buf, pos + 2)) 16) in \
   Int32.logor n (Int32.shift_left (UNSAFE_GET8_AS_INT32 (buf, pos + 3)) 24))

#define UNSAFE_GET64LE(buf, pos) \
  (let n = UNSAFE_GET8_AS_INT64(buf, pos) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 1)) 8) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 2)) 16) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 3)) 24) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 4)) 32) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 5)) 40) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 6)) 48) in \
   Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 7)) 56))

#define UNSAFE_GET16BE_UNSIGNED(buf, pos) \
  (let n = unsafe_get8 buf (pos + 1) in \
   n lor (unsafe_get8 buf pos lsl 8))

#define UNSAFE_GET16BE_SIGNED(buf, pos) \
  (let n = unsafe_get8 buf (pos + 1) in \
   n lor (unsafe_get8_signed buf pos lsl 8))

#define UNSAFE_GET32BE(buf, pos) \
  (let n = UNSAFE_GET8_AS_INT32(buf, pos + 3) in \
   let n = Int32.logor n (Int32.shift_left (UNSAFE_GET8_AS_INT32 (buf, pos + 2)) 8) in \
   let n = Int32.logor n (Int32.shift_left (UNSAFE_GET8_AS_INT32 (buf, pos + 1)) 16) in \
   Int32.logor n (Int32.shift_left (UNSAFE_GET8_AS_INT32 (buf, pos)) 24))

#define UNSAFE_GET64BE(buf, pos) \
  (let n = UNSAFE_GET8_AS_INT64(buf, pos + 7) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 6)) 8) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 5)) 16) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 4)) 24) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 3)) 32) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 2)) 40) in \
   let n = Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos + 1)) 48) in \
   Int64.logor n (Int64.shift_left (UNSAFE_GET8_AS_INT64 (buf, pos)) 56))

#endif

let bin_read_unit buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  if unsafe_get buf pos = '\000' then
    pos_ref := pos + 1
  else
    raise_read_error ReadError.Unit_code pos

let bin_read_bool buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\000' ->
    pos_ref := pos + 1;
    false
  | '\001' ->
    pos_ref := pos + 1;
    true
  | _ ->
    raise_read_error ReadError.Bool_code pos

let safe_bin_read_neg_int8 buf ~pos_ref ~pos =
  let next = pos + 1 in
  check_next buf next;
  let n = unsafe_get8_signed buf pos in
  if n >= 0 then raise_read_error ReadError.Neg_int8 !pos_ref;
  pos_ref := next;
  n

let safe_bin_read_int16 buf ~pos_ref ~pos =
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next; (* Can be above next line (no errors possible with 16bit).
                      This should improve the generated code. *)
  UNSAFE_GET16LE_SIGNED(buf, pos)

let safe_bin_read_int32 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next; (* No error possible either. *)
  UNSAFE_GET32LE(buf, pos)

let safe_bin_read_int64 buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next; (* No error possible either. *)
  UNSAFE_GET64LE(buf, pos)

let safe_bin_read_int32_as_int buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  let n = UNSAFE_GET32LE(buf, pos) in
  let n = SAFE_INT_OF_INT32(!pos_ref, n) in
  pos_ref := next;
  n

let safe_bin_read_int64_as_int buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = UNSAFE_GET64LE(buf, pos) in
  let n = SAFE_INT_OF_INT64(!pos_ref, n) in
  pos_ref := next;
  n

let safe_bin_read_int32_as_int64 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = UNSAFE_GET32LE(buf, pos) in
  Int64.of_int32 n

let safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = UNSAFE_GET32LE(buf, pos) in
  Nativeint.of_int32 n

let safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = UNSAFE_GET64LE(buf, pos) in
  let n = SAFE_NATIVEINT_OF_INT64(pos, n) in
  pos_ref := next;
  n

let safe_bin_read_nat0_16 buf ~pos_ref ~pos =
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  Nat0.unsafe_of_int (UNSAFE_GET16LE_UNSIGNED(buf, pos))

#ifdef ARCH_SIXTYFOUR

let safe_bin_read_nat0_32 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = Int32.to_int (UNSAFE_GET32LE(buf, pos)) in
  if n >= 0 then
    Nat0.unsafe_of_int n
  else
    (* Erase the upper bits that were set to 1 during the int32 -> int conversion. *)
    Nat0.unsafe_of_int (n land 0xffff_ffff)

let safe_bin_read_nat0_64 buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = UNSAFE_GET64LE(buf, pos) in
  if n >= 0L && n < 0x4000_0000_0000_0000L then begin
    let n = Nat0.unsafe_of_int (Int64.to_int n) in
    pos_ref := next;
    n
  end else
    raise_read_error ReadError.Nat0_overflow !pos_ref

#else

let safe_bin_read_nat0_32 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  let n = UNSAFE_GET32LE(buf, pos) in
  if n >= 0l && n < 0x4000_0000l then begin
    let n = Nat0.unsafe_of_int (Int32.to_int n) in
    pos_ref := next;
    n
  end else
    raise_read_error ReadError.Nat0_overflow !pos_ref

#endif

let bin_read_nat0 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Nat0.unsafe_of_int (Char.code ch)
  | CODE_INT16 ->
    safe_bin_read_nat0_16 buf ~pos_ref ~pos:(pos + 1)
  | CODE_INT32 ->
    safe_bin_read_nat0_32 buf ~pos_ref ~pos:(pos + 1)
#ifdef ARCH_SIXTYFOUR
  | CODE_INT64 ->
    safe_bin_read_nat0_64 buf ~pos_ref ~pos:(pos + 1)
#endif
  | _ ->
    raise_read_error ReadError.Nat0_code pos
;;

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

let bin_read_char buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  pos_ref := pos + 1;
  unsafe_get buf pos

let bin_read_int buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Char.code ch
  | CODE_NEG_INT8 ->
    safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1)
  | CODE_INT16 ->
    safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1)
  | CODE_INT32 ->
    safe_bin_read_int32_as_int buf ~pos_ref ~pos:(pos + 1)
#ifdef ARCH_SIXTYFOUR
  | CODE_INT64 ->
    safe_bin_read_int64_as_int buf ~pos_ref ~pos:(pos + 1)
#endif
  | _ ->
    raise_read_error ReadError.Int_code pos
;;


(* The C stubs returns the address of buf.{pos} as a float array. This is a hack to trick
   OCaml to read the float at this address. This way it can unbox when [bin_read_float] is
   inlined. *)
external get_float_offset : buf -> pos -> float array
  = "bin_prot_get_float_offset" "noalloc"

let bin_read_float buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* We must use the unsafe function to prevent OCaml from checking the length: the float
     array returned by [get_float_offset] has no header! *)
  Array.unsafe_get (get_float_offset buf pos) 0
;;

let bin_read_int32 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Int32.of_int (Char.code ch)
  | CODE_NEG_INT8 ->
    Int32.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | CODE_INT16 ->
    Int32.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | CODE_INT32 ->
    safe_bin_read_int32 buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Int32_code pos
;;

let bin_read_int64 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Int64.of_int (Char.code ch)
  | CODE_NEG_INT8 ->
    Int64.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | CODE_INT16 ->
    Int64.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | CODE_INT32 ->
    safe_bin_read_int32_as_int64 buf ~pos_ref ~pos:(pos + 1)
  | CODE_INT64 ->
    safe_bin_read_int64 buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Int64_code pos
;;

let bin_read_nativeint buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Nativeint.of_int (Char.code ch)
  | CODE_NEG_INT8 ->
    Nativeint.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | CODE_INT16 ->
    Nativeint.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | CODE_INT32 ->
    safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos:(pos + 1)
#ifdef ARCH_SIXTYFOUR
  | CODE_INT64 ->
    safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos:(pos + 1)
#endif
  | _ ->
    raise_read_error ReadError.Nativeint_code pos
;;

let bin_read_ref bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  ref el

let bin_read_lazy bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  Lazy.lazy_from_val el

let bin_read_option bin_read_el buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
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

let bin_read_n_rev_list bin_read_el buf ~pos_ref len =
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
let () = ignore (Write.bin_write_float dummy_float_buf ~pos:0 3.1)
let max_array_length_2 = Sys.max_array_length / 2
#endif

let bin_read_array bin_read_el buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len = 0 then [||]
  else begin
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
    res
  end

let bin_read_hashtbl bin_read_key bin_read_val buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let htbl = Hashtbl.create len in
  let read_kv_pair = bin_read_pair bin_read_key bin_read_val in
  let els = bin_read_n_rev_list read_kv_pair buf ~pos_ref len in
  copy_htbl_list htbl els
;;

external buf_of_vec32 : vec32 -> buf = "%identity"
external buf_of_vec64 : vec64 -> buf = "%identity"
external buf_of_mat32 : mat32 -> buf = "%identity"
external buf_of_mat64 : mat64 -> buf = "%identity"

let bin_read_float32_vec buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len * 4 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let vec = Array1.create float32 fortran_layout len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_vec32 vec) ~dst_pos:0 ~len:size;
  pos_ref := next;
  vec
;;

let bin_read_float64_vec buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let vec = Array1.create float64 fortran_layout len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_vec64 vec) ~dst_pos:0 ~len:size;
  pos_ref := next;
  vec
;;

let bin_read_vec = bin_read_float64_vec

let bin_read_float32_mat buf ~pos_ref =
  let len1 = (bin_read_nat0 buf ~pos_ref :> int) in
  let len2 = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len1 * len2 * 4 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let mat = Array2.create float32 fortran_layout len1 len2 in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_mat32 mat) ~dst_pos:0 ~len:size;
  pos_ref := next;
  mat
;;

let bin_read_float64_mat buf ~pos_ref =
  let len1 = (bin_read_nat0 buf ~pos_ref :> int) in
  let len2 = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len1 * len2 * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let mat = Array2.create float64 fortran_layout len1 len2 in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_mat64 mat) ~dst_pos:0 ~len:size;
  pos_ref := next;
  mat
;;

let bin_read_mat = bin_read_float64_mat

let bin_read_bigstring buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let pos = !pos_ref in
  let next = pos + len in
  check_next buf next;
  let str = create_buf len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:str ~dst_pos:0 ~len;
  pos_ref := next;
  str
;;

let bin_read_float_array buf ~pos_ref =
  let pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
#ifdef ARCH_SIXTYFOUR
  if len > Sys.max_array_length then raise_read_error ReadError.Array_too_long pos;
#else
  if len > max_array_length_2   then raise_read_error ReadError.Array_too_long pos;
#endif
  let size = len * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let arr = Array.create len 0. in
  unsafe_blit_buf_float_array buf arr ~src_pos:pos ~dst_pos:0 ~len;
  pos_ref := next;
  arr
;;

let bin_read_variant_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  let n = UNSAFE_GET32LE(buf, pos) in
  (* [n] must contain an integer already encoded, i.e. [n = 2 * k + 1]. *)
  if Int32.logand n 1l = 0l then
    raise (Read_error (ReadError.Variant_tag, pos))
  else begin
    (* We shift it by one bit to the right se we get back [2 * k + 1] in the end. *)
    pos_ref := next;
    Int32.to_int (Int32.shift_right n 1)
  end
;;

let bin_read_int_8bit buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  pos_ref := pos + 1;
  unsafe_get8 buf pos
;;

let bin_read_int_16bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  UNSAFE_GET16LE_UNSIGNED(buf, pos)

let bin_read_int_32bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = UNSAFE_GET32LE(buf, pos) in
  SAFE_INT_OF_INT32(pos, n)

let bin_read_int_64bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = UNSAFE_GET64LE(buf, pos) in
  SAFE_INT_OF_INT64(pos, n)

let bin_read_int64_bits buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  UNSAFE_GET64LE(buf, pos)

let bin_read_network16_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  UNSAFE_GET16BE_UNSIGNED(buf, pos)

let bin_read_network32_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = UNSAFE_GET32BE(buf, pos) in
  SAFE_INT_OF_INT32(pos, n)

let bin_read_network32_int32 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  UNSAFE_GET32BE(buf, pos)

let bin_read_network64_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = UNSAFE_GET64BE(buf, pos) in
  SAFE_INT_OF_INT64(pos, n)

let bin_read_network64_int64 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  UNSAFE_GET64BE(buf, pos)
