(* Read_ml: reading values from the binary protocol using (mostly) OCaml. *)

(* Note: the code is this file is carefully written to avoid unnecessary allocations. When
   touching this code, be sure to run the benchmarks to check for regressions. *)

open Bigarray
open Common
include Read_intf.Definitions

external unsafe_get : buf -> int -> char @@ portable = "%caml_ba_unsafe_ref_1"
external unsafe_get8 : buf -> int -> int @@ portable = "%caml_ba_unsafe_ref_1"

let unsafe_get8_signed buf pos =
  let c = unsafe_get8 buf pos in
  if c >= 128 then c - 256 else c
;;

(*$ open Bin_prot_cinaps $*)

let arch_sixtyfour = Sys.word_size = 64
let arch_big_endian = Sys.big_endian
let max_int_int32 = if arch_sixtyfour then Int32.max_int else Int32.of_int max_int
let min_int_int32 = if arch_sixtyfour then Int32.min_int else Int32.of_int min_int
let max_int_int64 = Int64.of_int max_int
let min_int_int64 = Int64.of_int min_int

external unsafe_get16 : buf -> int -> int @@ portable = "%caml_bigstring_get16u"

external unsafe_get32
  :  buf
  -> int
  -> (int32#[@unboxed])
  @@ portable
  = "%caml_bigstring_get32u#"

external unsafe_get64
  :  buf
  -> int
  -> (int64#[@unboxed])
  @@ portable
  = "%caml_bigstring_get64u#"

external unbox_int32
  :  (int32[@local_opt])
  -> (int32#[@unboxed])
  @@ portable
  = "%unbox_int32"

external unbox_int64
  :  (int64[@local_opt])
  -> (int64#[@unboxed])
  @@ portable
  = "%unbox_int64"

external unbox_nativeint
  :  (nativeint[@local_opt])
  -> (nativeint#[@unboxed])
  @@ portable
  = "%unbox_nativeint"

external box_int32 : (int32#[@unboxed]) -> (int32[@local_opt]) @@ portable = "%box_int32"
external box_int64 : (int64#[@unboxed]) -> (int64[@local_opt]) @@ portable = "%box_int64"

external box_nativeint
  :  (nativeint#[@unboxed])
  -> (nativeint[@local_opt])
  @@ portable
  = "%box_nativeint"

external bswap16 : int -> int @@ portable = "%bswap16"
external bswap32 : (int32[@local_opt]) -> (int32[@local_opt]) @@ portable = "%bswap_int32"
external bswap64 : (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%bswap_int64"

let bswap32 x = x |> box_int32 |> bswap32 |> unbox_int32 [@@inline]
let bswap64 x = x |> box_int64 |> bswap64 |> unbox_int64 [@@inline]

external int32_to_int : (int32[@local_opt]) -> int @@ portable = "%int32_to_int"
external int64_to_int : (int64[@local_opt]) -> int @@ portable = "%int64_to_int"

let int32_to_int x = x |> box_int32 |> int32_to_int [@@inline]
let int64_to_int x = x |> box_int64 |> int64_to_int [@@inline]

let[@inline always] safe_int_of_int32 pos x =
  if arch_sixtyfour
  then int32_to_int x
  else if box_int32 x >= min_int_int32 && box_int32 x <= max_int_int32
  then int32_to_int x
  else raise_read_error ReadError.Int_overflow pos
;;

let[@inline always] safe_int_of_int64 pos x =
  if box_int64 x >= min_int_int64 && box_int64 x <= max_int_int64
  then int64_to_int x
  else raise_read_error ReadError.Int_overflow pos
;;

external int64_to_nativeint
  :  (int64[@local_opt])
  -> (nativeint[@local_opt])
  @@ portable
  = "%int64_to_nativeint"

let int64_to_nativeint x = x |> box_int64 |> int64_to_nativeint |> unbox_nativeint
[@@inline]
;;

let safe_nativeint_of_int64 =
  if arch_sixtyfour
  then fun _pos x -> int64_to_nativeint x
  else
    fun [@inline always] pos x ->
    if box_int64 x >= Int64.of_nativeint Nativeint.min_int
       && box_int64 x <= Int64.of_nativeint Nativeint.max_int
    then int64_to_nativeint x
    else (
      match raise_read_error ReadError.Int_overflow pos with
      | (_ : Base.Nothing.t) -> .)
;;

let unsafe_get16be_unsigned =
  if arch_big_endian then unsafe_get16 else fun buf pos -> unsafe_get16 buf pos |> bswap16
;;

let unsafe_get32be =
  if arch_big_endian then unsafe_get32 else fun buf pos -> unsafe_get32 buf pos |> bswap32
;;

let unsafe_get64be =
  if arch_big_endian then unsafe_get64 else fun buf pos -> unsafe_get64 buf pos |> bswap64
;;

let unsafe_get16le_unsigned =
  if arch_big_endian then fun buf pos -> unsafe_get16 buf pos |> bswap16 else unsafe_get16
;;

let unsafe_get32le =
  if arch_big_endian then fun buf pos -> unsafe_get32 buf pos |> bswap32 else unsafe_get32
;;

let unsafe_get64le =
  if arch_big_endian then fun buf pos -> unsafe_get64 buf pos |> bswap64 else unsafe_get64
;;

let unsafe_get16le_signed buf pos =
  let x = unsafe_get16le_unsigned buf pos in
  if x > 32767 then x - 65536 else x
;;

let bin_read_unit buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  if unsafe_get buf pos = '\000'
  then pos_ref := pos + 1
  else raise_read_error ReadError.Unit_code pos
;;

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
  | _ -> raise_read_error ReadError.Bool_code pos
;;

let safe_bin_read_neg_int8 buf ~pos_ref ~pos =
  let next = pos + 1 in
  check_next buf next;
  let n = unsafe_get8_signed buf pos in
  if n >= 0 then raise_read_error ReadError.Neg_int8 !pos_ref;
  pos_ref := next;
  n
;;

let safe_bin_read_int16 buf ~pos_ref ~pos =
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  (* Can be above next line (no errors possible with 16bit).
     This should improve the generated code. *)
  unsafe_get16le_signed buf pos
;;

let safe_bin_read_int32 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  unsafe_get32le buf pos
;;

let safe_bin_read_int64 buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  unsafe_get64le buf pos
;;

let safe_bin_read_int32_as_int buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  let n = unsafe_get32le buf pos in
  let n = safe_int_of_int32 !pos_ref n in
  pos_ref := next;
  n
;;

let safe_bin_read_int64_as_int buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = unsafe_get64le buf pos in
  let n = safe_int_of_int64 !pos_ref n in
  pos_ref := next;
  n
;;

external int64_of_int32
  :  (int32[@local_opt])
  -> (int64[@local_opt])
  @@ portable
  = "%int64_of_int32"

let int64_of_int32 x = x |> box_int32 |> int64_of_int32 |> unbox_int64 [@@inline]

let safe_bin_read_int32_as_int64 buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  int64_of_int32 n
;;

external nativeint_of_int32
  :  (int32[@local_opt])
  -> (nativeint[@local_opt])
  @@ portable
  = "%nativeint_of_int32"

let nativeint_of_int32 x = x |> box_int32 |> nativeint_of_int32 |> unbox_nativeint
[@@inline]
;;

let safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos =
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  nativeint_of_int32 n
;;

let safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = unsafe_get64le buf pos in
  let n = safe_nativeint_of_int64 pos n in
  pos_ref := next;
  n
;;

let safe_bin_read_nat0_16 buf ~pos_ref ~pos =
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  Nat0.unsafe_of_int (unsafe_get16le_unsigned buf pos)
;;

let safe_bin_read_nat0_32 =
  if arch_sixtyfour
  then (
    let mask_32bit = Int64.to_int 0xffff_ffffL in
    fun buf ~pos_ref ~pos ->
      let next = pos + 4 in
      check_next buf next;
      pos_ref := next;
      let n = int32_to_int (unsafe_get32le buf pos) in
      if n >= 0
      then Nat0.unsafe_of_int n
      else
        (* Erase the upper bits that were set to 1 during the int32 -> int conversion. *)
        Nat0.unsafe_of_int (n land mask_32bit))
  else
    fun buf ~pos_ref ~pos ->
    let next = pos + 4 in
    check_next buf next;
    let n = unsafe_get32le buf pos in
    if box_int32 n >= 0l && box_int32 n <= max_int_int32
    then (
      let n = Nat0.unsafe_of_int (int32_to_int n) in
      pos_ref := next;
      n)
    else raise_read_error ReadError.Nat0_overflow !pos_ref
;;

let safe_bin_read_nat0_64 buf ~pos_ref ~pos =
  let next = pos + 8 in
  check_next buf next;
  let n = unsafe_get64le buf pos in
  if box_int64 n >= 0L && box_int64 n <= max_int_int64
  then (
    let n = Nat0.unsafe_of_int (int64_to_int n) in
    pos_ref := next;
    n)
  else raise_read_error ReadError.Nat0_overflow !pos_ref
;;

let bin_read_nat0 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    Nat0.unsafe_of_int (Char.code ch)
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) -> safe_bin_read_nat0_16 buf ~pos_ref ~pos:(pos + 1)
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) -> safe_bin_read_nat0_32 buf ~pos_ref ~pos:(pos + 1)
  (*$ Code.pipe_char INT64 *)
  | '\xfc' (*$*) ->
    if arch_sixtyfour
    then safe_bin_read_nat0_64 buf ~pos_ref ~pos:(pos + 1)
    else raise_read_error ReadError.Nat0_overflow pos
  | _ -> raise_read_error ReadError.Nat0_code pos
;;

[%%template
[@@@alloc.default a = (heap, stack)]

let unsafe_bin_read_bytes buf ~pos ~len =
  (let str = (Base.Bytes.create [@alloc a]) len in
   unsafe_blit_buf_bytes ~src_pos:pos buf ~dst_pos:0 str ~len;
   str)
  [@exclave_if_stack a]
;;

let bin_read_bytes buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > Sys.max_string_length then raise_read_error ReadError.String_too_long start_pos;
  let pos = !pos_ref in
  let next = pos + len in
  check_next_check_overflow buf pos next;
  pos_ref := next;
  (unsafe_bin_read_bytes [@alloc a]) buf ~pos ~len [@exclave_if_stack a]
[@@zero_alloc_if_stack a opt]
;;

let bin_read_string buf ~pos_ref =
  (let str = (bin_read_bytes [@alloc a]) buf ~pos_ref in
   Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str)
  [@exclave_if_stack a]
[@@zero_alloc_if_stack a opt]
;;]

let bin_read_char buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  pos_ref := pos + 1;
  unsafe_get buf pos
;;

let bin_read_int buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    Char.code ch
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) -> safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1)
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) -> safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1)
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) -> safe_bin_read_int32_as_int buf ~pos_ref ~pos:(pos + 1)
  (*$ Code.pipe_char INT64 *)
  | '\xfc' (*$*) ->
    if arch_sixtyfour
    then safe_bin_read_int64_as_int buf ~pos_ref ~pos:(pos + 1)
    else raise_read_error ReadError.Int_overflow pos
  | _ -> raise_read_error ReadError.Int_code pos
;;

external unbox_float
  :  (float[@local_opt])
  -> (float#[@unboxed])
  @@ portable
  = "%unbox_float"

external box_float : (float#[@unboxed]) -> (float[@local_opt]) @@ portable = "%box_float"

[%%template
[@@@alloc.default a = (heap, stack)]

let bin_read_float buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  let f = unsafe_get64le buf pos |> box_int64 |> Int64.float_of_bits |> unbox_float in
  box_float f [@exclave_if_stack a]
;;

let bin_read_int32 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    let n = Char.code ch in
    Int32.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) ->
    let n = safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1) in
    Int32.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) ->
    let n = safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1) in
    Int32.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) ->
    let n = safe_bin_read_int32 buf ~pos_ref ~pos:(pos + 1) in
    box_int32 n [@exclave_if_stack a]
  | _ -> raise_read_error ReadError.Int32_code pos
;;

let bin_read_int64 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    let n = Char.code ch in
    Int64.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) ->
    let n = safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1) in
    Int64.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) ->
    let n = safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1) in
    Int64.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) ->
    let n = safe_bin_read_int32_as_int64 buf ~pos_ref ~pos:(pos + 1) in
    box_int64 n [@exclave_if_stack a]
  (*$ Code.pipe_char INT64 *)
  | '\xfc' (*$*) ->
    let n = safe_bin_read_int64 buf ~pos_ref ~pos:(pos + 1) in
    box_int64 n [@exclave_if_stack a]
  | _ -> raise_read_error ReadError.Int64_code pos
;;

let bin_read_nativeint buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    let n = Char.code ch in
    Nativeint.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) ->
    let n = safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1) in
    Nativeint.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) ->
    let n = safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1) in
    Nativeint.of_int n [@exclave_if_stack a]
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) ->
    let n = safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos:(pos + 1) in
    box_nativeint n [@exclave_if_stack a]
  (*$ Code.pipe_char INT64 *)
  | '\xfc' (*$*) when arch_sixtyfour ->
    let n = safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos:(pos + 1) in
    box_nativeint n [@exclave_if_stack a]
  | _ -> raise_read_error ReadError.Nativeint_code pos
;;

let bin_read_ref bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  ref el [@exclave_if_stack a]
;;

let bin_read_option bin_read_el buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\000' ->
    pos_ref := pos + 1;
    None
  | '\001' ->
    pos_ref := pos + 1;
    (let el = bin_read_el buf ~pos_ref in
     Some el)
    [@exclave_if_stack a]
  | _ -> raise_read_error ReadError.Option_code pos
;;

let bin_read_pair bin_read_a bin_read_b buf ~pos_ref =
  (let a = bin_read_a buf ~pos_ref in
   let b = bin_read_b buf ~pos_ref in
   a, b)
  [@exclave_if_stack a]
;;

let bin_read_triple bin_read_a bin_read_b bin_read_c buf ~pos_ref =
  (let a = bin_read_a buf ~pos_ref in
   let b = bin_read_b buf ~pos_ref in
   let c = bin_read_c buf ~pos_ref in
   a, b, c)
  [@exclave_if_stack a]
;;]

let bin_read_lazy bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  Lazy.from_val el
;;

let[@tail_mod_cons] rec bin_read_n_list bin_read_el buf ~pos_ref len =
  if len = 0
  then []
  else (
    let el = bin_read_el buf ~pos_ref in
    el :: bin_read_n_list bin_read_el buf ~pos_ref (len - 1))
;;

let%template[@mode stack] bin_read_n_rev_list bin_read_el buf ~pos_ref len = exclave_
  let rec loop n acc = exclave_
    if n = 0 then acc else loop (n - 1) (bin_read_el buf ~pos_ref :: acc)
  in
  loop len []
;;

let rev l = exclave_
  let rec loop l acc = exclave_
    match l with
    | [] -> acc
    | hd :: rest -> loop rest (hd :: acc)
  in
  loop l []
;;

let%template[@mode stack] bin_read_n_list bin_read_el buf ~pos_ref len = exclave_
  rev ((bin_read_n_rev_list [@alloc stack]) bin_read_el buf ~pos_ref len)
;;

let%template[@alloc a = (heap, stack)] bin_read_list_with_max_len
  ~max_len
  bin_read_el
  buf
  ~pos_ref
  =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > max_len then raise_read_error (List_too_long { len; max_len }) !pos_ref;
  (bin_read_n_list [@alloc a]) bin_read_el buf ~pos_ref len [@exclave_if_stack a]
;;

let%template[@alloc a = (heap, stack)] bin_read_list bin_read_el buf ~pos_ref =
  (bin_read_list_with_max_len [@alloc a])
    ~max_len:max_int
    bin_read_el
    buf
    ~pos_ref [@exclave_if_stack a]
;;

let dummy_float_buf () =
  let buf = create_buf 8 in
  ignore (Write.bin_write_float buf ~pos:0 3.1 : int);
  buf
;;

let max_float_array_length =
  if arch_sixtyfour then Sys.max_array_length else Sys.max_array_length / 2
;;

let[@inline always] bin_read_float_array_gen ~create ~blit buf ~pos_ref =
  let pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > max_float_array_length then raise_read_error ReadError.Array_too_long pos;
  let size = len * 8 in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let arr = create len in
  blit ~src_pos:pos buf ~dst_pos:0 arr ~len;
  pos_ref := next;
  arr
;;

let bin_read_floatarray buf ~pos_ref =
  bin_read_float_array_gen
    ~create:Float.Array.create
    ~blit:unsafe_blit_buf_floatarray
    buf
    ~pos_ref
;;

let bin_read_float_array buf ~pos_ref =
  bin_read_float_array_gen
    ~create:Array.create_float
    ~blit:unsafe_blit_buf_float_array
    buf
    ~pos_ref
;;

let%template check_array_len (bin_read_el : (_ reader[@mode local])) ~len ~start_pos =
  let module Obj = Base.Obj in
  if arch_sixtyfour
  then (
    if len > Sys.max_array_length then raise_read_error ReadError.Array_too_long start_pos)
  else if len > Sys.max_array_length / 2
  then (
    let maybe_float =
      try
        let el = bin_read_el (dummy_float_buf ()) ~pos_ref:(ref 0) in
        Some el
      with
      | _ -> None
    in
    match maybe_float with
    | None ->
      if len > Sys.max_array_length
      then raise_read_error ReadError.Array_too_long start_pos
    | Some el ->
      if Obj.tag (Obj.repr el) = Stdlib.Obj.double_tag || len > Sys.max_array_length
      then raise_read_error ReadError.Array_too_long start_pos)
;;

let%template[@alloc a = (heap, stack)] [@inline] bin_read_array_aux__no_float_array_blit_optimization
  bin_read_el
  buf
  ~pos_ref
  =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len = 0
  then [||]
  else (
    check_array_len bin_read_el ~len ~start_pos;
    let first = bin_read_el buf ~pos_ref in
    (let res = (Base.Array.create [@alloc a]) ~len first in
     for i = 1 to len - 1 do
       let el = bin_read_el buf ~pos_ref in
       Base.Array.unsafe_set res i el
     done;
     res)
    [@exclave_if_stack a])
;;

let%template bin_read_array (type a) (bin_read_el : _ reader) buf ~pos_ref =
  if (Obj.magic (bin_read_el : a reader) : float reader) == bin_read_float
  then (Obj.magic (bin_read_float_array buf ~pos_ref : float array) : a array)
  else
    (bin_read_array_aux__no_float_array_blit_optimization [@alloc heap])
      bin_read_el
      buf
      ~pos_ref
;;

let%template[@alloc stack] bin_read_array bin_read_el buf ~pos_ref = exclave_
  (bin_read_array_aux__no_float_array_blit_optimization [@alloc stack])
    bin_read_el
    buf
    ~pos_ref
;;

let%template[@alloc a = (heap, stack)] bin_read_iarray bin_read_el buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  check_array_len bin_read_el ~len ~start_pos;
  (Base.Iarray.init [@alloc a])
    len
    ~f:(stack_ fun _ -> bin_read_el buf ~pos_ref [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
;;

external buf_of_array1
  :  (_ Stdlib.Bigarray.Array1.t[@local_opt])
  -> (buf[@local_opt])
  @@ portable
  = "%identity"

external buf_of_array2
  :  (_ Stdlib.Bigarray.Array2.t[@local_opt])
  -> (buf[@local_opt])
  @@ portable
  = "%identity"

let bin_read_bigarray1 (type k) ~(kind : (_, k) Stdlib.Bigarray.kind) ~layout buf ~pos_ref
  : (_, k, _) Stdlib.Bigarray.Array1.t
  =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len * Stdlib.Bigarray.kind_size_in_bytes kind in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let vec = Array1.create kind layout len in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_array1 vec) ~dst_pos:0 ~len:size;
  pos_ref := next;
  vec
;;

let bin_read_float64_vec =
  bin_read_bigarray1 ~kind:Bigarray.Float64 ~layout:Bigarray.Fortran_layout
;;

let bin_read_float32_vec =
  bin_read_bigarray1 ~kind:Bigarray.Float32 ~layout:Bigarray.Fortran_layout
;;

let bin_read_vec = bin_read_float64_vec

let bin_read_bigarray2 (type k) ~(kind : (_, k) Stdlib.Bigarray.kind) ~layout buf ~pos_ref
  : (_, k, _) Stdlib.Bigarray.Array2.t
  =
  let len1 = (bin_read_nat0 buf ~pos_ref :> int) in
  let len2 = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len1 * len2 * Stdlib.Bigarray.kind_size_in_bytes kind in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let mat = Array2.create kind layout len1 len2 in
  unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_array2 mat) ~dst_pos:0 ~len:size;
  pos_ref := next;
  mat
;;

let bin_read_float64_mat =
  bin_read_bigarray2 ~kind:Bigarray.Float64 ~layout:Bigarray.Fortran_layout
;;

let bin_read_float32_mat =
  bin_read_bigarray2 ~kind:Bigarray.Float32 ~layout:Bigarray.Fortran_layout
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

let bin_read_variant_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  let n = unsafe_get32le buf pos in
  (* [n] must contain an integer already encoded, i.e. [n = 2 * k + 1]. *)
  if Int32.logand (box_int32 n) 1l = 0l
  then raise (Read_error (ReadError.Variant_tag, pos))
  else (
    (* We shift it by one bit to the right se we get back [2 * k + 1] in the end. *)
    pos_ref := next;
    Int32.to_int (Int32.shift_right (box_int32 n) 1))
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
  unsafe_get16le_unsigned buf pos
;;

let bin_read_int_32bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  safe_int_of_int32 pos n
;;

let bin_read_int_64bit buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64le buf pos in
  safe_int_of_int64 pos n
;;

let%template[@alloc a = (heap, stack)] bin_read_int32_bits buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  box_int32 n [@exclave_if_stack a]
;;

let%template[@alloc a = (heap, stack)] bin_read_int64_bits buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64le buf pos in
  box_int64 n [@exclave_if_stack a]
;;

let bin_read_network16_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  pos_ref := next;
  unsafe_get16be_unsigned buf pos
;;

let bin_read_network32_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32be buf pos in
  safe_int_of_int32 pos n
;;

let%template[@alloc a = (heap, stack)] bin_read_network32_int32 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32be buf pos in
  box_int32 n [@exclave_if_stack a]
;;

let bin_read_network64_int buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64be buf pos in
  safe_int_of_int64 pos n
;;

let%template[@alloc a = (heap, stack)] bin_read_network64_int64 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64be buf pos in
  box_int64 n [@exclave_if_stack a]
;;

[%%if ocaml_version < (4, 07, 0)]

external unsafe_bytes_set32
  :  bytes
  -> int
  -> int32
  -> unit
  @@ portable
  = "%caml_string_set32u"

external unsafe_bytes_set64
  :  bytes
  -> int
  -> int64
  -> unit
  @@ portable
  = "%caml_string_set64u"

[%%else]

external unsafe_bytes_set32
  :  local_ bytes
  -> int
  -> (int32#[@unboxed])
  -> unit
  @@ portable
  = "%caml_bytes_set32u#"

external unsafe_bytes_set64
  :  local_ bytes
  -> int
  -> (int64#[@unboxed])
  -> unit
  @@ portable
  = "%caml_bytes_set64u#"

[%%endif]

let bin_read_md5_aux res buf pos =
  if arch_sixtyfour
  then (
    let a = unsafe_get64 buf pos in
    let b = unsafe_get64 buf (pos + 8) in
    unsafe_bytes_set64 res 0 a;
    unsafe_bytes_set64 res 8 b)
  else (
    let a = unsafe_get32 buf pos in
    let b = unsafe_get32 buf (pos + 4) in
    let c = unsafe_get32 buf (pos + 8) in
    let d = unsafe_get32 buf (pos + 12) in
    unsafe_bytes_set32 res 0 a;
    unsafe_bytes_set32 res 4 b;
    unsafe_bytes_set32 res 8 c;
    unsafe_bytes_set32 res 12 d)
;;

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let unsafe_bin_read_md5 buf pos =
  (let res = (Base.Bytes.create [@alloc a]) 16 in
   bin_read_md5_aux res buf pos;
   (Md5_lib.unsafe_of_binary [@mode m])
     (Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res))
  [@exclave_if_stack a]
;;

let bin_read_md5 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 16 in
  check_next buf next;
  pos_ref := next;
  (unsafe_bin_read_md5 [@alloc a]) buf pos [@exclave_if_stack a]
;;]

(* Local readers *)

[%%template
[@@@mode.default local]

let[@inline] bin_read_unit buf ~pos_ref = bin_read_unit buf ~pos_ref
let[@inline] bin_read_bool buf ~pos_ref = bin_read_bool buf ~pos_ref
let[@inline] bin_read_char buf ~pos_ref = bin_read_char buf ~pos_ref
let[@inline] bin_read_int buf ~pos_ref = bin_read_int buf ~pos_ref
let[@inline] bin_read_nat0 buf ~pos_ref = bin_read_nat0 buf ~pos_ref

let[@inline] bin_read_float buf ~pos_ref = exclave_
  (bin_read_float [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_int32 buf ~pos_ref = exclave_
  (bin_read_int32 [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_int64 buf ~pos_ref = exclave_
  (bin_read_int64 [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_nativeint buf ~pos_ref = exclave_
  (bin_read_nativeint [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_bytes buf ~pos_ref = exclave_
  (bin_read_bytes [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_string buf ~pos_ref = exclave_
  (bin_read_string [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_md5 buf ~pos_ref = exclave_
  (bin_read_md5 [@alloc stack]) buf ~pos_ref
;;

let bin_read_ref = (bin_read_ref [@alloc stack])
let bin_read_option = (bin_read_option [@alloc stack])
let bin_read_list = (bin_read_list [@alloc stack])
let bin_read_list_with_max_len = (bin_read_list_with_max_len [@alloc stack])
let bin_read_array = (bin_read_array [@alloc stack])
let bin_read_iarray = (bin_read_iarray [@alloc stack])
let bin_read_pair = (bin_read_pair [@alloc stack])
let bin_read_triple = (bin_read_triple [@alloc stack])

(* The vec and mat readers return bigarrays, which cannot be allocated on the stack, so we
   make no effort to localize them *)
let bin_read_float32_vec = bin_read_float32_vec
let bin_read_float64_vec = bin_read_float64_vec
let bin_read_vec = bin_read_vec
let bin_read_float32_mat = bin_read_float32_mat
let bin_read_float64_mat = bin_read_float64_mat
let bin_read_mat = bin_read_mat
let bin_read_bigstring = bin_read_bigstring
let[@inline] bin_read_variant_int buf ~pos_ref = bin_read_variant_int buf ~pos_ref
let[@inline] bin_read_int_8bit buf ~pos_ref = bin_read_int_8bit buf ~pos_ref
let[@inline] bin_read_int_16bit buf ~pos_ref = bin_read_int_16bit buf ~pos_ref
let[@inline] bin_read_int_32bit buf ~pos_ref = bin_read_int_32bit buf ~pos_ref
let[@inline] bin_read_int_64bit buf ~pos_ref = bin_read_int_64bit buf ~pos_ref

let[@inline] bin_read_int32_bits buf ~pos_ref = exclave_
  (bin_read_int32_bits [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_int64_bits buf ~pos_ref = exclave_
  (bin_read_int64_bits [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_network16_int buf ~pos_ref = bin_read_network16_int buf ~pos_ref
let[@inline] bin_read_network32_int buf ~pos_ref = bin_read_network32_int buf ~pos_ref
let[@inline] bin_read_network64_int buf ~pos_ref = bin_read_network64_int buf ~pos_ref

let[@inline] bin_read_network32_int32 buf ~pos_ref = exclave_
  (bin_read_network32_int32 [@alloc stack]) buf ~pos_ref
;;

let[@inline] bin_read_network64_int64 buf ~pos_ref = exclave_
  (bin_read_network64_int64 [@alloc stack]) buf ~pos_ref
;;]
