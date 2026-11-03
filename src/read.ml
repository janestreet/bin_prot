(* Read_ml: reading values from the binary protocol using (mostly) OCaml. *)

(* Note: the code is this file is carefully written to avoid unnecessary allocations. When
   touching this code, be sure to run the benchmarks to check for regressions. *)

open Bigarray
open Common
open! Basement.Primitives
include Read_intf.Definitions

external unsafe_get : buf -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_get8 : buf -> int -> int = "%caml_ba_unsafe_ref_1"

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

external unsafe_get16 : buf -> int -> int = "%caml_bigstring_get16u"
external unsafe_get32 : buf -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_get64 : buf -> int -> int64 = "%caml_bigstring_get64u"
external unbox_int32 : int32 -> int32 = "%identity"
external unbox_int64 : int64 -> int64 = "%identity"
external unbox_nativeint : nativeint -> nativeint = "%identity"
external box_int32 : int32 -> int32 = "%identity"
external box_int64 : int64 -> int64 = "%identity"
external box_nativeint : nativeint -> nativeint = "%identity"
external bswap16 : int -> int = "%bswap16"
external bswap32 : (int32[@local_opt]) -> (int32[@local_opt]) = "%bswap_int32"
external bswap64 : (int64[@local_opt]) -> (int64[@local_opt]) = "%bswap_int64"
external int32_to_int : (int32[@local_opt]) -> int = "%int32_to_int"
external int64_to_int : (int64[@local_opt]) -> int = "%int64_to_int"

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
  = "%int64_to_nativeint"

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
  (* Can be above next line (no errors possible with 16bit). This should improve the
     generated code. *)
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

external int64_of_int32 : (int32[@local_opt]) -> (int64[@local_opt]) = "%int64_of_int32"

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
  = "%nativeint_of_int32"

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

let unsafe_bin_read_bytes buf ~pos ~len =
  let str = Base.Bytes.create len in
  unsafe_blit_buf_bytes ~src_pos:pos buf ~dst_pos:0 str ~len;
  str
;;

let bin_read_bytes buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > Sys.max_string_length then raise_read_error ReadError.String_too_long start_pos;
  let pos = !pos_ref in
  let next = pos + len in
  check_next_check_overflow buf pos next;
  pos_ref := next;
  unsafe_bin_read_bytes buf ~pos ~len
;;

let bin_read_string buf ~pos_ref =
  let str = bin_read_bytes buf ~pos_ref in
  Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str
;;

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

external unbox_float : float -> float = "%identity"
external box_float : float -> float = "%identity"

let bin_read_float buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  let f = unsafe_get64le buf pos |> box_int64 |> Int64.float_of_bits |> unbox_float in
  box_float f
;;

let bin_read_int32 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    let n = Char.code ch in
    Int32.of_int n
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) ->
    let n = safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1) in
    Int32.of_int n
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) ->
    let n = safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1) in
    Int32.of_int n
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) ->
    let n = safe_bin_read_int32 buf ~pos_ref ~pos:(pos + 1) in
    box_int32 n
  | _ -> raise_read_error ReadError.Int32_code pos
;;

let bin_read_int64 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    let n = Char.code ch in
    Int64.of_int n
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) ->
    let n = safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1) in
    Int64.of_int n
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) ->
    let n = safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1) in
    Int64.of_int n
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) ->
    let n = safe_bin_read_int32_as_int64 buf ~pos_ref ~pos:(pos + 1) in
    box_int64 n
  (*$ Code.pipe_char INT64 *)
  | '\xfc' (*$*) ->
    let n = safe_bin_read_int64 buf ~pos_ref ~pos:(pos + 1) in
    box_int64 n
  | _ -> raise_read_error ReadError.Int64_code pos
;;

let bin_read_nativeint buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00' .. '\x7f' as ch ->
    pos_ref := pos + 1;
    let n = Char.code ch in
    Nativeint.of_int n
  (*$ Code.pipe_char NEG_INT8 *)
  | '\xff' (*$*) ->
    let n = safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1) in
    Nativeint.of_int n
  (*$ Code.pipe_char INT16 *)
  | '\xfe' (*$*) ->
    let n = safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1) in
    Nativeint.of_int n
  (*$ Code.pipe_char INT32 *)
  | '\xfd' (*$*) ->
    let n = safe_bin_read_int32_as_nativeint buf ~pos_ref ~pos:(pos + 1) in
    box_nativeint n
  (*$ Code.pipe_char INT64 *)
  | '\xfc' (*$*) when arch_sixtyfour ->
    let n = safe_bin_read_int64_as_nativeint buf ~pos_ref ~pos:(pos + 1) in
    box_nativeint n
  | _ -> raise_read_error ReadError.Nativeint_code pos
;;

let bin_read_ref bin_read_el buf ~pos_ref =
  let el = bin_read_el buf ~pos_ref in
  ref el
;;

let bin_read_pair bin_read_a bin_read_b buf ~pos_ref =
  let a = bin_read_a buf ~pos_ref in
  let b = bin_read_b buf ~pos_ref in
  a, b
;;

let bin_read_triple bin_read_a bin_read_b bin_read_c buf ~pos_ref =
  let a = bin_read_a buf ~pos_ref in
  let b = bin_read_b buf ~pos_ref in
  let c = bin_read_c buf ~pos_ref in
  a, b, c
;;

let[@inline] bin_read_option_like ~none ~some ~read_error bin_read_el buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\000' ->
    pos_ref := pos + 1;
    none
  | '\001' ->
    pos_ref := pos + 1;
    let el = bin_read_el buf ~pos_ref in
    (some [@inlined hint]) el
  | _ -> raise_read_error read_error pos
;;

let bin_read_option bin_read_el buf ~pos_ref =
  bin_read_option_like
    ~none:None
    ~some:Base.Option.some
    ~read_error:Option_code
    bin_read_el
    buf
    ~pos_ref
;;

let bin_read_or_null bin_read_el buf ~pos_ref =
  bin_read_option_like
    ~none:Null
    ~some:Base.Or_null.this
    ~read_error:Or_null_code
    bin_read_el
    buf
    ~pos_ref
;;

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

let rev l =
  let rec loop l acc =
    match l with
    | [] -> acc
    | hd :: rest -> loop rest (hd :: acc)
  in
  loop l []
;;

let bin_read_list_with_max_len ~max_len bin_read_el buf ~pos_ref =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  if len > max_len then raise_read_error (List_too_long { len; max_len }) !pos_ref;
  bin_read_n_list bin_read_el buf ~pos_ref len
;;

let bin_read_list bin_read_el buf ~pos_ref =
  bin_read_list_with_max_len ~max_len:max_int bin_read_el buf ~pos_ref
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

(* [is_float_reader reader] is [true] if an array of elements with the same type as
   [reader] produces may take two words per element. This is only true for flat-float
   arrays on 32-bit backends. *)
let is_float_reader (type a) (reader : a reader) =
  let module Obj = Base.Obj in
  match reader (dummy_float_buf ()) ~pos_ref:(ref 0) with
  | el -> Obj.Nullable.tag (Obj.Nullable.repr el) = Stdlib.Obj.double_tag
  | exception _ -> false
;;

(* Without extensions, behaves the same as [float] *)

(* Without extensions, is only ever a pointer *)

let check_array_len (type a) (bin_read_el : a reader) ~len ~start_pos =
  if len > Sys.max_array_length
     || ((not arch_sixtyfour)
         && len > Sys.max_array_length / 2
         && is_float_reader bin_read_el)
  then raise_read_error ReadError.Array_too_long start_pos
;;

let[@inline] bin_read_array_aux__no_float_array_blit_optimization
  (type a)
  (bin_read_el : a reader)
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
    let res = Base.Array.create ~len first in
    for i = 1 to len - 1 do
      let el = bin_read_el buf ~pos_ref in
      Base.Array.unsafe_set res i el
    done;
    res)
;;

let bin_read_array (type a) (bin_read_el : a reader) buf ~pos_ref =
  if (Obj.magic (bin_read_el : a reader) : float reader) == bin_read_float
  then (Obj.magic (bin_read_float_array buf ~pos_ref : float array) : a array)
  else bin_read_array_aux__no_float_array_blit_optimization bin_read_el buf ~pos_ref
;;

let bin_read_iarray (type a) (bin_read_el : a reader) buf ~pos_ref =
  let start_pos = !pos_ref in
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  check_array_len bin_read_el ~len ~start_pos;
  Base.Iarray.init len ~f:(fun _ -> bin_read_el buf ~pos_ref) [@nontail]
;;

external buf_of_array1 : _ Stdlib.Bigarray.Array1.t -> buf = "%identity"
external buf_of_array2 : _ Stdlib.Bigarray.Array2.t -> buf = "%identity"
external buf_of_genarray : _ Stdlib.Bigarray.Genarray.t -> buf = "%identity"

let checked_multiply_bigarray_length left right ~pos =
  if right <> 0 && left > Stdlib.max_int / right
  then raise_read_error ReadError.Array_too_long pos;
  left * right
;;

let bin_read_bigarray1 (type k) ~(kind : (_, k) Stdlib.Bigarray.kind) ~layout buf ~pos_ref
  : (_, k, _) Stdlib.Bigarray.Array1.t
  =
  let len = (bin_read_nat0 buf ~pos_ref :> int) in
  let size = len * Stdlib.Bigarray.kind_size_in_bytes kind in
  let pos = !pos_ref in
  let next = pos + size in
  check_next buf next;
  let vec = Array1.create kind layout len in
  let () =
    let vec = vec in
    unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_array1 vec) ~dst_pos:0 ~len:size
  in
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
  let () =
    let mat = mat in
    unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_array2 mat) ~dst_pos:0 ~len:size
  in
  pos_ref := next;
  mat
;;

let bin_read_genarray (type k) ~(kind : (_, k) Stdlib.Bigarray.kind) ~layout buf ~pos_ref
  : (_, k, _) Stdlib.Bigarray.Genarray.t
  =
  let start_pos = !pos_ref in
  let num_dims = (bin_read_nat0 buf ~pos_ref :> int) in
  if num_dims > Sys.max_array_length
  then raise_read_error ReadError.Array_too_long start_pos;
  let dimensions = Base.Array.create ~len:num_dims 0 in
  let num_elements = ref 1 in
  for dimension = 0 to num_dims - 1 do
    let length = (bin_read_nat0 buf ~pos_ref :> int) in
    dimensions.(dimension) <- length;
    num_elements := checked_multiply_bigarray_length !num_elements length ~pos:start_pos
  done;
  let size =
    checked_multiply_bigarray_length
      !num_elements
      (Stdlib.Bigarray.kind_size_in_bytes kind)
      ~pos:start_pos
  in
  let pos = !pos_ref in
  let next = pos + size in
  check_next_check_overflow buf pos next;
  let array = Bigarray.Genarray.create kind layout dimensions in
  let () =
    let array = array in
    unsafe_blit_buf
      ~src:buf
      ~src_pos:pos
      ~dst:(buf_of_genarray array)
      ~dst_pos:0
      ~len:size
  in
  pos_ref := next;
  array
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

let bin_read_int32_bits buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32le buf pos in
  box_int32 n
;;

let bin_read_int64_bits buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64le buf pos in
  box_int64 n
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

let bin_read_network32_int32 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get32be buf pos in
  box_int32 n
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

let bin_read_network64_int64 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  let n = unsafe_get64be buf pos in
  box_int64 n
;;

[%%if ocaml_version < (4, 07, 0)]

external unsafe_bytes_set32 : bytes -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_bytes_set64 : bytes -> int -> int64 -> unit = "%caml_string_set64u"

[%%else]

external unsafe_bytes_set32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external unsafe_bytes_set64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

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

let unsafe_bin_read_md5 buf pos =
  let res = Base.Bytes.create 16 in
  bin_read_md5_aux res buf pos;
  Md5_lib.unsafe_of_binary
    (Base.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)
;;

let bin_read_md5 buf ~pos_ref =
  let pos = !pos_ref in
  assert_pos pos;
  let next = pos + 16 in
  check_next buf next;
  pos_ref := next;
  unsafe_bin_read_md5 buf pos
;;

(* Local readers *)
