(* Write_ml: writing values to the binary protocol using (mostly) OCaml. *)

(* Note: the code is this file is carefully written to avoid unnecessary allocations. When
   touching this code, be sure to run the benchmarks to check for regressions. *)

open Common
include Write_intf.Definitions

external unsafe_set
  :  local_ buf
  -> int
  -> local_ char
  -> unit
  @@ portable
  = "%caml_ba_unsafe_set_1"

external unsafe_set8
  :  local_ buf
  -> int
  -> local_ int
  -> unit
  @@ portable
  = "%caml_ba_unsafe_set_1"

external unsafe_set16
  :  local_ buf
  -> int
  -> local_ int
  -> unit
  @@ portable
  = "%caml_bigstring_set16u"

external unsafe_set32
  :  local_ buf
  -> int
  -> local_ int32
  -> unit
  @@ portable
  = "%caml_bigstring_set32u"

external unsafe_set64
  :  local_ buf
  -> int
  -> local_ int64
  -> unit
  @@ portable
  = "%caml_bigstring_set64u"

external bswap16 : (int[@local_opt]) -> (int[@local_opt]) @@ portable = "%bswap16"
external bswap32 : (int32[@local_opt]) -> (int32[@local_opt]) @@ portable = "%bswap_int32"
external bswap64 : (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%bswap_int64"

(*$ open Bin_prot_cinaps $*)

let code_NEG_INT8 = (*$ Code.char NEG_INT8 *) '\xff' (*$*)
let code_INT16 = (*$ Code.char INT16 *) '\xfe' (*$*)
let code_INT32 = (*$ Code.char INT32 *) '\xfd' (*$*)
let code_INT64 = (*$ Code.char INT64 *) '\xfc' (*$*)
let arch_sixtyfour = Sys.word_size = 64
let arch_big_endian = Sys.big_endian

let unsafe_set16be =
  if arch_big_endian
  then unsafe_set16
  else fun buf pos x -> unsafe_set16 buf pos (bswap16 x)
;;

let unsafe_set32be =
  if arch_big_endian
  then unsafe_set32
  else fun buf pos x -> unsafe_set32 buf pos (bswap32 x)
;;

let unsafe_set64be =
  if arch_big_endian
  then unsafe_set64
  else fun buf pos x -> unsafe_set64 buf pos (bswap64 x)
;;

let unsafe_set16le =
  if arch_big_endian
  then fun buf pos x -> unsafe_set16 buf pos (bswap16 x)
  else unsafe_set16
;;

let unsafe_set32le =
  if arch_big_endian
  then fun buf pos x -> unsafe_set32 buf pos (bswap32 x)
  else unsafe_set32
;;

let unsafe_set64le =
  if arch_big_endian
  then fun buf pos x -> unsafe_set64 buf pos (bswap64 x)
  else unsafe_set64
;;

let bin_write_unit buf ~pos () =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set buf pos '\000';
  pos + 1
;;

let bin_write_bool buf ~pos b =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set buf pos (if b then '\001' else '\000');
  pos + 1
;;

let all_bin_write_small_int buf pos n =
  check_pos buf pos;
  unsafe_set8 buf pos n;
  pos + 1
;;

let all_bin_write_neg_int8 buf pos n =
  let next = pos + 2 in
  check_next buf next;
  unsafe_set buf pos code_NEG_INT8;
  unsafe_set8 buf (pos + 1) n;
  next
;;

let all_bin_write_int16 buf pos n =
  let next = pos + 3 in
  check_next buf next;
  unsafe_set buf pos code_INT16;
  unsafe_set16le buf (pos + 1) n;
  next
;;

let all_bin_write_int32 buf pos n =
  let next = pos + 5 in
  check_next buf next;
  unsafe_set buf pos code_INT32;
  unsafe_set32le buf (pos + 1) n;
  next
[@@inline]
;;

let all_bin_write_int64 buf pos n =
  let next = pos + 9 in
  check_next buf next;
  unsafe_set buf pos code_INT64;
  unsafe_set64le buf (pos + 1) n;
  next
[@@inline]
;;

let bin_write_char buf ~pos c =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set buf pos c;
  pos + 1
;;

let bin_write_int buf ~pos n =
  assert_pos pos;
  if n >= 0
  then
    if n < 0x00000080
    then all_bin_write_small_int buf pos n
    else if n < 0x00008000
    then all_bin_write_int16 buf pos n
    else if arch_sixtyfour && n >= 1 lsl 31
    then all_bin_write_int64 buf pos (Int64.of_int n)
    else all_bin_write_int32 buf pos (Int32.of_int n)
  else if n >= -0x00000080
  then all_bin_write_neg_int8 buf pos n
  else if n >= -0x00008000
  then all_bin_write_int16 buf pos n
  else if arch_sixtyfour && n < -(1 lsl 31)
  then all_bin_write_int64 buf pos (Int64.of_int n)
  else all_bin_write_int32 buf pos (Int32.of_int n)
;;

let bin_write_nat0 buf ~pos nat0 =
  assert_pos pos;
  let n = (nat0 : Nat0.t :> int) in
  if n < 0x00000080
  then all_bin_write_small_int buf pos n
  else if n < 0x00010000
  then all_bin_write_int16 buf pos n
  else if arch_sixtyfour && n >= 1 lsl 32
  then all_bin_write_int64 buf pos (Int64.of_int n)
  else all_bin_write_int32 buf pos (Int32.of_int n)
;;

let bin_write_string buf ~pos str =
  let len = Base.String.length str in
  let plen = Nat0.unsafe_of_int len in
  let new_pos = bin_write_nat0 buf ~pos plen in
  let next = new_pos + len in
  check_next buf next;
  (* TODO: optimize for small strings *)
  unsafe_blit_string_buf ~src_pos:0 str ~dst_pos:new_pos buf ~len;
  next
;;

let bin_write_bytes buf ~pos str =
  let len = Base.Bytes.length str in
  let plen = Nat0.unsafe_of_int len in
  let new_pos = bin_write_nat0 buf ~pos plen in
  let next = new_pos + len in
  check_next buf next;
  (* TODO: optimize for small bytes *)
  unsafe_blit_bytes_buf ~src_pos:0 str ~dst_pos:new_pos buf ~len;
  next
;;

let bin_write_float buf ~pos x =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64le buf pos (Base.Int64.bits_of_float x);
  next
[@@inline]
;;

let bin_write_int32 =
  if arch_sixtyfour
  then fun [@inline] buf ~pos n -> bin_write_int buf ~pos (Int32.to_int n)
  else
    fun [@inline] buf ~pos n ->
    if n >= 0x00008000l || n < -0x00008000l
    then (
      assert_pos pos;
      all_bin_write_int32 buf pos n)
    else bin_write_int buf ~pos (Int32.to_int n)
;;

let bin_write_int64 buf ~pos n =
  if n >= 0x80000000L || n < -0x80000000L
  then (
    assert_pos pos;
    all_bin_write_int64 buf pos n)
  else if arch_sixtyfour
  then bin_write_int buf ~pos (Int64.to_int n)
  else if n >= 0x00008000L || n < -0x00008000L
  then (
    assert_pos pos;
    all_bin_write_int32 buf pos (Base.Int64.to_int32_trunc n) [@nontail])
  else bin_write_int buf ~pos (Int64.to_int n)
[@@inline]
;;

let bin_write_nativeint buf ~pos n =
  if arch_sixtyfour
     && (n >= (* 0x80000000n *) Nativeint.shift_left 1n 31
         || n < (* -0x80000000n *) Nativeint.neg (Nativeint.shift_left 1n 31))
  then (
    assert_pos pos;
    all_bin_write_int64 buf pos (Base.Int64.of_nativeint n) [@nontail])
  else if ((not arch_sixtyfour) && n >= 0x8000n) || n < -0x8000n
  then (
    assert_pos pos;
    all_bin_write_int32 buf pos (Base.Nativeint.to_int32_trunc n) [@nontail])
  else bin_write_int buf ~pos (Nativeint.to_int n)
[@@inline]
;;

let bin_write_ref bin_write_el buf ~pos r = bin_write_el buf ~pos !r

let bin_write_lazy bin_write_el buf ~pos lv =
  let v = Base.Lazy.force lv in
  bin_write_el buf ~pos v
;;

[%%template
[@@@mode.default m = (global, local)]

let bin_write_option bin_write_el buf ~pos = function
  | None -> bin_write_bool buf ~pos false
  | Some v ->
    let next = bin_write_bool buf ~pos true in
    bin_write_el buf ~pos:next v
;;

let bin_write_pair bin_write_a bin_write_b buf ~pos (a, b) =
  let next = bin_write_a buf ~pos a in
  bin_write_b buf ~pos:next b
;;

let bin_write_triple bin_write_a bin_write_b bin_write_c buf ~pos (a, b, c) =
  let next1 = bin_write_a buf ~pos a in
  let next2 = bin_write_b buf ~pos:next1 b in
  bin_write_c buf ~pos:next2 c
;;

let bin_write_list =
  let rec loop ~bin_write_el ~buf ~els_pos lst =
    match lst with
    | [] -> els_pos
    | hd :: tl ->
      let new_els_pos = bin_write_el buf ~pos:els_pos hd in
      loop ~bin_write_el ~buf ~els_pos:new_els_pos tl
  in
  fun bin_write_el buf ~pos lst ->
    let len = Nat0.unsafe_of_int (Base.List.length lst) in
    let els_pos = bin_write_nat0 buf ~pos len in
    loop ~bin_write_el ~buf ~els_pos lst
;;]

let[@inline always] bin_write_float_array_gen ~length ~blit buf ~pos a =
  let len = length a in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let size = len * 8 in
  let next = pos + size in
  check_next buf next;
  blit ~src_pos:0 a ~dst_pos:pos buf ~len;
  next
;;

external float_array_length
  :  local_ Float.Array.t
  -> int
  @@ portable
  = "%floatarray_length"

let bin_write_floatarray buf ~pos a =
  bin_write_float_array_gen
    ~length:float_array_length
    ~blit:unsafe_blit_floatarray_buf
    buf
    ~pos
    a
;;

let bin_write_float_array buf ~pos a =
  bin_write_float_array_gen
    ~length:Base.Array.length
    ~blit:unsafe_blit_float_array_buf
    buf
    ~pos
    a
;;

let bin_write_array_loop bin_write_el buf ~els_pos ~n ar =
  let els_pos_ref = ref els_pos in
  for i = 0 to n - 1 do
    els_pos_ref := bin_write_el buf ~pos:!els_pos_ref (Base.Array.unsafe_get ar i)
  done;
  !els_pos_ref
;;

let bin_write_array (type a) bin_write_el buf ~pos ar =
  let module Obj = Base.Obj in
  if (Obj.magic (bin_write_el : a writer) : float writer)
     == (bin_write_float :> float writer)
  then bin_write_float_array buf ~pos (Obj.magic (ar : a array) : float array) [@nontail]
  else (
    let n = Base.Array.length ar in
    let pn = Nat0.unsafe_of_int n in
    let els_pos = bin_write_nat0 buf ~pos pn in
    bin_write_array_loop bin_write_el buf ~els_pos ~n ar)
;;

let bin_write_float_iarray buf ~pos (ar @ local) =
  let module Obj = Base.Obj in
  bin_write_float_array
    buf
    ~pos
    (Obj.magic (ar : float Base.Iarray.t) : float array) [@nontail]
;;

[%%template
[@@@mode.default m = (global, local)]

let bin_write_iarray_loop bin_write_el buf ~els_pos ~n ar =
  let els_pos_ref = ref els_pos in
  for i = 0 to n - 1 do
    els_pos_ref := bin_write_el buf ~pos:!els_pos_ref (Base.Iarray.unsafe_get ar i)
  done;
  !els_pos_ref
;;

let bin_write_iarray (type a) bin_write_el buf ~pos (ar @ m) =
  let module Obj = Base.Obj in
  if (Obj.magic (bin_write_el : (a writer[@mode m])) : float writer)
     == (bin_write_float :> float writer)
  then
    bin_write_float_iarray
      buf
      ~pos
      (Obj.magic (ar : a Base.Iarray.t) : float Base.Iarray.t) [@nontail]
  else (
    let n = Base.Iarray.length ar in
    let pn = Nat0.unsafe_of_int n in
    let els_pos = bin_write_nat0 buf ~pos pn in
    (bin_write_iarray_loop [@mode m]) bin_write_el buf ~els_pos ~n ar)
;;]

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

module Bigarray = struct
  include Bigarray
  include Local_bigarray
end

let bin_write_bigarray1
  (type k)
  ~(kind : (_, k) Stdlib.Bigarray.kind)
  buf
  ~pos
  (local_ (v : (_, k, _) Stdlib.Bigarray.Array1.t))
  =
  let len = Bigarray.Array1.dim v in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let size = len * Stdlib.Bigarray.kind_size_in_bytes kind in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_buf ~src:(buf_of_array1 v) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
  next
;;

let bin_write_float32_vec = bin_write_bigarray1 ~kind:Bigarray.Float32
let bin_write_float64_vec = bin_write_bigarray1 ~kind:Bigarray.Float64
let bin_write_vec = bin_write_float64_vec

let bin_write_bigarray2
  (type k)
  ~(kind : (_, k) Stdlib.Bigarray.kind)
  buf
  ~pos
  (local_ (m : (_, k, _) Stdlib.Bigarray.Array2.t))
  =
  let len1 = Bigarray.Array2.dim1 m in
  let len2 = Bigarray.Array2.dim2 m in
  let pos = bin_write_nat0 buf ~pos (Nat0.unsafe_of_int len1) in
  let pos = bin_write_nat0 buf ~pos (Nat0.unsafe_of_int len2) in
  let size = len1 * len2 * Stdlib.Bigarray.kind_size_in_bytes kind in
  let next = pos + size in
  check_next buf next;
  unsafe_blit_buf ~src:(buf_of_array2 m) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
  next
;;

let bin_write_float32_mat = bin_write_bigarray2 ~kind:Bigarray.Float32
let bin_write_float64_mat = bin_write_bigarray2 ~kind:Bigarray.Float64
let bin_write_mat = bin_write_float64_mat

let bin_write_bigstring buf ~pos s =
  let len = Bigarray.Array1.dim s in
  let plen = Nat0.unsafe_of_int len in
  let pos = bin_write_nat0 buf ~pos plen in
  let next = pos + len in
  check_next buf next;
  unsafe_blit_buf ~src:s ~src_pos:0 ~dst:buf ~dst_pos:pos ~len;
  next
;;

let bin_write_variant_int buf ~pos x =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32le buf pos (Int32.logor (Int32.shift_left (Int32.of_int x) 1) 1l);
  next
;;

let bin_write_int_8bit buf ~pos n =
  assert_pos pos;
  check_pos buf pos;
  unsafe_set8 buf pos n;
  pos + 1
;;

let bin_write_int_16bit buf ~pos n =
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  unsafe_set16le buf pos n;
  next
;;

let bin_write_int_32bit buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32le buf pos (Int32.of_int n);
  next
;;

let bin_write_int_64bit buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64le buf pos (Int64.of_int n);
  next
;;

let bin_write_int32_bits buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32le buf pos n;
  next
;;

let bin_write_int64_bits buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64le buf pos n;
  next
;;

let bin_write_network16_int buf ~pos n =
  assert_pos pos;
  let next = pos + 2 in
  check_next buf next;
  unsafe_set16be buf pos n;
  next
;;

let bin_write_network32_int buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32be buf pos (Int32.of_int n);
  next
;;

let bin_write_network32_int32 buf ~pos n =
  assert_pos pos;
  let next = pos + 4 in
  check_next buf next;
  unsafe_set32be buf pos n;
  next
;;

let bin_write_network64_int buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64be buf pos (Int64.of_int n);
  next
;;

let bin_write_network64_int64 buf ~pos n =
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  unsafe_set64be buf pos n;
  next
;;

external unsafe_string_get32
  :  (string[@local_opt])
  -> int
  -> (int32[@local_opt])
  @@ portable
  = "%caml_string_get32u"

external unsafe_string_get64
  :  (string[@local_opt])
  -> int
  -> (int64[@local_opt])
  @@ portable
  = "%caml_string_get64u"

let bin_write_md5 buf ~pos x =
  let x = Md5_lib.to_binary_local x in
  assert (Base.String.length x = 16);
  assert_pos pos;
  let next = pos + 16 in
  check_next buf next;
  if arch_sixtyfour
  then (
    let a = unsafe_string_get64 x 0 in
    let b = unsafe_string_get64 x 8 in
    unsafe_set64 buf pos a;
    unsafe_set64 buf (pos + 8) b)
  else (
    let a = unsafe_string_get32 x 0 in
    let b = unsafe_string_get32 x 4 in
    let c = unsafe_string_get32 x 8 in
    let d = unsafe_string_get32 x 12 in
    unsafe_set32 buf pos a;
    unsafe_set32 buf (pos + 4) b;
    unsafe_set32 buf (pos + 8) c;
    unsafe_set32 buf (pos + 12) d);
  next
;;

(* Local versions *)

[%%template
let[@mode local] bin_write_unit = bin_write_unit
let[@mode local] bin_write_bool = bin_write_bool
let[@mode local] bin_write_string = bin_write_string
let[@mode local] bin_write_bytes = bin_write_bytes
let[@mode local] bin_write_char = bin_write_char
let[@mode local] bin_write_int = bin_write_int
let[@mode local] bin_write_nat0 = bin_write_nat0
let[@mode local] bin_write_float = bin_write_float
let[@mode local] bin_write_int32 = bin_write_int32
let[@mode local] bin_write_int64 = bin_write_int64
let[@mode local] bin_write_nativeint = bin_write_nativeint
let[@mode local] bin_write_ref = bin_write_ref
let[@mode local] bin_write_lazy = bin_write_lazy
let[@mode local] bin_write_array = bin_write_array
let[@mode local] bin_write_bigarray1 = bin_write_bigarray1
let[@mode local] bin_write_bigarray2 = bin_write_bigarray2
let[@mode local] bin_write_float32_vec = bin_write_float32_vec
let[@mode local] bin_write_float64_vec = bin_write_float64_vec
let[@mode local] bin_write_vec = bin_write_vec
let[@mode local] bin_write_float32_mat = bin_write_float32_mat
let[@mode local] bin_write_float64_mat = bin_write_float64_mat
let[@mode local] bin_write_mat = bin_write_mat
let[@mode local] bin_write_bigstring = bin_write_bigstring
let[@mode local] bin_write_floatarray = bin_write_floatarray
let[@mode local] bin_write_md5 = bin_write_md5
let[@mode local] bin_write_variant_int = bin_write_variant_int
let[@mode local] bin_write_int_8bit = bin_write_int_8bit
let[@mode local] bin_write_int_16bit = bin_write_int_16bit
let[@mode local] bin_write_int_32bit = bin_write_int_32bit
let[@mode local] bin_write_int_64bit = bin_write_int_64bit
let[@mode local] bin_write_int32_bits = bin_write_int32_bits
let[@mode local] bin_write_int64_bits = bin_write_int64_bits
let[@mode local] bin_write_network16_int = bin_write_network16_int
let[@mode local] bin_write_network32_int = bin_write_network32_int
let[@mode local] bin_write_network32_int32 = bin_write_network32_int32
let[@mode local] bin_write_network64_int = bin_write_network64_int
let[@mode local] bin_write_network64_int64 = bin_write_network64_int64]
