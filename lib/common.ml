(* Common: common definitions used by binary protocol converters *)

open Printf
open Bigarray

type pos = int

(* Errors and exceptions *)

exception Buffer_short
exception No_variant_match

module ReadError = struct
  (* Order of variants matters.  It must jibe with read_stubs.c. *)
  type t =
    | Neg_int8
    | Int_code
    | Int_overflow
    | Nat0_code
    | Nat0_overflow
    | Int32_code
    | Int64_code
    | Nativeint_code
    | Unit_code
    | Bool_code
    | Option_code
    | String_too_long
    | Variant_tag
    | Array_too_long
    | Hashtbl_too_long
    | Sum_tag of string
    | Variant of string
    | Poly_rec_bound of string
    | Variant_wrong_type of string
    | Silly_type of string
    | Empty_type of string

  let to_string = function
    | Neg_int8 -> "Neg_int8"
    | Int_code -> "Int_code"
    | Int_overflow -> "Int_overflow"
    | Nat0_code -> "Nat0_code"
    | Nat0_overflow -> "Nat0_overflow"
    | Int32_code -> "Int32_code"
    | Int64_code -> "Int64_code"
    | Nativeint_code -> "Nativeint_code"
    | Unit_code -> "Unit_code"
    | Bool_code -> "Bool_code"
    | Option_code -> "Option_code"
    | String_too_long -> "String_too_long"
    | Variant_tag -> "Variant_tag"
    | Array_too_long -> "Array_too_long"
    | Hashtbl_too_long -> "Hashtbl_too_long"
    | Sum_tag loc -> "Sum_tag / " ^ loc
    | Variant loc -> "Variant / " ^ loc
    | Poly_rec_bound loc -> "Poly_rec_bound / " ^ loc
    | Variant_wrong_type loc -> "Variant_wrong_type / " ^ loc
    | Silly_type loc -> "Silly_type / " ^ loc
    | Empty_type loc -> "Empty_type / " ^ loc
end

exception Read_error of ReadError.t * pos
exception Read_exc of exn * pos
exception Poly_rec_write of string
exception Empty_type of string

let raise_read_error err pos = raise (Read_error (err, pos))
let raise_read_exc exc pos = raise (Read_exc (exc, pos))

let raise_concurrent_modification loc =
  failwith (loc ^ ": concurrent modification")

let array_bound_error () = invalid_arg "index out of bounds"


(* Buffers *)

type pos_ref = pos ref
type buf = (char, int8_unsigned_elt, c_layout) Array1.t

let create_buf n = Array1.create Bigarray.char c_layout n

let check_pos (buf : buf) pos =
  if pos >= Array1.dim buf then raise Buffer_short

let safe_get_pos buf pos_ref =
  let pos = !pos_ref in
  check_pos buf pos;
  pos

let check_next (buf : buf) next =
  if next > Array1.dim buf then raise Buffer_short

let get_opt_pos ~loc ~var = function
  | Some pos ->
      if pos < 0 then
        invalid_arg (sprintf "Bin_prot.Common.%s: %s < 0" loc var);
      pos
  | None -> 0

external unsafe_blit_buf :
  src_pos : int -> src : buf -> dst_pos : int -> dst : buf -> len : int -> unit
  = "bin_prot_blit_buf_stub"

let blit_buf ?src_pos ~src ?dst_pos ~dst len =
  let loc = "blit_buf" in
  let src_pos = get_opt_pos ~loc ~var:"src_pos" src_pos in
  let dst_pos = get_opt_pos ~loc ~var:"dst_pos" dst_pos in
  if len < 0 then invalid_arg "Bin_prot.Common.blit_buf: len < 0"
  else if len = 0 then (
    if src_pos > Array1.dim src then
      invalid_arg "Bin_prot.Common.blit_buf: src_pos > src_len";
    if dst_pos > Array1.dim dst then
      invalid_arg "Bin_prot.Common.blit_buf: dst_pos > dst_len")
  else (
    if src_pos + len > Array1.dim src then
      invalid_arg "Bin_prot.Common.blit_buf: src_pos + len > src_len"
    else if dst_pos + len > Array1.dim dst then
      invalid_arg "Bin_prot.Common.blit_buf: dst_pos + len > dst_len"
    else unsafe_blit_buf ~src_pos ~src ~dst_pos ~dst ~len)

external unsafe_blit_string_buf :
  src_pos : int -> string -> dst_pos : int -> buf -> len : int -> unit
  = "bin_prot_blit_string_buf_stub" "noalloc"

let blit_string_buf ?src_pos str ?dst_pos buf ~len =
  let loc = "blit_string_buf" in
  let src_pos = get_opt_pos ~loc ~var:"src_pos" src_pos in
  let dst_pos = get_opt_pos ~loc ~var:"dst_pos" dst_pos in
  if len < 0 then invalid_arg "Bin_prot.Common.blit_string_buf: len < 0"
  else if len = 0 then (
    if src_pos > String.length str then
      invalid_arg "Bin_prot.Common.blit_string_buf: src_pos > str_len";
    if dst_pos > Array1.dim buf then
      invalid_arg "Bin_prot.Common.blit_string_buf: src_pos > buf")
  else (
    if src_pos + len > String.length str then
      invalid_arg "Bin_prot.Common.blit_string_buf: src_pos + len > str_len"
    else if dst_pos + len > Array1.dim buf then
      invalid_arg "Bin_prot.Common.blit_string_buf: src_pos + len > buf"
    else unsafe_blit_string_buf ~src_pos str ~dst_pos buf ~len)

external unsafe_blit_buf_string :
  src_pos : int -> buf -> dst_pos : int -> string -> len : int -> unit
  = "bin_prot_blit_buf_string_stub" "noalloc"

let blit_buf_string ?src_pos buf ?dst_pos str ~len =
  let loc = "blit_buf_string" in
  let src_pos = get_opt_pos ~loc ~var:"src_pos" src_pos in
  let dst_pos = get_opt_pos ~loc ~var:"dst_pos" dst_pos in
  if len < 0 then invalid_arg "Bin_prot.Common.blit_buf_string: len < 0"
  else if len = 0 then (
    if src_pos > Array1.dim buf then
      invalid_arg "Bin_prot.Common.blit_buf_string: src_pos > buf_len";
    if dst_pos > String.length str then
      invalid_arg "Bin_prot.Common.blit_buf_string: src_pos > str_len")
  else (
    if src_pos + len > Array1.dim buf then
      invalid_arg "Bin_prot.Common.blit_buf_string: src_pos + len > buf_len"
    else if dst_pos + len > String.length str then
      invalid_arg "Bin_prot.Common.blit_buf_string: src_pos + len > str_len"
    else unsafe_blit_buf_string ~src_pos buf ~dst_pos str ~len)


(* Miscellaneous *)

let rec copy_htbl_list htbl = function
  | [] -> htbl
  | (k, v) :: rest -> Hashtbl.add htbl k v; copy_htbl_list htbl rest

external variant_of_int : int -> [> ] = "%identity"


(* Bigarrays *)

type vec32 = (float, float32_elt, fortran_layout) Array1.t
type vec64 = (float, float64_elt, fortran_layout) Array1.t
type vec = vec64

type mat32 = (float, float32_elt, fortran_layout) Array2.t
type mat64 = (float, float64_elt, fortran_layout) Array2.t
type mat = mat64


(* Initialisation *)

external init : unit -> unit = "bin_prot_common_init_stub"

let () =
  Callback.register_exception "Bin_prot.Common.Buffer_short" Buffer_short;
  init ()
