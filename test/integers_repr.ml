open Base
open Poly
open Stdio
open Import

(* This module generates reference serialized output for various functions of
   [Bin_prot.Write]. The expected output is checked in the files [integers_repr_32bit.ml]
   [integers_repr_64bit.ml] using inline expect tests. This way when we change something
   in bin_prot there is a check that the representation of integers doesn't change. The
   diff is easy to understand.

   We can't generate reference output for every possible values as it would be huge,
   instead we choose a few interesting points and generate tests in a window around
   them. The points we choose are:

   - 0
   - min value
   - max value
   - all powers of 2 between min and max
   - all points where the length of the serialized output change

   For the last item, instead of hard-coding these points for every function, we find
   them. This is to avoid errors. To find them we make the assumption that functions using
   a variable length encoding respect the following:

   - the encoded size is decreasing from min value to 0
   - the encoded size is increasing from 0 to max value

   Which is the basic assumption make by bin_prot: integers close to 0 are more frequent
   and should occupy less space.
*)

(* Number of tests to generate around each point *)
let test_window_len = 16L

module Read = Bin_prot.Read
module Write = Bin_prot.Write

type%template 'a to_test =
  { name : string
  ; writer : 'a Write.writer
  ; writer_local : ('a Write.writer[@mode local])
  ; reader : 'a Read.reader
  ; reader_local : ('a Read.reader[@mode local])
  ; globalize : local_ 'a -> 'a
  ; to_int64 : 'a -> Int64.t
  ; of_int64 : Int64.t -> 'a
  ; min : 'a
  ; max : 'a (* Bounds on the bin_protted size *)
  ; hi_bound : int
  ; lo_bound : int
  }

type to_test_packed = T : _ to_test -> to_test_packed

let min_int_32bit, max_int_32bit, min_int_64bit, max_int_64bit =
  match Word_size.word_size with
  | W32 -> -1 lsl 30, (1 lsl 30) - 1, -1 lsl 30, (1 lsl 30) - 1
  | W64 -> -1 lsl 31, (1 lsl 31) - 1, -1 lsl 62, (1 lsl 62) - 1
;;

module Nat0 = Bin_prot.Nat0

let%template tests =
  [ T
      { name = "int"
      ; writer = Write.bin_write_int
      ; writer_local = Write.bin_write_int [@mode local]
      ; reader = Read.bin_read_int
      ; reader_local = Read.bin_read_int [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = Int.min_value
      ; max = Int.max_value
      ; hi_bound = Maximum.bin_size_int
      ; lo_bound = Minimum.bin_size_int
      }
  ; T
      { name = "int32"
      ; writer = Write.bin_write_int32
      ; writer_local = Write.bin_write_int32 [@mode local]
      ; reader = Read.bin_read_int32
      ; reader_local = Read.bin_read_int32 [@mode local]
      ; globalize = globalize_int32
      ; to_int64 = Int64.of_int32
      ; of_int64 = Int64.to_int32_exn
      ; min = Int32.min_value
      ; max = Int32.max_value
      ; hi_bound = Maximum.bin_size_int32
      ; lo_bound = Minimum.bin_size_int32
      }
  ; T
      { name = "int64"
      ; writer = Write.bin_write_int64
      ; writer_local = Write.bin_write_int64 [@mode local]
      ; reader = Read.bin_read_int64
      ; reader_local = Read.bin_read_int64 [@mode local]
      ; globalize = globalize_int64
      ; to_int64 = Fn.id
      ; of_int64 = Fn.id
      ; min = Int64.min_value
      ; max = Int64.max_value
      ; hi_bound = Maximum.bin_size_int64
      ; lo_bound = Minimum.bin_size_int64
      }
  ; T
      { name = "nat0"
      ; writer = Write.bin_write_nat0
      ; writer_local = Write.bin_write_nat0 [@mode local]
      ; reader = Read.bin_read_nat0
      ; reader_local = Read.bin_read_nat0 [@mode local]
      ; globalize = (fun x -> x)
      ; to_int64 = (fun x -> Int64.of_int (x : Nat0.t :> int))
      ; of_int64 = (fun x -> Nat0.of_int (Int64.to_int_exn x))
      ; min = Nat0.of_int 0
      ; max = Nat0.of_int Int.max_value
      ; hi_bound = Maximum.bin_size_nat0
      ; lo_bound = Minimum.bin_size_nat0
      }
  ; T
      { name = "variant_int"
      ; writer = Write.bin_write_variant_int
      ; writer_local = Write.bin_write_variant_int [@mode local]
      ; reader = Read.bin_read_variant_int
      ; reader_local = Read.bin_read_variant_int [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = -1 lsl 30
      ; max = (1 lsl 30) - 1
      ; hi_bound = Maximum.bin_size_variant_int
      ; lo_bound = Minimum.bin_size_variant_int
      }
  ; T
      { name = "int_16bit"
      ; writer = Write.bin_write_int_16bit
      ; writer_local = Write.bin_write_int_16bit [@mode local]
      ; reader = Read.bin_read_int_16bit
      ; reader_local = Read.bin_read_int_16bit [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = 0
      ; max = (1 lsl 16) - 1
      ; hi_bound = Maximum.bin_size_int_16bit
      ; lo_bound = Minimum.bin_size_int_16bit
      }
  ; T
      { name = "int_32bit"
      ; writer = Write.bin_write_int_32bit
      ; writer_local = Write.bin_write_int_32bit [@mode local]
      ; reader = Read.bin_read_int_32bit
      ; reader_local = Read.bin_read_int_32bit [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = min_int_32bit
      ; max = max_int_32bit
      ; hi_bound = Maximum.bin_size_int_32bit
      ; lo_bound = Minimum.bin_size_int_32bit
      }
  ; T
      { name = "int_64bit"
      ; writer = Write.bin_write_int_64bit
      ; writer_local = Write.bin_write_int_64bit [@mode local]
      ; reader = Read.bin_read_int_64bit
      ; reader_local = Read.bin_read_int_64bit [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = min_int_64bit
      ; max = max_int_64bit
      ; hi_bound = Maximum.bin_size_int_64bit
      ; lo_bound = Minimum.bin_size_int_64bit
      }
  ; T
      { name = "int32_bits"
      ; writer = Write.bin_write_int32_bits
      ; writer_local = Write.bin_write_int32_bits [@mode local]
      ; reader = Read.bin_read_int32_bits
      ; reader_local = Read.bin_read_int32_bits [@mode local]
      ; globalize = globalize_int32
      ; to_int64 = Int32.to_int64
      ; of_int64 = Int32.of_int64_exn
      ; min = Int32.min_value
      ; max = Int32.max_value
      ; hi_bound = Maximum.bin_size_int32_bits
      ; lo_bound = Minimum.bin_size_int32_bits
      }
  ; T
      { name = "int64_bits"
      ; writer = Write.bin_write_int64_bits
      ; writer_local = Write.bin_write_int64_bits [@mode local]
      ; reader = Read.bin_read_int64_bits
      ; reader_local = Read.bin_read_int64_bits [@mode local]
      ; globalize = globalize_int64
      ; to_int64 = Fn.id
      ; of_int64 = Fn.id
      ; min = Int64.min_value
      ; max = Int64.max_value
      ; hi_bound = Maximum.bin_size_int64_bits
      ; lo_bound = Minimum.bin_size_int64_bits
      }
  ; T
      { name = "network16_int"
      ; writer = Write.bin_write_network16_int
      ; writer_local = Write.bin_write_network16_int [@mode local]
      ; reader = Read.bin_read_network16_int
      ; reader_local = Read.bin_read_network16_int [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = 0
      ; max = (1 lsl 16) - 1
      ; hi_bound = Maximum.bin_size_network16_int
      ; lo_bound = Minimum.bin_size_network16_int
      }
  ; T
      { name = "network32_int"
      ; writer = Write.bin_write_network32_int
      ; writer_local = Write.bin_write_network32_int [@mode local]
      ; reader = Read.bin_read_network32_int
      ; reader_local = Read.bin_read_network32_int [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = min_int_32bit
      ; max = max_int_32bit
      ; hi_bound = Maximum.bin_size_network32_int
      ; lo_bound = Minimum.bin_size_network32_int
      }
  ; T
      { name = "network64_int"
      ; writer = Write.bin_write_network64_int
      ; writer_local = Write.bin_write_network64_int [@mode local]
      ; reader = Read.bin_read_network64_int
      ; reader_local = Read.bin_read_network64_int [@mode local]
      ; globalize = globalize_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min = min_int_64bit
      ; max = max_int_64bit
      ; hi_bound = Maximum.bin_size_network64_int
      ; lo_bound = Minimum.bin_size_network64_int
      }
  ; T
      { name = "network32_int32"
      ; writer = Write.bin_write_network32_int32
      ; writer_local = Write.bin_write_network32_int32 [@mode local]
      ; reader = Read.bin_read_network32_int32
      ; reader_local = Read.bin_read_network32_int32 [@mode local]
      ; globalize = globalize_int32
      ; to_int64 = Int64.of_int32
      ; of_int64 = Int64.to_int32_exn
      ; min = Int32.min_value
      ; max = Int32.max_value
      ; hi_bound = Maximum.bin_size_network32_int32
      ; lo_bound = Minimum.bin_size_network32_int32
      }
  ; T
      { name = "network64_int64"
      ; writer = Write.bin_write_network64_int64
      ; writer_local = Write.bin_write_network64_int64 [@mode local]
      ; reader = Read.bin_read_network64_int64
      ; reader_local = Read.bin_read_network64_int64 [@mode local]
      ; globalize = globalize_int64
      ; to_int64 = Fn.id
      ; of_int64 = Fn.id
      ; min = Int64.min_value
      ; max = Int64.max_value
      ; hi_bound = Maximum.bin_size_network64_int64
      ; lo_bound = Minimum.bin_size_network64_int64
      }
  ]
;;

let buf = Base_bigstring.create 32
let bin_protted_size_of t n = t.writer buf ~pos:0 (t.of_int64 n)

let mean a b =
  if Bool.equal (a < 0L) (b < 0L)
  then Int64.O.(b + Int64.shift_right (a - b) 1)
  else Int64.O.(Int64.shift_right (a + b) 1)
;;

let rec find_size_increase t ~size a b =
  assert (a < b);
  let m = mean a b in
  let n = Int64.succ m in
  assert (n <= b);
  let size_m = bin_protted_size_of t m in
  let size_n = bin_protted_size_of t n in
  assert (size_m <= size_n);
  if size_m = size && size_m < size_n
  then m
  else if size_m <= size
  then find_size_increase t ~size (Int64.succ m) b
  else find_size_increase t ~size a m
;;

let rec find_size_decrease t ~size a b =
  assert (a < b);
  let m = mean a b in
  let n = Int64.succ m in
  assert (n <= b);
  let size_m = bin_protted_size_of t m in
  let size_n = bin_protted_size_of t n in
  assert (size_m >= size_n);
  if size_m = size && size_m > size_n
  then m
  else if size_n >= size
  then find_size_decrease t ~size n b
  else find_size_decrease t ~size a m
;;

let rec find_size_increase_points t ~size1 ~size2 a b acc =
  if size1 = size2
  then acc
  else (
    let p = find_size_increase t ~size:size1 a b in
    let a = Int64.succ p in
    let size1 = bin_protted_size_of t a in
    find_size_increase_points t ~size1 ~size2 a b (Set.add acc p))
;;

let rec find_size_decrease_points t ~size1 ~size2 a b acc =
  if size1 = size2
  then acc
  else (
    let p = find_size_decrease t ~size:size1 a b in
    let a = Int64.succ p in
    let size1 = bin_protted_size_of t a in
    find_size_decrease_points t ~size1 ~size2 a b (Set.add acc p))
;;

let find_interesting_points t =
  let a = t.to_int64 t.min in
  let b = t.to_int64 t.max in
  assert (a <= 0L && b >= 0L);
  let size0 = bin_protted_size_of t 0L in
  let acc = Set.of_list (module Int64) [ 0L; a; b ] in
  let acc =
    if a < 0L
    then
      find_size_decrease_points t ~size1:(bin_protted_size_of t a) ~size2:size0 a 0L acc
    else acc
  in
  let acc =
    if b > 0L
    then
      find_size_increase_points t ~size1:size0 ~size2:(bin_protted_size_of t b) 0L b acc
    else acc
  in
  acc
;;

(* { 2 ^ n | 0 <= n <= 63 } \/ { -(2 ^ n) | 0 <= n <= 63 } *)
let power_of_twos =
  let rec loop n acc =
    if n = 64
    then acc
    else (
      let x = Int64.shift_left 1L n in
      loop (n + 1) (Set.add (Set.add acc x) (Int64.neg x)))
  in
  loop 0 (Set.empty (module Int64))
;;

let valid_power_of_twos t =
  let min = t.to_int64 t.min in
  let max = t.to_int64 t.max in
  Set.filter power_of_twos ~f:(fun n -> n >= min && n <= max)
;;

let add_windows_around_points t points =
  let min = t.to_int64 t.min in
  let max = t.to_int64 t.max in
  let rec add_between a b acc =
    let acc = Set.add acc a in
    if a = b then acc else add_between (Int64.succ a) b acc
  in
  let rec loop points acc =
    match points with
    | [] -> acc
    | i :: rest ->
      let d = Int64.( / ) test_window_len 2L in
      let a = if i <= Int64.( + ) min d then min else Int64.( - ) i d in
      let b = if i >= Int64.( - ) max d then max else Int64.( + ) i d in
      loop rest (add_between a b acc)
  in
  loop (Set.elements points) (Set.empty (module Int64))
;;

let interesting_points t =
  Set.union (find_interesting_points t) (valid_power_of_twos t)
  |> add_windows_around_points t
;;

let gen_tests (T t) =
  let points = interesting_points t in
  let min, max =
    Set.fold points ~init:(Int.max_value, 0) ~f:(fun (min, max) n ->
      let len = t.writer buf ~pos:0 (t.of_int64 n) in
      let s = Base_bigstring.To_string.sub buf ~pos:0 ~len in
      printf "%s| %s -> %Ld" t.name (to_hex s 9) n;
      let len_local = t.writer_local buf ~pos:0 (t.of_int64 n) in
      let s_local = Base_bigstring.To_string.sub buf ~pos:0 ~len:len_local in
      if String.( <> ) s s_local
      then printf ", write_local output (%s) differs from write output (%s)" s_local s;
      let pos_ref = ref 0 in
      let n' = t.reader buf ~pos_ref |> t.to_int64 in
      let len' = !pos_ref in
      let n'_local =
        let pos_ref = ref 0 in
        t.reader_local buf ~pos_ref |> t.globalize |> t.to_int64
      in
      if n' <> n'_local
      then printf ", read_local output (%Ld) differs from read output (%Ld)" n'_local n';
      if len < t.lo_bound || len > t.hi_bound
      then printf ", bin_size outside of range %d..%d: %d" t.lo_bound t.hi_bound len;
      if n <> n' || len <> len'
      then
        printf
          ", read test failed: read %d byte%s as %Ld"
          len'
          (if len' = 1 then "" else "s")
          n';
      Out_channel.output_char stdout '\n';
      Int.min min len, Int.max max len)
  in
  if min <> t.lo_bound || max <> t.hi_bound
  then
    printf
      "%s| invalid bounds: %d..%d, expected: %d..%d\n"
      t.name
      min
      max
      t.lo_bound
      t.hi_bound
;;

let run_tests () = List.iter tests ~f:gen_tests
