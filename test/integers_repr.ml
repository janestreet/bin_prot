open Core.Std

(* This program generates reference serialized output for various functions of
   [Bin_prot.Write]. The initial output of this program is committed in hg and the jbuild
   compare the output to the expected one. This way when we change something in bin_prot
   there is a check that the representation of integers doesn't change. The output is such
   that the diff is easy to understand.

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

type 'a to_test =
  { name     : string
  ; writer   : 'a Bin_prot.Write.writer
  ; reader   : 'a Bin_prot.Read.reader
  ; to_int64 : 'a -> Int64.t
  ; of_int64 : Int64.t -> 'a
  ; min      : 'a
  ; max      : 'a
  }

type to_test_packed = T : _ to_test -> to_test_packed

module Nat0 = Bin_prot.Nat0

let min_int_32bit, max_int_32bit, min_int_64bit, max_int_64bit =
  match Word_size.word_size with
  | W32 ->
    ((-1) lsl 30,
     (1 lsl 30) - 1,
     (-1) lsl 30,
     (1 lsl 30) - 1)
  | W64 ->
    ((-1) lsl 31,
     (1 lsl 31) - 1,
     (-1) lsl 62,
     (1 lsl 62) - 1)

let tests =
  [ T { name     = "int"
      ; writer   = Bin_prot.Write.bin_write_int
      ; reader   = Bin_prot.Read .bin_read_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = Int.min_value
      ; max      = Int.max_value
      }
  ; T { name     = "int32"
      ; writer   = Bin_prot.Write.bin_write_int32
      ; reader   = Bin_prot.Read .bin_read_int32
      ; to_int64 = Int64.of_int32
      ; of_int64 = Int64.to_int32_exn
      ; min      = Int32.min_value
      ; max      = Int32.max_value
      }
  ; T { name     = "int64"
      ; writer   = Bin_prot.Write.bin_write_int64
      ; reader   = Bin_prot.Read .bin_read_int64
      ; to_int64 = Fn.id
      ; of_int64 = Fn.id
      ; min      = Int64.min_value
      ; max      = Int64.max_value
      }
  ; T { name     = "nat0"
      ; writer   = Bin_prot.Write.bin_write_nat0
      ; reader   = Bin_prot.Read .bin_read_nat0
      ; to_int64 = (fun x -> Int64.of_int (x : Nat0.t :> int))
      ; of_int64 = (fun x -> Nat0.of_int (Int64.to_int_exn x))
      ; min      = Nat0.of_int 0
      ; max      = Nat0.of_int Int.max_value
      }
  ; T { name     = "variant_int"
      ; writer   = Bin_prot.Write.bin_write_variant_int
      ; reader   = Bin_prot.Read .bin_read_variant_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = (-1) lsl 30
      ; max      = (1 lsl 30) - 1
      }
  ; T { name     = "int_16bit"
      ; writer   = Bin_prot.Write.bin_write_int_16bit
      ; reader   = Bin_prot.Read .bin_read_int_16bit
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = 0
      ; max      = 1 lsl 16 - 1
      }
  ; T { name     = "int_32bit"
      ; writer   = Bin_prot.Write.bin_write_int_32bit
      ; reader   = Bin_prot.Read .bin_read_int_32bit
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = min_int_32bit
      ; max      = max_int_32bit
      }
  ; T { name     = "int_64bit"
      ; writer   = Bin_prot.Write.bin_write_int_64bit
      ; reader   = Bin_prot.Read .bin_read_int_64bit
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = min_int_64bit
      ; max      = max_int_64bit
      }
  ; T { name     = "int64_bits"
      ; writer   = Bin_prot.Write.bin_write_int64_bits
      ; reader   = Bin_prot.Read .bin_read_int64_bits
      ; to_int64 = Fn.id
      ; of_int64 = Fn.id
      ; min      = Int64.min_value
      ; max      = Int64.max_value
      }
  ; T { name     = "network16_int"
      ; writer   = Bin_prot.Write.bin_write_network16_int
      ; reader   = Bin_prot.Read .bin_read_network16_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = 0
      ; max      = 1 lsl 16 - 1
      }
  ; T { name     = "network32_int"
      ; writer   = Bin_prot.Write.bin_write_network32_int
      ; reader   = Bin_prot.Read .bin_read_network32_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = min_int_32bit
      ; max      = max_int_32bit
      }
  ; T { name     = "network64_int"
      ; writer   = Bin_prot.Write.bin_write_network64_int
      ; reader   = Bin_prot.Read .bin_read_network64_int
      ; to_int64 = Int64.of_int
      ; of_int64 = Int64.to_int_exn
      ; min      = min_int_64bit
      ; max      = max_int_64bit
      }
  ; T { name     = "network32_int32"
      ; writer   = Bin_prot.Write.bin_write_network32_int32
      ; reader   = Bin_prot.Read .bin_read_network32_int32
      ; to_int64 = Int64.of_int32
      ; of_int64 = Int64.to_int32_exn
      ; min      = Int32.min_value
      ; max      = Int32.max_value
      }
  ; T { name     = "network64_int64"
      ; writer   = Bin_prot.Write.bin_write_network64_int64
      ; reader   = Bin_prot.Read .bin_read_network64_int64
      ; to_int64 = Fn.id
      ; of_int64 = Fn.id
      ; min      = Int64.min_value
      ; max      = Int64.max_value
      }
  ]

let hex_char n =
  if n < 10 then
    Char.of_int_exn (n + Char.to_int '0')
  else
    Char.of_int_exn (n - 10 + Char.to_int 'a')

let to_hex buf =
  let len = String.length buf in
  let max_len = 9 in
  assert (len <= max_len);
  let str = String.make (max_len * 3 - 1) ' ' in
  for column = 0 to max_len - 1 do
    let ofs = (max_len - 1 - column) in
    if ofs >= len then begin
      str.[column * 3 + 0] <- '.';
      str.[column * 3 + 1] <- '.';
    end else begin
      let byte = Char.to_int buf.[ofs] in
      str.[column * 3 + 0] <- hex_char (byte lsr 4);
      str.[column * 3 + 1] <- hex_char (byte land 0xf);
    end
  done;
  str

let buf = Bigstring.create 32

let bin_protted_size_of t n =
  t.writer buf ~pos:0 (t.of_int64 n)

let mean a b =
  if Bool.equal (a < 0L) (b < 0L) then
    Int64.O.(b + Int64.shift_right (a - b) 1)
  else
    Int64.O.(Int64.shift_right (a + b) 1)

let rec find_size_increase t ~size a b =
  assert (a < b);
  let m = mean a b     in
  let n = Int64.succ m in
  assert (n <= b);
  let size_m = bin_protted_size_of t m in
  let size_n = bin_protted_size_of t n in
  assert (size_m <= size_n);
  if size_m = size && size_m < size_n then
    m
  else if size_m <= size then
    find_size_increase t ~size (Int64.succ m) b
  else
    find_size_increase t ~size a m

let rec find_size_decrease t ~size a b =
  assert (a < b);
  let m = mean a b     in
  let n = Int64.succ m in
  assert (n <= b);
  let size_m = bin_protted_size_of t m in
  let size_n = bin_protted_size_of t n in
  assert (size_m >= size_n);
  if size_m = size && size_m > size_n then
    m
  else if size_n >= size then
    find_size_decrease t ~size n b
  else
    find_size_decrease t ~size a m

let rec find_size_increase_points t ~size1 ~size2 a b acc =
  if size1 = size2 then
    acc
  else begin
    let p = find_size_increase t ~size:size1 a b in
    let a = Int64.succ p in
    let size1 = bin_protted_size_of t a in
    find_size_increase_points t ~size1 ~size2 a b (Set.add acc p)
  end

let rec find_size_decrease_points t ~size1 ~size2 a b acc =
  if size1 = size2 then
    acc
  else begin
    let p = find_size_decrease t ~size:size1 a b in
    let a = Int64.succ p in
    let size1 = bin_protted_size_of t a in
    find_size_decrease_points t ~size1 ~size2 a b (Set.add acc p)
  end

let find_interesting_points t =
  let a = t.to_int64 t.min in
  let b = t.to_int64 t.max in
  assert (a <= 0L && b >= 0L);
  let size0 = bin_protted_size_of t 0L in
  let acc = Int64.Set.of_list [0L; a; b] in
  let acc =
    if a < 0L then
      find_size_decrease_points t ~size1:(bin_protted_size_of t a) ~size2:size0 a 0L acc
    else
      acc
  in
  let acc =
    if b > 0L then
      find_size_increase_points t ~size1:size0 ~size2:(bin_protted_size_of t b) 0L b acc
    else
      acc
  in
  acc

(* { 2 ^ n | 0 <= n <= 63 } \/ { -(2 ^ n) | 0 <= n <= 63 } *)
let power_of_twos =
  let rec loop n acc =
    if n = 64 then
      acc
    else begin
      let x = Int64.shift_left 1L n in
      loop (n + 1) (Set.add (Set.add acc x) (Int64.neg x))
    end
  in
  loop 0 Int64.Set.empty

let valid_power_of_twos t =
  let min = t.to_int64 t.min in
  let max = t.to_int64 t.max in
  Set.filter power_of_twos ~f:(fun n -> n >= min && n <= max)

let add_windows_around_points t points =
  let min = t.to_int64 t.min in
  let max = t.to_int64 t.max in
  let rec add_between a b acc =
    let acc = Set.add acc a in
    if a = b then
      acc
    else
      add_between (Int64.succ a) b acc
  in
  let rec loop points acc =
    match points with
    | [] -> acc
    | i :: rest ->
      let d = Int64.(/) test_window_len 2L in
      let a = if i <= Int64.(+) min d then min else Int64.(-) i d in
      let b = if i >= Int64.(-) max d then max else Int64.(+) i d in
      loop rest (add_between a b acc)
  in
  loop (Set.elements points) Int64.Set.empty

let gen_tests (T t) =
  let points =
    Set.union (find_interesting_points t) (valid_power_of_twos t)
    |> add_windows_around_points t
  in
  Set.iter points ~f:(fun n ->
    let len = t.writer buf ~pos:0 (t.of_int64 n) in
    let s = Bigstring.To_string.sub buf ~pos:0 ~len in
    printf "%s| %s -> %Ld" t.name (to_hex s) n;
    let pos_ref = ref 0 in
    let n' = t.reader buf ~pos_ref |> t.to_int64 in
    let len' = !pos_ref in
    if n <> n' || len <> len' then
      printf ", read test failed: read %d byte%s as %Ld" len'
        (if len' = 1 then "" else "s") n';
    output_char stdout '\n')

let () = List.iter tests ~f:gen_tests
