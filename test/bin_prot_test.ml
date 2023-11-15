open! Core
open Bigarray
open Bin_prot
open Common
open Utils
open Type_class

module Bigstring = struct
  type t = buf

  let create = create_buf

  let of_string str =
    let len = String.length str in
    let buf = create len in
    blit_string_buf str buf ~len;
    buf
  ;;

  let length buf = Array1.dim buf
end

let require_does_raise here expected_exn f =
  try
    ignore (f () : _);
    Expect_test_helpers_base.print_cr here [%message "did not raise"]
  with
  | exn ->
    Expect_test_helpers_base.require
      here
      (Poly.equal exn expected_exn)
      ~if_false_then_print_s:
        [%lazy_message
          "wrong exn raised" ~expected:(expected_exn : exn) ~actual:(exn : exn)]
;;

let out_of_bounds = Invalid_argument "index out of bounds"

let check_write_bounds_checks buf write arg =
  require_does_raise [%here] out_of_bounds (fun () -> write buf ~pos:~-1 arg);
  require_does_raise [%here] Buffer_short (fun () ->
    write buf ~pos:(Bigstring.length buf) arg)
;;

let check_read_bounds_checks buf read =
  require_does_raise [%here] out_of_bounds (fun () ->
    ignore (read buf ~pos_ref:(ref ~-1) : _));
  require_does_raise [%here] Buffer_short (fun () ->
    ignore (read buf ~pos_ref:(ref (Bigstring.length buf)) : _))
;;

let check_write_result name buf pos write arg exp_len =
  let res_pos = write buf ~pos arg in
  Expect_test_helpers_base.require_equal
    [%here]
    (module Int)
    ~message:(name ^ " returned wrong write position")
    res_pos
    (pos + exp_len)
;;

let check_read_result m name buf pos read exp_ret exp_len =
  let pos_ref = ref pos in
  Expect_test_helpers_base.require_equal
    [%here]
    m
    ~message:(name ^ " returned wrong result")
    (read buf ~pos_ref)
    exp_ret;
  Expect_test_helpers_base.require_equal
    [%here]
    (module Int)
    ~message:(name ^ " returned wrong read position")
    !pos_ref
    (pos + exp_len)
;;

let check_all_args m tp_name read write buf args =
  let write_name = "write_" ^ tp_name ^ " " in
  let read_name = "read_" ^ tp_name ^ " " in
  let buf_len = Bigstring.length buf in
  let act (arg, str_arg, arg_len) =
    let write_name_arg = write_name ^ str_arg in
    let read_name_arg = read_name ^ str_arg in
    for pos = 0 to 8 do
      check_write_bounds_checks buf write arg;
      check_read_bounds_checks buf read;
      check_write_result write_name_arg buf pos write arg arg_len;
      check_read_result m read_name_arg buf pos read arg arg_len
    done;
    Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
      ignore (write buf ~pos:(buf_len - arg_len) arg : int));
    Expect_test_helpers_base.require_does_not_raise [%here] (fun () ->
      Expect_test_helpers_base.require_equal
        [%here]
        m
        ~message:(read_name_arg ^ ": read near bound returned wrong result")
        (read buf ~pos_ref:(ref (buf_len - arg_len)))
        arg);
    let small_buf = Array1.sub buf 0 (buf_len - 1) in
    require_does_raise [%here] Buffer_short (fun () ->
      write small_buf ~pos:(buf_len - arg_len) arg);
    require_does_raise [%here] Buffer_short (fun () ->
      read small_buf ~pos_ref:(ref (buf_len - arg_len)))
  in
  List.iter ~f:act args
;;

let mk_buf n =
  let bstr = Bigstring.create n in
  for i = 0 to n - 1 do
    bstr.{i} <- '\255'
  done;
  bstr
;;

let check_all m extra_buf_size tp_name read write args =
  let buf_len = extra_buf_size + 8 in
  let buf = mk_buf buf_len in
  match args with
  | [] -> assert false
  | (arg, _, _) :: _ ->
    check_write_bounds_checks buf write arg;
    check_read_bounds_checks buf read;
    check_all_args m tp_name read write buf args
;;

let check_all_with_local m extra_buf_size tp_name read write write_local args =
  let check write = check_all m extra_buf_size tp_name read write args in
  check write;
  check (fun buf ~pos v -> write_local buf ~pos v)
;;

module Random () = struct
  module Gen = Base_quickcheck.Generator

  let random = Splittable_random.of_int 0
  let generate ?(size = 1) generator = Gen.generate generator ~size ~random
  let string length = generate (Gen.string_with_length ~length)
  let bigstring length = generate (Gen.bigstring_with_length ~length)
end

let mk_int_test ~n ~len = n, Printf.sprintf "%x" n, len
let mk_nat0_test ~n ~len = Nat0.of_int n, Printf.sprintf "%x" n, len
let mk_float_test n = n, Printf.sprintf "%g" n, 8
let mk_int32_test ~n ~len = n, Printf.sprintf "%lx" n, len
let mk_int64_test ~n ~len = n, Printf.sprintf "%Lx" n, len
let mk_nativeint_test ~n ~len = n, Printf.sprintf "%nx" n, len

let mk_gen_float_vec tp n =
  let vec = Array1.create tp fortran_layout n in
  for i = 1 to n do
    vec.{i} <- float i
  done;
  vec
;;

let mk_float32_vec = mk_gen_float_vec float32
let mk_float64_vec = mk_gen_float_vec float64

let mk_gen_float_mat tp m n =
  let mat = Array2.create tp fortran_layout m n in
  let fn = float m in
  for c = 1 to n do
    let ofs = float (c - 1) *. fn in
    for r = 1 to m do
      mat.{r, c} <- ofs +. float r
    done
  done;
  mat
;;

let mk_float32_mat = mk_gen_float_mat float32
let mk_float64_mat = mk_gen_float_mat float64

let%expect_test "unit" =
  check_all_with_local
    (module Unit)
    1
    "unit"
    Read.bin_read_unit
    Write.bin_write_unit
    Write.bin_write_unit__local
    [ (), "()", 1 ]
;;

let%expect_test "bool" =
  check_all_with_local
    (module Bool)
    1
    "bool"
    Read.bin_read_bool
    Write.bin_write_bool
    Write.bin_write_bool__local
    [ true, "true", 1; false, "false", 1 ]
;;

let%expect_test ("string" [@tags "no-js"]) =
  let module Random = Random () in
  check_all_with_local
    (module String)
    66000
    "string"
    Read.bin_read_string
    Write.bin_write_string
    Write.bin_write_string__local
    [ "", "\"\"", 1
    ; Random.string 1, "random 1", 1 + 1
    ; Random.string 10, "random 10", 10 + 1
    ; Random.string 127, "random 127", 127 + 1
    ; Random.string 128, "long 128", 128 + 3
    ; Random.string 65535, "long 65535", 65535 + 3
    ; Random.string 65536, "long 65536", 65536 + 5
    ];
  if Core.Sys.word_size_in_bits = 32
  then (
    let bad_buf = Bigstring.of_string "\253\252\255\255\000" in
    require_does_raise
      [%here]
      (Read_error (String_too_long, 0))
      (fun () -> Read.bin_read_string bad_buf ~pos_ref:(ref 0));
    let bad_buf = Bigstring.of_string "\253\251\255\255\000" in
    require_does_raise [%here] Buffer_short (fun () ->
      Read.bin_read_string bad_buf ~pos_ref:(ref 0)))
  else (
    let bad_buf = Bigstring.of_string "\252\248\255\255\255\255\255\255\001" in
    require_does_raise
      [%here]
      (Read_error (String_too_long, 0))
      (fun () -> Read.bin_read_string bad_buf ~pos_ref:(ref 0));
    let bad_buf = Bigstring.of_string "\252\247\255\255\255\255\255\255\001" in
    require_does_raise [%here] Buffer_short (fun () ->
      Read.bin_read_string bad_buf ~pos_ref:(ref 0)))
;;

let%expect_test "char" =
  check_all_with_local
    (module Char)
    1
    "char"
    Read.bin_read_char
    Write.bin_write_char
    Write.bin_write_char__local
    [ 'x', "x", 1; 'y', "y", 1 ]
;;

let%expect_test ("int" [@tags "no-js"]) =
  let small_int_tests =
    [ mk_int_test ~n:~-0x01 ~len:2
    ; mk_int_test ~n:0x00 ~len:1
    ; mk_int_test ~n:0x01 ~len:1
    ; mk_int_test ~n:0x7e ~len:1
    ; mk_int_test ~n:0x7f ~len:1
    ; mk_int_test ~n:0x80 ~len:3
    ; mk_int_test ~n:0x81 ~len:3
    ; mk_int_test ~n:0x7ffe ~len:3
    ; mk_int_test ~n:0x7fff ~len:3
    ; mk_int_test ~n:0x8000 ~len:5
    ; mk_int_test ~n:0x8001 ~len:5
    ; mk_int_test ~n:0x3ffffffe ~len:5
    ; mk_int_test ~n:0x3fffffff ~len:5
    ; mk_int_test ~n:~-0x7f ~len:2
    ; mk_int_test ~n:~-0x80 ~len:2
    ; mk_int_test ~n:~-0x81 ~len:3
    ; mk_int_test ~n:~-0x82 ~len:3
    ; mk_int_test ~n:~-0x7fff ~len:3
    ; mk_int_test ~n:~-0x8000 ~len:3
    ; mk_int_test ~n:~-0x8001 ~len:5
    ; mk_int_test ~n:~-0x8002 ~len:5
    ]
  in
  let all_int_tests =
    if Core.Sys.word_size_in_bits = 32
    then small_int_tests
    else
      mk_int_test ~n:(int_of_string "-0x40000001") ~len:5
      :: mk_int_test ~n:(int_of_string "-0x40000000") ~len:5
      :: mk_int_test ~n:(int_of_string "0x7ffffffe") ~len:5
      :: mk_int_test ~n:(int_of_string "0x7fffffff") ~len:5
      :: mk_int_test ~n:(int_of_string "0x80000000") ~len:9
      :: mk_int_test ~n:(int_of_string "0x80000001") ~len:9
      :: mk_int_test ~n:Int.max_value ~len:9
      :: mk_int_test ~n:(int_of_string "-0x000000007fffffff") ~len:5
      :: mk_int_test ~n:(int_of_string "-0x0000000080000000") ~len:5
      :: mk_int_test ~n:(int_of_string "-0x0000000080000001") ~len:9
      :: mk_int_test ~n:(int_of_string "-0x0000000080000002") ~len:9
      :: mk_int_test ~n:Int.min_value ~len:9
      :: small_int_tests
  in
  check_all_with_local
    (module Int)
    9
    "int"
    Read.bin_read_int
    Write.bin_write_int
    Write.bin_write_int__local
    all_int_tests;
  let bad_buf = Bigstring.of_string "\132" in
  require_does_raise
    [%here]
    (Read_error (Int_code, 0))
    (fun () -> Read.bin_read_int bad_buf ~pos_ref:(ref 0));
  if Core.Sys.word_size_in_bits = 32
  then (
    let bad_buf = Bigstring.of_string "\253\255\255\255\064" in
    require_does_raise
      [%here]
      (Read_error (Int_overflow, 0))
      (fun () -> Read.bin_read_int bad_buf ~pos_ref:(ref 0));
    let bad_buf = Bigstring.of_string "\253\255\255\255\191" in
    require_does_raise
      [%here]
      (Read_error (Int_overflow, 0))
      (fun () -> Read.bin_read_int bad_buf ~pos_ref:(ref 0)))
  else (
    let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\064" in
    require_does_raise
      [%here]
      (Read_error (Int_overflow, 0))
      (fun () -> Read.bin_read_int bad_buf ~pos_ref:(ref 0));
    let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\191" in
    require_does_raise
      [%here]
      (Read_error (Int_overflow, 0))
      (fun () -> Read.bin_read_int bad_buf ~pos_ref:(ref 0)))
;;

let%expect_test ("nat0" [@tags "no-js"]) =
  let small_int_tests =
    [ mk_nat0_test ~n:0x00 ~len:1
    ; mk_nat0_test ~n:0x01 ~len:1
    ; mk_nat0_test ~n:0x7e ~len:1
    ; mk_nat0_test ~n:0x7f ~len:1
    ; mk_nat0_test ~n:0x80 ~len:3
    ; mk_nat0_test ~n:0x81 ~len:3
    ; mk_nat0_test ~n:0x7fff ~len:3
    ; mk_nat0_test ~n:0x8000 ~len:3
    ; mk_nat0_test ~n:0xffff ~len:3
    ; mk_nat0_test ~n:0x10000 ~len:5
    ; mk_nat0_test ~n:0x10001 ~len:5
    ; mk_nat0_test ~n:0x3ffffffe ~len:5
    ; mk_nat0_test ~n:0x3fffffff ~len:5
    ]
  in
  let all_int_tests =
    if Core.Sys.word_size_in_bits = 32
    then small_int_tests
    else
      mk_nat0_test ~n:(int_of_string "0x7fffffff") ~len:5
      :: mk_nat0_test ~n:(int_of_string "0x80000000") ~len:5
      :: mk_nat0_test ~n:(int_of_string "0xffffffff") ~len:5
      :: mk_nat0_test ~n:(int_of_string "0x100000000") ~len:9
      :: mk_nat0_test ~n:(int_of_string "0x100000001") ~len:9
      :: mk_nat0_test ~n:Int.max_value ~len:9
      :: small_int_tests
  in
  let module Nat0 = struct
    include Nat0

    let equal = Comparable.lift Int.equal ~f:(fun t -> (t : t :> int))
    let sexp_of_t t = Int.sexp_of_t (t : t :> int)
  end
  in
  check_all_with_local
    (module Nat0)
    9
    "nat0"
    Read.bin_read_nat0
    Write.bin_write_nat0
    Write.bin_write_nat0__local
    all_int_tests;
  let bad_buf = Bigstring.of_string "\128" in
  require_does_raise
    [%here]
    (Read_error (Nat0_code, 0))
    (fun () -> Read.bin_read_nat0 bad_buf ~pos_ref:(ref 0));
  if Core.Sys.word_size_in_bits = 32
  then (
    let bad_buf = Bigstring.of_string "\253\255\255\255\064" in
    require_does_raise
      [%here]
      (Read_error (Nat0_overflow, 0))
      (fun () -> Read.bin_read_nat0 bad_buf ~pos_ref:(ref 0)))
  else (
    let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\064" in
    require_does_raise
      [%here]
      (Read_error (Nat0_overflow, 0))
      (fun () -> Read.bin_read_nat0 bad_buf ~pos_ref:(ref 0)))
;;

let%expect_test "float" =
  let float_tests =
    [ mk_float_test 0.
    ; mk_float_test (-0.)
    ; mk_float_test (-1.)
    ; mk_float_test 1.
    ; mk_float_test Float.infinity
    ; mk_float_test Float.neg_infinity
    ; mk_float_test 1e-310
    ; (* subnormal *)
      mk_float_test (-1e-310)
    ; (* subnormal *)
      mk_float_test 3.141595
    ]
  in
  check_all_with_local
    (module Float)
    8
    "float"
    Read.bin_read_float
    Write.bin_write_float
    Write.bin_write_float__local
    float_tests
;;

let%expect_test "int32" =
  let int32_tests =
    [ mk_int32_test ~n:(-0x01l) ~len:2
    ; mk_int32_test ~n:0x00l ~len:1
    ; mk_int32_test ~n:0x01l ~len:1
    ; mk_int32_test ~n:0x7el ~len:1
    ; mk_int32_test ~n:0x7fl ~len:1
    ; mk_int32_test ~n:0x80l ~len:3
    ; mk_int32_test ~n:0x81l ~len:3
    ; mk_int32_test ~n:0x7ffel ~len:3
    ; mk_int32_test ~n:0x7fffl ~len:3
    ; mk_int32_test ~n:0x8000l ~len:5
    ; mk_int32_test ~n:0x8001l ~len:5
    ; mk_int32_test ~n:0x7ffffffel ~len:5
    ; mk_int32_test ~n:0x7fffffffl ~len:5
    ; mk_int32_test ~n:(-0x7fl) ~len:2
    ; mk_int32_test ~n:(-0x80l) ~len:2
    ; mk_int32_test ~n:(-0x81l) ~len:3
    ; mk_int32_test ~n:(-0x82l) ~len:3
    ; mk_int32_test ~n:(-0x7fffl) ~len:3
    ; mk_int32_test ~n:(-0x8000l) ~len:3
    ; mk_int32_test ~n:(-0x8001l) ~len:5
    ; mk_int32_test ~n:(-0x8002l) ~len:5
    ; mk_int32_test ~n:(-0x80000001l) ~len:5
    ; mk_int32_test ~n:(-0x80000000l) ~len:5
    ]
  in
  check_all_with_local
    (module Int32)
    5
    "int32"
    Read.bin_read_int32
    Write.bin_write_int32
    Write.bin_write_int32__local
    int32_tests;
  let bad_buf = Bigstring.of_string "\132" in
  require_does_raise
    [%here]
    (Read_error (Int32_code, 0))
    (fun () -> Read.bin_read_int32 bad_buf ~pos_ref:(ref 0))
;;

let%expect_test "int64" =
  let int64_tests =
    [ mk_int64_test ~n:(-0x01L) ~len:2
    ; mk_int64_test ~n:0x00L ~len:1
    ; mk_int64_test ~n:0x01L ~len:1
    ; mk_int64_test ~n:0x7eL ~len:1
    ; mk_int64_test ~n:0x7fL ~len:1
    ; mk_int64_test ~n:0x80L ~len:3
    ; mk_int64_test ~n:0x81L ~len:3
    ; mk_int64_test ~n:0x7ffeL ~len:3
    ; mk_int64_test ~n:0x7fffL ~len:3
    ; mk_int64_test ~n:0x8000L ~len:5
    ; mk_int64_test ~n:0x8001L ~len:5
    ; mk_int64_test ~n:0x7ffffffeL ~len:5
    ; mk_int64_test ~n:0x7fffffffL ~len:5
    ; mk_int64_test ~n:0x80000000L ~len:9
    ; mk_int64_test ~n:0x80000001L ~len:9
    ; mk_int64_test ~n:0x7ffffffffffffffeL ~len:9
    ; mk_int64_test ~n:0x7fffffffffffffffL ~len:9
    ; mk_int64_test ~n:(-0x7fL) ~len:2
    ; mk_int64_test ~n:(-0x80L) ~len:2
    ; mk_int64_test ~n:(-0x81L) ~len:3
    ; mk_int64_test ~n:(-0x82L) ~len:3
    ; mk_int64_test ~n:(-0x7fffL) ~len:3
    ; mk_int64_test ~n:(-0x8000L) ~len:3
    ; mk_int64_test ~n:(-0x8001L) ~len:5
    ; mk_int64_test ~n:(-0x8002L) ~len:5
    ; mk_int64_test ~n:(-0x7fffffffL) ~len:5
    ; mk_int64_test ~n:(-0x80000000L) ~len:5
    ; mk_int64_test ~n:(-0x80000001L) ~len:9
    ; mk_int64_test ~n:(-0x80000002L) ~len:9
    ; mk_int64_test ~n:(-0x8000000000000001L) ~len:9
    ; mk_int64_test ~n:(-0x8000000000000000L) ~len:9
    ]
  in
  check_all_with_local
    (module Int64)
    9
    "int64"
    Read.bin_read_int64
    Write.bin_write_int64
    Write.bin_write_int64__local
    int64_tests;
  let bad_buf = Bigstring.of_string "\132" in
  require_does_raise
    [%here]
    (Read_error (Int64_code, 0))
    (fun () -> Read.bin_read_int64 bad_buf ~pos_ref:(ref 0))
;;

let%expect_test "nativeint" =
  let small_nativeint_tests =
    [ mk_nativeint_test ~n:(-0x01n) ~len:2
    ; mk_nativeint_test ~n:0x00n ~len:1
    ; mk_nativeint_test ~n:0x01n ~len:1
    ; mk_nativeint_test ~n:0x7en ~len:1
    ; mk_nativeint_test ~n:0x7fn ~len:1
    ; mk_nativeint_test ~n:0x80n ~len:3
    ; mk_nativeint_test ~n:0x81n ~len:3
    ; mk_nativeint_test ~n:0x7ffen ~len:3
    ; mk_nativeint_test ~n:0x7fffn ~len:3
    ; mk_nativeint_test ~n:0x8000n ~len:5
    ; mk_nativeint_test ~n:0x8001n ~len:5
    ; mk_nativeint_test ~n:0x7ffffffen ~len:5
    ; mk_nativeint_test ~n:0x7fffffffn ~len:5
    ; mk_nativeint_test ~n:(-0x7fn) ~len:2
    ; mk_nativeint_test ~n:(-0x80n) ~len:2
    ; mk_nativeint_test ~n:(-0x81n) ~len:3
    ; mk_nativeint_test ~n:(-0x82n) ~len:3
    ; mk_nativeint_test ~n:(-0x7fffn) ~len:3
    ; mk_nativeint_test ~n:(-0x8000n) ~len:3
    ; mk_nativeint_test ~n:(-0x8001n) ~len:5
    ; mk_nativeint_test ~n:(-0x8002n) ~len:5
    ; mk_nativeint_test ~n:(-0x7fffffffn) ~len:5
    ; mk_nativeint_test ~n:(-0x80000000n) ~len:5
    ]
  in
  let nativeint_tests =
    if Core.Sys.word_size_in_bits = 32
    then small_nativeint_tests
    else
      mk_nativeint_test ~n:(Nativeint.of_string "0x80000000") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "0x80000001") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "-0x80000001") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "-0x80000002") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "0x7ffffffffffffffe") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "0x7fffffffffffffff") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "-0x8000000000000001") ~len:9
      :: mk_nativeint_test ~n:(Nativeint.of_string "-0x8000000000000000") ~len:9
      :: small_nativeint_tests
  in
  let size = if Core.Sys.word_size_in_bits = 32 then 5 else 9 in
  check_all_with_local
    (module Nativeint)
    size
    "nativeint"
    Read.bin_read_nativeint
    Write.bin_write_nativeint
    Write.bin_write_nativeint__local
    nativeint_tests;
  let bad_buf = Bigstring.of_string "\251" in
  require_does_raise
    [%here]
    (Read_error (Nativeint_code, 0))
    (fun () -> Read.bin_read_nativeint bad_buf ~pos_ref:(ref 0));
  if Core.Sys.word_size_in_bits = 32
  then (
    let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\255\255" in
    require_does_raise
      [%here]
      (Read_error (Nativeint_code, 0))
      (fun () -> Read.bin_read_nativeint bad_buf ~pos_ref:(ref 0)))
;;

let%expect_test "ref" =
  check_all_with_local
    (module struct
      type t = int ref [@@deriving equal, sexp_of]
    end)
    1
    "ref"
    (Read.bin_read_ref Read.bin_read_int)
    (Write.bin_write_ref Write.bin_write_int)
    (Write.bin_write_ref__local Write.bin_write_int__local)
    [ ref 42, "ref 42", 1 ]
;;

let%expect_test "option" =
  check_all_with_local
    (module struct
      type t = int option [@@deriving equal, sexp_of]
    end)
    2
    "option"
    (Read.bin_read_option Read.bin_read_int)
    (Write.bin_write_option Write.bin_write_int)
    (Write.bin_write_option__local Write.bin_write_int__local)
    [ Some 42, "Some 42", 2; None, "None", 1 ]
;;

let%expect_test "pair" =
  check_all_with_local
    (module struct
      type t = float * int [@@deriving equal, sexp_of]
    end)
    9
    "pair"
    (Read.bin_read_pair Read.bin_read_float Read.bin_read_int)
    (Write.bin_write_pair Write.bin_write_float Write.bin_write_int)
    (Write.bin_write_pair__local Write.bin_write_float__local Write.bin_write_int__local)
    [ (3.141, 42), "(3.141, 42)", 9 ]
;;

let%expect_test "triple" =
  check_all_with_local
    (module struct
      type t = float * int * string [@@deriving equal, sexp_of]
    end)
    14
    "triple"
    (Read.bin_read_triple Read.bin_read_float Read.bin_read_int Read.bin_read_string)
    (Write.bin_write_triple
       Write.bin_write_float
       Write.bin_write_int
       Write.bin_write_string)
    (Write.bin_write_triple__local
       Write.bin_write_float__local
       Write.bin_write_int__local
       Write.bin_write_string__local)
    [ (3.141, 42, "test"), "(3.141, 42, \"test\")", 14 ]
;;

let%expect_test "list" =
  check_all_with_local
    (module struct
      type t = int list [@@deriving equal, sexp_of]
    end)
    12
    "list"
    (Read.bin_read_list Read.bin_read_int)
    (Write.bin_write_list Write.bin_write_int)
    (Write.bin_write_list__local Write.bin_write_int__local)
    [ [ 42; -1; 200; 33000 ], "[42; -1; 200; 33000]", 12; [], "[]", 1 ]
;;

let%expect_test ("array" [@tags "no-js"]) =
  let bin_read_int_array = Read.bin_read_array Read.bin_read_int in
  check_all_with_local
    (module struct
      type t = int array [@@deriving equal, sexp_of]
    end)
    12
    "array"
    bin_read_int_array
    (Write.bin_write_array Write.bin_write_int)
    (Write.bin_write_array__local Write.bin_write_int__local)
    [ [| 42; -1; 200; 33000 |], "[|42; -1; 200; 33000|]", 12; [||], "[||]", 1 ];
  if Core.Sys.word_size_in_bits = 32
  then (
    let bad_buf = Bigstring.of_string "\253\000\000\064\000" in
    require_does_raise
      [%here]
      (Read_error (Array_too_long, 0))
      (fun () -> bin_read_int_array bad_buf ~pos_ref:(ref 0));
    let bad_buf = Bigstring.of_string "\253\255\255\063\000" in
    require_does_raise [%here] Buffer_short (fun () ->
      bin_read_int_array bad_buf ~pos_ref:(ref 0)))
  else (
    let bad_buf = Bigstring.of_string "\252\000\000\000\000\000\000\064\000" in
    require_does_raise
      [%here]
      (Read_error (Array_too_long, 0))
      (fun () -> bin_read_int_array bad_buf ~pos_ref:(ref 0));
    let bad_buf = Bigstring.of_string "\252\255\255\255\255\255\255\063\000" in
    require_does_raise [%here] Buffer_short (fun () ->
      bin_read_int_array bad_buf ~pos_ref:(ref 0)))
;;

let%expect_test "hashtbl" =
  let bindings = List.rev [ 42, 3.; 17, 2.; 42, 4. ] in
  let htbl = Stdlib.Hashtbl.create (List.length bindings) in
  List.iter ~f:(fun (k, v) -> Stdlib.Hashtbl.add htbl k v) bindings;
  check_all
    (module struct
      type t = (int, float) Stdlib.Hashtbl.t

      let to_map t =
        Stdlib.Hashtbl.fold
          (fun key data acc -> Map.add_multi acc ~key ~data)
          t
          Int.Map.empty
      ;;

      let equal = Comparable.lift (Int.Map.equal (List.equal Float.equal)) ~f:to_map
      let sexp_of_t t = Int.Map.sexp_of_t (List.sexp_of_t Float.sexp_of_t) (to_map t)
    end)
    28
    "hashtbl"
    (Read.bin_read_hashtbl Read.bin_read_int Read.bin_read_float)
    (Write.bin_write_hashtbl Write.bin_write_int Write.bin_write_float)
    [ htbl, "[(42, 3.); (17, 2.); (42, 4.)]", 28; Stdlib.Hashtbl.create 0, "[]", 1 ]
;;

module Array1_extras (M : Expect_test_helpers_base.With_equal) = struct
  let to_list : type a b c. (a, b, c) Array1.t -> a list =
    fun t ->
    let get i =
      match Array1.layout t with
      | C_layout -> Array1.get t i
      | Fortran_layout -> Array1.get t (i + 1)
    in
    List.init (Array1.dim t) ~f:get
  ;;

  let equal t1 t2 = Comparable.lift (List.equal M.equal) ~f:to_list t1 t2
  let sexp_of_t t = List.sexp_of_t M.sexp_of_t (to_list t)
end

let%expect_test "float32_vec" =
  let n = 128 in
  let header = 3 in
  let size = header + (n * 4) in
  let vec = mk_float32_vec n in
  check_all_with_local
    (module struct
      type t = vec32

      include Array1_extras (Float)
    end)
    size
    "float32_vec"
    Read.bin_read_float32_vec
    Write.bin_write_float32_vec
    Write.bin_write_float32_vec__local
    [ vec, "[| ... |]", size; mk_float32_vec 0, "[||]", 1 ]
;;

let%expect_test "float64_vec" =
  let n = 127 in
  let header = 1 in
  let size = header + (n * 8) in
  let vec = mk_float64_vec n in
  check_all_with_local
    (module struct
      type t = vec64

      include Array1_extras (Float)
    end)
    size
    "float64_vec"
    Read.bin_read_float64_vec
    Write.bin_write_float64_vec
    Write.bin_write_float64_vec__local
    [ vec, "[| ... |]", size; mk_float64_vec 0, "[||]", 1 ]
;;

let%expect_test "vec" =
  let n = 128 in
  let header = 3 in
  let size = header + (n * 8) in
  let vec = mk_float64_vec n in
  check_all_with_local
    (module struct
      type t = vec

      include Array1_extras (Float)
    end)
    size
    "vec"
    Read.bin_read_vec
    Write.bin_write_vec
    Write.bin_write_vec__local
    [ vec, "[| ... |]", size; mk_float64_vec 0, "[||]", 1 ]
;;

module Array2_extras (M : Expect_test_helpers_base.With_equal) = struct
  let to_list : type a b c. (a, b, c) Array2.t -> a list list =
    fun t ->
    let get i1 i2 =
      match Array2.layout t with
      | C_layout -> Array2.get t i1 i2
      | Fortran_layout -> Array2.get t (i1 + 1) (i2 + 1)
    in
    List.init (Array2.dim1 t) ~f:(fun dim1 ->
      List.init (Array2.dim2 t) ~f:(fun dim2 -> get dim1 dim2))
  ;;

  let equal t1 t2 = Comparable.lift (List.equal (List.equal M.equal)) ~f:to_list t1 t2
  let sexp_of_t t = List.sexp_of_t (List.sexp_of_t M.sexp_of_t) (to_list t)
end

let%expect_test "float32_mat" =
  let m = 128 in
  let n = 127 in
  let header = 3 + 1 in
  let size = header + (m * n * 4) in
  let mat = mk_float32_mat m n in
  check_all_with_local
    (module struct
      type t = mat32

      include Array2_extras (Float)
    end)
    size
    "float32_mat"
    Read.bin_read_float32_mat
    Write.bin_write_float32_mat
    Write.bin_write_float32_mat__local
    [ mat, "[| ... |]", size; mk_float32_mat 0 0, "[||]", 2 ]
;;

let%expect_test "float64_mat" =
  let m = 10 in
  let n = 12 in
  let header = 1 + 1 in
  let size = header + (m * n * 8) in
  let mat = mk_float64_mat m n in
  check_all_with_local
    (module struct
      type t = mat64

      include Array2_extras (Float)
    end)
    size
    "float64_mat"
    Read.bin_read_float64_mat
    Write.bin_write_float64_mat
    Write.bin_write_float64_mat__local
    [ mat, "[| ... |]", size; mk_float64_mat 0 0, "[||]", 2 ]
;;

let%expect_test "mat" =
  let m = 128 in
  let n = 128 in
  let header = 3 + 3 in
  let size = header + (m * n * 8) in
  let mat = mk_float64_mat m n in
  check_all_with_local
    (module struct
      type t = mat

      include Array2_extras (Float)
    end)
    size
    "mat"
    Read.bin_read_mat
    Write.bin_write_mat
    Write.bin_write_mat__local
    [ mat, "[| ... |]", size; mk_float64_mat 0 0, "[||]", 2 ]
;;

let%expect_test "bigstring" =
  let module Random = Random () in
  let n = 128 in
  let header = 3 in
  let size = header + n in
  let bstr = Random.bigstring n in
  check_all_with_local
    (module struct
      type t = Bigstring.t

      include Array1_extras (Char)
    end)
    size
    "bigstring"
    Read.bin_read_bigstring
    Write.bin_write_bigstring
    Write.bin_write_bigstring__local
    [ bstr, "[| ... |]", size; Random.bigstring 0, "[||]", 1 ]
;;

let%expect_test "bigstring (big)" =
  let module Random = Random () in
  (* [n] is a 16bits integer that will be serialized differently depending on
     whether it is considered as an integer or an unsigned integer. *)
  let n = 40_000 in
  let header = 3 in
  let size = header + n in
  let bstr = Random.bigstring n in
  check_all_with_local
    (module struct
      type t = Bigstring.t

      include Array1_extras (Char)
    end)
    size
    "bigstring"
    Read.bin_read_bigstring
    Write.bin_write_bigstring
    Write.bin_write_bigstring__local
    [ bstr, "[| ... |]", size; Random.bigstring 0, "[||]", 1 ]
;;

let%expect_test "variant_tag" =
  check_all_with_local
    (module Int)
    4
    "variant_tag"
    Read.bin_read_variant_int
    Write.bin_write_variant_int
    Write.bin_write_variant_int__local
    [ (Obj.magic `Foo : int), "`Foo", 4; (Obj.magic `Bar : int), "`Bar", 4 ];
  let bad_buf = Bigstring.of_string "\000\000\000\000" in
  require_does_raise
    [%here]
    (Read_error (Variant_tag, 0))
    (fun () -> Read.bin_read_variant_int bad_buf ~pos_ref:(ref 0))
;;

let%expect_test "int64_bits" =
  check_all_with_local
    (module Int64)
    8
    "int64_bits"
    Read.bin_read_int64_bits
    Write.bin_write_int64_bits
    Write.bin_write_int64_bits__local
    [ Int64.min_value, "min_value", 8
    ; Int64.( + ) Int64.min_value Int64.one, "min_value + 1", 8
    ; Int64.minus_one, "-1", 8
    ; Int64.zero, "0", 8
    ; Int64.one, "1", 8
    ; Int64.( - ) Int64.max_value Int64.one, "max_value - 1", 8
    ; Int64.max_value, "max_value", 8
    ]
;;

let%expect_test "int_64bit" =
  check_all_with_local
    (module Int)
    8
    "int_64bit"
    Read.bin_read_int_64bit
    Write.bin_write_int_64bit
    Write.bin_write_int_64bit__local
    [ Int.min_value, "min_value", 8
    ; Int.min_value + 1, "min_value + 1", 8
    ; -1, "-1", 8
    ; 0, "0", 8
    ; 1, "1", 8
    ; Int.max_value - 1, "max_value - 1", 8
    ; Int.max_value, "max_value", 8
    ];
  let bad_buf_max =
    bin_dump bin_int64_bits.writer (Int64.succ (Int64.of_int Int.max_value))
  in
  require_does_raise
    [%here]
    (Read_error (Int_overflow, 0))
    (fun () -> Read.bin_read_int_64bit bad_buf_max ~pos_ref:(ref 0));
  let bad_buf_min =
    bin_dump bin_int64_bits.writer (Int64.pred (Int64.of_int Int.min_value))
  in
  require_does_raise
    [%here]
    (Read_error (Int_overflow, 0))
    (fun () -> Read.bin_read_int_64bit bad_buf_min ~pos_ref:(ref 0))
;;

let%expect_test "network16_int" =
  check_all_with_local
    (module Int)
    2
    "network16_int"
    Read.bin_read_network16_int
    Write.bin_write_network16_int
    Write.bin_write_network16_int__local
    [ (* No negative numbers - ambiguous on 64bit platforms *) 0, "0", 2; 1, "1", 2 ]
;;

let%expect_test "network32_int" =
  check_all_with_local
    (module Int)
    4
    "network32_int"
    Read.bin_read_network32_int
    Write.bin_write_network32_int
    Write.bin_write_network32_int__local
    [ (* No negative numbers - ambiguous on 64bit platforms *) 0, "0", 4; 1, "1", 4 ]
;;

let%expect_test "network32_int32" =
  check_all_with_local
    (module Int32)
    4
    "network32_int32"
    Read.bin_read_network32_int32
    Write.bin_write_network32_int32
    Write.bin_write_network32_int32__local
    [ -1l, "-1", 4; 0l, "0", 4; 1l, "1", 4 ]
;;

let%expect_test "network64_int" =
  check_all_with_local
    (module Int)
    8
    "network64_int"
    Read.bin_read_network64_int
    Write.bin_write_network64_int
    Write.bin_write_network64_int__local
    [ -1, "-1", 8; 0, "0", 8; 1, "1", 8 ]
;;

let%expect_test "network64_int64" =
  check_all_with_local
    (module Int64)
    8
    "network64_int64"
    Read.bin_read_network64_int64
    Write.bin_write_network64_int64
    Write.bin_write_network64_int64__local
    [ -1L, "-1", 8; 0L, "0", 8; 1L, "1", 8 ]
;;
