open! Base
open Expect_test_helpers_base

module _ : module type of Bin_prot.Std = struct
  open Bin_prot.Common
  open Bin_prot.Std

  include struct
    include Bin_prot.Size

    let bin_shape_lazy = bin_shape_lazy
    let bin_read_lazy = bin_read_lazy
    let __bin_read_lazy__ = __bin_read_lazy__
    let bin_size_lazy = bin_size_lazy
    let bin_write_lazy = bin_write_lazy
    let bin_write_lazy__local = bin_write_lazy__local
    let bin_reader_lazy = bin_reader_lazy
    let bin_writer_lazy = bin_writer_lazy
    let bin_lazy = bin_lazy
  end

  open struct
    type bigstring = Base_bigstring.t [@@deriving equal, sexp_of]
    type float32_mat = mat32
    type float64_mat = mat64
    type float32_vec = vec32
    type float64_vec = vec64

    module type S0 = sig
      type t [@@deriving bin_io ~localize, equal, sexp_of]

      val examples : t list
    end

    let test (type a) (module T : S0 with type t = a) =
      print_s [%message "digest" (Bin_prot.Shape.eval_to_digest_string T.bin_shape_t)];
      print_and_check_round_trip
        (module T)
        [ (module struct
            type t = T.t
            type repr = Base_bigstring.t [@@deriving sexp_of]

            let repr_name = "bigstring"

            let to_repr t =
              let size = T.bin_size_t t in
              let buf = Base_bigstring.of_string (String.make size '.') in
              let pos = T.bin_write_t buf ~pos:0 t in
              assert (pos = size);
              buf
            ;;

            let of_repr buf =
              let pos_ref = ref 0 in
              let t = T.bin_read_t buf ~pos_ref in
              assert (!pos_ref = Base_bigstring.length buf);
              t
            ;;
          end)
        ]
        T.examples
    ;;
  end

  (* 0-ary *)

  type nonrec unit = unit [@@deriving bin_io ~localize]

  let%expect_test "unit" =
    test
      (module struct
        type t = unit [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = Unit.all
      end);
    [%expect
      {|
      (digest 86ba5df747eec837f0b391dd49f33f9e)
      "\000"
      |}]
  ;;

  type nonrec bool = bool [@@deriving bin_io ~localize]

  let%expect_test "bool" =
    test
      (module struct
        type t = bool [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = Bool.all
      end);
    [%expect
      {|
      (digest a25306e4c5d30d35adbb5b0462a6b1b3)
      "\000"
      "\001"
      |}]
  ;;

  type nonrec string = string [@@deriving bin_io ~localize]

  let%expect_test "string" =
    test
      (module struct
        type t = string [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ ""; "cat"; "\000\255" ]
      end);
    [%expect
      {|
      (digest d9a8da25d5656b016fb4dbdc2e4197fb)
      "\000"
      "\003cat"
      "\002\000\255"
      |}]
  ;;

  type nonrec bytes = bytes [@@deriving bin_io ~localize]

  let%expect_test "bytes" =
    test
      (module struct
        type t = bytes [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ ""; "cat"; "\000\255" ] |> List.map ~f:Bytes.of_string
      end);
    [%expect
      {|
      (digest 06c5811b990697b0a0c71e285a10e7d4)
      "\000"
      "\003cat"
      "\002\000\255"
      |}]
  ;;

  type nonrec char = char [@@deriving bin_io ~localize]

  let%expect_test "char" =
    test
      (module struct
        type t = char [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ '\000'; 'A'; 'z'; '0'; '-'; '\255' ]
      end);
    [%expect
      {|
      (digest 84610d32d63dcff5c93f1033ec8cb1d5)
      "\000"
      A
      z
      0
      -
      "\255"
      |}]
  ;;

  type nonrec int = int [@@deriving bin_io ~localize]

  let%expect_test ("int" [@tags "no-js", "no-wasm"]) =
    test
      (module struct
        type t = int [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ Int.min_value; Int.minus_one; Int.zero; Int.one; Int.max_value ]
      end);
    [%expect
      {|
      (digest 698cfa4093fe5e51523842d37b92aeac)
      "\252\000\000\000\000\000\000\000\192"
      "\255\255"
      "\000"
      "\001"
      "\252\255\255\255\255\255\255\255?"
      |}]
  ;;

  let%expect_test ("int" [@tags "js-only", "no-wasm"]) =
    test
      (module struct
        type t = int [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ Int.min_value; Int.minus_one; Int.zero; Int.one; Int.max_value ]
      end);
    [%expect
      {|
      (digest 698cfa4093fe5e51523842d37b92aeac)
      "\253\000\000\000\128"
      "\255\255"
      "\000"
      "\001"
      "\253\255\255\255\127"
      |}]
  ;;

  let%expect_test ("int" [@tags "wasm-only"]) =
    test
      (module struct
        type t = int [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ Int.min_value; Int.minus_one; Int.zero; Int.one; Int.max_value ]
      end);
    [%expect
      {|
      (digest 698cfa4093fe5e51523842d37b92aeac)
      "\253\000\000\000\192"
      "\255\255"
      "\000"
      "\001"
      "\253\255\255\255?"
      |}]
  ;;

  type nonrec int32 = int32 [@@deriving bin_io ~localize]

  let%expect_test "int32" =
    test
      (module struct
        type t = int32 [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ Int32.min_value; Int32.minus_one; Int32.zero; Int32.one; Int32.max_value ]
        ;;
      end);
    [%expect
      {|
      (digest 0892f5f3797659e9ecf8a0faa5f76829)
      "\253\000\000\000\128"
      "\255\255"
      "\000"
      "\001"
      "\253\255\255\255\127"
      |}]
  ;;

  type nonrec int64 = int64 [@@deriving bin_io ~localize]

  let%expect_test "int64" =
    test
      (module struct
        type t = int64 [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ Int64.min_value; Int64.minus_one; Int64.zero; Int64.one; Int64.max_value ]
        ;;
      end);
    [%expect
      {|
      (digest 0078f5c24ad346a7066cb6673cd5c3cb)
      "\252\000\000\000\000\000\000\000\128"
      "\255\255"
      "\000"
      "\001"
      "\252\255\255\255\255\255\255\255\127"
      |}]
  ;;

  type nonrec nativeint = nativeint [@@deriving bin_io ~localize]

  let%expect_test ("nativeint" [@tags "no-js", "no-wasm"]) =
    test
      (module struct
        type t = nativeint [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ Nativeint.min_value
          ; Nativeint.minus_one
          ; Nativeint.zero
          ; Nativeint.one
          ; Nativeint.max_value
          ]
        ;;
      end);
    [%expect
      {|
      (digest 48d60b2896ac632fd68e45fccd6774ab)
      "\252\000\000\000\000\000\000\000\128"
      "\255\255"
      "\000"
      "\001"
      "\252\255\255\255\255\255\255\255\127"
      |}]
  ;;

  let%expect_test ("nativeint" [@tags "js-only", "no-wasm"]) =
    test
      (module struct
        type t = nativeint [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ Nativeint.min_value
          ; Nativeint.minus_one
          ; Nativeint.zero
          ; Nativeint.one
          ; Nativeint.max_value
          ]
        ;;
      end);
    [%expect
      {|
      (digest 48d60b2896ac632fd68e45fccd6774ab)
      "\253\000\000\000\128"
      "\255\255"
      "\000"
      "\001"
      "\253\255\255\255\127"
      |}]
  ;;

  let%expect_test ("nativeint" [@tags "wasm-only"]) =
    test
      (module struct
        type t = nativeint [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ Nativeint.min_value
          ; Nativeint.minus_one
          ; Nativeint.zero
          ; Nativeint.one
          ; Nativeint.max_value
          ]
        ;;
      end);
    [%expect
      {|
      (digest 48d60b2896ac632fd68e45fccd6774ab)
      "\253\000\000\000\128"
      "\255\255"
      "\000"
      "\001"
      "\253\255\255\255\127"
      |}]
  ;;

  type nonrec bigstring = bigstring [@@deriving bin_io ~localize]

  let%expect_test "bigstring" =
    test
      (module struct
        type t = bigstring [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ ""; "cat"; "\000\255" ] |> List.map ~f:Base_bigstring.of_string
      end);
    [%expect
      {|
      (digest e2d261c6c291b94bf6aa68ec2b08cb00)
      "\000"
      "\003cat"
      "\002\000\255"
      |}]
  ;;

  type nonrec float = float [@@deriving bin_io ~localize]

  let%expect_test ("float" [@tags "64-bits-only", "no-js", "no-wasm"]) =
    test
      (module struct
        type t = float [@@deriving bin_io ~localize, sexp_of]

        let equal = Comparable.lift Int64.equal ~f:Int64.bits_of_float

        let examples =
          [ Float.minus_one
          ; Float.zero
          ; Float.one
          ; Float.epsilon_float
          ; Float.min_positive_normal_value
          ; Float.min_positive_subnormal_value
          ; Float.max_finite_value
          ; Float.nan
          ; Float.infinity
          ; Float.neg_infinity
          ]
        ;;
      end);
    [%expect
      {|
      (digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      "\000\000\000\000\000\000\240\191"
      "\000\000\000\000\000\000\000\000"
      "\000\000\000\000\000\000\240?"
      "\000\000\000\000\000\000\176<"
      "\000\000\000\000\000\000\016\000"
      "\001\000\000\000\000\000\000\000"
      "\255\255\255\255\255\255\239\127"
      "\001\000\000\000\000\000\248\127"
      "\000\000\000\000\000\000\240\127"
      "\000\000\000\000\000\000\240\255"
      |}]
  ;;

  let%expect_test ("float" [@tags "js-only", "no-wasm"]) =
    test
      (module struct
        type t = float [@@deriving bin_io ~localize, sexp_of]

        let equal = Comparable.lift Int64.equal ~f:Int64.bits_of_float

        let examples =
          [ Float.minus_one
          ; Float.zero
          ; Float.one
          ; Float.epsilon_float
          ; Float.min_positive_normal_value
          ; Float.min_positive_subnormal_value
          ; Float.max_finite_value
          ; Float.nan
          ; Float.infinity
          ; Float.neg_infinity
          ]
        ;;
      end);
    [%expect
      {|
      (digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      "\000\000\000\000\000\000\240\191"
      "\000\000\000\000\000\000\000\000"
      "\000\000\000\000\000\000\240?"
      "\000\000\000\000\000\000\176<"
      "\000\000\000\000\000\000\016\000"
      "\001\000\000\000\000\000\000\000"
      "\255\255\255\255\255\255\239\127"
      "\001\000\000\000\000\000\240\127"
      "\000\000\000\000\000\000\240\127"
      "\000\000\000\000\000\000\240\255"
      |}]
  ;;

  let%expect_test ("float" [@tags "wasm-only"]) =
    test
      (module struct
        type t = float [@@deriving bin_io ~localize, sexp_of]

        let equal = Comparable.lift Int64.equal ~f:Int64.bits_of_float

        let examples =
          [ Float.minus_one
          ; Float.zero
          ; Float.one
          ; Float.epsilon_float
          ; Float.min_positive_normal_value
          ; Float.min_positive_subnormal_value
          ; Float.max_finite_value
          ; Float.nan
          ; Float.infinity
          ; Float.neg_infinity
          ]
        ;;
      end);
    [%expect
      {|
      (digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
      "\000\000\000\000\000\000\240\191"
      "\000\000\000\000\000\000\000\000"
      "\000\000\000\000\000\000\240?"
      "\000\000\000\000\000\000\176<"
      "\000\000\000\000\000\000\016\000"
      "\001\000\000\000\000\000\000\000"
      "\255\255\255\255\255\255\239\127"
      "\001\000\000\000\000\000\248\127"
      "\000\000\000\000\000\000\240\127"
      "\000\000\000\000\000\000\240\255"
      |}]
  ;;

  type nonrec floatarray = floatarray [@@deriving bin_io ~localize]

  let%expect_test ("floatarray" [@tags "no-js", "no-wasm"]) =
    test
      (module struct
        type t = floatarray [@@deriving bin_io ~localize]

        let equal = Float_array.equal (Comparable.lift Int64.equal ~f:Int64.bits_of_float)
        let sexp_of_t = Float_array.sexp_of_t

        let examples =
          [ []
          ; [ Float.minus_one ]
          ; [ Float.zero; Float.one ]
          ; [ Float.epsilon_float
            ; Float.min_positive_normal_value
            ; Float.min_positive_subnormal_value
            ]
          ; [ Float.max_finite_value; Float.nan; Float.infinity; Float.neg_infinity ]
          ]
          |> List.map ~f:Float_array.of_list
        ;;
      end);
    [%expect
      {|
      (digest bc19a6ee781b520d0caf0bd4d4ce64fb)
      "\000"
      "\001\000\000\000\000\000\000\240\191"
      "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?"
      "\003\000\000\000\000\000\000\176<\000\000\000\000\000\000\016\000\001\000\000\000\000\000\000\000"
      "\004\255\255\255\255\255\255\239\127\001\000\000\000\000\000\248\127\000\000\000\000\000\000\240\127\000\000\000\000\000\000\240\255"
      |}]
  ;;

  let%expect_test ("floatarray" [@tags "js-only", "no-wasm"]) =
    test
      (module struct
        type t = floatarray [@@deriving bin_io ~localize]

        let equal = Float_array.equal (Comparable.lift Int64.equal ~f:Int64.bits_of_float)
        let sexp_of_t = Float_array.sexp_of_t

        let examples =
          [ []
          ; [ Float.minus_one ]
          ; [ Float.zero; Float.one ]
          ; [ Float.epsilon_float
            ; Float.min_positive_normal_value
            ; Float.min_positive_subnormal_value
            ]
          ; [ Float.max_finite_value; Float.nan; Float.infinity; Float.neg_infinity ]
          ]
          |> List.map ~f:Float_array.of_list
        ;;
      end);
    [%expect
      {|
      (digest bc19a6ee781b520d0caf0bd4d4ce64fb)
      "\000"
      "\001\000\000\000\000\000\000\240\191"
      "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?"
      "\003\000\000\000\000\000\000\176<\000\000\000\000\000\000\016\000\001\000\000\000\000\000\000\000"
      "\004\255\255\255\255\255\255\239\127\000\000\000\000\000\000\248\127\000\000\000\000\000\000\240\127\000\000\000\000\000\000\240\255"
      |}]
  ;;

  let%expect_test ("floatarray" [@tags "wasm-only"]) =
    test
      (module struct
        type t = floatarray [@@deriving bin_io ~localize]

        let equal = Float_array.equal (Comparable.lift Int64.equal ~f:Int64.bits_of_float)
        let sexp_of_t = Float_array.sexp_of_t

        let examples =
          [ []
          ; [ Float.minus_one ]
          ; [ Float.zero; Float.one ]
          ; [ Float.epsilon_float
            ; Float.min_positive_normal_value
            ; Float.min_positive_subnormal_value
            ]
          ; [ Float.max_finite_value; Float.nan; Float.infinity; Float.neg_infinity ]
          ]
          |> List.map ~f:Float_array.of_list
        ;;
      end);
    [%expect
      {|
      (digest bc19a6ee781b520d0caf0bd4d4ce64fb)
      "\000"
      "\001\000\000\000\000\000\000\240\191"
      "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\240?"
      "\003\000\000\000\000\000\000\176<\000\000\000\000\000\000\016\000\001\000\000\000\000\000\000\000"
      "\004\255\255\255\255\255\255\239\127\001\000\000\000\000\000\248\127\000\000\000\000\000\000\240\127\000\000\000\000\000\000\240\255"
      |}]
  ;;

  type nonrec float32_mat = float32_mat [@@deriving bin_io ~localize]
  type nonrec float64_mat = float64_mat [@@deriving bin_io ~localize]
  type nonrec float32_vec = float32_vec [@@deriving bin_io ~localize]
  type nonrec float64_vec = float64_vec [@@deriving bin_io ~localize]
  type nonrec vec = vec [@@deriving bin_io ~localize]
  type nonrec mat = mat [@@deriving bin_io ~localize]

  (* 1-ary *)

  type nonrec 'a ref = 'a ref [@@deriving bin_io ~localize]

  let%expect_test "ref" =
    test
      (module struct
        type t = int64 ref [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ ref Int64.min_value; ref 0L; ref 1L ]
      end);
    [%expect
      {|
      (digest 100a47590a9a31a7565346f65973414e)
      "\252\000\000\000\000\000\000\000\128"
      "\000"
      "\001"
      |}]
  ;;

  type nonrec 'a lazy_t = 'a lazy_t [@@deriving bin_io ~localize]

  let%expect_test "lazy_t" =
    test
      (module struct
        type t = int64 lazy_t [@@deriving bin_io ~localize]

        let equal = Lazy.equal Int64.equal
        let sexp_of_t = Lazy.sexp_of_t Int64.sexp_of_t
        let examples = [ lazy Int64.min_value; lazy 0L; lazy 1L ]
      end);
    [%expect
      {|
      (digest 0078f5c24ad346a7066cb6673cd5c3cb)
      "\252\000\000\000\000\000\000\000\128"
      "\000"
      "\001"
      |}]
  ;;

  type nonrec 'a list = 'a list [@@deriving bin_io ~localize]

  let%expect_test "list" =
    test
      (module struct
        type t = int64 list [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ []; [ Int64.min_value ]; [ 0L; 1L ]; [ 2L; 1_000_000_000L; Int64.max_value ] ]
        ;;
      end);
    [%expect
      {|
      (digest 1ff718f726f41da9325e70d478baaa51)
      "\000"
      "\001\252\000\000\000\000\000\000\000\128"
      "\002\000\001"
      "\003\002\253\000\202\154;\252\255\255\255\255\255\255\255\127"
      |}]
  ;;

  type nonrec 'a array = 'a array [@@deriving bin_io ~localize]

  let%expect_test "array" =
    test
      (module struct
        type t = int64 array [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ [||]
          ; [| Int64.min_value |]
          ; [| 0L; 1L |]
          ; [| 2L; 1_000_000_000L; Int64.max_value |]
          ]
        ;;
      end);
    [%expect
      {|
      (digest 526e819d0478047aea6c008f10d78701)
      "\000"
      "\001\252\000\000\000\000\000\000\000\128"
      "\002\000\001"
      "\003\002\253\000\202\154;\252\255\255\255\255\255\255\255\127"
      |}]
  ;;

  type nonrec 'a iarray = 'a iarray [@@deriving bin_io ~localize]

  let%expect_test "iarray" =
    test
      (module struct
        type t = int64 iarray [@@deriving bin_io ~localize, equal, sexp_of]

        let examples =
          [ []; [ Int64.min_value ]; [ 0L; 1L ]; [ 2L; 1_000_000_000L; Int64.max_value ] ]
          |> List.map ~f:Iarray.of_list
        ;;
      end);
    [%expect
      {|
      (digest 526e819d0478047aea6c008f10d78701)
      "\000"
      "\001\252\000\000\000\000\000\000\000\128"
      "\002\000\001"
      "\003\002\253\000\202\154;\252\255\255\255\255\255\255\255\127"
      |}]
  ;;

  type nonrec 'a option = 'a option [@@deriving bin_io ~localize]

  let%expect_test "option" =
    test
      (module struct
        type t = int64 option [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ None; Some Int64.min_value; Some 0L; Some 1L ]
      end);
    [%expect
      {|
      (digest 4aeb106f47d6b4b035c25cd4327f578d)
      "\000"
      "\001\252\000\000\000\000\000\000\000\128"
      "\001\000"
      "\001\001"
      |}]
  ;;

  type nonrec 'a or_null = 'a or_null [@@deriving bin_io ~localize]

  let%expect_test "or_null" =
    test
      (module struct
        type t = int64 or_null [@@deriving bin_io ~localize, equal, sexp_of]

        let examples = [ Null; This Int64.min_value; This 0L; This 1L ]
      end);
    [%expect
      {|
      (digest 9b883eb873d7cc508dd54069f77504ae)
      "\000"
      "\001\252\000\000\000\000\000\000\000\128"
      "\001\000"
      "\001\001"
      |}]
  ;;
end
