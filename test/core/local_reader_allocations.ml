open! Core
open! Import
module Read = Bin_prot.Read
module Write = Bin_prot.Write
module Size = Bin_prot.Size

let buf = Bigstring.create 1024
let pos_ref = ref 0

let run_test ~here reader_local writer values =
  List.iter values ~f:(fun v ->
    let (_ : int) = writer buf ~pos:0 v in
    pos_ref := 0;
    Gc.For_testing.assert_no_allocation ~here (fun () ->
      (reader_local buf ~pos_ref : 'a) |> ignore))
;;

let run_test_non_integer ~(here : [%call_pos]) (test : 'a Non_integers_repr.to_test) =
  match test.reader_local with
  | None -> failwith "Expected to have local reader"
  | Some { reader_local; globalize = (_ : local_ 'a -> 'a) } ->
    run_test ~here reader_local test.writer test.values
;;

module%template Extra_tests = struct
  open Non_integers_repr

  let string =
    { writer = Write.bin_write_string
    ; writer_local = Some (Write.bin_write_string [@mode local])
    ; reader = Read.bin_read_string
    ; reader_local =
        Some
          { reader_local = Read.bin_read_string [@mode local]
          ; globalize = [%globalize: string]
          }
    ; values = [ ""; "hello world"; "\000" ]
    ; equal = String.equal
    ; sexp_of = [%sexp_of: string]
    ; hi_bound = None
    ; lo_bound = Size.Minimum.bin_size_string
    }
  ;;

  let bytes =
    { writer = Write.bin_write_bytes
    ; writer_local = Some (Write.bin_write_bytes [@mode local])
    ; reader = Read.bin_read_bytes
    ; reader_local =
        Some
          { reader_local = Read.bin_read_bytes [@mode local]
          ; globalize = [%globalize: bytes]
          }
    ; values =
        [ Bytes.of_string ""; Bytes.of_string "hello world"; Bytes.of_string "\000" ]
    ; equal = Bytes.equal
    ; sexp_of = [%sexp_of: bytes]
    ; hi_bound = None
    ; lo_bound = Size.Minimum.bin_size_bytes
    }
  ;;
end

let%expect_test "Non-integer local read allocation tests" =
  run_test_non_integer Non_integers_repr.Tests.unit;
  run_test_non_integer Non_integers_repr.Tests.bool;
  run_test_non_integer Non_integers_repr.Tests.char;
  run_test_non_integer Non_integers_repr.Tests.digest;
  run_test_non_integer Non_integers_repr.Tests.float;
  run_test_non_integer Non_integers_repr.Tests.float_nan;
  run_test_non_integer Non_integers_repr.Tests.option;
  run_test_non_integer Non_integers_repr.Tests.pair;
  run_test_non_integer Non_integers_repr.Tests.triple;
  run_test_non_integer Non_integers_repr.Tests.list;
  run_test_non_integer Extra_tests.string;
  run_test_non_integer Extra_tests.bytes;
  [%expect {| |}]
;;

let run_test_integer ~(here : [%call_pos]) (T test : Integers_repr.to_test_packed) =
  let points =
    Integers_repr.interesting_points test |> Set.to_list |> List.map ~f:test.of_int64
  in
  run_test ~here test.reader_local test.writer points
;;

let%expect_test "Integer local read allocation tests" =
  List.iter Integers_repr.tests ~f:run_test_integer;
  [%expect {| |}]
;;
