module Type_class = Bin_prot.Type_class
module Common = Bin_prot.Common
open Bin_prot.Std

type t = unit with bin_io
type t1 = t with bin_io
type t2 = t1 with bin_io
type t3 = t2 with bin_io
type t4 = t3 with bin_io
type t5 = t4 with bin_io
type t6 = t5 with bin_io
type t7 = t6 with bin_io

let io
    { Type_class.
      writer = { Type_class.write; size; _ };
      reader = { Type_class.read; _ }
    }
    v
    =
  let buf = Common.create_buf (size v) in
  let before = Unix.gettimeofday () in
  for i = 1 to 10_000_000 do
    ignore (write buf v ~pos:0 : int)
  done;
  Printf.printf "Write took %f sec\n%!" (Unix.gettimeofday () -. before);

  let before = Unix.gettimeofday () in
  let pos_ref = ref 0 in
  for i=1 to 10_000_000 do
    pos_ref := 0;
    ignore (read ~pos_ref buf)
  done;
  Printf.printf "read took %f sec\n%!" (Unix.gettimeofday () -. before)


let () =
  print_endline "===== t";
  io bin_t ();
  print_endline "===== t7";
  io bin_t7 ()
(*let mk_buf n =
  let bstr = Bigstring.create n in
  for i = 0 to n - 1 do bstr.{i} <- '\255' done;
  bstr

let check_all extra_buf_size tp_name read write args =
  let buf_len = extra_buf_size + 8 in
  let buf = mk_buf buf_len in
  match args with
  | [] -> assert false
  | (arg, _, _) :: _ ->
      let write_name = "write_" ^ tp_name in
      check_write_bounds_checks write_name buf write arg;
      let read_name = "read_" ^ tp_name in
      check_read_bounds_checks read_name buf read;
      check_all_args tp_name read write buf args
*)
