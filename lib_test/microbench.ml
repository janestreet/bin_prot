module Type_class = Bin_prot.Type_class
module Common = Bin_prot.Common
open Bin_prot.Std

type t = { x : int; y : int } with bin_io
type u = t array with bin_io

let () =
  let v = Array.create 10_000_000 { x = 42; y = 33298742 } in
  let size = bin_size_u v in
  let buf = Common.create_buf size in

  let before = Unix.gettimeofday () in
  let pos = bin_write_u buf v ~pos:0 in
  Printf.printf "Write took %f sec\n%!" (Unix.gettimeofday () -. before);
  assert (pos = size);

  let before = Unix.gettimeofday () in
  let pos_ref = ref 0 in
  let v' = bin_read_u buf ~pos_ref in
  Printf.printf "read took %f sec\n%!" (Unix.gettimeofday () -. before);
  assert (v = v')
