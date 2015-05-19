open Bin_prot
open Core.Std

BENCH_MODULE "float array" = struct
  let a = Array.create ~len:1000 0.
  let buf =
    let buf = Common.create_buf (1000 * 8 + 8) in
    let _ = Write.bin_write_float_array buf ~pos:0 a in
    buf

  module Price = struct
    type t = float with bin_io
  end
  let price_array : Price.t array = Array.create ~len:1000 0.

  let size_float f = Size.bin_size_float f
  BENCH "size    non optimal" = Size.bin_size_array size_float a
  BENCH "size    float array" = Size.bin_size_float_array a
  BENCH "size  Price.t array" = Size.bin_size_array Price.bin_size_t price_array

  let write_float buf ~pos f = Write.bin_write_float buf ~pos f
  BENCH "write   non optimal" =
    let _ = Write.bin_write_array write_float buf ~pos:0 a in ()
  BENCH "write   float array" =
    let _ = Write.bin_write_float_array buf ~pos:0 a in ()
  BENCH "write Price.t array" =
    let _ = Write.bin_write_array Price.bin_write_t buf ~pos:0 a in ()

  let read_float buf ~pos_ref = Read.bin_read_float buf ~pos_ref
  BENCH "read    non optimal" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_array read_float buf ~pos_ref in ()
  BENCH "read    float array" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_float_array buf ~pos_ref in ()
  BENCH "read  Price.t array" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_array Price.bin_read_t buf ~pos_ref in ()

  let int_array = Array.create ~len:1000 0

  BENCH "int array  size" = Size.bin_size_array Size.bin_size_int int_array
  BENCH "int array write" =
    let _ = Write.bin_write_array Write.bin_write_int buf ~pos:0 int_array in ()
  BENCH "int array  read" =
    let pos_ref = ref 0 in
    let _ = Read.bin_read_array Read.bin_read_int buf ~pos_ref in ()

  module Book = struct
    type t = {
      a : Price.t array;
    } with bin_io
  end

  let book = { Book.a = Array.create ~len:1000 0. }
  let buf =
    let buf = Common.create_buf (2100 * 8) in
    let _ = Book.bin_write_t buf ~pos:0 book in
    buf

  BENCH "size  field" = Book.bin_size_t book
  BENCH "write field" = Book.bin_write_t buf ~pos:0 book
  BENCH "read  field" =
    let pos_ref = ref 0 in
    let _ = Book.bin_read_t buf ~pos_ref in ()
end
