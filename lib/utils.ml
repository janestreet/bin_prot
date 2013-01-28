(* Utils: utility functions for user convenience *)

open Bigarray
open Common
open Read_ml
open Write_ml
open Size
open Type_class

let header_len = 8

let bin_dump ?(header = false) writer v =
  let buf, pos, pos_len =
    let v_len = writer.size v in
    if header then
      let tot_len = v_len + header_len in
      let buf = create_buf tot_len in
      let pos = bin_write_int64_bits buf ~pos:0 (Int64.of_int v_len) in
      buf, pos, pos + v_len
    else
      let buf = create_buf v_len in
      buf, 0, v_len
  in
  let pos = writer.write buf ~pos v in
  if pos = pos_len then buf
  else failwith "Bin_prot.Utils.bin_dump: size changed during writing"


(* Reading from streams *)

let bin_read_stream ?max_size ~read reader =
  let buf = create_buf header_len in
  read buf ~pos:0 ~len:header_len;
  let pos_ref = ref 0 in
  let len64 = bin_read_int64_bits buf ~pos_ref in
  let len = Int64.to_int len64 in
  if Int64.of_int len <> len64 then
    failwith (
      Printf.sprintf
        "Bin_prot.Utils.bin_read_stream: size header overflow: %Ld" len64)
  else
    match max_size with
    | Some max_size when len > max_size ->
        failwith (
          Printf.sprintf
            "Bin_prot.Utils.bin_read_stream: size exceeds max_size: %d > %d"
            len max_size)
    | _ ->
        let buf = if len > header_len then create_buf len else buf in
        read buf ~pos:0 ~len;
        pos_ref := 0;
        let res = reader.read buf ~pos_ref in
        if !pos_ref = len then res
        else
          let msg =
            Printf.sprintf
              "Bin_prot.Utils.bin_read_stream: \
              protocol lied about length of value: expected %d, received %d"
              len !pos_ref
          in
          failwith msg


(* Conversion of binable types *)

module type Make_binable_spec = sig
  module Binable : Binable.S

  type t

  val to_binable : t -> Binable.t
  val of_binable : Binable.t -> t
end

module Make_binable (S : Make_binable_spec) = struct
  module B = S.Binable

  type t = S.t

  let bin_size_t t = B.bin_size_t (S.to_binable t)
  let bin_write_t buf ~pos t = B.bin_write_t buf ~pos (S.to_binable t)
  let bin_write_t_ sptr eptr t = B.bin_write_t_ sptr eptr (S.to_binable t)
  let bin_read_t buf ~pos_ref = S.of_binable (B.bin_read_t buf ~pos_ref)
  let bin_read_t_ sptr_ptr eptr = S.of_binable (B.bin_read_t_ sptr_ptr eptr)

  let bin_read_t__ sptr_ptr eptr n =
    S.of_binable (B.bin_read_t__ sptr_ptr eptr n)

  let bin_writer_t =
    {
      size = bin_size_t;
      write = bin_write_t;
      unsafe_write = bin_write_t_;
    }

  let bin_reader_t =
    {
      read = bin_read_t;
      unsafe_read = bin_read_t_;
      unsafe_vtag_read = bin_read_t__;
    }

  let bin_t =
    {
      writer = bin_writer_t;
      reader = bin_reader_t;
    }
end

module type Make_binable1_spec = sig
  module Binable : Binable.S1

  type 'a t

  val to_binable : 'a t -> 'a Binable.t
  val of_binable : 'a Binable.t -> 'a t
end

module Make_binable1 (S : Make_binable1_spec) = struct
  module B = S.Binable

  type 'a t = 'a S.t

  let bin_size_t bin_size_el t = B.bin_size_t bin_size_el (S.to_binable t)

  let bin_write_t bin_write_el buf ~pos t =
    B.bin_write_t bin_write_el buf ~pos (S.to_binable t)

  let bin_write_t_ bin_write_el sptr eptr t =
    B.bin_write_t_ bin_write_el sptr eptr (S.to_binable t)

  let bin_read_t bin_read_el buf ~pos_ref =
    S.of_binable (B.bin_read_t bin_read_el buf ~pos_ref)

  let bin_read_t_ bin_read_el sptr_ptr eptr =
    S.of_binable (B.bin_read_t_ bin_read_el sptr_ptr eptr)

  let bin_read_t__ bin_read_el sptr_ptr eptr n =
    S.of_binable (B.bin_read_t__ bin_read_el sptr_ptr eptr n)

  let bin_writer_t bin_writer =
    {
      size = (fun v -> bin_size_t bin_writer.size v);
      write = (fun buf ~pos v ->
        bin_write_t bin_writer.unsafe_write buf ~pos v);
      unsafe_write = (fun sptr eptr v ->
        bin_write_t_ bin_writer.unsafe_write sptr eptr v);
    }

  let bin_reader_t bin_reader =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t bin_reader.unsafe_read buf ~pos_ref);
      unsafe_read = (fun sptr_ptr eptr ->
        bin_read_t_ bin_reader.unsafe_read sptr_ptr eptr);
      unsafe_vtag_read = (fun _sptr_ptr _eptr _n ->
        Unsafe_read_c.raise_variant_wrong_type
          "Bin_prot.Utils.Make_binable1.bin_reader_t")
    }

  let bin_t type_class =
    {
      writer = bin_writer_t type_class.writer;
      reader = bin_reader_t type_class.reader;
    }
end

module type Make_binable2_spec = sig
  module Binable : Binable.S2

  type ('a, 'b) t

  val to_binable : ('a, 'b) t -> ('a, 'b) Binable.t
  val of_binable : ('a, 'b) Binable.t -> ('a, 'b) t
end

module Make_binable2 (S : Make_binable2_spec) = struct
  module B = S.Binable

  type ('a, 'b) t = ('a, 'b) S.t

  let bin_size_t bin_size_el1 bin_size_el2 t =
    B.bin_size_t bin_size_el1 bin_size_el2 (S.to_binable t)

  let bin_write_t bin_write_el1 bin_write_el2 buf ~pos t =
    B.bin_write_t bin_write_el1 bin_write_el2 buf ~pos (S.to_binable t)

  let bin_write_t_ bin_write_el1 bin_write_el2 sptr eptr t =
    B.bin_write_t_ bin_write_el1 bin_write_el2 sptr eptr (S.to_binable t)

  let bin_read_t bin_read_el1 bin_read_el2 buf ~pos_ref =
    S.of_binable (B.bin_read_t bin_read_el1 bin_read_el2 buf ~pos_ref)

  let bin_read_t_ bin_read_el1 bin_read_el2 sptr_ptr eptr =
    S.of_binable (B.bin_read_t_ bin_read_el1 bin_read_el2 sptr_ptr eptr)

  let bin_read_t__ bin_read_el1 bin_read_el2 sptr_ptr eptr n =
    S.of_binable (B.bin_read_t__ bin_read_el1 bin_read_el2 sptr_ptr eptr n)

  let bin_writer_t bin_writer1 bin_writer2 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.unsafe_write bin_writer2.unsafe_write buf ~pos v);
      unsafe_write = (fun sptr eptr v ->
        bin_write_t_
          bin_writer1.unsafe_write bin_writer2.unsafe_write sptr eptr v);
    }

  let bin_reader_t bin_reader1 bin_reader2 =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t
          bin_reader1.unsafe_read bin_reader2.unsafe_read buf ~pos_ref);
      unsafe_read = (fun sptr_ptr eptr ->
        bin_read_t_
          bin_reader1.unsafe_read bin_reader2.unsafe_read sptr_ptr eptr);
      unsafe_vtag_read = (fun _sptr_ptr _eptr _n ->
        Unsafe_read_c.raise_variant_wrong_type
          "Bin_prot.Utils.Make_binable2.bin_reader_t")
    }

  let bin_t type_class1 type_class2 =
    {
      writer = bin_writer_t type_class1.writer type_class2.writer;
      reader = bin_reader_t type_class1.reader type_class2.reader;
    }
end

module type Make_iterable_binable_spec = sig
  type t
  type el
  type acc

  val module_name : string option
  val length : t -> int
  val iter : t -> f : (el -> unit) -> unit
  val init : int -> acc
  val insert : acc -> el -> int -> acc
  val finish : acc -> t
  val bin_size_el : el Size.sizer
  val bin_write_el_ : el Unsafe_write_c.writer
  val bin_read_el_ : el Unsafe_read_c.reader
end

module Make_iterable_binable (S : Make_iterable_binable_spec) = struct
  open S

  type t = S.t

  let raise_concurrent_modification =
    match module_name with
    | None -> raise_concurrent_modification
    | Some module_name ->
        (fun msg ->
          let msg = Printf.sprintf "%s.%s" module_name msg in
          raise_concurrent_modification msg)

  let bin_size_t t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else raise_concurrent_modification "bin_size_t"

  let bin_write_t_ sptr eptr t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let els_sptr = Unsafe_write_c.bin_write_nat0 sptr eptr plen in
    let cnt_ref = ref 0 in
    let cur_ref = ref els_sptr in
    iter t ~f:(fun el ->
      cur_ref := bin_write_el_ !cur_ref eptr el;
      incr cnt_ref);
    if !cnt_ref = len then !cur_ref
    else raise_concurrent_modification "bin_write_t_"

  let bin_write_t buf ~pos t =
    let start, sptr, eptr = Write_c.unsafe_get_init buf ~pos in
    let cur = bin_write_t_ sptr eptr t in
    Unsafe_common.get_safe_buf_pos buf ~start ~cur

  let bin_read_t_ sptr_ptr eptr =
    let len = (Unsafe_read_c.bin_read_nat0 sptr_ptr eptr :> int) in
    let rec loop acc i =
      if i = len then finish acc
      else
        let new_acc = insert acc (bin_read_el_ sptr_ptr eptr) i in
        loop new_acc (i + 1)
    in
    loop (init len) 0

  let bin_read_t buf ~pos_ref =
    let sptr_ptr, eptr = Unsafe_common.get_read_init buf ~pos_ref in
    let el =
      try bin_read_t_ sptr_ptr eptr with
      | Unsafe_read_c.Error read_err ->
          Read_c.handle_error buf sptr_ptr read_err
      | exc -> Read_c.handle_exc buf sptr_ptr exc
    in
    Read_c.at_end buf sptr_ptr pos_ref el

  let bin_read_t__ _sptr_ptr _eptr _n =
    Unsafe_read_c.raise_variant_wrong_type "t"

  let bin_writer_t =
    {
      size = bin_size_t;
      write = bin_write_t;
      unsafe_write = bin_write_t_;
    }

  let bin_writer_t =
    {
      size = bin_size_t;
      write = bin_write_t;
      unsafe_write = bin_write_t_;
    }

  let bin_reader_t =
    {
      read = bin_read_t;
      unsafe_read = bin_read_t_;
      unsafe_vtag_read = bin_read_t__;
    }

  let bin_t =
    {
      writer = bin_writer_t;
      reader = bin_reader_t;
    }
end

module type Make_iterable_binable1_spec = sig
  type 'a t
  type 'a el
  type 'a acc

  val module_name : string option
  val length : 'a t -> int
  val iter : 'a t -> f : ('a el -> unit) -> unit
  val init : int -> 'a acc
  val insert : 'a acc -> 'a el -> int -> 'a acc
  val finish : 'a acc -> 'a t
  val bin_size_el : ('a, 'a el) Size.sizer1
  val bin_write_el_ : ('a, 'a el) Unsafe_write_c.writer1
  val bin_read_el_ : ('a, 'a el) Unsafe_read_c.reader1
end

module Make_iterable_binable1 (S : Make_iterable_binable1_spec) = struct
  open S

  type 'a t = 'a S.t

  let raise_concurrent_modification =
    match module_name with
    | None -> raise_concurrent_modification
    | Some module_name ->
        (fun msg ->
          let msg = Printf.sprintf "%s.%s" module_name msg in
          raise_concurrent_modification msg)

  let bin_size_t bin_size_a t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el bin_size_a el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else raise_concurrent_modification "bin_size_t"

  let bin_write_t_ bin_write_a_ sptr eptr t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let els_sptr = Unsafe_write_c.bin_write_nat0 sptr eptr plen in
    let cnt_ref = ref 0 in
    let cur_ref = ref els_sptr in
    iter t ~f:(fun el ->
      cur_ref := bin_write_el_ bin_write_a_ !cur_ref eptr el;
      incr cnt_ref);
    if !cnt_ref = len then !cur_ref
    else raise_concurrent_modification "bin_write_t_"

  let bin_write_t bin_write_a_ buf ~pos t =
    let start, sptr, eptr = Write_c.unsafe_get_init buf ~pos in
    let cur = bin_write_t_ bin_write_a_ sptr eptr t in
    Unsafe_common.get_safe_buf_pos buf ~start ~cur

  let bin_read_t_ bin_read_a_ sptr_ptr eptr =
    let len = (Unsafe_read_c.bin_read_nat0 sptr_ptr eptr :> int) in
    let rec loop acc i =
      if i = len then finish acc
      else
        let new_acc = insert acc (bin_read_el_ bin_read_a_ sptr_ptr eptr) i in
        loop new_acc (i + 1)
    in
    loop (init len) 0

  let bin_read_t bin_read_a_ buf ~pos_ref =
    let sptr_ptr, eptr = Unsafe_common.get_read_init buf ~pos_ref in
    let el =
      try bin_read_t_ bin_read_a_ sptr_ptr eptr with
      | Unsafe_read_c.Error read_err ->
          Read_c.handle_error buf sptr_ptr read_err
      | exc -> Read_c.handle_exc buf sptr_ptr exc
    in
    Read_c.at_end buf sptr_ptr pos_ref el

  let bin_read_t__ _sptr_ptr _eptr _n =
    Unsafe_read_c.raise_variant_wrong_type "t"

  let bin_writer_t bin_writer =
    {
      size = (fun v -> bin_size_t bin_writer.size v);
      write = (fun buf ~pos v ->
        bin_write_t bin_writer.unsafe_write buf ~pos v);
      unsafe_write = (fun sptr eptr v ->
        bin_write_t_ bin_writer.unsafe_write sptr eptr v);
    }

  let bin_writer_t bin_writer =
    {
      size = (fun v -> bin_size_t bin_writer.size v);
      write = (fun buf ~pos v ->
        bin_write_t bin_writer.unsafe_write buf ~pos v);
      unsafe_write = (fun sptr eptr v ->
        bin_write_t_ bin_writer.unsafe_write sptr eptr v);
    }

  let bin_reader_t bin_reader =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t bin_reader.unsafe_read buf ~pos_ref);
      unsafe_read = (fun sptr_ptr eptr ->
        bin_read_t_ bin_reader.unsafe_read sptr_ptr eptr);
      unsafe_vtag_read = (fun sptr_ptr eptr n -> bin_read_t__ sptr_ptr eptr n)
    }

  let bin_t type_class =
    {
      writer = bin_writer_t type_class.writer;
      reader = bin_reader_t type_class.reader;
    }
end

module type Make_iterable_binable2_spec = sig
  type ('a, 'b) t
  type ('a, 'b) el
  type ('a, 'b) acc

  val module_name : string option
  val length : ('a, 'b) t -> int
  val iter : ('a, 'b) t -> f : (('a, 'b) el -> unit) -> unit
  val init : int -> ('a, 'b) acc
  val insert : ('a, 'b) acc -> ('a, 'b) el -> int -> ('a, 'b) acc
  val finish : ('a, 'b) acc -> ('a, 'b) t
  val bin_size_el : ('a, 'b, ('a, 'b) el) Size.sizer2
  val bin_write_el_ : ('a, 'b, ('a, 'b) el) Unsafe_write_c.writer2
  val bin_read_el_ : ('a, 'b, ('a, 'b) el) Unsafe_read_c.reader2
end

module Make_iterable_binable2 (S : Make_iterable_binable2_spec) = struct
  open S

  type ('a, 'b) t = ('a, 'b) S.t

  let raise_concurrent_modification =
    match module_name with
    | None -> raise_concurrent_modification
    | Some module_name ->
        (fun msg ->
          let msg = Printf.sprintf "%s.%s" module_name msg in
          raise_concurrent_modification msg)

  let bin_size_t bin_size_a bin_size_b t =
    let size_ref = ref 0 in
    let cnt_ref = ref 0 in
    iter t ~f:(fun el ->
      size_ref := !size_ref + bin_size_el bin_size_a bin_size_b el;
      incr cnt_ref);
    let len = length t in
    if !cnt_ref = len then bin_size_nat0 (Nat0.unsafe_of_int len) + !size_ref
    else raise_concurrent_modification "bin_size_t"

  let bin_write_t_ bin_write_a_ bin_write_b_ sptr eptr t =
    let len = length t in
    let plen = Nat0.unsafe_of_int len in
    let els_sptr = Unsafe_write_c.bin_write_nat0 sptr eptr plen in
    let cnt_ref = ref 0 in
    let cur_ref = ref els_sptr in
    iter t ~f:(fun el ->
      cur_ref := bin_write_el_ bin_write_a_ bin_write_b_ !cur_ref eptr el;
      incr cnt_ref);
    if !cnt_ref = len then !cur_ref
    else raise_concurrent_modification "bin_write_t_"

  let bin_write_t bin_write_a_ bin_write_b_ buf ~pos t =
    let start, sptr, eptr = Write_c.unsafe_get_init buf ~pos in
    let cur = bin_write_t_ bin_write_a_ bin_write_b_ sptr eptr t in
    Unsafe_common.get_safe_buf_pos buf ~start ~cur

  let bin_read_t_ bin_read_a_ bin_read_b_ sptr_ptr eptr =
    let len = (Unsafe_read_c.bin_read_nat0 sptr_ptr eptr :> int) in
    let rec loop acc i =
      if i = len then finish acc
      else
        let new_acc =
          insert acc (bin_read_el_ bin_read_a_ bin_read_b_ sptr_ptr eptr) i
        in
        loop new_acc (i + 1)
    in
    loop (init len) 0

  let bin_read_t bin_read_a_ bin_read_b_ buf ~pos_ref =
    let sptr_ptr, eptr = Unsafe_common.get_read_init buf ~pos_ref in
    let el =
      try bin_read_t_ bin_read_a_ bin_read_b_ sptr_ptr eptr with
      | Unsafe_read_c.Error read_err ->
          Read_c.handle_error buf sptr_ptr read_err
      | exc -> Read_c.handle_exc buf sptr_ptr exc
    in
    Read_c.at_end buf sptr_ptr pos_ref el

  let bin_read_t__ _sptr_ptr _eptr _n =
    Unsafe_read_c.raise_variant_wrong_type "t"

  let bin_writer_t bin_writer1 bin_writer2 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.unsafe_write bin_writer2.unsafe_write buf ~pos v);
      unsafe_write = (fun sptr eptr v ->
          bin_write_t_
            bin_writer1.unsafe_write bin_writer2.unsafe_write sptr eptr v);
    }

  let bin_writer_t bin_writer1 bin_writer2 =
    {
      size = (fun v -> bin_size_t bin_writer1.size bin_writer2.size v);
      write = (fun buf ~pos v ->
        bin_write_t
          bin_writer1.unsafe_write bin_writer2.unsafe_write buf ~pos v);
      unsafe_write = (fun sptr eptr v ->
        bin_write_t_
          bin_writer1.unsafe_write bin_writer2.unsafe_write sptr eptr v);
    }

  let bin_reader_t bin_reader1 bin_reader2 =
    {
      read = (fun buf ~pos_ref ->
        bin_read_t
          bin_reader1.unsafe_read bin_reader2.unsafe_read buf ~pos_ref);
      unsafe_read = (fun sptr_ptr eptr ->
        bin_read_t_
          bin_reader1.unsafe_read bin_reader2.unsafe_read sptr_ptr eptr);
      unsafe_vtag_read = (fun sptr_ptr eptr n -> bin_read_t__ sptr_ptr eptr n)
    }

  let bin_t type_class1 type_class2 =
    {
      writer = bin_writer_t type_class1.writer type_class2.writer;
      reader = bin_reader_t type_class1.reader type_class2.reader;
    }
end
