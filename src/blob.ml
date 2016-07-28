open Common

module Opaque = struct
  (* [buf] is the bin-io data excluding the size header. When (de-)serialized, the size
     header is included. *)
  type t = buf

  let bin_shape_t =
    Shape.(basetype (Uuid.of_string "85a1f76e-490a-11e6-86a9-5bef585f2602") [])

  let bin_size_t t =
    Utils.size_header_length + (buf_len t)

  let bin_write_t buf ~pos t =
    let size = buf_len t in
    let pos = Utils.bin_write_size_header buf ~pos size in
    blit_buf
      ~src:t   ~src_pos:0
      ~dst:buf ~dst_pos:pos
      size;
    pos + size

  let bin_read_t buf ~pos_ref =
    let size = Utils.bin_read_size_header buf ~pos_ref in
    let t = create_buf size in
    blit_buf
      ~src:buf ~src_pos:(!pos_ref)
      ~dst:t   ~dst_pos:0
      size;
    pos_ref := !pos_ref + size;
    t

  let __bin_read_t__ _ ~pos_ref =
    raise_variant_wrong_type "Bin_prot.Blob.Opaque.t" !pos_ref
  ;;

  let bin_writer_t =
    { Type_class.
      size = bin_size_t
    ; write = bin_write_t
    }

  let bin_reader_t =
    { Type_class.
      read = bin_read_t
    ; vtag_read = __bin_read_t__
    }

  let bin_t =
    { Type_class.
      writer = bin_writer_t
    ; reader = bin_reader_t
    ; shape = bin_shape_t
    }
end

module Ignored = struct
  (* The representation of an ignored value is just the size of the value it was created
     from (i.e., the number of bytes that were ignored from the buffer we were reading
     -- we exclude the 8 byte size header from which the size was read). *)
  type t = int

  let bin_size_t size =
    Utils.size_header_length + size

  let bin_read_t buf ~pos_ref =
    let size = Utils.bin_read_size_header buf ~pos_ref in
    pos_ref := !pos_ref + size;
    size

  let __bin_read_t__ _ ~pos_ref =
    raise_variant_wrong_type
      "Bin_prot.Blob.Ignored.t"
      !pos_ref
  ;;

  let bin_reader_t =
    { Type_class.
      read = bin_read_t
    ; vtag_read = __bin_read_t__
    }
end

type 'a t = 'a

let bin_shape_t t =
  Shape.(basetype (Uuid.of_string "85a2557e-490a-11e6-98ac-4b8953d525fe") [t])

let bin_size_t bin_size_a a = Utils.size_header_length + bin_size_a a

let bin_write_t bin_write_a =
  fun buf ~pos a ->
    let start_a = pos + Utils.size_header_length in
    let end_a = bin_write_a buf ~pos:start_a a in
    let size = end_a - start_a in
    let written = Utils.bin_write_size_header buf ~pos size in
    assert (written = start_a);
    end_a

let bin_read_t bin_read_a =
  fun buf ~pos_ref ->
    let expected_size = Utils.bin_read_size_header buf ~pos_ref in
    let start_a = !pos_ref in
    let a = bin_read_a buf ~pos_ref in
    let end_a = !pos_ref in
    if end_a - start_a <> expected_size then
      failwith
        (Printf.sprintf "Bin_prot.Blob.bin_read_t: size (%d) <> expected (%d)"
           (end_a - start_a) expected_size);
    a

let __bin_read_t__ _ _ ~pos_ref =
  raise_variant_wrong_type
    "Bin_prot.Blob.t"
    !pos_ref
;;

let bin_writer_t { Type_class. size = bin_size_a; write = bin_write_a } =
  { Type_class.
    size = bin_size_t bin_size_a
  ; write = bin_write_t bin_write_a
  }

let bin_reader_t { Type_class. read = bin_read_a; vtag_read = __bin_read_a__ } =
  { Type_class.
    read = bin_read_t bin_read_a
  ; vtag_read = __bin_read_t__ __bin_read_a__
  }

let bin_t { Type_class. writer = bin_writer_a; reader = bin_reader_a
          ; shape = bin_shape_a } =
  { Type_class.
    writer = bin_writer_t bin_writer_a
  ; reader = bin_reader_t bin_reader_a
  ; shape = bin_shape_t bin_shape_a
  }

let to_opaque t bin_writer = Utils.bin_dump bin_writer t

let of_opaque_exn (buffer : Opaque.t) bin_reader =
  bin_reader.Type_class.read buffer ~pos_ref:(ref 0)
