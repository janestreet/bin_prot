open! Base
open Common
open Type_class

module Definitions = struct
  module type Make_binable_without_uuid_spec = sig
    module Binable : Binable.Minimal.S

    type t

    val to_binable : t -> Binable.t
    val of_binable : Binable.t -> t
  end

  module type Make_binable_with_uuid_spec = sig
    include Make_binable_without_uuid_spec

    (** [caller_identity] is necessary to ensure different callers of
        [Make_binable_with_uuid] are not shape compatible. *)
    val caller_identity : Shape.Uuid.t
  end

  module type Make_binable1_without_uuid_spec = sig
    module Binable : Binable.Minimal.S1

    type 'a t

    val to_binable : 'a. 'a t -> 'a Binable.t
    val of_binable : 'a. 'a Binable.t -> 'a t
  end

  module type Make_binable1_with_uuid_spec = sig
    include Make_binable1_without_uuid_spec

    val caller_identity : Shape.Uuid.t
  end

  module type Make_binable2_without_uuid_spec = sig
    module Binable : Binable.Minimal.S2

    type ('a, 'b) t

    val to_binable : 'a 'b. ('a, 'b) t -> ('a, 'b) Binable.t
    val of_binable : 'a 'b. ('a, 'b) Binable.t -> ('a, 'b) t
  end

  module type Make_binable2_with_uuid_spec = sig
    include Make_binable2_without_uuid_spec

    val caller_identity : Shape.Uuid.t
  end

  module type Make_binable3_without_uuid_spec = sig
    module Binable : Binable.Minimal.S3

    type ('a, 'b, 'c) t

    val to_binable : 'a 'b 'c. ('a, 'b, 'c) t -> ('a, 'b, 'c) Binable.t
    val of_binable : 'a 'b 'c. ('a, 'b, 'c) Binable.t -> ('a, 'b, 'c) t
  end

  module type Make_binable3_with_uuid_spec = sig
    include Make_binable3_without_uuid_spec

    val caller_identity : Shape.Uuid.t
  end

  module type Make_iterable_binable_spec = sig
    type t
    type el

    (** [caller_identity] is necessary to ensure different callers of
        [Make_iterable_binable] are not shape compatible. *)
    val caller_identity : Shape.Uuid.t

    val module_name : string option
    val init : len:int -> next:(unit -> el) -> t

    include sig
      val length : t -> int
      val iter : t -> f:(el -> unit) -> unit
      val bin_size_el : el Size.sizer
      val bin_write_el : el Write.writer
    end

    val bin_read_el : el Read.reader
    val bin_shape_el : Shape.t
  end

  module type Make_iterable_binable1_spec = sig
    type 'a t
    type 'a el

    val caller_identity : Shape.Uuid.t
    val module_name : string option
    val init : len:int -> next:(unit -> 'a el) -> 'a t

    include sig
      val length : 'a t -> int
      val iter : 'a t -> f:('a el -> unit) -> unit
      val bin_size_el : ('a, 'a el) Size.sizer1
      val bin_write_el : ('a, 'a el) Write.writer1
    end

    val bin_read_el : ('a, 'a el) Read.reader1
    val bin_shape_el : Shape.t -> Shape.t
  end

  module type Make_iterable_binable2_spec = sig
    type ('a, 'b) t
    type ('a, 'b) el

    val caller_identity : Shape.Uuid.t
    val module_name : string option
    val init : len:int -> next:(unit -> ('a, 'b) el) -> ('a, 'b) t

    include sig
      val length : ('a, 'b) t -> int
      val iter : ('a, 'b) t -> f:(('a, 'b) el -> unit) -> unit
      val bin_size_el : ('a, 'b, ('a, 'b) el) Size.sizer2
      val bin_write_el : ('a, 'b, ('a, 'b) el) Write.writer2
    end

    val bin_read_el : ('a, 'b, ('a, 'b) el) Read.reader2
    val bin_shape_el : Shape.t -> Shape.t -> Shape.t
  end

  module type Make_iterable_binable3_spec = sig
    type ('a, 'b, 'c) t
    type ('a, 'b, 'c) el

    val caller_identity : Shape.Uuid.t
    val module_name : string option
    val init : len:int -> next:(unit -> ('a, 'b, 'c) el) -> ('a, 'b, 'c) t

    include sig
      val length : ('a, 'b, 'c) t -> int
      val iter : ('a, 'b, 'c) t -> f:(('a, 'b, 'c) el -> unit) -> unit
      val bin_size_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Size.sizer3
      val bin_write_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Write.writer3
    end

    val bin_read_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Read.reader3
    val bin_shape_el : Shape.t -> Shape.t -> Shape.t -> Shape.t
  end
end

(** Utility functions for user convenience *)
module type Utils = sig
  include module type of struct
    include Definitions
  end

  (** [size_header_length] is the standard number of bytes allocated for the size header
      in size-prefixed bin-io payloads. This size-prefixed layout is used by the
      [bin_dump] and [bin_read_stream] functions below, as well as:
      - [Core.Bigstring.{read,write}_bin_prot]
      - [Core.Unpack_buffer.unpack_bin_prot]
      - [Async.{Reader,Writer}.{read,write}_bin_prot] among others.

      The size prefix is always 8 bytes at present. This is exposed so your program does
      not have to know this fact too.

      We do not use a variable length header because we want to know how many bytes to
      read to get the size without having to peek into the payload. *)
  val size_header_length : int

  val bin_read_size_header : int Read.reader

  (** [bin_read_size_header] and [bin_write_size_header] are bin-prot serializers for the
      size header described above. *)
  val bin_write_size_header : int Write.writer

  (** [bin_dump ?header writer v] uses [writer] to first compute the size of [v] in the
      binary protocol, then allocates a buffer of exactly this size, and then writes out
      the value. If [header] is [true], the size of the resulting binary string will be
      prefixed as a signed 64bit integer.

      @return the buffer containing the written out value.

      @param header default = [false]

      @raise Failure
        if the size of the value changes during writing, and any other exceptions that the
        binary writer in [writer] can raise. *)
  val bin_dump : ?header:bool -> 'a writer -> 'a -> buf

  (** [bin_dump_aux ~header ~v_size ~write] is like [bin_dump], but the caller is required
      to do the polymorphic computation. [v] is the value we want to write, [v_size] is
      the size of [v] when serialized, and [write buf ~pos] writes [v] to [buf] at
      position [pos], returning the end position.

      [bin_dump] can't be used if [v] is unboxed, but [bin_dump_aux] can. Thus,
      [bin_dump_aux] is exposed so it can be used with the PPX [bin_io ~util]. *)
  val bin_dump_aux : header:bool -> v_size:int -> write:(buf -> pos:pos -> pos) -> buf

  (** [bin_read_stream ?max_size ~read reader] reads binary protocol data from a stream as
      generated by the [read] function, which places data of a given length into a given
      buffer. Requires a header. The [reader] type class will be used for conversion to
      OCaml-values.

      @param max_size default = nothing

      @raise Failure
        if the size of the value disagrees with the one specified in the header, and any
        other exceptions that the binary reader associated with [reader] can raise.

      @raise Failure if the size reported in the data header is longer than [max_size]. *)
  val bin_read_stream
    :  ?max_size:int
    -> read:(buf -> pos:int -> len:int -> unit)
    -> 'a reader
    -> 'a

  (** Conversion of binable types *)

  module Of_minimal (S : Binable.Minimal.S) : Binable.S with type t := S.t
  module Of_minimal1 (S : Binable.Minimal.S1) : Binable.S1 with type 'a t := 'a S.t

  module Make_binable_with_uuid (Bin_spec : Make_binable_with_uuid_spec) :
    Binable.S with type t := Bin_spec.t

  module Make_binable_without_uuid (Bin_spec : Make_binable_without_uuid_spec) :
    Binable.S with type t := Bin_spec.t
  [@@alert legacy "Use [Make_binable_with_uuid] if possible."]

  module Make_binable1_with_uuid (Bin_spec : Make_binable1_with_uuid_spec) :
    Binable.S1 with type 'a t := 'a Bin_spec.t

  module Make_binable1_without_uuid (Bin_spec : Make_binable1_without_uuid_spec) :
    Binable.S1 with type 'a t := 'a Bin_spec.t
  [@@alert legacy "Use [Make_binable1_with_uuid] if possible."]

  module Make_binable2_with_uuid (Bin_spec : Make_binable2_with_uuid_spec) :
    Binable.S2 with type ('a, 'b) t := ('a, 'b) Bin_spec.t

  module Make_binable2_without_uuid (Bin_spec : Make_binable2_without_uuid_spec) :
    Binable.S2 with type ('a, 'b) t := ('a, 'b) Bin_spec.t
  [@@alert legacy "Use [Make_binable2_with_uuid] if possible."]

  module Make_binable3_with_uuid (Bin_spec : Make_binable3_with_uuid_spec) :
    Binable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) Bin_spec.t

  module Make_binable3_without_uuid (Bin_spec : Make_binable3_without_uuid_spec) :
    Binable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) Bin_spec.t
  [@@alert legacy "Use [Make_binable3_with_uuid] if possible."]

  (** Conversion of iterable types *)

  module Make_iterable_binable (Iterable_spec : Make_iterable_binable_spec) :
    Binable.S with type t := Iterable_spec.t

  module Make_iterable_binable1 (Iterable_spec : Make_iterable_binable1_spec) :
    Binable.S1 with type 'a t := 'a Iterable_spec.t

  module Make_iterable_binable2 (Iterable_spec : Make_iterable_binable2_spec) :
    Binable.S2 with type ('a, 'b) t := ('a, 'b) Iterable_spec.t

  module Make_iterable_binable3 (Iterable_spec : Make_iterable_binable3_spec) :
    Binable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) Iterable_spec.t
end
