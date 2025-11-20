open! Base
open Common
open Type_class

module Definitions = struct
  [%%template
  [@@@mode.default m = (global, local)]

  module type Make_binable_without_uuid_spec = sig
    module Binable : Binable.Minimal.S [@mode m]

    type t : value_or_null

    val to_binable : t @ m -> Binable.t @ m [@@mode m = (global, m)]
    val of_binable : Binable.t -> t
  end

  module type Make_binable_with_uuid_spec = sig
    include Make_binable_without_uuid_spec [@mode m]

    (** [caller_identity] is necessary to ensure different callers of
        [Make_binable_with_uuid] are not shape compatible. *)
    val caller_identity : Shape.Uuid.t
  end

  [@@@kind.default ka = (value, any)]

  module type Make_binable1_without_uuid_spec = sig
    module Binable : Binable.Minimal.S1 [@kind ka] [@mode m]

    type ('a : ka) t : value_or_null

    val to_binable : ('a : ka). 'a t @ m -> 'a Binable.t @ m [@@mode m = (global, m)]
    val of_binable : ('a : ka). 'a Binable.t -> 'a t
  end

  module type Make_binable1_with_uuid_spec = sig
    include Make_binable1_without_uuid_spec [@kind ka] [@mode m]

    val caller_identity : Shape.Uuid.t
  end

  [@@@kind.default kb = (value, any)]

  module type Make_binable2_without_uuid_spec = sig
    module Binable : Binable.Minimal.S2 [@kind ka kb] [@mode m]

    type ('a : ka, 'b : kb) t : value_or_null

    val to_binable : ('a : ka) ('b : kb). ('a, 'b) t @ m -> ('a, 'b) Binable.t @ m
    [@@mode m = (global, m)]

    val of_binable : ('a : ka) ('b : kb). ('a, 'b) Binable.t -> ('a, 'b) t
  end

  module type Make_binable2_with_uuid_spec = sig
    include Make_binable2_without_uuid_spec [@kind ka kb] [@mode m]

    val caller_identity : Shape.Uuid.t
  end

  [@@@kind.default kc = (value, any)]

  module type Make_binable3_without_uuid_spec = sig
    module Binable : Binable.Minimal.S3 [@kind ka kb kc] [@mode m]

    type ('a : ka, 'b : kb, 'c : kc) t : value_or_null

    val to_binable
      : ('a : ka) ('b : kb) ('c : kc).
      ('a, 'b, 'c) t @ m -> ('a, 'b, 'c) Binable.t @ m
    [@@mode m = (global, m)]

    val of_binable
      : ('a : ka) ('b : kb) ('c : kc).
      ('a, 'b, 'c) Binable.t -> ('a, 'b, 'c) t
  end

  module type Make_binable3_with_uuid_spec = sig
    include Make_binable3_without_uuid_spec [@kind ka kb kc] [@mode m]

    val caller_identity : Shape.Uuid.t
  end

  module type Make_iterable_binable_spec = sig
    type t
    type el

    (** [caller_identity] is necessary to ensure different callers of
        [Make_iterable_binable] are not shape compatible. *)
    val caller_identity : Shape.Uuid.t

    val module_name : string option
    val init : len:int -> next:local_ (unit -> el) -> t

    include sig
      [@@@mode.default m = (global, m)]

      val length : t @ m -> int
      val iter : t @ m -> f:local_ (el @ m -> unit) -> unit
      val bin_size_el : (el Size.sizer[@mode m])
      val bin_write_el : (el Write.writer[@mode m])
    end

    val bin_read_el : el Read.reader
    val bin_shape_el : Shape.t
  end

  module type Make_iterable_binable1_spec = sig
    type 'a t
    type 'a el

    val caller_identity : Shape.Uuid.t
    val module_name : string option
    val init : len:int -> next:local_ (unit -> 'a el) -> 'a t

    include sig
      [@@@mode.default m = (global, m)]

      val length : 'a t @ m -> int
      val iter : 'a t @ m -> f:local_ ('a el @ m -> unit) -> unit
      val bin_size_el : (('a, 'a el) Size.sizer1[@mode m])
      val bin_write_el : (('a, 'a el) Write.writer1[@mode m])
    end

    val bin_read_el : ('a, 'a el) Read.reader1
    val bin_shape_el : Shape.t -> Shape.t
  end

  module type Make_iterable_binable2_spec = sig
    type ('a, 'b) t
    type ('a, 'b) el

    val caller_identity : Shape.Uuid.t
    val module_name : string option
    val init : len:int -> next:local_ (unit -> ('a, 'b) el) -> ('a, 'b) t

    include sig
      [@@@mode.default m = (global, m)]

      val length : ('a, 'b) t @ m -> int
      val iter : ('a, 'b) t @ m -> f:local_ (('a, 'b) el @ m -> unit) -> unit
      val bin_size_el : (('a, 'b, ('a, 'b) el) Size.sizer2[@mode m])
      val bin_write_el : (('a, 'b, ('a, 'b) el) Write.writer2[@mode m])
    end

    val bin_read_el : ('a, 'b, ('a, 'b) el) Read.reader2
    val bin_shape_el : Shape.t -> Shape.t -> Shape.t
  end

  module type Make_iterable_binable3_spec = sig
    type ('a, 'b, 'c) t
    type ('a, 'b, 'c) el

    val caller_identity : Shape.Uuid.t
    val module_name : string option
    val init : len:int -> next:local_ (unit -> ('a, 'b, 'c) el) -> ('a, 'b, 'c) t

    include sig
      [@@@mode.default m = (global, m)]

      val length : ('a, 'b, 'c) t @ m -> int
      val iter : ('a, 'b, 'c) t @ m -> f:local_ (('a, 'b, 'c) el @ m -> unit) -> unit
      val bin_size_el : (('a, 'b, 'c, ('a, 'b, 'c) el) Size.sizer3[@mode m])
      val bin_write_el : (('a, 'b, 'c, ('a, 'b, 'c) el) Write.writer3[@mode m])
    end

    val bin_read_el : ('a, 'b, 'c, ('a, 'b, 'c) el) Read.reader3
    val bin_shape_el : Shape.t -> Shape.t -> Shape.t -> Shape.t
  end]
end

(** Utility functions for user convenience *)
module type Utils = sig @@ portable
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

  [%%template:
  [@@@mode.default m = (global, local)]

  module%template.portable Of_minimal (S : Binable.Minimal.S [@mode m]) :
    Binable.S [@mode m] with type t := S.t

  module%template.portable Of_minimal1 (S : Binable.Minimal.S1 [@mode m]) :
    Binable.S1 [@mode m] with type 'a t := 'a S.t

  module%template.portable Make_binable_with_uuid
      (Bin_spec : Make_binable_with_uuid_spec
    [@mode m]) : Binable.S [@mode m] with type t := Bin_spec.t

  module%template.portable Make_binable_without_uuid
      (Bin_spec : Make_binable_without_uuid_spec
    [@mode m]) : Binable.S [@mode m] with type t := Bin_spec.t
  [@@alert legacy "Use [Make_binable_with_uuid] if possible."]

  [@@@kind.default ka = (value, any)]

  module%template.portable Make_binable1_with_uuid
      (Bin_spec : Make_binable1_with_uuid_spec
    [@kind ka] [@mode m]) :
    Binable.S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a Bin_spec.t

  module%template.portable Make_binable1_without_uuid
      (Bin_spec : Make_binable1_without_uuid_spec
    [@kind ka] [@mode m]) :
    Binable.S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a Bin_spec.t
  [@@alert legacy "Use [Make_binable1_with_uuid] if possible."]

  [@@@kind.default kb = (value, any)]

  module%template.portable Make_binable2_with_uuid
      (Bin_spec : Make_binable2_with_uuid_spec
    [@kind ka kb] [@mode m]) :
    Binable.S2
    [@kind ka kb] [@mode m]
    with type ('a : ka, 'b : kb) t := ('a, 'b) Bin_spec.t

  module%template.portable Make_binable2_without_uuid
      (Bin_spec : Make_binable2_without_uuid_spec
    [@kind ka kb] [@mode m]) :
    Binable.S2
    [@kind ka kb] [@mode m]
    with type ('a : ka, 'b : kb) t := ('a, 'b) Bin_spec.t
  [@@alert legacy "Use [Make_binable2_with_uuid] if possible."]

  [@@@kind.default kc = (value, any)]

  module%template.portable Make_binable3_with_uuid
      (Bin_spec : Make_binable3_with_uuid_spec
    [@kind ka kb kc] [@mode m]) :
    Binable.S3
    [@kind ka kb kc] [@mode m]
    with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) Bin_spec.t

  module%template.portable Make_binable3_without_uuid
      (Bin_spec : Make_binable3_without_uuid_spec
    [@kind ka kb kc] [@mode m]) :
    Binable.S3
    [@kind ka kb kc] [@mode m]
    with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) Bin_spec.t
  [@@alert legacy "Use [Make_binable3_with_uuid] if possible."]]

  (** Conversion of iterable types *)

  [%%template:
  [@@@mode.default m = (global, local)]

  module%template.portable Make_iterable_binable
      (Iterable_spec : Make_iterable_binable_spec
    [@mode m]) : Binable.S [@mode m] with type t := Iterable_spec.t

  module%template.portable Make_iterable_binable1
      (Iterable_spec : Make_iterable_binable1_spec
    [@mode m]) : Binable.S1 [@mode m] with type 'a t := 'a Iterable_spec.t

  module%template.portable Make_iterable_binable2
      (Iterable_spec : Make_iterable_binable2_spec
    [@mode m]) : Binable.S2 [@mode m] with type ('a, 'b) t := ('a, 'b) Iterable_spec.t

  module%template.portable Make_iterable_binable3
      (Iterable_spec : Make_iterable_binable3_spec
    [@mode m]) :
    Binable.S3 [@mode m] with type ('a, 'b, 'c) t := ('a, 'b, 'c) Iterable_spec.t]
end
