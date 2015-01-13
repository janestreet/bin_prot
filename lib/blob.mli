open Common

(** ['a Blob.t] is type-equivalent to ['a], but has different bin-prot serializers that
    prefix the representation with the size of ['a].

    To understand where this is useful, imagine we have an event type where many
    applications look at some parts of an event, but not all applications need to deal
    with all parts of an event. We might define:

    {[
      type 'a event =
        { time : Time.t
        ; source : string
        ; details : 'a
        } with bin_io
    ]}

    Applications that need to understand all the details of an event could use:

      {[type concrete_event = Details.t Blob.t event with bin_io]}

    An application that filters events to downsteam consumers based on just [source] or
    [time] (but doesn't need to parse [details]) may use:

      {[type opaque_event = Blob.Opaque.t event with bin_io]}

    This has two advantages:
      - (de)serializing messages is faster because potentially costly (de)serialization of
        [details] is avoided
      - the application can be compiled without any knowledge of any conrete [Details.t]
        type, so it's robust to changes in [Details.t]

    An application that's happy to throw away [details] may use:

      {[type ignored_event = Blob.Ignored.t event with bin_read]}

   Whereas [opaque_event]s roundtrip, [ignored_event]s actually drop the bytes
   representing [details] when deserializing, and therefore do not roundtrip.
*)
include Binable.S1 with type 'a t = 'a

(** An [Opaque.t] is an arbitrary piece of bin-prot. The bin-prot (de-)serializers simply
    read/write the data, prefixed with its size.

    When reading bin-prot data, sometimes you won't care about deserializing a particular
    piece: perhaps you want to operate on a bin-prot stream, transforming some bits of
    the stream and passing the others through untouched. In these cases you can
    deserialize using the bin-prot converters for a type involving [Opaque.t]. This is
    anologuous to reading a sexp file / operating on a sexp stream and using
    (de-)serialization functions for a type involving [Sexp.t].
*)
module Opaque : Binable.S

(** An [Ignored.t] is an unusable value with special bin-prot converters. The reader reads
    the size and drops that much data from the buffer. Writing is not supported, however
    the size of [t] is kept, so [bin_size_t] is available.

    This can be used in similar situations to [Opaque.t]. If instead of transforming a
    bin-prot stream, you are simply consuming it (and not passing it on anywhere), there
    is no need to remember the bin-prot representation for the bits you're ignoring. E.g.
    if you wish to extract a subset of information from a bin-prot file, which contains
    the serialized representation of some type T (or a bunch of Ts in a row, or something
    similar), you can define a type which is similar to T but has various components
    replaced with [Ignored.t].
*)
module Ignored : sig
  type t

  val bin_size_t : t Size.sizer
  val bin_read_t : t Read.reader
  val __bin_read_t__ : (int -> t) Read.reader
  val bin_reader_t : t Type_class.reader
end

val to_opaque : 'a t -> 'a Type_class.writer -> Opaque.t

val of_opaque_exn : Opaque.t -> 'a Type_class.reader -> 'a
