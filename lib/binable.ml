(* Binable: signatures defining generated functions for the binary protocol *)

module type S = sig
  type t

  val bin_size_t : t Size.sizer
  val bin_write_t : t Map_to_safe.writer
  val bin_write_t_ : t Unsafe_write_c.writer
  val bin_read_t : t Read_ml.reader
  val bin_read_t_ : t Unsafe_read_c.reader
  val bin_read_t__ : (int -> t) Unsafe_read_c.reader
  val bin_writer_t : t Type_class.writer
  val bin_reader_t : t Type_class.reader
  val bin_t : t Type_class.t
end

module type S1 = sig
  type 'a t

  val bin_size_t : ('a, 'a t) Size.sizer1
  val bin_write_t :('a, 'a t) Map_to_safe.writer1
  val bin_write_t_ :('a, 'a t) Unsafe_write_c.writer1
  val bin_read_t : ('a, 'a t) Map_to_safe.reader1
  val bin_read_t_ : ('a, 'a t) Unsafe_read_c.reader1
  val bin_read_t__ : ('a, int -> 'a t) Unsafe_read_c.reader1
  val bin_writer_t : ('a, 'a t) Type_class.S1.writer
  val bin_reader_t : ('a, 'a t) Type_class.S1.reader
  val bin_t : ('a, 'a t) Type_class.S1.t
end

module type S2 = sig
  type ('a, 'b) t

  val bin_size_t : ('a, 'b, ('a, 'b) t) Size.sizer2
  val bin_write_t :('a, 'b, ('a, 'b) t) Map_to_safe.writer2
  val bin_write_t_ :('a, 'b, ('a, 'b) t) Unsafe_write_c.writer2
  val bin_read_t : ('a, 'b, ('a, 'b) t) Map_to_safe.reader2
  val bin_read_t_ : ('a, 'b, ('a, 'b) t) Unsafe_read_c.reader2
  val bin_read_t__ : ('a, 'b, int -> ('a, 'b) t) Unsafe_read_c.reader2
  val bin_writer_t : ('a, 'b, ('a, 'b) t) Type_class.S2.writer
  val bin_reader_t : ('a, 'b, ('a, 'b) t) Type_class.S2.reader
  val bin_t : ('a, 'b, ('a, 'b) t) Type_class.S2.t
end
