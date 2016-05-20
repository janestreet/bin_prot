(* Binable: signatures defining generated functions for the binary protocol *)

(* the subset of S containing only functions, so that one can recursively define modules
   implementing this interface *)
module type S_only_functions = sig
  type t

  val bin_size_t : t Size.sizer
  val bin_write_t : t Write.writer
  val bin_read_t : t Read.reader

  (**
     This function only needs implementation if [t] exposed to be a polymorphic variant.
     Despite what the type reads, this does *not* produce a function after reading;
     instead it takes the constructor tag (int) before reading and reads the rest of the
     variant [t] afterwards.
  *)
  val __bin_read_t__ : (int -> t) Read.reader
end

module type S = sig
  type t
  include S_only_functions with type t := t

  val bin_writer_t : t Type_class.writer
  val bin_reader_t : t Type_class.reader
  val bin_t : t Type_class.t
end

module type S1 = sig
  type 'a t

  val bin_size_t : ('a, 'a t) Size.sizer1
  val bin_write_t :('a, 'a t) Write.writer1
  val bin_read_t : ('a, 'a t) Read.reader1
  val __bin_read_t__ : ('a, int -> 'a t) Read.reader1
  val bin_writer_t : ('a, 'a t) Type_class.S1.writer
  val bin_reader_t : ('a, 'a t) Type_class.S1.reader
  val bin_t : ('a, 'a t) Type_class.S1.t
end

module type S2 = sig
  type ('a, 'b) t

  val bin_size_t : ('a, 'b, ('a, 'b) t) Size.sizer2
  val bin_write_t :('a, 'b, ('a, 'b) t) Write.writer2
  val bin_read_t : ('a, 'b, ('a, 'b) t) Read.reader2
  val __bin_read_t__ : ('a, 'b, int -> ('a, 'b) t) Read.reader2
  val bin_writer_t : ('a, 'b, ('a, 'b) t) Type_class.S2.writer
  val bin_reader_t : ('a, 'b, ('a, 'b) t) Type_class.S2.reader
  val bin_t : ('a, 'b, ('a, 'b) t) Type_class.S2.t
end
