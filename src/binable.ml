(* Binable: signatures defining generated functions for the binary protocol. [S, S1, etc]
   are the signatures satisfied by the generated code and [Minimal.S, Minimal.S1, etc] are
   the signatures that generated code uses. *)

[%%template
[@@@mode.default m = (global, local)]

(* the subset of S containing only functions, so that one can recursively define modules
   implementing this interface *)
module type S_only_functions = sig
  type t

  include sig
    [@@@mode.default m = (global, m)]

    val bin_size_t : (t Size.sizer[@mode m])
    val bin_write_t : (t Write.writer[@mode m])
  end

  val bin_read_t : t Read.reader

  (** This function only needs implementation if [t] exposed to be a polymorphic variant.
      Despite what the type reads, this does *not* produce a function after reading;
      instead it takes the constructor tag (int) before reading and reads the rest of the
      variant [t] afterwards. *)
  val __bin_read_t__ : t Read.vtag_reader
end

module type S = sig
  type t

  include S_only_functions [@mode m] with type t := t

  val bin_shape_t : Shape.t
  val bin_writer_t : t Type_class.writer
  val bin_reader_t : t Type_class.reader
  val bin_t : t Type_class.t
end

[@@@kind.default ka = (value, any)]

module type S1 = sig
  type 'a t

  val bin_shape_t : Shape.t -> Shape.t

  include sig
    [@@@mode.default m = (global, m)]

    val bin_size_t : 'a. (('a, 'a t) Size.sizer1[@mode m])
    val bin_write_t : 'a. (('a, 'a t) Write.writer1[@mode m])
  end

  val bin_read_t : 'a. ('a, 'a t) Read.reader1
  val __bin_read_t__ : 'a. ('a, 'a t) Read.vtag_reader1
  val bin_writer_t : 'a. ('a, 'a t) Type_class.S1.writer
  val bin_reader_t : 'a. ('a, 'a t) Type_class.S1.reader
  val bin_t : 'a. ('a, 'a t) Type_class.S1.t
end

[@@@kind.default kb = (value, any)]

module type S2 = sig
  type ('a, 'b) t

  val bin_shape_t : Shape.t -> Shape.t -> Shape.t

  include sig
    [@@@mode.default m = (global, m)]

    val bin_size_t : 'a 'b. (('a, 'b, ('a, 'b) t) Size.sizer2[@mode m])
    val bin_write_t : 'a 'b. (('a, 'b, ('a, 'b) t) Write.writer2[@mode m])
  end

  val bin_read_t : 'a 'b. ('a, 'b, ('a, 'b) t) Read.reader2
  val __bin_read_t__ : 'a 'b. ('a, 'b, ('a, 'b) t) Read.vtag_reader2
  val bin_writer_t : 'a 'b. ('a, 'b, ('a, 'b) t) Type_class.S2.writer
  val bin_reader_t : 'a 'b. ('a, 'b, ('a, 'b) t) Type_class.S2.reader
  val bin_t : 'a 'b. ('a, 'b, ('a, 'b) t) Type_class.S2.t
end

[@@@kind.default kc = (value, any)]

module type S3 = sig
  type ('a, 'b, 'c) t

  val bin_shape_t : Shape.t -> Shape.t -> Shape.t -> Shape.t

  include sig
    [@@@mode.default m = (global, m)]

    val bin_size_t : 'a 'b 'c. (('a, 'b, 'c, ('a, 'b, 'c) t) Size.sizer3[@mode m])
    val bin_write_t : 'a 'b 'c. (('a, 'b, 'c, ('a, 'b, 'c) t) Write.writer3[@mode m])
  end

  val bin_read_t : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Read.reader3
  val __bin_read_t__ : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Read.vtag_reader3
  val bin_writer_t : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Type_class.S3.writer
  val bin_reader_t : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Type_class.S3.reader
  val bin_t : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Type_class.S3.t
end]

module Minimal = struct
  [%%template
  [@@@mode.default m = (global, local)]

  module type S = sig
    type t

    val bin_shape_t : Shape.t

    include sig
      [@@@mode.default m = (global, m)]

      val bin_size_t : (t Size.sizer[@mode m])
      val bin_write_t : (t Write.writer[@mode m])
    end

    val bin_read_t : t Read.reader
    val __bin_read_t__ : t Read.vtag_reader
  end

  [@@@kind.default ka = (value, any)]

  module type S1 = sig
    type 'a t

    val bin_shape_t : Shape.t -> Shape.t

    include sig
      [@@@mode.default m = (global, m)]

      val bin_size_t : 'a. (('a, 'a t) Size.sizer1[@mode m])
      val bin_write_t : 'a. (('a, 'a t) Write.writer1[@mode m])
    end

    val bin_read_t : 'a. ('a, 'a t) Read.reader1
    val __bin_read_t__ : 'a. ('a, 'a t) Read.vtag_reader1
  end

  [@@@kind.default kb = (value, any)]

  module type S2 = sig
    type ('a, 'b) t

    val bin_shape_t : Shape.t -> Shape.t -> Shape.t

    include sig
      [@@@mode.default m = (global, m)]

      val bin_size_t : 'a 'b. (('a, 'b, ('a, 'b) t) Size.sizer2[@mode m])
      val bin_write_t : 'a 'b. (('a, 'b, ('a, 'b) t) Write.writer2[@mode m])
    end

    val bin_read_t : 'a 'b. ('a, 'b, ('a, 'b) t) Read.reader2
    val __bin_read_t__ : 'a 'b. ('a, 'b, ('a, 'b) t) Read.vtag_reader2
  end

  [@@@kind.default kc = (value, any)]

  module type S3 = sig
    type ('a, 'b, 'c) t

    val bin_shape_t : Shape.t -> Shape.t -> Shape.t -> Shape.t

    include sig
      [@@@mode.default m = (global, m)]

      val bin_size_t : 'a 'b 'c. (('a, 'b, 'c, ('a, 'b, 'c) t) Size.sizer3[@mode m])
      val bin_write_t : 'a 'b 'c. (('a, 'b, 'c, ('a, 'b, 'c) t) Write.writer3[@mode m])
    end

    val bin_read_t : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Read.reader3
    val __bin_read_t__ : 'a 'b 'c. ('a, 'b, 'c, ('a, 'b, 'c) t) Read.vtag_reader3
  end]
end
