module Reader_local : sig
  type%template 'a t =
    { reader_local : ('a Bin_prot.Read.reader[@mode local])
    ; globalize : 'a -> 'a
    }
end

type%template 'a to_test =
  { writer : 'a Bin_prot.Write.writer
  ; writer_local : ('a Bin_prot.Write.writer[@mode local]) option
  ; reader : 'a Bin_prot.Read.reader
  ; reader_local : 'a Reader_local.t option
  ; values : 'a list
  ; equal : 'a -> 'a -> bool
  ; sexp_of : 'a -> Base.Sexp.t
  ; hi_bound : int option
  ; lo_bound : int
  }

val gen_tests : _ to_test -> unit

module Tests : sig
  val unit : unit to_test
  val bool : bool to_test
  val char : char to_test
  val digest : Md5_lib.t to_test
  val float : float to_test
  val float_nan : float to_test
  val vec : Bin_prot.Common.vec64 to_test
  val float32_vec : Bin_prot.Common.vec32 to_test
  val float64_vec : Bin_prot.Common.vec64 to_test
  val mat : Bin_prot.Common.mat64 to_test
  val float32_mat : Bin_prot.Common.mat32 to_test
  val float64_mat : Bin_prot.Common.mat64 to_test
  val bigstring : Bin_prot.Common.buf to_test
  val floatarray : floatarray to_test
  val ref : int32 ref to_test
  val lazy_t : int32 lazy_t to_test
  val option : int32 option to_test
  val pair : (int32 * int32) to_test
  val triple : (int32 * int32 * int32) to_test
  val list : int32 list to_test
  val array : int32 array to_test

  module R1 : sig
    type t
  end

  val record1 : R1.t to_test

  module R2 : sig
    type t
  end

  val record2 : R2.t to_test

  module Inline_record : sig
    type t
  end

  val inline_record : Inline_record.t to_test
end
