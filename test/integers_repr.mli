type%template 'a to_test =
  { name : string
  ; writer : 'a Bin_prot.Write.writer
  ; writer_local : ('a Bin_prot.Write.writer[@mode local])
  ; reader : 'a Bin_prot.Read.reader
  ; reader_local : ('a Bin_prot.Read.reader[@mode local])
  ; globalize : local_ 'a -> 'a
  ; to_int64 : 'a -> Int64.t
  ; of_int64 : Int64.t -> 'a
  ; min : 'a
  ; max : 'a (* Bounds on the bin_protted size *)
  ; hi_bound : int
  ; lo_bound : int
  }

type to_test_packed = T : _ to_test -> to_test_packed

val interesting_points : 'a to_test -> Base.Set.M(Base.Int64).t
val tests : to_test_packed list
val run_tests : unit -> unit
