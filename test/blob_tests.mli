open! Base
open! Import

(* This is exposed for use in [bin_prot_test_core], in addition to the non-core-dependent
   tests in this module. *)
val run_stability_test : 'a Bin_prot.Type_class.t -> ('a -> 'a -> bool) -> 'a -> unit
