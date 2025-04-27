@@ portable

open Bigarray

(** Local versions of some bigarray functions *)

module Array1 : sig
  open Array1

  external dim : (('a, 'b, 'c) t[@local_opt]) -> int = "%caml_ba_dim_1"
end

module Array2 : sig
  open Array2

  external dim1 : (('a, 'b, 'c) t[@local_opt]) -> int = "%caml_ba_dim_1"
  external dim2 : (('a, 'b, 'c) t[@local_opt]) -> int = "%caml_ba_dim_2"
end
