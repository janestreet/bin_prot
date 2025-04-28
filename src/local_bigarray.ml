open Stdlib
open Bigarray

module Array1 = struct
  open Array1

  external dim : (('a, 'b, 'c) t[@local_opt]) -> int = "%caml_ba_dim_1"
end

module Array2 = struct
  open Array2

  external dim1 : (('a, 'b, 'c) t[@local_opt]) -> int = "%caml_ba_dim_1"
  external dim2 : (('a, 'b, 'c) t[@local_opt]) -> int = "%caml_ba_dim_2"
end
