open Bin_prot.Std
module type S = sig end

include (struct
  type t = int
  with bin_io
end : S)

include (struct
  type t = int32
  with bin_io
end : S)

include (struct
  type t = int64
  with bin_io
end : S)

include (struct
  type t = nativeint
  with bin_io
end : S)

include (struct
  type t = float
  with bin_io
end : S)

include (struct
  type t = char
  with bin_io
end : S)

include (struct
  type t = int list
  with bin_io
end : S)

include (struct
  type t = float array
  with bin_io
end : S)

include (struct
  type t = int64 array
  with bin_io
end : S)

include (struct
  type t = int * float * char
  with bin_io
end : S)

include (struct
  type t = A | B
  with bin_io

  type u = C | D | E of t
  with bin_io
end : S)

include (struct
  type t = [ `A | `B ]
  with bin_io

  type u = [ `C | `D | `E of t ]
  with bin_io
end : S)

include (struct
  type a = [ `A1 | `A2 ]
  with bin_io

  type b = [ `B1 | `B2 ]
  with bin_io

  type t = [ a | b ]
  with bin_io
end : S)

include (struct
  type t = {
    foo : char;
    bar : int;
    baz : string;
  } with bin_io
end : S)

include (struct
  type 'a t = 'a
  with bin_io
end : S)

include (struct
  type 'a t = 'a * int
  with bin_io
end : S)

include (struct
  type ('a, 'b) t = 'a * 'b
  with bin_io
end : S)

include (struct
  type 'a t = 'a constraint 'a = [< `A | `B ]
  with bin_io

  type 'a u = [`A] t
  with bin_io
end : S)

include (struct
  type 'a t = {
    foo : 'a;
    bar : int;
  } with bin_io
end : S)
