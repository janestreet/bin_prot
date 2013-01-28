open Bin_prot.Std

type t = float with bin_io

module M : sig
  type t = float with bin_io
end = struct
  type nonrec t = t with bin_io
end

module M1 : sig
  type t = float list with bin_io
end = struct
  type nonrec t = t list with bin_io
end

module M2 : sig
  type nonrec t = t list with bin_io
end = struct
  type nonrec t = t list with bin_io
end

module M3 : sig
  type nonrec t = [ `A of t ] with bin_io
end = struct
  type nonrec t = [ `A of t ] with bin_io
end
