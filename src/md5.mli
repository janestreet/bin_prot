@@ portable

type t = Md5_lib.t [@@deriving compare ~localize]

include Binable.Minimal.S with type t := t

val to_hex : t -> string
val of_hex_exn : string -> t

val%template to_binary : t @ m -> string @ m [@@mode m = (local, global)]

val of_binary_exn : string -> t
val unsafe_of_binary : string -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving compare ~localize]

    val stable_witness : t Stable_witness.t

    include Binable.Minimal.S with type t := t
  end
end
