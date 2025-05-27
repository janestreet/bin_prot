module Stable = struct
  module V1 = struct
    include Md5_lib

    let bin_shape_t =
      Shape.basetype (Shape.Uuid.of_string "f6bdcdd0-9f75-11e6-9a7e-d3020428efed") []
    ;;

    let bin_size_t = Size.bin_size_md5
    let bin_write_t = Write.bin_write_md5
    let bin_read_t = Read.bin_read_md5

    let __bin_read_t__ _buf ~pos_ref _vdigest =
      Common.raise_variant_wrong_type "Shape.Md5.t" !pos_ref
    ;;

    let stable_witness : t Stable_witness.t =
      (* we assume this type to have a stable format *)
      Stable_witness.assert_stable
    ;;
  end
end

include Stable.V1

let%template[@mode local] to_binary = to_binary_local
