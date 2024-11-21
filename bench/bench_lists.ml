open Bin_prot

module%bench [@name "bin_read_list"] _ = struct
  let buf =
    let list : int list = ListLabels.init ~len:64 ~f:(fun i -> i) in
    let buf = Common.create_buf (Size.bin_size_list Size.bin_size_int list) in
    let _ : int = Write.bin_write_list Write.bin_write_int buf ~pos:0 list in
    buf
  ;;

  let pos_ref = ref 0

  let%bench "via [@tail_mod_cons]" =
    pos_ref := 0;
    let _ : int list =
      Sys.opaque_identity (Read.bin_read_list Read.bin_read_int buf ~pos_ref)
    in
    ()
  ;;
end
