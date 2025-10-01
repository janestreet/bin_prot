open! Base
open Bin_prot.Std
open Expect_test_helpers_base

module Expect_test_config = struct
  include Expect_test_config

  (* Renumber [gid]s as initialization can vary between, e.g., js and native builds. *)
  let sanitize string =
    let table = Hashtbl.create (module String) in
    let gid_re =
      Re.compile (Re.seq [ Re.str "(gid "; Re.group (Re.rep1 Re.digit); Re.str ")" ])
    in
    let intern_group group =
      Hashtbl.find_or_add table (Re.Group.get group 1) ~default:(fun () ->
        let gid = Int.to_string (Hashtbl.length table) in
        Printf.sprintf "(gid %s)" gid)
    in
    Re.replace gid_re ~f:intern_group string
  ;;
end

let test shapes =
  Dynamic.with_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
    print_and_check_sexpable ~hide_positions:true (module Bin_prot.Shape.Stable.V1) shapes)
;;

let%expect_test "built-ins" =
  test
    [ bin_shape_bigstring
    ; bin_shape_bool
    ; bin_shape_bytes
    ; bin_shape_char
    ; bin_shape_float
    ; bin_shape_float32_mat
    ; bin_shape_float32_vec
    ; bin_shape_float64_mat
    ; bin_shape_float64_vec
    ; bin_shape_floatarray
    ; bin_shape_int
    ; bin_shape_int32
    ; bin_shape_int64
    ; bin_shape_mat
    ; bin_shape_nativeint
    ; bin_shape_string
    ; bin_shape_unit
    ; bin_shape_vec
    ; bin_shape_array bin_shape_int
    ; bin_shape_iarray bin_shape_int
    ; bin_shape_lazy bin_shape_int
    ; bin_shape_lazy_t bin_shape_int
    ; bin_shape_list bin_shape_int
    ; bin_shape_option bin_shape_int
    ; bin_shape_ref bin_shape_int
    ];
  [%expect
    {|
    (Base bigstring ())
    (Base bool ())
    (Base bytes ())
    (Base char ())
    (Base float ())
    (Base float32_mat ())
    (Base float32_vec ())
    (Base float64_mat ())
    (Base float64_vec ())
    (Base floatarray ())
    (Base int ())
    (Base int32 ())
    (Base int64 ())
    (Base mat ())
    (Base nativeint ())
    (Base string ())
    (Base unit ())
    (Base vec ())
    (Base array ((Base int ())))
    (Base array ((Base int ())))
    (Base int ())
    (Base int ())
    (Base list ((Base int ())))
    (Base option ((Base int ())))
    (Base ref ((Base int ())))
    |}]
;;

module Murec_poly = struct
  type 'a tree =
    | Leaf
    | Node of 'a node

  and 'a node =
    { key : 'a
    ; children : 'a tree list
    }
  [@@deriving bin_io]
end

module Murec_mono = struct
  type tree = int Murec_poly.tree
  and node = int Murec_poly.node [@@deriving bin_io]
end

module Polyvar = struct
  type 'a t =
    [ `A of 'a
    | `B
    ]
  [@@deriving bin_io]
end

module Inherit = struct
  type t =
    [ int Polyvar.t
    | `C
    ]
  [@@deriving bin_io]
end

module Record = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving bin_io]
end

module Tuple = struct
  type t = int * string [@@deriving bin_io]
end

let%expect_test "user-defined types" =
  test
    [ Murec_mono.bin_shape_node
    ; Murec_mono.bin_shape_tree
    ; Inherit.bin_shape_t
    ; Record.bin_shape_t
    ; Tuple.bin_shape_t
    ];
  [%expect
    {|
    (Top_app
     ((gid 0)
      (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
      (members
       ((tree
         (()
          (Top_app
           ((gid 1)
            (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
            (members
             ((tree
               ((a)
                (Variant
                 ((Leaf ())
                  (Node
                   ((
                    Rec_app
                    node
                    ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))
              (node
               ((a)
                (Record
                 ((key (Var (lib/bin_prot/test/test_shape.ml:LINE:COL a)))
                  (children
                   (Base
                    list
                    ((
                     Rec_app
                     tree
                     ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))))))
           tree
           ((Base int ())))))
        (node
         (()
          (Top_app
           ((gid 1)
            (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
            (members
             ((tree
               ((a)
                (Variant
                 ((Leaf ())
                  (Node
                   ((
                    Rec_app
                    node
                    ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))
              (node
               ((a)
                (Record
                 ((key (Var (lib/bin_prot/test/test_shape.ml:LINE:COL a)))
                  (children
                   (Base
                    list
                    ((
                     Rec_app
                     tree
                     ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))))))
           node
           ((Base int ()))))))))
     node
     ())
    (Top_app
     ((gid 0)
      (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
      (members
       ((tree
         (()
          (Top_app
           ((gid 1)
            (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
            (members
             ((tree
               ((a)
                (Variant
                 ((Leaf ())
                  (Node
                   ((
                    Rec_app
                    node
                    ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))
              (node
               ((a)
                (Record
                 ((key (Var (lib/bin_prot/test/test_shape.ml:LINE:COL a)))
                  (children
                   (Base
                    list
                    ((
                     Rec_app
                     tree
                     ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))))))
           tree
           ((Base int ())))))
        (node
         (()
          (Top_app
           ((gid 1)
            (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
            (members
             ((tree
               ((a)
                (Variant
                 ((Leaf ())
                  (Node
                   ((
                    Rec_app
                    node
                    ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))
              (node
               ((a)
                (Record
                 ((key (Var (lib/bin_prot/test/test_shape.ml:LINE:COL a)))
                  (children
                   (Base
                    list
                    ((
                     Rec_app
                     tree
                     ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a))))))))))))))
           node
           ((Base int ()))))))))
     tree
     ())
    (Top_app
     ((gid 2)
      (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
      (members
       ((
        t
        (()
         (Poly_variant
          (lib/bin_prot/test/test_shape.ml:LINE:COL
           ((Inherit
             (lib/bin_prot/test/test_shape.ml:LINE:COL
              (Top_app
               ((gid 3)
                (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
                (members
                 ((
                  t
                  ((a)
                   (Poly_variant
                    (lib/bin_prot/test/test_shape.ml:LINE:COL
                     ((Constr
                       (A ((Var (lib/bin_prot/test/test_shape.ml:LINE:COL a)))))
                      (Constr (B ()))))))))))
               t
               ((Base int ())))))
            (Constr (C ()))))))))))
     t
     ())
    (Top_app
     ((gid 4)
      (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
      (members
       ((t (() (Record ((x (Base int ())) (y (Base int ())) (z (Base int ())))))))))
     t
     ())
    (Top_app
     ((gid 5)
      (loc lib/bin_prot/test/test_shape.ml:LINE:COL)
      (members ((t (() (Tuple ((Base int ()) (Base string ()))))))))
     t
     ())
    |}]
;;
