(** Pa_bin_prot: Preprocessing Module for a Type Safe Binary Protocol *)

open Printf
open Lexing

open Camlp4
open PreCast
open Ast
open Pa_type_conv

(* Utility functions *)

let ( *** ) f g x = f (g x)

let get_n_vars _loc name n =
  let rec loop patts exprs n =
    if n <= 0 then patts, exprs
    else
      let var = name ^ string_of_int n in
      let patts = <:patt< $lid:var$ >> :: patts in
      let exprs = <:expr< $lid:var$ >> :: exprs in
      loop patts exprs (n - 1)
  in
  loop [] [] n

let rec sig_of_tds cnv = function
  | TyDcl (_loc, type_name, tps, _rhs, _cl) -> cnv _loc type_name tps
  | TyAnd (_loc, tp1, tp2) ->
      <:sig_item< $sig_of_tds cnv tp1$; $sig_of_tds cnv tp2$ >>
  | _ -> assert false  (* impossible *)


(* Generators for the binary protocol *)

(* Generates the signature for binary protocol writers *)
module Sig_generate_writer = struct
  let sig_of_td _loc type_name tps =
    let rec loop this_type = function
      | [] ->
          <:ctyp< Bin_prot.Write_ml.writer $this_type$ >>,
          <:ctyp< Bin_prot.Unsafe_write_c.writer $this_type$ >>,
          <:ctyp< Bin_prot.Size.sizer $this_type$ >>,
          <:ctyp< Bin_prot.Type_class.writer $this_type$ >>
      | tp :: tps ->
          let tp = Gen.drop_variance_annotations tp in
          let bin_write, bin_write_low, bin_size, bin_writer =
            loop <:ctyp< $this_type$ $tp$ >> tps
          in
          <:ctyp< Bin_prot.Unsafe_write_c.writer $tp$ -> $bin_write$ >>,
          <:ctyp< Bin_prot.Unsafe_write_c.writer $tp$ -> $bin_write_low$ >>,
          <:ctyp< Bin_prot.Size.sizer $tp$ -> $bin_size$ >>,
          <:ctyp< Bin_prot.Type_class.writer $tp$ -> $bin_writer$ >>
    in
    let bin_write, bin_write_low, bin_size, bin_writer =
      loop <:ctyp< $lid:type_name$ >> tps
    in
    <:sig_item<
      value $lid:"bin_size_" ^ type_name$ : $bin_size$;
      value $lid:"bin_write_" ^ type_name$ : $bin_write$;
      value $lid:"bin_write_" ^ type_name ^ "_"$ : $bin_write_low$;
      value $lid:"bin_writer_" ^ type_name$ : $bin_writer$
    >>

  let mk_sig tds = <:sig_item< $sig_of_tds sig_of_td tds$ >>

  let () = add_sig_generator "bin_write" mk_sig
end


(* Generates the signature for binary protocol readers *)
module Sig_generate_reader = struct
  let sig_of_td _loc type_name tps =
    let rec loop this_tp = function
      | [] ->
          <:ctyp< Bin_prot.Read_ml.reader $this_tp$ >>,
          <:ctyp< Bin_prot.Unsafe_read_c.reader $this_tp$ >>,
          <:ctyp< Bin_prot.Unsafe_read_c.reader (int -> $this_tp$) >>,
          <:ctyp< Bin_prot.Type_class.reader $this_tp$ >>
      | tp :: tps ->
          let tp = Gen.drop_variance_annotations tp in
          let bin_read, bin_read_, bin_read__, bin_reader =
            loop <:ctyp< $this_tp$ $tp$ >> tps
          in
          <:ctyp< Bin_prot.Unsafe_read_c.reader $tp$ -> $bin_read$ >>,
          <:ctyp< Bin_prot.Unsafe_read_c.reader $tp$ -> $bin_read_$ >>,
          <:ctyp< Bin_prot.Unsafe_read_c.reader $tp$ -> $bin_read__$ >>,
          <:ctyp< Bin_prot.Type_class.reader $tp$ -> $bin_reader$ >>
    in
    let bin_read, bin_read_, bin_read__, bin_reader =
      loop <:ctyp< $lid:type_name$ >> tps
    in
    <:sig_item<
      value $lid:"bin_read_" ^ type_name$: $bin_read$;
      value $lid:"bin_read_" ^ type_name ^ "_"$ : $bin_read_$;
      value $lid:"bin_read_" ^ type_name ^ "__"$ : $bin_read__$;
      value $lid:"bin_reader_" ^ type_name$ : $bin_reader$
    >>

  let mk_sig tds = <:sig_item< $sig_of_tds sig_of_td tds$ >>

  let () = add_sig_generator "bin_read" mk_sig
end


(* Generates the signature for binary protocol type classes *)
module Sig_generate_tp_class = struct
  let sig_of_td _loc type_name tps =
    let rec loop this_tp = function
      | [] -> <:ctyp< Bin_prot.Type_class.t $this_tp$ >>
      | tp :: tps ->
          let tp = Gen.drop_variance_annotations tp in
          let bin_tp_class = loop <:ctyp< $this_tp$ $tp$ >> tps in
          <:ctyp< Bin_prot.Type_class.t $tp$ -> $bin_tp_class$ >>
    in
    let bin_tp_class = loop <:ctyp< $lid:type_name$ >> tps in
    <:sig_item< value $lid:"bin_" ^ type_name$ : $bin_tp_class$ >>

  let mk_sig tds = <:sig_item< $sig_of_tds sig_of_td tds$ >>
  let () = add_sig_generator "bin_type_class" mk_sig
end


(* Generates the signature for binary protocol *)
module Sig_generate = struct
  let () =
    add_sig_generator "bin_io" (fun tds ->
      let _loc = Loc.ghost in
      <:sig_item<
        $Sig_generate_writer.mk_sig tds$;
        $Sig_generate_reader.mk_sig tds$;
        $Sig_generate_tp_class.mk_sig tds$;
      >>)
end


(* Generator for size computation of OCaml-values for the binary protocol *)
module Generate_bin_size = struct
  let mk_abst_call _loc tn rev_path =
    <:expr< $id:Gen.ident_of_rev_path _loc (("bin_size_" ^ tn) :: rev_path)$ >>

  (* Conversion of type paths *)
  let bin_size_path_fun _loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call _loc tn rev_path
    | [] -> assert false  (* impossible *)

  (* Conversion of types *)
  let rec bin_size_type full_type_name _loc = function
    | <:ctyp< $tp1$ $tp2$ >> ->
        `Fun (bin_size_appl_fun full_type_name _loc tp1 tp2)
    | <:ctyp< ( $tup:tp$ ) >> -> bin_size_tuple full_type_name _loc tp
    | <:ctyp< '$parm$ >> -> `Fun <:expr< $lid:"_size_of_" ^ parm$ >>
    | <:ctyp< $id:id$ >> -> `Fun (bin_size_path_fun _loc id)
    | <:ctyp< $_$ -> $_$ >> ->
        failwith
          "bin_size_type: cannot convert functions to the binary protocol"
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        bin_size_variant full_type_name _loc row_fields
    | <:ctyp< ! $parms$ . $tp$ >> -> bin_size_poly full_type_name _loc parms tp
    | _ ->
        prerr_endline (
          get_loc_err _loc "bin_size_type: unknown type construct");
        exit 1

  (* Conversion of polymorphic types *)
  and bin_size_appl_fun full_type_name _loc tp1 tp2 =
    match
      bin_size_type full_type_name _loc tp1,
      bin_size_type full_type_name _loc tp2
    with
    | `Fun <:expr< Bin_prot.Size.bin_size_array >>,
      `Fun <:expr< Bin_prot.Size.bin_size_float >> ->
        <:expr< Bin_prot.Size.bin_size_float_array >>
    | `Fun fun_expr1, `Fun fun_expr2 -> <:expr< $fun_expr1$ $fun_expr2$ >>
    | `Fun fun_expr, `Match matching ->
        <:expr< $fun_expr$ (fun [ $matching$ ]) >>
    | _ -> assert false  (* impossible *)


  (* Conversion of tuples and records *)

  and bin_size_args full_type_name _loc get_tp mk_patt tp =
    let rec loop i = function
      | el :: rest ->
          let tp = get_tp el in
          let v_name = "v" ^ string_of_int i in
          let v_expr =
            match bin_size_type full_type_name _loc tp with
            | `Fun fun_expr ->
                <:expr< Pervasives.(+) size ($fun_expr$ $lid:v_name$) >>
            | `Match matchings ->
                <:expr<
                  Pervasives.(+) size (match $lid:v_name$ with [ $matchings$ ])
                >>
          in
          let patt = mk_patt _loc v_name el in
          if rest = [] then [patt], v_expr
          else
            let patts, in_expr = loop (i + 1) rest in
            patt :: patts, <:expr< let size = $v_expr$ in $in_expr$ >>
      | [] -> assert false  (* impossible *)
    in
    loop 1 (list_of_ctyp tp [])

  and bin_size_tup_rec full_type_name _loc cnv_patts get_tp mk_patt tp =
    let patts, expr = bin_size_args full_type_name _loc get_tp mk_patt tp in
    `Match <:match_case< $cnv_patts patts$ -> let size = 0 in $expr$ >>

  (* Conversion of tuples *)
  and bin_size_tuple full_type_name _loc tp =
    let cnv_patts patts = <:patt< ( $tup:paCom_of_list patts$ ) >> in
    let get_tp tp = tp in
    let mk_patt _loc v_name _ = <:patt< $lid:v_name$ >> in
    bin_size_tup_rec full_type_name _loc cnv_patts get_tp mk_patt tp

  (* Conversion of records *)
  and bin_size_record full_type_name _loc tp =
    let cnv_patts patts = <:patt< { $paSem_of_list patts$ } >> in
    let get_tp = function
      | <:ctyp< $_$ : mutable $tp$ >>
      | <:ctyp< $_$ : $tp$ >> -> tp
      | _ -> assert false  (* impossible *)
    in
    let mk_patt _loc v_name = function
      | <:ctyp< $lid:r_name$ : $_$ >> -> <:patt< $lid:r_name$ = $lid:v_name$ >>
      | _ -> assert false  (* impossible *)
    in
    bin_size_tup_rec full_type_name _loc cnv_patts get_tp mk_patt tp

  (* Conversion of variant types *)
  and bin_size_variant full_type_name _loc row_fields =
    let has_atoms = ref false in
    let rec loop = function
      | <:ctyp< $tp1$ | $tp2$ >> -> <:match_case< $loop tp1$ | $loop tp2$ >>
      | <:ctyp< `$_$ >> -> has_atoms := true; <:match_case< >>
      | <:ctyp< `$cnstr$ of $tp$ >> ->
          let size_args =
            match bin_size_type full_type_name _loc tp with
            | `Fun fun_expr -> <:expr< $fun_expr$ args >>
            | `Match matchings -> <:expr< match args with [ $matchings$ ] >>
          in
          <:match_case<
            `$cnstr$ args ->
              let size_args = $size_args$ in
              Pervasives.(+) size_args 4
          >>
      | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
      | <:ctyp< [= $row_fields$ ] >> -> loop row_fields
      | <:ctyp< $tp1$ $tp2$ >> ->
          let id_path = Gen.get_appl_path _loc tp1 in
          let call = bin_size_appl_fun full_type_name _loc tp1 tp2 in
          <:match_case< #$id_path$ as v -> $call$ v >>
      | <:ctyp< $id:id$ >> | <:ctyp< #$id:id$ >> ->
          let call =
            match Gen.get_rev_id_path id [] with
            | tn :: path -> mk_abst_call _loc tn path
            | [] -> assert false  (* impossible *)
          in
          <:match_case< #$id$ as v -> $call$ v >>
      | _ -> failwith "bin_size_variant: unknown type"
    in
    let nonatom_matchings = loop row_fields in
    let matchings =
      if !has_atoms then <:match_case< $nonatom_matchings$ | _ -> 4 >>
      else nonatom_matchings
    in
    `Match matchings

  (* Polymorphic record fields *)
  and bin_size_poly full_type_name _loc parms tp =
    let bindings =
      let mk_binding parm =
        <:binding<
          $lid:"_size_of_" ^ parm$ = fun _v ->
            raise (Bin_prot.Common.Poly_rec_write $str:full_type_name$)
        >>
      in
      List.map mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match bin_size_type full_type_name _loc tp with
    | `Fun fun_expr -> `Fun <:expr< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Conversion of sum types *)

  let rec count_alts = function
    | <:ctyp< $tp1$ | $tp2$ >> -> count_alts tp1 + count_alts tp2
    | _ -> 1

  let bin_size_sum full_type_name _loc alts =
    let n_alts = count_alts alts in
    let size_tag =
      if n_alts <= 256 then <:expr< 1 >>
      else if n_alts <= 65536 then <:expr< 2 >>
      else (
        prerr_endline (
          get_loc_err _loc "bin_size_sum: too many alternatives (> 65536)");
        exit 1)
    in
    let has_atoms = ref false in
    let rec loop = function
      | <:ctyp< $tp1$ | $tp2$ >> -> <:match_case< $loop tp1$ | $loop tp2$ >>
      | <:ctyp< $uid:_$ >> -> has_atoms := true; <:match_case< >>
      | <:ctyp< $uid:cnstr$ of $tp$ >> ->
          let get_tp tp = tp in
          let mk_patt _loc v_name _ = <:patt< $lid:v_name$ >> in
          let patts, size_args =
            bin_size_args full_type_name _loc get_tp mk_patt tp
          in
          let args =
            match patts with
            | [patt] -> patt
            | _ -> <:patt< $tup:paCom_of_list patts$ >>
          in
          <:match_case< $uid:cnstr$ $args$ ->
            let size = $size_tag$ in
            $size_args$
          >>
      | _ -> failwith "branch_sum: unknown type"
    in
    let nonatom_matchings = loop alts in
    let matchings =
      if !has_atoms then <:match_case< $nonatom_matchings$ | _ -> $size_tag$ >>
      else nonatom_matchings
    in
    `Match matchings


  (* Empty types *)
  let bin_size_nil full_type_name _loc =
    `Fun <:expr< fun _v ->
      raise (Bin_prot.Common.Empty_type $str:full_type_name$) >>


  (* Generate code from type definitions *)
  let bin_size_td _loc type_name tps rhs =
    let full_type_name = sprintf "%s.%s" (get_conv_path ()) type_name in
    let is_nil = ref false in
    let body =
      let rec loop _loc =
        Gen.switch_tp_def
          ~alias:(bin_size_type full_type_name)
          ~sum:(bin_size_sum full_type_name)
          ~record:(bin_size_record full_type_name)
          ~variants:(bin_size_variant full_type_name)
          ~mani:(fun _loc _tp1 -> loop _loc)
          ~nil:(fun _loc -> is_nil := true; bin_size_nil full_type_name _loc)
      in
      match loop _loc rhs with
      | `Fun fun_expr when !is_nil -> fun_expr
      | `Fun fun_expr -> <:expr< fun v -> $fun_expr$ v >>
      | `Match matchings -> <:expr< fun [ $matchings$ ] >>
    in
    let tparam_cnvs = List.map ((^) "_size_of_" *** Gen.get_tparam_id) tps in
    let mk_pat id = <:patt< $lid:id$ >> in
    let tparam_patts = List.map mk_pat tparam_cnvs in
    <:binding<
      $lid:"bin_size_" ^ type_name$ = $Gen.abstract _loc tparam_patts body$
    >>

  let rec bin_size_tds acc = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        bin_size_td _loc type_name tps rhs :: acc
    | TyAnd (_loc, tp1, tp2) -> bin_size_tds (bin_size_tds acc tp2) tp1
    | _ -> assert false  (* impossible *)

  let bin_size tds =
    let bindings, recursive, _loc =
      match tds with
      | TyDcl (_loc, type_name, tps, rhs, _cl) ->
          let binding = bin_size_td _loc type_name tps rhs in
          [binding], Gen.type_is_recursive type_name rhs, _loc
      | TyAnd (_loc, _, _) -> bin_size_tds [] tds, true, _loc
      | _ -> assert false  (* impossible *)
    in
    if recursive then <:str_item< value rec $list:bindings$ >>
    else <:str_item< value $list:bindings$ >>
end


(* Generator for converters of OCaml-values to the binary protocol *)
module Generate_bin_write = struct
  let mk_abst_call _loc tn rev_path =
    <:expr<
      $id:Gen.ident_of_rev_path _loc (("bin_write_" ^ tn ^ "_") :: rev_path)$
    >>

  (* Conversion of type paths *)
  let bin_write_path_fun _loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call _loc tn rev_path
    | [] -> assert false  (* impossible *)

  (* Conversion of types *)
  let rec bin_write_type full_type_name _loc = function
    | <:ctyp< $tp1$ $tp2$ >> ->
        `Fun (bin_write_appl_fun full_type_name _loc tp1 tp2)
    | <:ctyp< ( $tup:tp$ ) >> -> bin_write_tuple full_type_name _loc tp
    | <:ctyp< '$parm$ >> -> `Fun <:expr< $lid:"_write_" ^ parm$ >>
    | <:ctyp< $id:id$ >> -> `Fun (bin_write_path_fun _loc id)
    | <:ctyp< $_$ -> $_$ >> ->
        failwith
          "bin_write_type: cannot convert functions to the binary protocol"
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        bin_write_variant full_type_name _loc row_fields
    | <:ctyp< ! $parms$ . $tp$ >> -> bin_write_poly full_type_name _loc parms tp
    | _ ->
        prerr_endline (
          get_loc_err _loc "bin_write_type: unknown type construct");
        exit 1

  (* Conversion of polymorphic types *)
  and bin_write_appl_fun full_type_name _loc tp1 tp2 =
    match
      bin_write_type full_type_name _loc tp1,
      bin_write_type full_type_name _loc tp2
    with
    | `Fun <:expr< Bin_prot.Unsafe_write_c.bin_write_array >>,
      `Fun <:expr< Bin_prot.Unsafe_write_c.bin_write_float >>
        ->
        <:expr< Bin_prot.Unsafe_write_c.bin_write_float_array >>
    | `Fun fun_expr1, `Fun fun_expr2 -> <:expr< $fun_expr1$ $fun_expr2$ >>
    | `Fun fun_expr, `Match matching ->
        <:expr< $fun_expr$ (fun sptr eptr -> fun [ $matching$ ]) >>
    | _ -> assert false  (* impossible *)


  (* Conversion of tuples and records *)

  and bin_write_args full_type_name _loc get_tp mk_patt tp =
    let rec loop i = function
      | el :: rest ->
          let tp = get_tp el in
          let v_name = "v" ^ string_of_int i in
          let v_expr =
            match bin_write_type full_type_name _loc tp with
            | `Fun fun_expr -> <:expr< $fun_expr$ sptr eptr $lid:v_name$ >>
            | `Match matchings ->
                <:expr< match $lid:v_name$ with [ $matchings$ ] >>
          in
          let patt = mk_patt _loc v_name el in
          if rest = [] then [patt], v_expr
          else
            let patts, in_expr = loop (i + 1) rest in
            patt :: patts, <:expr< let sptr = $v_expr$ in $in_expr$ >>
      | [] -> assert false  (* impossible *)
    in
    loop 1 (list_of_ctyp tp [])

  and bin_write_tup_rec full_type_name _loc cnv_patts get_tp mk_patt tp =
    let patts, expr = bin_write_args full_type_name _loc get_tp mk_patt tp in
    `Match <:match_case< $cnv_patts patts$ -> $expr$ >>

  (* Conversion of tuples *)
  and bin_write_tuple full_type_name _loc tp =
    let cnv_patts patts = <:patt< ( $tup:paCom_of_list patts$ ) >> in
    let get_tp tp = tp in
    let mk_patt _loc v_name _ = <:patt< $lid:v_name$ >> in
    bin_write_tup_rec full_type_name _loc cnv_patts get_tp mk_patt tp

  (* Conversion of records *)
  and bin_write_record full_type_name _loc tp =
    let cnv_patts patts = <:patt< { $paSem_of_list patts$ } >> in
    let get_tp = function
      | <:ctyp< $_$ : mutable $tp$ >>
      | <:ctyp< $_$ : $tp$ >> -> tp
      | _ -> assert false  (* impossible *)
    in
    let mk_patt _loc v_name = function
      | <:ctyp< $lid:r_name$ : $_$ >> -> <:patt< $lid:r_name$ = $lid:v_name$ >>
      | _ -> assert false  (* impossible *)
    in
    bin_write_tup_rec full_type_name _loc cnv_patts get_tp mk_patt tp

  (* Conversion of variant types *)
  and bin_write_variant full_type_name _loc row_fields =
    let rec loop = function
      | <:ctyp< $tp1$ | $tp2$ >> -> <:match_case< $loop tp1$ | $loop tp2$ >>
      | <:ctyp< `$cnstr$ >> ->
          <:match_case<
            `$cnstr$ as v ->
              Bin_prot.Unsafe_write_c.bin_write_variant_tag sptr eptr v
          >>
      | <:ctyp< `$cnstr$ of $tp$ >> ->
          let write_args =
            match bin_write_type full_type_name _loc tp with
            | `Fun fun_expr -> <:expr< $fun_expr$ sptr eptr args >>
            | `Match matchings -> <:expr< match args with [ $matchings$ ] >>
          in
          <:match_case<
            `$cnstr$ args as v ->
              let sptr =
                Bin_prot.Unsafe_write_c.bin_write_variant_tag sptr eptr v
              in
              $write_args$
          >>
      | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
      | <:ctyp< [= $row_fields$ ] >> -> loop row_fields
      | <:ctyp< $tp1$ $tp2$ >> ->
          let id_path = Gen.get_appl_path _loc tp1 in
          let call = bin_write_appl_fun full_type_name _loc tp1 tp2 in
          <:match_case< #$id_path$ as v -> $call$ sptr eptr v >>
      | <:ctyp< $id:id$ >> | <:ctyp< #$id:id$ >> ->
          let call =
            match Gen.get_rev_id_path id [] with
            | tn :: path -> mk_abst_call _loc tn path
            | [] -> assert false  (* impossible *)
          in
          <:match_case< #$id$ as v -> $call$ sptr eptr v >>
      | _ -> failwith "bin_write_variant: unknown type"
    in
    `Match (loop row_fields)

  (* Polymorphic record fields *)
  and bin_write_poly full_type_name _loc parms tp =
    let bindings =
      let mk_binding parm =
        <:binding<
          $lid:"_write_" ^ parm$ = fun _sptr _eptr _v ->
            raise (Bin_prot.Common.Poly_rec_write $str:full_type_name$)
        >>
      in
      List.map mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    match bin_write_type full_type_name _loc tp with
    | `Fun fun_expr -> `Fun <:expr< let $list:bindings$ in $fun_expr$ >>
    | `Match matchings ->
        `Match
          <:match_case<
            arg ->
              let $list:bindings$ in
              match arg with
              [ $matchings$ ]
          >>


  (* Conversion of sum types *)

  let rec count_alts = function
    | <:ctyp< $tp1$ | $tp2$ >> -> count_alts tp1 + count_alts tp2
    | _ -> 1

  let bin_write_sum full_type_name _loc alts =
    let n_alts = count_alts alts in
    let write_tag =
      if n_alts <= 256 then
        <:expr< Bin_prot.Unsafe_write_c.bin_write_int_8bit sptr eptr >>
      else if n_alts <= 65536 then
        <:expr< Bin_prot.Unsafe_write_c.bin_write_int_16bit sptr eptr >>
      else (
        prerr_endline (
          get_loc_err _loc "bin_write_sum: too many alternatives (> 65536)");
        exit 1)
    in
    let rec loop i = function
      | <:ctyp< $tp1$ | $tp2$ >> ->
          let i1, case1 = loop i tp1 in
          let i2, case2 = loop i1 tp2 in
          i2, <:match_case< $case1$ | $case2$ >>
      | <:ctyp< $uid:cnstr$ >> ->
          i + 1, <:match_case< $uid:cnstr$ -> $write_tag$ $`int:i$ >>
      | <:ctyp< $uid:cnstr$ of $tp$ >> ->
          let get_tp tp = tp in
          let mk_patt _loc v_name _ = <:patt< $lid:v_name$ >> in
          let patts, write_args =
            bin_write_args full_type_name _loc get_tp mk_patt tp
          in
          let args =
            match patts with
            | [patt] -> patt
            | _ -> <:patt< $tup:paCom_of_list patts$ >>
          in
          let case =
            <:match_case< $uid:cnstr$ $args$ ->
              let sptr = $write_tag$ $`int:i$ in
              $write_args$
            >>
          in
          i + 1, case
      | _ -> failwith "branch_sum: unknown type"
    in
    `Match (snd (loop 0 alts))


  (* Empty types *)
  let bin_write_nil full_type_name _loc =
    `Fun <:expr< raise (Bin_prot.Common.Empty_type $str:full_type_name$) >>


  (* Generate code from type definitions *)
  let bin_write_td _loc type_name tps rhs =
    let full_type_name = sprintf "%s.%s" (get_conv_path ()) type_name in
    let is_nil = ref false in
    let int_body =
      let rec loop _loc =
        Gen.switch_tp_def
          ~alias:(bin_write_type full_type_name)
          ~sum:(bin_write_sum full_type_name)
          ~record:(bin_write_record full_type_name)
          ~variants:(bin_write_variant full_type_name)
          ~mani:(fun _loc _tp1 -> loop _loc)
          ~nil:(fun _loc -> is_nil := true; bin_write_nil full_type_name _loc)
      in
      match loop _loc rhs with
      | `Fun expr when !is_nil -> <:expr< fun _sptr _eptr _v -> $expr$ >>
      | `Fun fun_expr -> <:expr< fun sptr eptr v -> $fun_expr$ sptr eptr v >>
      | `Match matchings -> <:expr< fun sptr eptr -> fun [ $matchings$ ] >>
    in
    let tparam_cnvs = List.map ( (^) "_write_" *** Gen.get_tparam_id) tps in
    let mk_pat id = <:patt< $lid:id$ >> in
    let tparam_patts = List.map mk_pat tparam_cnvs in
    let int_call = "bin_write_" ^ type_name ^ "_" in
    let ext_fun =
      let ext_body =
        match int_body with
        | <:expr<
            fun sptr eptr v -> Bin_prot.Unsafe_write_c.$call$ sptr eptr v >> ->
            <:expr< Bin_prot.Write_ml.$call$ buf ~pos v >>
        | _ ->
            let app_call =
              let mk_expr name = <:expr< $lid:name$ >> in
              let tparam_exprs = List.map mk_expr tparam_cnvs in
              Gen.apply _loc <:expr< $lid:int_call$ >> tparam_exprs
            in
            <:expr<
              if Pervasives.(<) pos 0 then Bin_prot.Common.array_bound_error ()
              else
                let buf_len = Bigarray.Array1.dim buf in
                if Pervasives.(>) pos buf_len then
                  raise Bin_prot.Common.Buffer_short
                else
                  let start = Bin_prot.Unsafe_common.get_sptr buf ~pos:0 in
                  let sptr = Bin_prot.Unsafe_common.get_sptr buf ~pos in
                  let eptr = Bin_prot.Unsafe_common.get_eptr buf ~pos:buf_len in
                  let cur = $app_call$ sptr eptr v in
                  Bin_prot.Unsafe_common.get_safe_buf_pos buf ~start ~cur
            >>
      in
      <:expr< fun buf ~pos v -> $ext_body$ >>
    in
    let ext_name = "bin_write_" ^ type_name in
    let size_name = "bin_size_" ^ type_name in
    (
      <:binding< $lid:int_call$ = $Gen.abstract _loc tparam_patts int_body$ >>,
      (
        <:binding<
          $lid:ext_name$ = $Gen.abstract _loc tparam_patts ext_fun$
        >>,
        let size =
          let tparam_size_exprs =
            List.map (fun tp ->
              <:expr<
                $lid:"bin_writer_" ^ Gen.get_tparam_id tp$
                .Bin_prot.Type_class.size
              >>)
              tps
          in
          let call =
            Gen.apply _loc <:expr< $lid:size_name$ >> tparam_size_exprs
          in
          <:expr< fun v -> $call$ v >>
        in
        let tparam_unsafe_write_exprs =
          List.map (fun tp ->
            <:expr<
              $lid:"bin_writer_" ^ Gen.get_tparam_id tp$
              .Bin_prot.Type_class.unsafe_write
            >>)
            tps
        in
        let write =
          let call =
            Gen.apply _loc <:expr< $lid:ext_name$ >> tparam_unsafe_write_exprs
          in
          <:expr< fun buf ~pos v -> $call$ buf ~pos v >>
        in
        let unsafe_write =
          let call =
            Gen.apply _loc <:expr< $lid:int_call$ >> tparam_unsafe_write_exprs
          in
          <:expr< fun sptr eptr v -> $call$ sptr eptr v >>
        in
        let write =
          <:expr<
            {
              Bin_prot.Type_class.
              size = $size$;
              unsafe_write = $unsafe_write$;
              write = $write$;
            }
          >>
        in
        let tparam_writer_patts =
          List.map (fun tp ->
            <:patt< $lid:"bin_writer_" ^ Gen.get_tparam_id tp$ >>)
            tps
        in
        <:binding<
          $lid:"bin_writer_" ^ type_name$ =
            $Gen.abstract _loc tparam_writer_patts write$
        >>
      )
    )

  let rec bin_write_tds acc = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        bin_write_td _loc type_name tps rhs :: acc
    | TyAnd (_loc, tp1, tp2) -> bin_write_tds (bin_write_tds acc tp2) tp1
    | _ -> assert false  (* impossible *)

  let bin_write tds =
    let internals, externals1, externals2, recursive, _loc =
      match tds with
      | TyDcl (_loc, type_name, tps, rhs, _cl) ->
          let internal, (external1, external2) =
            bin_write_td _loc type_name tps rhs
          in
          [internal], [external1], [external2],
          Gen.type_is_recursive type_name rhs, _loc
      | TyAnd (_loc, _, _) ->
          let res = bin_write_tds [] tds in
          let internals, many_externals = List.split res in
          let externals1, externals2 = List.split many_externals in
          internals, externals1, externals2, true, _loc
      | _ -> assert false  (* impossible *)
    in
    let internals_item =
      if recursive then <:str_item< value rec $list:internals$ >>
      else <:str_item< value $list:internals$ >>
    in
    <:str_item<
      $Generate_bin_size.bin_size tds$;
      $internals_item$;
      value $list:externals1$;
      value $list:externals2$;
    >>

  (* Add code generator to the set of known generators *)
  let () = add_generator "bin_write" bin_write
end


(* Generator for converters of binary protocol to OCaml-values *)
module Generate_bin_read = struct
  let mk_abst_call _loc tn ?(internal = false) rev_path =
    let tnp =
      let tnn = "bin_read_" ^ tn in
      if internal then tnn ^ "__" else tnn ^ "_"
    in
    <:expr< $id:Gen.ident_of_rev_path _loc (tnp :: rev_path)$ >>

  (* Conversion of type paths *)
  let bin_read_path_fun _loc id =
    match Gen.get_rev_id_path id [] with
    | tn :: rev_path -> mk_abst_call _loc tn rev_path
    | [] -> assert false  (* no empty paths *)

  let get_closed_expr _loc = function
    | `Open expr -> <:expr< fun sptr_ptr eptr -> $expr$ >>
    | `Closed expr -> expr

  let get_open_expr _loc = function
    | `Open expr -> expr
    | `Closed expr -> <:expr< $expr$ sptr_ptr eptr >>

  (* Conversion of arguments *)
  let rec handle_arg_tp _loc full_type_name arg_tp =
    let n_args1, args, bindings =
      let rec arg_loop ai = function
        | <:ctyp< $tp1$ and $tp2$ >> ->
            let ai1, args1, abs1 = arg_loop ai tp1 in
            let ai2, args2, abs2 = arg_loop ai1 tp2 in
            (
              ai2,
              <:expr< $args1$, $args2$ >>,
              <:binding< $abs1$ and $abs2$ >>
            )
        | tp ->
            let f =
              get_open_expr _loc (bin_read_type full_type_name _loc tp)
            in
            let arg_name = "arg_" ^ string_of_int ai in
            (
              ai + 1,
              <:expr< $lid:arg_name$ >>,
              <:binding< $lid:arg_name$ = $f$ >>
            )
      in
      arg_loop 1 arg_tp
    in
    let args_expr =
      if n_args1 = 2 then <:expr< $args$ >>
      else <:expr< ( $tup:args$ ) >>
    in
    bindings, args_expr

  (* Conversion of types *)
  and bin_read_type full_type_name _loc = function
    | <:ctyp< $tp1$ $tp2$ >> ->
        let arg_expr =
          get_closed_expr _loc (bin_read_type full_type_name _loc tp2)
        in
        let expr =
          match bin_read_type full_type_name _loc tp1, arg_expr with
          | `Closed <:expr< Bin_prot.Unsafe_read_c.bin_read_array >>,
            <:expr< Bin_prot.Unsafe_read_c.bin_read_float >> ->
              `Closed <:expr< Bin_prot.Unsafe_read_c.bin_read_float_array >>
          | `Closed expr, _ -> `Closed <:expr< $expr$ $arg_expr$ >>
          | _ -> assert false  (* impossible *)
        in
        expr
    | <:ctyp< ( $tup:tp$ ) >> -> bin_read_tuple full_type_name _loc tp
    | <:ctyp< '$parm$ >> -> `Closed <:expr< $lid:"_of__" ^ parm$ >>
    | <:ctyp< $id:id$ >> -> `Closed (bin_read_path_fun _loc id)
    | <:ctyp< $_$ -> $_$ >> -> failwith "bin_read_arrow: cannot convert functions"
    | <:ctyp< [< $row_fields$ ] >> | <:ctyp< [> $row_fields$ ] >>
    | <:ctyp< [= $row_fields$ ] >> ->
        fst (bin_read_variant full_type_name _loc ?full_type:None row_fields)
    | <:ctyp< ! $parms$ . $poly_tp$ >> ->
        bin_read_poly full_type_name _loc parms poly_tp
    | _ ->
        prerr_endline (
          get_loc_err _loc "bin_read_type: unknown type construct");
        exit 1

  (* Conversion of tuples *)
  and bin_read_tuple full_type_name _loc tps =
    let _, bindings, expr =
      let rec loop i = function
        | <:ctyp< $tp1$ * $tp2$ >> ->
            let i1, bs1, exprs1 = loop i tp1 in
            let i2, bs2, exprs2 = loop i1 tp2 in
            i2, <:binding< $bs1$ and $bs2$ >>, <:expr< $exprs1$, $exprs2$ >>
        | tp ->
            let v_name = "v" ^ string_of_int i in
            let expr =
              get_open_expr _loc (bin_read_type full_type_name _loc tp)
            in
            (
              i + 1,
              <:binding< $lid:v_name$ = $expr$ >>,
              <:expr< $lid:v_name$ >>
            )
      in
      loop 1 tps
    in
    `Open <:expr< let $bindings$ in ( $tup:expr$ ) >>


  (* Variant conversions *)

  (* Generate internal call *)
  and mk_internal_call full_type_name _loc = function
    | <:ctyp< $id:id$ >> | <:ctyp< #$id:id$ >> ->
        let call =
          match Gen.get_rev_id_path id [] with
          | tn :: rev_path -> mk_abst_call _loc tn ~internal:true rev_path
          | [] -> assert false  (* impossible *)
        in
        call
    | <:ctyp< $tp1$ $tp2$ >> ->
        let arg_expr =
          get_closed_expr _loc (bin_read_type full_type_name _loc tp2)
        in
        <:expr< $mk_internal_call full_type_name _loc tp1$ $arg_expr$ >>
    | _ -> assert false  (* impossible *)

  (* Generate matching code for variants *)
  and bin_read_variant full_type_name _loc ?full_type row_tp =
    let is_contained, full_type =
      match full_type with
      | None -> true, <:ctyp< [= $row_tp$ ] >>
      | Some full_type -> false, full_type
    in
    let atoms_only = ref true in
    let code =
      let mk_check_vint mcs =
        <:expr< match Bin_prot.Common.variant_of_int vint with [ $mcs$ ] >>
      in
      let mk_try_next_expr call next_expr =
        <:expr<
          try $call$ with
          [ Bin_prot.Common.No_variant_match -> $next_expr$ ]
        >>
      in
      let raise_nvm = <:expr< raise Bin_prot.Common.No_variant_match >> in
      let rec loop_many next = function
        | h :: t -> loop_one next t h
        | [] ->
            match next with
            | `Matches mcs -> mk_check_vint mcs
            | `Expr expr -> expr
            | `None -> raise_nvm
      and loop_one next t = function
        | <:ctyp< `$cnstr$ >> ->
            let this_mc = <:match_case< `$cnstr$ as tag -> tag >> in
            add_mc next this_mc t
        | <:ctyp< `$cnstr$ of $arg_tp$ >> ->
            atoms_only := false;
            let bnds, args_expr = handle_arg_tp _loc full_type_name arg_tp in
            let rhs = <:expr< let $bnds$ in `$cnstr$ $args_expr$ >> in
            let this_mc = <:match_case< `$cnstr$ -> $rhs$ >> in
            add_mc next this_mc t
        | (<:ctyp< $id:_$ >> | <:ctyp< $_$ $_$ >>
        | <:ctyp< #$id:_$ >>) as inh ->
            atoms_only := false;
            let call =
              <:expr<
                (
                  $mk_internal_call
                    full_type_name _loc inh$ sptr_ptr eptr vint
                    :> $full_type$
                )
              >>
            in
            let expr =
              match next with
              | `Matches mcs -> mk_try_next_expr call (mk_check_vint mcs)
              | `Expr expr -> mk_try_next_expr call expr
              | `None -> call
            in
            loop_many (`Expr expr) t
        | _ -> assert false  (* impossible *)
      and add_mc next this_mc t =
        let next_mcs =
          match next with
          | `Matches mcs -> mcs
          | `Expr expr -> <:match_case< _ -> $expr$ >>
          | `None -> <:match_case< _ -> $raise_nvm$ >>
        in
        loop_many (`Matches <:match_case< $this_mc$ | $next_mcs$ >>) t
      in
      loop_many `None (List.rev (list_of_ctyp row_tp []))
    in
    let res =
      if is_contained then
        `Open
          <:expr<
            let vint =
              Bin_prot.Unsafe_read_c.bin_read_variant_int sptr_ptr eptr
            in
            try $code$
            with
            [ Bin_prot.Common.No_variant_match ->
                raise (
                  Bin_prot.Unsafe_read_c.Error (
                    Bin_prot.Common.ReadError.Variant $str:full_type_name$))
            ]
          >>
      else `Open code
    in
    res, !atoms_only


  (* Polymorphic record field conversion *)
  and bin_read_poly full_type_name _loc parms tp =
    let bindings =
      let mk_binding parm =
        <:binding<
          $lid:"_of__" ^ parm$ =
            fun _sptr_ptr _eptr ->
              raise (
                Bin_prot.Unsafe_read_c.Error (
                  Bin_prot.Common.ReadError.Poly_rec_bound
                    $str:full_type_name$))
        >>
      in
      List.map mk_binding (Gen.ty_var_list_of_ctyp parms [])
    in
    let f = get_open_expr _loc (bin_read_type full_type_name _loc tp) in
    `Open <:expr< let $list:bindings$ in $f$ >>

  (* Sum type conversions *)
  let bin_read_sum full_type_name _loc alts =
    let rec loop mi = function
      | <:ctyp< $tp1$ | $tp2$ >> ->
          let i1, mcs1 = loop mi tp1 in
          let i2, mcs2 = loop i1 tp2 in
          i2, <:match_case< $mcs1$ | $mcs2$ >>
      | <:ctyp< $uid:atom$ >> ->
          mi + 1, <:match_case< $`int:mi$ -> $uid:atom$ >>
      | <:ctyp< $uid:atom$ of $arg_tp$ >> ->
          let bindings, args_expr = handle_arg_tp _loc full_type_name arg_tp in
          let rhs = <:expr< let $bindings$ in $uid:atom$ $args_expr$ >> in
          mi + 1, <:match_case< $`int:mi$ -> $rhs$ >>
      | _ -> assert false  (* impossible *)
    in
    let n_alts, mcs = loop 0 alts in
    let read_fun =
      if n_alts <= 256 then
        <:expr< Bin_prot.Unsafe_read_c.bin_read_int_8bit >>
      else if n_alts <= 65536 then
        <:expr< Bin_prot.Unsafe_read_c.bin_read_int_16bit >>
      else (
        prerr_endline (
          get_loc_err _loc "bin_read_sum: more than 65536 constructors");
        exit 1)
    in
    `Open
      <:expr<
        match $read_fun$ sptr_ptr eptr with
        [ $mcs$
        | _ ->
            raise (
              Bin_prot.Unsafe_read_c.Error (
                Bin_prot.Common.ReadError.Sum_tag $str:full_type_name$)) ]
      >>

  (* Record conversions *)
  let bin_read_record full_type_name _loc tps =
    let bindings, rec_bindings =
      let rec loop = function
        | <:ctyp< $tp1$; $tp2$ >> ->
            let bs1, rec_bs1 = loop tp1 in
            let bs2, rec_bs2 = loop tp2 in
            (
              <:binding< $bs1$ and $bs2$ >>,
              <:rec_binding< $rec_bs1$; $rec_bs2$ >>
            )
        | <:ctyp< $lid:field_name$ : mutable $tp$ >>
        | <:ctyp< $lid:field_name$ : $tp$ >> ->
            let v_name = "v_" ^ field_name in
            let f = get_open_expr _loc (bin_read_type full_type_name _loc tp) in
            (
              <:binding< $lid:v_name$ = $f$ >>,
              <:rec_binding< $lid:field_name$ = $lid:v_name$ >>
            )
        | _ -> assert false  (* impossible *)
      in
      loop tps
    in
    `Open <:expr< let $bindings$ in { $rec_bindings$ } >>


  (* Empty types *)
  let bin_read_nil full_type_name _loc =
    `Closed
      <:expr< fun _sptr_ptr _eptr ->
        raise (
          Bin_prot.Unsafe_read_c.Error
            (Bin_prot.Common.ReadError.Empty_type
              $str:full_type_name$))
      >>


  (* Generate code from type definitions *)

  let bin_read_td _loc type_name tps rhs =
    let full_type_name = sprintf "%s.%s" (get_conv_path ()) type_name in
    let coll_args tp param =
      <:ctyp< $tp$ $Gen.drop_variance_annotations param$ >>
    in
    let full_type = List.fold_left coll_args <:ctyp< $lid:type_name$ >> tps in
    let is_alias_ref = ref false in
    let handle_alias _loc tp =
      is_alias_ref := true;
      bin_read_type full_type_name _loc tp
    in
    let is_variant_ref = ref false in
    let atoms_only_ref = ref true in
    let handle_variant _loc tp =
      is_variant_ref := true;
      let res, atoms_only =
        bin_read_variant full_type_name ~full_type _loc tp
      in
      atoms_only_ref := atoms_only;
      res
    in
    let arg_patts, arg_exprs =
      List.split (
        List.map (function tp ->
            let name = "_of__" ^ Gen.get_tparam_id tp in
            <:patt< $lid:name$ >>, <:expr< $lid:name$ >>
          )
          tps)
    in
    let oc_body =
      let rec loop _loc =
        Gen.switch_tp_def
          ~alias:handle_alias
          ~sum:(bin_read_sum full_type_name)
          ~record:(bin_read_record full_type_name)
          ~variants:handle_variant
          ~mani:(fun _loc _tp1 -> loop _loc)
          ~nil:(bin_read_nil type_name)
      in
      loop _loc rhs
    in

    let variant_int_call =
      let maybe_poly_var_name = "bin_read_" ^ type_name ^ "__" in
      let maybe_poly_var_expr = <:expr< $lid:maybe_poly_var_name$ >> in
      <:expr<
        let vint =
          Bin_prot.Unsafe_read_c.bin_read_variant_int sptr_ptr eptr
        in
        $Gen.apply _loc maybe_poly_var_expr arg_exprs$ sptr_ptr eptr vint
      >>
    in

    let user_binding_name = "bin_read_" ^ type_name in

    let user_binding =
      let exc_handling =
        let normal_exc_handling =
          <:match_case<
              Bin_prot.Unsafe_read_c.Error (
                Bin_prot.Common.ReadError.Variant _ as err) ->
                let err_pos4 =
                  Bin_prot.Unsafe_common.dealloc_sptr_ptr buf sptr_ptr
                in
                let err_pos = Pervasives.(-) err_pos4 4 in
                Bin_prot.Common.raise_read_error err err_pos
            | Bin_prot.Unsafe_read_c.Error err ->
                let err_pos =
                  Bin_prot.Unsafe_common.dealloc_sptr_ptr buf sptr_ptr
                in
                Bin_prot.Common.raise_read_error err err_pos
            | exc ->
                let err_pos =
                  Bin_prot.Unsafe_common.dealloc_sptr_ptr buf sptr_ptr
                in
                Bin_prot.Common.raise_read_exc exc err_pos
          >>
        in
        if !is_variant_ref then
          <:match_case<
              Bin_prot.Common.No_variant_match ->
                let err_pos4 =
                  Bin_prot.Unsafe_common.dealloc_sptr_ptr buf sptr_ptr
                in
                let err_pos = Pervasives.(-) err_pos4 4 in
                let err =
                  Bin_prot.Common.ReadError.Variant $str:full_type_name$
                in
                Bin_prot.Common.raise_read_error err err_pos
            | $normal_exc_handling$
          >>
        else normal_exc_handling
      in

      let abst_call =
        if !is_alias_ref then
          match oc_body with
          | `Closed expr -> <:expr< $expr$ sptr_ptr eptr >>
          | `Open body -> body
        else if !is_variant_ref then variant_int_call
        else
          let abst_name = "bin_read_" ^ type_name ^ "_" in
          let abst_expr = <:expr< $lid:abst_name$ >> in
          <:expr< $Gen.apply _loc abst_expr arg_exprs$ sptr_ptr eptr >>
      in
      let user_fun =
        let user_body =
          match abst_call with
          | <:expr< Bin_prot.Unsafe_read_c.$call$ sptr_ptr eptr >> ->
              <:expr< Bin_prot.Read_ml.$call$ buf ~pos_ref >>
          | _ ->
              <:expr<
                let pos = !pos_ref in
                if Pervasives.(<) pos 0 then
                  Bin_prot.Common.array_bound_error ()
                else
                  let buf_len = Bigarray.Array1.dim buf in
                  if Pervasives.(>) pos buf_len then
                    raise Bin_prot.Common.Buffer_short
                  else
                    let sptr_ptr =
                      Bin_prot.Unsafe_common.alloc_sptr_ptr buf ~pos
                    in
                    let eptr =
                      Bin_prot.Unsafe_common.get_eptr buf ~pos:buf_len
                    in
                    let v = try $abst_call$ with [ $exc_handling$ ] in
                    let cur =
                      Bin_prot.Unsafe_common.dealloc_sptr_ptr buf sptr_ptr
                    in
                    do { pos_ref.contents := cur; v }
              >>
        in
        Gen.abstract _loc arg_patts <:expr< fun buf ~pos_ref -> $user_body$ >>
      in
      <:binding< $lid:user_binding_name$ = $user_fun$ >>
    in

    let unsafe_read_name = "bin_read_" ^ type_name ^ "_" in

    let abst_binding =
      let abst_body =
        if !is_alias_ref then
          match oc_body with
          | `Closed f -> <:expr< fun sptr_ptr eptr -> $f$ sptr_ptr eptr >>
          | `Open body -> <:expr< fun sptr_ptr eptr -> $body$ >>
        else if !is_variant_ref then
          <:expr<
            fun sptr_ptr eptr ->
              try $variant_int_call$ with
              [ Bin_prot.Common.No_variant_match ->
                  raise
                    (Bin_prot.Unsafe_read_c.Error (
                      Bin_prot.Common.ReadError.Variant $str:full_type_name$)) ]
          >>
        else
          match oc_body with
          | `Open body -> <:expr< fun sptr_ptr eptr -> $body$ >>
          | `Closed f -> <:expr< $f$ >>
      in
      <:binding<
        $lid:unsafe_read_name$ = $Gen.abstract _loc arg_patts abst_body$
      >>
    in

    let unsafe_vtag_read_name = "bin_read_" ^ type_name ^ "__" in

    let maybe_poly_var_binding =
      let maybe_poly_var_body =
        let wrong_type =
          <:expr<
            fun _sptr_ptr _eptr _vint ->
              Bin_prot.Unsafe_read_c.raise_variant_wrong_type
                $str:full_type_name$
          >>
        in
        if !is_alias_ref then
          match oc_body with
          | `Closed call ->
              let rec rewrite_call cnv = function
                | <:expr< $f$ $arg$ >> ->
                    rewrite_call (fun new_f -> cnv (<:expr< $new_f$ $arg$ >>)) f
                | <:expr< Bin_prot.Unsafe_read_c.$_$ >> -> wrong_type
                | <:expr< $lid:name$ >> when name.[0] = '_' && name.[1] = 'o' ->
                    <:expr<
                      fun _sptr_ptr _eptr _vint ->
                        raise (
                          Bin_prot.Unsafe_read_c.Error
                            (Bin_prot.Common.ReadError.Silly_type
                              $str:full_type_name$))
                    >>
                | <:expr< $id:id$ >> ->
                    (match Gen.get_rev_id_path id [] with
                    | call :: rest ->
                        let expr =
                          <:expr<
                            $id:Gen.ident_of_rev_path
                              _loc ((call ^ "_") :: rest)$
                          >>
                        in
                        <:expr<
                          fun sptr_ptr eptr vint ->
                            $cnv expr$ sptr_ptr eptr vint
                        >>
                    | _ -> assert false)  (* impossible *)
                | _ -> assert false  (* impossible *)
              in
              rewrite_call (fun x -> x) call
          | _ -> wrong_type
        else if !is_variant_ref then
          match oc_body with
          | `Open body when !atoms_only_ref ->
              <:expr< fun _sptr_ptr _eptr vint -> $body$ >>
          | `Open body -> <:expr< fun sptr_ptr eptr vint -> $body$ >>
          | _ -> assert false  (* impossible *)
        else wrong_type
      in
      let full_body = Gen.abstract _loc arg_patts maybe_poly_var_body in
      <:binding< $lid:unsafe_vtag_read_name$ = $full_body$ >>
    in

    let tparam_unsafe_read_exprs =
      List.map (fun tp ->
        <:expr<
          $lid:"bin_reader_" ^ Gen.get_tparam_id tp$
          .Bin_prot.Type_class.unsafe_read
        >>)
        tps
    in
    let read =
      let call =
        Gen.apply _loc <:expr< $lid:user_binding_name$ >>
          tparam_unsafe_read_exprs
      in
      <:expr< fun buf ~pos_ref -> $call$ buf ~pos_ref >>
    in
    let unsafe_read =
      let call =
        Gen.apply _loc <:expr< $lid:unsafe_read_name$ >>
          tparam_unsafe_read_exprs
      in
      <:expr< fun sptr_ptr eptr -> $call$ sptr_ptr eptr >>
    in
    let unsafe_vtag_read =
      let call =
        Gen.apply _loc <:expr< $lid:unsafe_vtag_read_name$ >>
          tparam_unsafe_read_exprs
      in
      <:expr< fun sptr_ptr eptr vtag -> $call$ sptr_ptr eptr vtag >>
    in
    let reader =
      <:expr<
        {
          Bin_prot.Type_class.
          read = $read$;
          unsafe_read = $unsafe_read$;
          unsafe_vtag_read = $unsafe_vtag_read$;
        }
      >>
    in
    let tparam_reader_patts =
      List.map (fun tp ->
        <:patt< $lid:"bin_reader_" ^ Gen.get_tparam_id tp$ >>)
        tps
    in
    let reader_binding =
      <:binding< $lid:"bin_reader_" ^ type_name$ =
        $Gen.abstract _loc tparam_reader_patts reader$
      >>
    in
    (
      (
        maybe_poly_var_binding,
        abst_binding
      ),
      (
        user_binding,
        reader_binding
      )
    )

  let rec bin_read_tds acc = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        bin_read_td _loc type_name tps rhs :: acc
    | TyAnd (_loc, tp1, tp2) -> bin_read_tds (bin_read_tds acc tp2) tp1
    | _ -> assert false  (* impossible *)

  (* Generate code from type definitions *)
  let bin_read tds =
    let res, recursive, _loc =
      match tds with
      | TyDcl (_loc, type_name, tps, rhs, _cl) ->
          let res = bin_read_td _loc type_name tps rhs in
          [res], Gen.type_is_recursive type_name rhs, _loc
      | TyAnd (_loc, _, _) -> bin_read_tds [] tds, true, _loc
      | _ -> assert false  (* impossible *)
    in
    let poly_abst, user_bindings_readers = List.split res in
    let user_bindings, readers = List.split user_bindings_readers in
    if recursive then
      (* Improve code locality *)
      let cnv (maybe_poly_var_binding, abst_binding) =
        <:binding< $maybe_poly_var_binding$ and $abst_binding$ >>
      in
      let internal_bindings = List.map cnv poly_abst in
      <:str_item<
        value rec $list:internal_bindings$;
        value $list:user_bindings$;
        value $list:readers$
      >>
    else
      (* Improve code locality *)
      let cnv (maybe_poly_var_binding, abst_binding) =
        <:str_item< value $maybe_poly_var_binding$; value $abst_binding$ >>
      in
      let internal_items = List.map cnv poly_abst in
      <:str_item<
        $list:internal_items$;
        value $list:user_bindings$;
        value $list:readers$
      >>

  (* Add code generator to the set of known generators *)
  let () = add_generator "bin_read" bin_read
end


(* Generator for binary protocol type classes *)
module Generate_tp_class = struct
  let bin_tp_class_td _loc type_name tps _rhs =
    let tparam_cnvs =
      List.map (fun tp -> "bin_" ^  Gen.get_tparam_id tp) tps
    in
    let mk_pat id = <:patt< $lid:id$ >> in
    let tparam_patts = List.map mk_pat tparam_cnvs in
    let writer =
      let tparam_exprs =
        List.map (fun tp ->
          <:expr<
            $lid:"bin_" ^ Gen.get_tparam_id tp$
            .Bin_prot.Type_class.writer
          >>)
          tps
      in
      Gen.apply _loc <:expr< $lid:"bin_writer_" ^ type_name$ >> tparam_exprs
    in
    let reader =
      let tparam_exprs =
        List.map (fun tp ->
          <:expr<
            $lid:"bin_" ^ Gen.get_tparam_id tp$
            .Bin_prot.Type_class.reader
          >>)
          tps
      in
      Gen.apply _loc <:expr< $lid:"bin_reader_" ^ type_name$ >> tparam_exprs
    in
    let body =
      <:expr<
        {
          Bin_prot.Type_class.
          writer = $writer$;
          reader = $reader$;
        }
      >>
    in
    <:binding<
      $lid:"bin_" ^ type_name$ = $Gen.abstract _loc tparam_patts body$
    >>

  let rec bin_tp_class_tds acc = function
    | TyDcl (_loc, type_name, tps, rhs, _cl) ->
        bin_tp_class_td _loc type_name tps rhs :: acc
    | TyAnd (_loc, tp1, tp2) ->
        bin_tp_class_tds (bin_tp_class_tds acc tp2) tp1
    | _ -> assert false  (* impossible *)

  (* Generate code from type definitions *)
  let bin_tp_class tds =
    let _loc = Loc.ghost in
    <:str_item< value rec $list:bin_tp_class_tds [] tds$ >>

  (* Add code generator to the set of known generators *)
  let () = add_generator "bin_type_class" bin_tp_class
end

(* Add "bin_read", "bin_write" and "bin_tp_class" as "bin_io" to the
   set of generators *)
let () =
  add_generator
    "bin_io"
    (fun tds ->
      let _loc = Loc.ghost in
      <:str_item<
        $Generate_bin_write.bin_write tds$;
        $Generate_bin_read.bin_read tds$;
        $Generate_tp_class.bin_tp_class tds$;
      >>)
