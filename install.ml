#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"bin_prot"
  [ oasis_lib "bin_prot"
  ; oasis_lib "bin_shape_lib"
  ; file "META" ~section:"lib"
  ; oasis_lib "bin_prot_xen"
  ]
