(** Functions common to unsafe binary protocol conversion. *)

(** NOTE: these functions are not supposed to be called by the casual
    user.  They are required by automatically generated code, or if a
    developer really needs to get down and dirty for performance reasons.

    USE WITH CAUTION!
*)

open Common

(** Type of start pointers *)
type sptr

(** Type of end pointers *)
type eptr

(** Type of pointers to start pointers *)
type sptr_ptr

external get_sptr : buf -> pos : pos -> sptr = "get_buf_ptr_stub" "noalloc"
(** [get_sptr buf ~pos] @return a start pointer given buffer [buf]
    and start position [pos]. *)

external get_eptr : buf -> pos : pos -> eptr = "get_buf_ptr_stub" "noalloc"
(** [get_eptr buf ~pos] @return an end pointer given buffer [buf]
    and end position [pos]. *)

external shift_sptr : sptr -> int -> sptr = "shift_sptr_stub" "noalloc"
(** [shift_sptr sptr n] @return a start pointer by shifting [sptr] by [n]
    characters. *)

external get_eptr_from_sptr_ptr :
  sptr_ptr -> pos : pos -> eptr = "get_eptr_from_sptr_ptr" "noalloc"
(** [get_eptr_from_sptr_ptr sptr_ptr ~pos] @return an end pointer by
    obtaining the position [pos] after location [sptr_ptr]. *)

external get_buf_pos :
  start : sptr -> cur : sptr -> pos = "get_buf_pos_stub" "noalloc"
(** [get_buf_pos ~start ~cur] @return a buffer position as difference
    between start pointers [start] and [cur]. *)

external get_safe_buf_pos :
  buf -> start : sptr -> cur : sptr -> pos = "get_safe_buf_pos_stub" "noalloc"
(** [get_safe_buf_pos buf ~start ~cur] @return a buffer position as
    difference between start pointers [start] and [cur].  [buf] is
    ignored, but prevents the buffer from being reclaimed by the GC,
    which it needs to until this function gets called. *)

external alloc_sptr_ptr :
  buf -> pos : pos -> sptr_ptr = "alloc_sptr_ptr_stub" "noalloc"
(** [alloc_sptr_ptr buf ~pos] allocate a pointer to a start pointer.
    NOTE: do not forget to deallocate it, otherwise there will be a
    space leak!
    NOTE: The "noalloc" attribute is correct, because it indicates
    there is no OCaml allocation.  [alloc_sptr_ptr] only does C
    allocation.
*)

external dealloc_sptr_ptr :
  buf -> sptr_ptr -> pos = "dealloc_sptr_ptr_stub" "noalloc"
(** [dealloc_sptr_ptr buf sptr_ptr] deallocate a pointer to a start
    pointer and return its position.  NOTE: do not do this more than
    once, otherwise the program may crash! *)

external get_sptr_ptr : sptr_ptr -> buf -> pos = "get_sptr_ptr_stub" "noalloc"
(** [get_sptr_ptr sptr_ptr buf] @return the position in buffer [buf]
    denoted by the pointer stored in [sptr_ptr]. *)

external set_sptr_ptr :
  sptr_ptr -> buf -> pos : pos -> unit = "set_sptr_ptr_stub" "noalloc"
(** [set_sptr_ptr sptr_ptr buf ~pos] sets the pointer in [sptr_ptr]
    to the location denoted by position [pos] in buffer [buf]. *)

external get_sptr_ptr_sptr :
  sptr_ptr -> sptr = "get_sptr_ptr_sptr_stub" "noalloc"
(** [get_sptr_ptr_sptr sptr_ptr] @return the pointer in [sptr_ptr]. *)

external set_sptr_ptr_sptr :
  sptr_ptr -> sptr -> unit = "set_sptr_ptr_sptr_stub" "noalloc"
(** [set_sptr_ptr_sptr sptr_ptr sptr] sets the pointer in
    [sptr_ptr] to [sptr]. *)

external get_ptr_string : sptr -> eptr -> string = "get_ptr_string_stub"
(** [get_ptr_string sptr_ptr eptr] @return the string in the range from
    start pointer [sptr] to end pointer [eptr]. *)

val get_read_init : buf -> pos_ref : pos ref -> sptr_ptr * eptr
(** [get_read_init buf ~pos_ref] @return the [sptr_ptr] denoting the
    start and the [eptr] denoting the end of buffer [buf].  NOTE: do
    not forget to deallocate the [sptr_ptr]! *)
