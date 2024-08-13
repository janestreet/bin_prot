(module
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "env" "caml_ba_create"
      (func $caml_ba_create
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_get_kind"
      (func $caml_ba_get_kind (param (ref eq)) (result i32)))
   (import "env" "caml_ba_to_typed_array"
      (func $caml_ba_to_typed_array (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_get_1"
      (func  $caml_ba_get_1 (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_set_1"
      (func  $caml_ba_set_1
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_sub"
      (func $caml_ba_sub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_blit"
      (func $caml_ba_blit (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_ba"
      (func $caml_bigstring_blit_ba_to_ba
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_string_to_ba"
      (func $caml_bigstring_blit_string_to_ba
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_bytes"
      (func $caml_bigstring_blit_ba_to_bytes
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "bigstring_of_typed_array"
      (func $bigstring_of_typed_array (param (ref eq)) (result (ref eq))))
   (import "env" "caml_floatarray_unsafe_get"
      (func $caml_floatarray_unsafe_get
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_floatarray_unsafe_set"
      (func $caml_floatarray_unsafe_set
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $block (array (mut (ref eq))))

   (func (export "bin_prot_blit_buf_float_array_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $vdst_pos (ref eq)) (param $dst (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $dst_pos i32) (local $len i32) (local $i i32)
      (local $view (ref eq)) (local $buffer (ref eq))
      (local.set $dst_pos (i31.get_u (ref.cast (ref i31) (local.get $vdst_pos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.eqz (local.get $len))
         (then (return (ref.i31 (i32.const 0)))))
      (local.set $view
         (call $caml_ba_create
            (ref.i31 (i32.const 1 (;double;)))
            (ref.i31 (i32.const 0))
            (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (local.get $vlen))))
      (local.set $buffer
         (call $bigstring_of_typed_array
            (call $caml_ba_to_typed_array (local.get $view))))
      (drop
         (call $caml_ba_blit
            (call $caml_ba_sub
               (local.get $src)
               (local.get $src_pos)
               (ref.i31 (i32.mul (local.get $len) (i32.const 8))))
            (local.get $buffer)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (drop (call $caml_floatarray_unsafe_set
                  (local.get $dst)
                  (ref.i31 (i32.add (local.get $dst_pos) (local.get $i)))
                  (call $caml_ba_get_1
                     (local.get $view) (ref.i31 (local.get $i)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (ref.i31 (i32.const 0)))

   (func (export "bin_prot_blit_buf_bytes_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (return_call $caml_bigstring_blit_ba_to_bytes
         (local.get $src) (local.get $src_pos)
         (local.get $dst) (local.get $dst_pos)
         (local.get $len)))

   (func (export "bin_prot_blit_float_array_buf_stub")
      (param $vsrc_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $vlen (ref eq)) (result (ref eq))
      (local $src_pos i32)
      (local $len i32) (local $i i32)
      (local $float64_array (ref eq)) (local $float64_uint8 (ref eq))
      (local $view (ref eq))
      (local.set $src_pos (i31.get_u (ref.cast (ref i31) (local.get $vsrc_pos))))
      (local.set $len (i31.get_u (ref.cast (ref i31) (local.get $vlen))))
      (if (i32.eqz (local.get $len))
         (then (return (ref.i31 (i32.const 0)))))
      (local.set $float64_array
         (call $caml_ba_create
            (ref.i31 (i32.const 1 (;double;)))
            (ref.i31 (i32.const 0))
            (array.new_fixed $block 2 (ref.i31 (i32.const 0))
               (local.get $vlen))))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (drop
                  (call $caml_ba_set_1
                     (local.get $float64_array) (ref.i31 (local.get $i))
                     (call $caml_floatarray_unsafe_get
                        (local.get $src)
                        (ref.i31
                           (i32.add (local.get $src_pos) (local.get $i))))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.set $float64_uint8
         (call $bigstring_of_typed_array
            (call $caml_ba_to_typed_array (local.get $float64_array))))
      (drop
         (call $caml_ba_blit
            (local.get $float64_uint8)
            (call $caml_ba_sub (local.get $dst) (local.get $dst_pos)
               (ref.i31 (i32.mul (local.get $len) (i32.const 8))))))
      (ref.i31 (i32.const 0)))

   (export "bin_prot_blit_bytes_buf_stub" (func $bin_prot_blit_string_buf_stub))
   (func $bin_prot_blit_string_buf_stub (export "bin_prot_blit_string_buf_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (return_call $caml_bigstring_blit_string_to_ba
         (local.get $src) (local.get $src_pos)
         (local.get $dst) (local.get $dst_pos)
         (local.get $len)))

   (func (export "bin_prot_blit_buf_stub")
      (param $src_pos (ref eq)) (param $src (ref eq))
      (param $dst_pos (ref eq)) (param $dst (ref eq))
      (param $len (ref eq)) (result (ref eq))
      (if (i32.ne (call $caml_ba_get_kind (local.get $src)) (i32.const 12))
         (then
            (local.set $src
               (call $bigstring_of_typed_array
                  (call $caml_ba_to_typed_array (local.get $src))))))
      (if (i32.ne (call $caml_ba_get_kind (local.get $dst)) (i32.const 12))
         (then
            (local.set $dst
               (call $bigstring_of_typed_array
                  (call $caml_ba_to_typed_array (local.get $dst))))))
      (return_call $caml_bigstring_blit_ba_to_ba
         (local.get $src) (local.get $src_pos)
         (local.get $dst) (local.get $dst_pos)
         (local.get $len)))
)
