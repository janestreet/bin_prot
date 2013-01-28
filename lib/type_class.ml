(* Tp_class: sizers, writers, and readers in records *)

open Common

type 'a writer =
  {
    size : 'a Size.sizer;
    write : 'a Write_ml.writer;
    unsafe_write : 'a Unsafe_write_c.writer;
  }

type 'a reader =
  {
    read : 'a Read_ml.reader;
    unsafe_read : 'a Unsafe_read_c.reader;
    unsafe_vtag_read : (int -> 'a) Unsafe_read_c.reader;
  }

type 'a t =
  {
    writer : 'a writer;
    reader : 'a reader;
  }

type 'a writer0 = 'a writer
type 'a reader0 = 'a reader
type 'a t0 = 'a t

module S1 = struct
  type ('a, 'b) writer = 'a writer0 -> 'b writer0
  type ('a, 'b) reader = 'a reader0 -> 'b reader0
  type ('a, 'b) t = 'a t0 -> 'b t0
end

module S2 = struct
  type ('a, 'b, 'c) writer = 'a writer0 -> ('b, 'c) S1.writer
  type ('a, 'b, 'c) reader = 'a reader0 -> ('b, 'c) S1.reader
  type ('a, 'b, 'c) t = 'a t0 -> ('b, 'c) S1.t
end

module S3 = struct
  type ('a, 'b, 'c, 'd) writer = 'a writer0 -> ('b, 'c, 'd) S2.writer
  type ('a, 'b, 'c, 'd) reader = 'a reader0 -> ('b, 'c, 'd) S2.reader
  type ('a, 'b, 'c, 'd) t = 'a t0 -> ('b, 'c, 'd) S2.t
end

#define MK_BASE(NAME) \
  let bin_writer_/**/NAME = \
    { \
      size = Size.bin_size_/**/NAME; \
      write = Write_ml.bin_write_/**/NAME; \
      unsafe_write = Unsafe_write_c.bin_write_/**/NAME; \
    } \
  let bin_reader_/**/NAME = \
    { \
      read = Read_ml.bin_read_/**/NAME; \
      unsafe_read = Unsafe_read_c.bin_read_/**/NAME; \
      unsafe_vtag_read = fun _sptr_ptr _eptr _vint -> \
        Unsafe_read_c.raise_variant_wrong_type \
          "NAME"; \
    } \
  let bin_/**/NAME = \
    { \
      writer = bin_writer_/**/NAME; \
      reader = bin_reader_/**/NAME; \
    }

MK_BASE(unit)
MK_BASE(bool)
MK_BASE(string)
MK_BASE(char)
MK_BASE(int)
MK_BASE(float)
MK_BASE(int32)
MK_BASE(int64)
MK_BASE(nativeint)
MK_BASE(nat0)

#define MK_WRITER_BASE1(NAME) \
  let bin_writer_/**/NAME bin_writer_el = \
    { \
      size = (fun v -> Size.bin_size_/**/NAME bin_writer_el.size v); \
      write = (fun buf ~pos v -> \
        Write_ml.bin_write_/**/NAME bin_writer_el.write buf ~pos v); \
      unsafe_write = (fun sptr eptr v -> \
        Unsafe_write_c.bin_write_/**/NAME \
          bin_writer_el.unsafe_write sptr eptr v); \
    }

#define MK_BASE1(NAME) \
  MK_WRITER_BASE1(NAME) \
  let bin_reader_/**/NAME bin_reader_el = \
    { \
      read = (fun buf ~pos_ref -> \
        Read_ml.bin_read_/**/NAME bin_reader_el.read buf ~pos_ref); \
      unsafe_read = (fun sptr_ptr eptr -> \
        Unsafe_read_c.bin_read_/**/NAME \
          bin_reader_el.unsafe_read sptr_ptr eptr); \
      unsafe_vtag_read = (fun _sptr_ptr _eptr _vint -> \
        Unsafe_read_c.raise_variant_wrong_type "NAME"); \
    } \
  let bin_/**/NAME bin_el = \
    { \
      writer = bin_writer_/**/NAME bin_el.writer; \
      reader = bin_reader_/**/NAME bin_el.reader; \
    }

#define MK_BASE2(NAME) \
  let bin_writer_/**/NAME bin_writer_el1 bin_writer_el2 = \
    { \
      size = (fun v -> \
        Size.bin_size_/**/NAME bin_writer_el1.size bin_writer_el2.size v); \
      write = (fun buf ~pos v -> \
        Write_ml.bin_write_/**/NAME \
          bin_writer_el1.write bin_writer_el2.write buf ~pos v); \
      unsafe_write = (fun sptr eptr v -> \
        Unsafe_write_c.bin_write_/**/NAME \
          bin_writer_el1.unsafe_write bin_writer_el2.unsafe_write \
          sptr eptr v); \
    } \
  let bin_reader_/**/NAME bin_reader_el1 bin_reader_el2 = \
    { \
      read = (fun buf ~pos_ref -> \
        Read_ml.bin_read_/**/NAME \
          bin_reader_el1.read bin_reader_el2.read buf ~pos_ref); \
      unsafe_read = (fun sptr_ptr eptr -> \
        Unsafe_read_c.bin_read_/**/NAME \
          bin_reader_el1.unsafe_read bin_reader_el2.unsafe_read \
          sptr_ptr eptr); \
      unsafe_vtag_read = (fun _sptr_ptr _eptr _vint -> \
        Unsafe_read_c.raise_variant_wrong_type "NAME"); \
    } \
  let bin_/**/NAME bin_el1 bin_el2 = \
    { \
      writer = bin_writer_/**/NAME bin_el1.writer bin_el2.writer; \
      reader = bin_reader_/**/NAME bin_el1.reader bin_el2.reader; \
    }

#define MK_BASE3(NAME) \
  let bin_writer_/**/NAME bin_writer_el1 bin_writer_el2 bin_writer_el3 = \
    { \
      size = (fun v -> \
        Size.bin_size_/**/NAME \
          bin_writer_el1.size bin_writer_el2.size bin_writer_el3.size v); \
      write = (fun buf ~pos v -> \
        Write_ml.bin_write_/**/NAME \
          bin_writer_el1.write bin_writer_el2.write \
          bin_writer_el3.write buf ~pos v); \
      unsafe_write = (fun sptr eptr v -> \
        Unsafe_write_c.bin_write_/**/NAME \
          bin_writer_el1.unsafe_write bin_writer_el2.unsafe_write \
          bin_writer_el3.unsafe_write sptr eptr v); \
    } \
  let bin_reader_/**/NAME bin_reader_el1 bin_reader_el2 bin_reader_el3 = \
    { \
      read = (fun buf ~pos_ref -> \
        Read_ml.bin_read_/**/NAME \
          bin_reader_el1.read bin_reader_el2.read \
          bin_reader_el3.read buf ~pos_ref); \
      unsafe_read = (fun sptr_ptr eptr -> \
        Unsafe_read_c.bin_read_/**/NAME \
          bin_reader_el1.unsafe_read bin_reader_el2.unsafe_read \
          bin_reader_el3.unsafe_read sptr_ptr eptr); \
      unsafe_vtag_read = (fun _sptr_ptr _eptr _vint -> \
        Unsafe_read_c.raise_variant_wrong_type "NAME"); \
    } \
  let bin_/**/NAME bin_el1 bin_el2 bin_el3 = \
    { \
      writer = \
        bin_writer_/**/NAME bin_el1.writer bin_el2.writer bin_el3.writer; \
      reader = \
        bin_reader_/**/NAME bin_el1.reader bin_el2.reader bin_el3.reader; \
    }

MK_BASE1(ref)
MK_BASE1(lazy)
MK_BASE1(option)

MK_BASE2(pair)

MK_BASE3(triple)

MK_BASE1(list)
MK_BASE1(array)

MK_BASE2(hashtbl)

MK_BASE(float32_vec)
MK_BASE(float64_vec)
MK_BASE(vec)
MK_BASE(float32_mat)
MK_BASE(float64_mat)
MK_BASE(mat)
MK_BASE(bigstring)
MK_BASE(float_array)
MK_BASE(variant_tag)
MK_BASE(int_8bit)
MK_BASE(int_16bit)
MK_BASE(int_32bit)
MK_BASE(int_64bit)
MK_BASE(int64_bits)

MK_BASE(network16_int)
MK_BASE(network32_int)
MK_BASE(network32_int32)
MK_BASE(network64_int)
MK_BASE(network64_int64)

MK_WRITER_BASE1(array_no_length)


(* Conversion of binable types *)

let cnv_writer cnv tp_class =
  {
    size = (fun v -> tp_class.size (cnv v));
    write = (fun buf ~pos v -> tp_class.write buf ~pos (cnv v));
    unsafe_write = (fun sptr eptr v ->
      tp_class.unsafe_write sptr eptr (cnv v));
  }

let cnv_reader cnv tp_class =
  {
    read = (fun buf ~pos_ref -> cnv (tp_class.read buf ~pos_ref));
    unsafe_read = (fun sptr_ptr eptr ->
      cnv (tp_class.unsafe_read sptr_ptr eptr));
    unsafe_vtag_read = (fun sptr_ptr eptr vtag ->
      cnv (tp_class.unsafe_vtag_read sptr_ptr eptr vtag));
  }

let cnv for_writer for_reader tp_class =
  {
    writer = cnv_writer for_writer tp_class.writer;
    reader = cnv_reader for_reader tp_class.reader;
  }
