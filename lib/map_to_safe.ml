type 'a reader = 'a Read_ml.reader
type ('a, 'b) reader1 = 'a Unsafe_read_c.reader -> 'b Read_ml.reader
type ('a, 'b, 'c) reader2 = 'a Unsafe_read_c.reader -> ('b, 'c) reader1
type ('a, 'b, 'c, 'd) reader3 = 'a Unsafe_read_c.reader -> ('b, 'c, 'd) reader2

type 'a writer = 'a Write_ml.writer
type ('a, 'b) writer1 = 'a Unsafe_write_c.writer -> 'b Write_ml.writer
type ('a, 'b, 'c) writer2 = 'a Unsafe_write_c.writer -> ('b, 'c) writer1
type ('a, 'b, 'c, 'd) writer3 = 'a Unsafe_write_c.writer -> ('b, 'c, 'd) writer2
