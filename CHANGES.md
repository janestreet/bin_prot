## 111.03.00

- Fixed build on ARM.

## 109.53.00

- Bump version number

## 109.47.00

- Compilation fix for 32-bit systems

## 109.44.00

- Remove "unwrapped" pointers used by `Bin_prot`, with the bug from
  109.41 fixed.

    Unwrapped pointers cannot coexist with the remove-page-table
    optimization.

    Removed all the C stubs for reading/writing and used instead either
    the new primitives of the next OCaml or standard OCaml code
    reading/writing integers byte by byte.

    Since we don't have unsafe/safe functions anymore but only safe
    ones, removed all the `bin_{read,write}_t_` functions.

    Also renamed `bin_read_t__` to `__bin_read_t__` for the same reason
    as sexplib: to avoid confusion with the function generated for `t_`
    and hide it in the toplevel.

## 109.42.00

- Backed out the changes introduced in 109.41

## 109.41.00

- Remove all uses of "unwrapped" pointers

    Unwrapped pointers cannot coexist with the remove-page-table
    optimization.

    Removed all the C stubs for reading/writing and used instead either
    the new primitives of the next OCaml or standard OCaml code
    reading/writing integers byte by byte.

    Since we don't have unsafe/safe functions anymore but only safe ones,
    removed all the `bin_{read,write}_t_` functions.

    Also renamed `bin_read_t__` to `__bin_read_t__` for the same reason as
    sexplib: to avoid confusion with the function generated for `t_` and
    hide it in the toplevel.

## 109.10.00

- Improved error messages in presence of GADTs.

## 2012-07-15

- Rewrote README in Markdown and improved documentation.
- Eliminated new warnings available in OCaml 4.00.

## 2012-02-28

- Improved portability by better supporting the C99-standard and
  non-GNU compilers.

## 2011-11-10

- Improved portability to older glibc distributions.

## 2011-09-15

- Fixes to improve package dependency resolution.

## 2011-07-04

- Internal updates to sync with Jane Street.

## 2011-06-29

- Fixed bigstring layout bug, which should only affect value
  comparisons with OCaml 3.12.1 or later.
- Made 64-bit detection more reliable on Mac OS X.

## 2010-03-20

- Fixed linking of toplevels to require bigarrays.
- Improved compilation on Mac OS X.

## 2010-03-17

- Fixed small name capture bug.

## 2009-12-21

- Updated contact information.

## 2009-09-19

- Added missing type cases for supporting variant types.
- Fixed handling of variance annotations.

## 2009-07-27

- Fixed build problem with gcc 4.4 due to stricter checking
  for empty macro arguments.

    Thanks to Nobuyuki Tomiza <nobuyuki.tomizawa@gmail.com>
    for the patch!

## 2009-07-20

- Merged tiny Jane Street improvements.

## 2009-07-03

- Made byte swapping more portable.

## 2009-07-02

- Added support for network byte order integers.

## 2009-04-22

- Added macro support for all kinds of vectors (vec,
  float32_vec, float64_vec) and matrices (mat, float32_mat,
  float64_mat), and for bigstrings (bigstring).

## 2009-04-16

- Fixed a bug leading to an exception when writing
  extremely large values (>4 GB buffer size).  Does not cause
  data corruption.

