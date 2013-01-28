open OUnit

let all =
  TestList
    [
      Bin_prot_test.ML.test;
      Bin_prot_test.C.test;
      Bin_prot_test.Common.test;
    ]
