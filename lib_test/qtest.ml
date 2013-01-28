(** Regression test runner. *)

open Core.Std;;

let tests = Qtest_lib.Std.Test.tests_of_ounit Test.all

let () =
  let module Runner = Qtest_lib.Std.Runner.Make(Version_util) in
  Runner.main tests
