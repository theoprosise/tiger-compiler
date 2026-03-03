(* tests.sml *)

val _ = CM.make "sources.cm";

(* If Main isn't in sources.cm, load it explicitly *)
val _ = use "main.sml";

val testDir = "testcases";

fun isTigerFile name = String.isSuffix ".tig" name;

(* Expected output for each test case - compare compiler output against these *)
val expectedOutputs : (string * string) list = [
  ("merge.tig", "OK"),
  ("queens.tig", "OK"),
  ("test1.tig", "OK"),
  ("test2.tig", "OK"),
  ("test3.tig", "OK"),
  ("test4.tig", "OK"),
  ("test5.tig", "OK"),
  ("test6.tig", "OK"),
  ("test7.tig", "OK"),
  ("test8.tig", "OK"),
  ("test9.tig", "Error: types of then-else differ"),
  ("test10.tig", "Error : body of while not unit"),
  ("test11.tig", "Error: hi expr is not int, and index variable erroneously assigned to"),
  ("test12.tig", "OK"),
  ("test13.tig", "Error: comparison of incompatible types"),
  ("test14.tig", "Error: compare rec with array"),
  ("test15.tig", "Error: if-then returns non unit"),
  ("test16.tig", "Error: mutually recursive types that do not pass through record or array"),
  ("test17.tig", "Error: definition of recursive types is interrupted"),
  ("test18.tig", "Error: definition of recursive functions is interrupted"),
  ("test19.tig", "Error: second function uses variables local to the first one, undeclared variable"),
  ("test20.tig", "Error: procedure returns value and procedure is used in arexpr"),
  ("test21.tig", "Error: field not in record type"),
  ("test22.tig", "Error: type mismatch"),
  ("test23.tig", "Error: type mismatch"),
  ("test24.tig", "Error: variable not array"),
  ("test25.tig", "Error: variable not record"),
  ("test26.tig", "Error: integer required"),
  ("test27.tig", "OK"),
  ("test28.tig", "Error: different record types"),
  ("test29.tig", "Error: different array types"),
  ("test30.tig", "OK"),
  ("test31.tig", "Error: type constraint and init value differ"),
  ("test32.tig", "Error: initializing exp and array type differ"),
  ("test33.tig", "Error: unknown type"),
  ("test34.tig", "Error: formals and actuals have different types"),
  ("test35.tig", "Error: formals are more than actuals"),
  ("test36.tig", "Error: formals are fewer than actuals"),
  ("test37.tig", "OK"),
  ("test38.tig", "Error: two types with same name in same batch of mutually recursive types"),
  ("test39.tig", "Error: two functions with same name in same batch of mutually recursive functions"),
  ("test40.tig", "Error: procedure returns value"),
  ("test41.tig", "OK"),
  ("test42.tig", "OK"),
  ("test43.tig", "Error: initialize with unit and causing type mismatch in addition"),
  ("test44.tig", "OK"),
  ("test45.tig", "Error: initializing nil expressions not constrained by record type"),
  ("test46.tig", "OK"),
  ("test47.tig", "OK"),
  ("test48.tig", "OK"),
  ("test49.tig", "Error: syntax error, nil should not be preceded by type-id")
];

fun lookupExpected filename =
  case List.find (fn (f, _) => f = filename) expectedOutputs of
      SOME (_, exp) => exp
    | NONE => "??? (no expected output defined)";

fun runFile filename =
  let
    val fullpath = OS.Path.concat (testDir, filename);
    val expected = lookupExpected filename;
    val _ = print ("\n========== " ^ filename ^ " ==========\n");
    val _ = print ("EXPECTED: " ^ expected ^ "\n");
    val _ = print ("ACTUAL:\n");
    val _ = Main.main fullpath;
    val _ = print ("\n");
  in
    ()
  end;

val _ =
  let
    val testFiles = map #1 expectedOutputs;
    val _ = print "===== Running Tiger Compiler Test Cases =====\n";
    val _ = List.app runFile testFiles;
    val _ = print "===== All tests complete - compare ACTUAL vs EXPECTED above =====\n";
  in
    ()
  end;
