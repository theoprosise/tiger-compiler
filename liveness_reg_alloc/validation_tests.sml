(* validation_tests.sml
 *
 * Small backend-oriented validation suite.
 * Each case writes a local .tig file and runs it through Main.main.
 * The intent is to smoke-test simple edge cases without depending on
 * the external "testcases/" corpus used by tests.sml.
 *)

val _ = CM.make "sources.cm";
val _ = use "main.sml";

val validationDir = "validation_cases";

type validation_case =
  { name : string
  , expected : string
  , source : string
  };

val validationCases : validation_case list =
  [ { name = "validation_const"
    , expected = "Returns 42"
    , source =
        "42\n"
    }
  , { name = "validation_if_else"
    , expected = "Returns 7"
    , source =
        "if 1 then 7 else 9\n"
    }
  , { name = "validation_while_sum"
    , expected = "Returns 6"
    , source =
        "let\n\
        \  var i := 0\n\
        \  var sum := 0\n\
        \in\n\
        \  (while i < 4 do\n\
        \     (sum := sum + i;\n\
        \      i := i + 1);\n\
        \   sum)\n\
        \end\n"
    }
  , { name = "validation_many_args"
    , expected = "Returns 108"
    , source =
        "let\n\
        \  function f(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int) : int =\n\
        \    let\n\
        \      var x1 := a + b\n\
        \      var x2 := c + d\n\
        \      var x3 := e + f\n\
        \      var x4 := g + h\n\
        \      var x5 := x1 + x2\n\
        \      var x6 := x3 + x4\n\
        \      var x7 := x5 + x6\n\
        \      var x8 := x7 + a + c + e + g\n\
        \      var x9 := x8 + b + d + f + h\n\
        \      var x10 := x9 + x1 + x2 + x3 + x4\n\
        \    in\n\
        \      x10\n\
        \    end\n\
        \in\n\
        \  f(1,2,3,4,5,6,7,8)\n\
        \end\n"
    }
  , { name = "validation_record"
    , expected = "Returns 3"
    , source =
        "let\n\
        \  type pair = {x:int, y:int}\n\
        \  var p := pair{x=1, y=2}\n\
        \in\n\
        \  p.x + p.y\n\
        \end\n"
    }
  , { name = "validation_array"
    , expected = "Returns 15"
    , source =
        "let\n\
        \  type intArray = array of int\n\
        \  var a := intArray[3] of 5\n\
        \in\n\
        \  a[0] + a[1] + a[2]\n\
        \end\n"
    }
  , { name = "validation_string_literal"
    , expected = "Returns 0 and emits a STRING fragment in .data"
    , source =
        "let\n\
        \  var s := \"hi\"\n\
        \in\n\
        \  0\n\
        \end\n"
    }
  ];

fun writeFile (path, contents) =
  let
    val out = TextIO.openOut path
  in
    (TextIO.output (out, contents) before TextIO.closeOut out)
    handle e => (TextIO.closeOut out; raise e)
  end;

fun runCase ({name, expected, source} : validation_case) =
  let
    val tigPath = OS.Path.concat (validationDir, name ^ ".tig")
    val asmPath = tigPath ^ ".s"
    val _ = writeFile (tigPath, source)
    val _ = print ("\n===== " ^ name ^ " =====\n")
    val _ = print ("EXPECTED: " ^ expected ^ "\n")
    val _ = Main.main tigPath
    val _ = print ("GENERATED: " ^ asmPath ^ "\n")
  in
    ()
  end;

val _ =
  (OS.FileSys.mkDir validationDir handle OS.SysErr _ => ();
   print "===== Running validation suite =====\n";
   List.app runCase validationCases;
   print "===== Validation suite complete =====\n");
