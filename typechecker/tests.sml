(* tests.sml *)

val _ = CM.make "sources.cm";

(* If Main isn't in sources.cm, load it explicitly *)
val _ = use "main.sml";

val testDir = "testcases";

fun isTigerFile name = String.isSuffix ".tig" name;

fun runFile filename =
  let
    val fullpath = OS.Path.concat (testDir, filename);
    val _ = print ("Running " ^ fullpath ^ "...\n");
    val _ = Main.main fullpath;
    val _ = print "\n";
  in
    ()
  end;

val _ =
  let
    val dir = OS.FileSys.openDir testDir;

    fun loop () =
      case OS.FileSys.readDir dir of
          NONE => ()
        | SOME name =>
            (if isTigerFile name then runFile name else ();
             loop ());

    val _ = loop ();
    val _ = OS.FileSys.closeDir dir;
  in
    ()
  end;