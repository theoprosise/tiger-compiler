To make:
CM.make "sources.cm";

Parse tokens on a file:
Parse.parse "test.tig";

Function to parse and print for a test file:
fun dump file =
  let val ast = Parse.parse file
  in PrintAbsyn.print (TextIO.stdOut, ast)
  end;

dump "test.tig";


Check positions:
python3 show_positions.py test.tig

ML-Yacc may be used from the interactive system or built as a stand-alone program which may be run from the Unix command line. See the file README in the mlyacc directory for directions on installing ML-Yacc. We recommend that ML-Yacc be installed as a stand-alone program.

If you are using the stand-alone version of ML-Yacc, invoke the program ``sml-yacc'' with the name of the specifcation file. If you are using ML-Yacc in the interactive system, load the file ``smlyacc.sml''. The end result is a structure ParseGen, with one value parseGen in it. Apply parseGen to a string containing the name of the specification file.

Two files will be created, one named by attaching ``.sig'' to the name of the specification, the other named by attaching ``.sml'' to the name of the specification.

