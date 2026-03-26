structure Main =
struct
    fun showFrag frag =
        case frag of
            Translate.PROC {body, frame} =>
              (
                print "=== PROC ===\n";
                Printtree.printtree (TextIO.stdOut, body);
                print "\n"
              )
          | Translate.STRING (lab, s) =>
              (
                print "=== STRING ===\n";
                print ("label: " ^ Symbol.name lab ^ "\n");
                print ("value: " ^ s ^ "\n\n")
              )

    fun main (filename : string) : unit =
        let
            val _ = Translate.resetResult ()
            val ast = Parse.parse filename
            val _ = Semant.transProg ast
            val frags = Translate.getResult ()
        in
            app showFrag frags
        end
end