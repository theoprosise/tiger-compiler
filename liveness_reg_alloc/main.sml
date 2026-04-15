structure Main = struct

   structure Tr = Translate
   structure F = MipsFrame

   fun getsome (SOME x) = x

   fun tempName allocation t =
     case Temp.Table.look (allocation, t) of
       SOME regtemp =>
         (case Temp.Table.look (F.tempMap, regtemp) of
            SOME name => name
          | NONE => Temp.makestring regtemp)
     | NONE =>
         (case Temp.Table.look (F.tempMap, t) of
            SOME name => name
          | NONE => Temp.makestring t)

   fun emitproc out (Tr.PROC{body,frame}) =
     let
         val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")

         val stms = Canon.linearize body
         val stms' = Canon.traceSchedule (Canon.basicBlocks stms)
         val rawInstrs = List.concat (map (Mips.codegen frame) stms')
         val instrs = F.procEntryExit2 (frame, rawInstrs)

         val {instrs = finalInstrs, allocation = allocation} =
             RegAlloc.alloc (instrs, frame)

         val {prolog, body = framedBody, epilog} =
             F.procEntryExit3 (frame, finalInstrs)

         val format0 = Assem.format (tempName allocation)

      in
         ((case framedBody of
              firstInstr :: rest =>
                (case firstInstr of
                    Assem.LABEL _ =>
                      (TextIO.output (out, format0 firstInstr);
                       TextIO.output (out, prolog);
                       app (fn i => TextIO.output (out, format0 i)) rest)
                  | _ =>
                      (TextIO.output (out, prolog);
                       app (fn i => TextIO.output (out, format0 i)) framedBody))
            | [] =>
                 TextIO.output (out, prolog));
          TextIO.output (out, epilog))
     end
     | emitproc out (Tr.STRING(lab,s)) =
         TextIO.output(out, F.string(lab,s))

   fun withOpenFile fname f =
       let
           val out = TextIO.openOut fname
       in
           (f out before TextIO.closeOut out)
           handle e => (TextIO.closeOut out; raise e)
       end

   fun compile filename =
       let
           val absyn = Parse.parse filename
           val _ = Tr.resetResult ()
           val _ = Semant.transProg absyn
           val frags = Tr.getResult ()
           datatype section = TEXT | DATA
           val currentSection = ref TEXT

           fun switchSection (out, section) =
             if !currentSection = section then
               ()
             else
               let
                 val directive =
                   case section of
                     TEXT => ".text\n"
                   | DATA => ".data\n"
               in
                 currentSection := section;
                 TextIO.output (out, directive)
               end

           fun emitfrag out frag =
             (case frag of
                 Tr.PROC _ => switchSection (out, TEXT)
               | Tr.STRING _ => switchSection (out, DATA);
              emitproc out frag)
        in
            withOpenFile (filename ^ ".s")
              (fn out =>
                  (TextIO.output (out, ".text\n.globl main\n.globl tigermain\nmain:\n");
                   TextIO.output (out, "jal tigermain\n");
                   TextIO.output (out, "move $a0, $v0\n");
                   TextIO.output (out, "li $v0, 1\n");
                   TextIO.output (out, "syscall\n");
                   TextIO.output (out, "li $v0, 10\n");
                   TextIO.output (out, "syscall\n");
                   app (emitfrag out) frags))
        end

   fun main filename = compile filename

end
