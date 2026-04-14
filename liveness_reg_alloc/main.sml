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
         val instrs = List.concat (map (Mips.codegen frame) stms')

         val {instrs = finalInstrs, allocation = allocation} =
             RegAlloc.alloc (instrs, frame)

         val format0 = Assem.format (tempName allocation)

      in
         app (fn i => TextIO.output(out, format0 i)) finalInstrs
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
       in
           withOpenFile (filename ^ ".s")
             (fn out => app (emitproc out) frags)
       end

   fun main filename = compile filename

end