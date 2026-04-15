structure RegAlloc =
struct
  structure F = MipsFrame
  structure T = Tree

  fun accessOffset access =
    case F.exp access (T.TEMP F.FP) of
      T.MEM (T.BINOP (T.PLUS, T.TEMP fp, T.CONST k)) =>
        if fp = F.FP then
          k
        else
          ErrorMsg.impossible "spill slot not based on FP"
    | _ =>
        ErrorMsg.impossible "spill slot is not an FP-relative memory access"

  fun asmInt i =
    if i < 0 then
      "-" ^ Int.toString (~i)
    else
      Int.toString i

  fun sameColor allocation (t1, t2) =
    case (Temp.Table.look (allocation, t1), Temp.Table.look (allocation, t2)) of
      (SOME r1, SOME r2) => (r1 = r2)
    | _ => false

  fun removeUselessMoves (instrs, allocation) =
    List.filter
      (fn instr =>
          case instr of
            Assem.MOVE {dst, src, ...} => not (sameColor allocation (dst, src))
          | _ => true)
      instrs

  fun addPrecolored (regs, tbl) =
    foldl (fn (r, acc) => Temp.Table.enter (acc, r, r)) tbl regs

  fun rewriteInstrs (instrs, frame, spilledTemps) =
    let
      fun memberTemp (t, ts) = List.exists (fn t' => t' = t) ts

      fun addUnique (t, ts) =
        if memberTemp (t, ts) then ts else t :: ts

      fun uniqueTemps ts = rev (foldl (fn (t, acc) => addUnique (t, acc)) [] ts)

      fun allocSlot (t, slots) =
        Temp.Table.enter (slots, t, F.allocLocal frame true)

      val slotTable = foldl allocSlot Temp.Table.empty spilledTemps

      fun slotOffset t =
        case Temp.Table.look (slotTable, t) of
          SOME slot => accessOffset slot
        | NONE => ErrorMsg.impossible "missing spill slot"

      fun loadInstr (fresh, offset) =
        Assem.OPER
          { assem = "lw `d0, " ^ asmInt offset ^ "(`s0)\n"
          , dst = [fresh]
          , src = [F.FP]
          , jump = NONE
          }

      fun storeInstr (fresh, offset) =
        Assem.OPER
          { assem = "sw `s0, " ^ asmInt offset ^ "(`s1)\n"
          , dst = []
          , src = [fresh, F.FP]
          , jump = NONE
          }

      fun rewriteInstr instr =
        let
          fun replacementTable temps =
            let
              val spilledHere =
                uniqueTemps (List.filter (fn t => memberTemp (t, spilledTemps)) temps)

              fun bind ([], _, tbl) = tbl
                | bind (_, [], _) =
                    ErrorMsg.impossible "not enough scratch registers for spill rewrite"
                | bind (t :: ts, r :: rs, tbl) =
                    bind (ts, rs, Temp.Table.enter (tbl, t, r))
            in
              bind (spilledHere, F.callersaves, Temp.Table.empty)
            end

          fun replaceTemp (tbl, t) =
            case Temp.Table.look (tbl, t) of
              SOME scratch => scratch
            | NONE => t

          fun loadsFor (tbl, srcTemps) =
            let
              fun build t =
                case Temp.Table.look (tbl, t) of
                  SOME scratch => SOME (loadInstr (scratch, slotOffset t))
                | NONE => NONE
            in
              List.mapPartial build
                (uniqueTemps (List.filter (fn t => memberTemp (t, spilledTemps)) srcTemps))
            end

          fun storesFor (tbl, dstTemps) =
            let
              fun build t =
                case Temp.Table.look (tbl, t) of
                  SOME scratch => SOME (storeInstr (scratch, slotOffset t))
                | NONE => NONE
            in
              List.mapPartial build
                (uniqueTemps (List.filter (fn t => memberTemp (t, spilledTemps)) dstTemps))
            end

          fun rewriteTemps (tbl, temps) = map (fn t => replaceTemp (tbl, t)) temps
        in
          case instr of
            Assem.OPER {assem, dst, src, jump} =>
              let
                val tbl = replacementTable (src @ dst)
                val loads = loadsFor (tbl, src)
                val stores = storesFor (tbl, dst)
                val instr' =
                  Assem.OPER
                    { assem = assem
                    , dst = rewriteTemps (tbl, dst)
                    , src = rewriteTemps (tbl, src)
                    , jump = jump
                    }
              in
                loads @ (instr' :: stores)
              end
          | Assem.MOVE {assem, dst, src} =>
              let
                val tbl = replacementTable [src, dst]
                val loads = loadsFor (tbl, [src])
                val stores = storesFor (tbl, [dst])
                val instr' =
                  Assem.MOVE
                    { assem = assem
                    , dst = replaceTemp (tbl, dst)
                    , src = replaceTemp (tbl, src)
                    }
              in
                loads @ (instr' :: stores)
              end
          | Assem.LABEL _ =>
              [instr]
        end
    in
      List.concat (map rewriteInstr instrs)
    end

  fun alloc (instrs, frame) =
    let
      (* registers allocator may assign *)
      val registers = F.registers

      (* machine registers with fixed identities *)
      val precolored =
        F.registers @ F.argregs @ [F.FP, F.SP, F.RA, F.RV]

      val initial =
        addPrecolored (precolored, Temp.Table.empty)

      fun spillCost n = 1

      fun loop instrs =
        let
          val (fg, _) = Flow.instrs2graph instrs
          val ig as Liveness.IGRAPH {gtemp, ...} = Liveness.interferenceGraph fg

          val {allocation, spills} =
            Color.color {
              interference = ig,
              initial = initial,
              spillCost = spillCost,
              registers = registers
            }
        in
          if null spills then
            (print "No spills\n";
             {instrs = removeUselessMoves (instrs, allocation), allocation = allocation})
          else
            let
              val spilledTemps = map gtemp spills
              val _ = print ("Spills: " ^ Int.toString (length spilledTemps) ^ "\n")
              val rewritten = rewriteInstrs (instrs, frame, spilledTemps)
            in
              loop rewritten
            end
        end
    in
      loop instrs
    end
end
