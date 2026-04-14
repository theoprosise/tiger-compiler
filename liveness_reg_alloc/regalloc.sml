structure RegAlloc =
struct
  structure F = MipsFrame

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

  fun alloc (instrs, frame) =
    let
      val (fg, _) = Flow.instrs2graph instrs
      val ig = Liveness.interferenceGraph fg

      (* registers allocator may assign *)
      val registers = F.registers

      (* machine registers with fixed identities *)
      val precolored =
        F.registers @ F.argregs @ [F.FP, F.SP, F.RA, F.RV]

      val initial =
        addPrecolored (precolored, Temp.Table.empty)

      fun spillCost n = 1

      val {allocation, spills} =
        Color.color {
          interference = ig,
          initial = initial,
          spillCost = spillCost,
          registers = registers
        }

      val _ =
        if null spills then
          print "No spills\n"
        else
          print ("Spills: " ^ Int.toString (length spills) ^ "\n")

      val cleanedInstrs = removeUselessMoves (instrs, allocation)
    in
      {instrs = cleanedInstrs, allocation = allocation}
    end
end