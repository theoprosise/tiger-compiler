structure RegAlloc =
struct
  structure F = MipsFrame

  fun alloc (instrs, frame) =
    let
      (* build flowgraph *)
      val (fg, _) = Flow.instrs2graph instrs

      (* build interference graph *)
      val ig = Liveness.interferenceGraph fg

      (* machine registers (temps) *)
      val registers = F.registers

      (* initial coloring: registers map to themselves *)
      val initial =
        foldl (fn (r, tbl) => Temp.Table.enter (tbl, r, r))
              Temp.Table.empty
              registers

      (* simple spill cost *)
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

    in
      (* for now, return original instrs *)
      {instrs = instrs, allocation = allocation}
    end
end