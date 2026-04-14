structure Color =
struct
  type allocation = Temp.temp Temp.Table.table

  fun tempLookup (tbl, t) =
    case Temp.Table.look (tbl, t) of
      SOME x => SOME x
    | NONE => NONE

  fun memberNode (x, []) = false
  | memberNode (x, y :: ys) = Graph.eq (x, y) orelse memberNode (x, ys)

  fun memberTemp (x, []) = false
    | memberTemp (x, y :: ys) = (x = y) orelse memberTemp (x, ys)

  fun addTemp (x, xs) = if memberTemp (x, xs) then xs else x :: xs

  fun removeNode (x, xs) = List.filter (fn y => y <> x) xs

  fun removeTemps (xs, ys) = List.filter (fn x => not (memberTemp (x, ys))) xs

  fun lookupAlloc (alloc, t) =
    case Temp.Table.look (alloc, t) of
      SOME r => SOME r
    | NONE => NONE

  fun isPrecolored (initial, t) =
    case lookupAlloc (initial, t) of
      SOME _ => true
    | NONE => false

  fun nodeTemp (gtemp, n) = gtemp n

  fun nodeNeighbors n =
    Graph.adj n

  fun filteredNeighbors (removed, n) =
    List.filter (fn m => not (memberNode (m, removed))) (nodeNeighbors n)

  fun degree (removed, n) =
    length (filteredNeighbors (removed, n))

  fun findSimplifiable (k, initial, gtemp, removed, nodes) =
    let
      fun ok n =
        let
          val t = nodeTemp (gtemp, n)
        in
          (not (isPrecolored (initial, t))) andalso degree (removed, n) < k
        end
    in
      List.find ok nodes
    end

  fun findSpillCandidate (initial, gtemp, removed, spillCost, nodes) =
    let
      fun candidates [] best = best
        | candidates (n :: ns) best =
            let
              val t = nodeTemp (gtemp, n)
            in
              if isPrecolored (initial, t) orelse memberNode (n, removed) then
                candidates ns best
              else
                case best of
                  NONE => candidates ns (SOME n)
                | SOME b =>
                    if spillCost n < spillCost b then
                      candidates ns (SOME n)
                    else
                      candidates ns best
            end
    in
      candidates nodes NONE
    end

  fun buildStack (k, initial, gtemp, spillCost, nodes) =
    let
      fun loop (removed, stack) =
        let
          val remaining = List.filter (fn n => not (memberNode (n, removed))) nodes
        in
          if null remaining then
            stack
          else
            case findSimplifiable (k, initial, gtemp, removed, remaining) of
              SOME n => loop (n :: removed, n :: stack)
            | NONE =>
                (case findSpillCandidate (initial, gtemp, removed, spillCost, remaining) of
                    SOME n => loop (n :: removed, n :: stack)
                  | NONE => stack)
        end
    in
      loop ([], [])
    end

  fun usedColorsOfNeighbors (alloc, initial, gtemp, neighbors) =
    let
      fun getColor n =
        let
          val t = gtemp n
        in
          case lookupAlloc (alloc, t) of
            SOME c => SOME c
          | NONE =>
              (case lookupAlloc (initial, t) of
                 SOME c => SOME c
               | NONE => NONE)
        end

      fun loop [] used = used
        | loop (n :: ns) used =
            (case getColor n of
               SOME c => loop ns (addTemp (c, used))
             | NONE => loop ns used)
    in
      loop neighbors []
    end

  fun chooseColor (registers, used) =
    List.find (fn r => not (memberTemp (r, used))) registers

  fun selectColors (stack, initial, gtemp, registers) =
    let
      fun colorOne (n, (alloc, spills)) =
        let
          val t = gtemp n
        in
          if isPrecolored (initial, t) then
            (alloc, spills)
          else
            let
              val used = usedColorsOfNeighbors (alloc, initial, gtemp, nodeNeighbors n)
            in
              case chooseColor (registers, used) of
                SOME c => (Temp.Table.enter (alloc, t, c), spills)
              | NONE => (alloc, n :: spills)
            end
        end
    in
      foldl colorOne (initial, []) stack
    end

  fun color {interference, initial, spillCost, registers} =
    let
      val Liveness.IGRAPH {graph, gtemp, tnode, moves} = interference
      val nodes = Graph.nodes graph
      val k = length registers
      val stack = buildStack (k, initial, gtemp, spillCost, nodes)
      val (allocation, spills) = selectColors (stack, initial, gtemp, registers)
    in
      {allocation = allocation, spills = spills}
    end
end