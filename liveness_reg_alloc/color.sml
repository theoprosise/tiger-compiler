structure Color =
struct
  type allocation = Temp.temp Temp.Table.table

  fun memberNode (x, []) = false
    | memberNode (x, y :: ys) = Graph.eq (x, y) orelse memberNode (x, ys)

  fun memberTemp (x, []) = false
    | memberTemp (x, y :: ys) = (x = y) orelse memberTemp (x, ys)

  fun addNode (x, xs) = if memberNode (x, xs) then xs else x :: xs
  fun addTemp (x, xs) = if memberTemp (x, xs) then xs else x :: xs

  fun uniqueNodes xs = foldl (fn (x, acc) => addNode (x, acc)) [] xs

  fun lookupAlloc (alloc, t) = Temp.Table.look (alloc, t)

  fun isPrecolored (initial, t) =
    case lookupAlloc (initial, t) of
      SOME _ => true
    | NONE => false

  fun nodeMapLook [] _ = NONE
    | nodeMapLook ((k, v) :: rest) x =
        if Graph.eq (k, x) then SOME v else nodeMapLook rest x

  fun nodeMapEnter (m, k, v) =
    let
      fun loop [] = [(k, v)]
        | loop ((k', v') :: rest) =
            if Graph.eq (k, k') then (k, v) :: rest
            else (k', v') :: loop rest
    in
      loop m
    end

  fun makeFind parentRef =
    let
      fun find n =
        case nodeMapLook (!parentRef) n of
          NONE => n
        | SOME p =>
            if Graph.eq (p, n) then n
            else
              let
                val r = find p
                val _ = parentRef := nodeMapEnter (!parentRef, n, r)
              in
                r
              end
    in
      find
    end

  fun nodeNeighbors n = Graph.adj n

  fun color {interference, initial, spillCost, registers} =
    let
      val Liveness.IGRAPH {graph, gtemp, tnode, moves} = interference
      val allNodes = Graph.nodes graph
      val k = length registers

      val parentRef =
        ref (map (fn n => (n, n)) allNodes)

      val find = makeFind parentRef

      fun union (a, b) =
        let
          val ra = find a
          val rb = find b
        in
          if Graph.eq (ra, rb) then ()
          else parentRef := nodeMapEnter (!parentRef, rb, ra)
        end

      fun reps () = uniqueNodes (map find allNodes)

      fun members rep =
        List.filter (fn n => Graph.eq (find n, rep)) allNodes

      fun repNeighbors rep =
        let
          val raw =
            List.concat (map nodeNeighbors (members rep))

          val repd =
            map find raw

          val filtered =
            List.filter (fn n => not (Graph.eq (n, rep))) repd
        in
          uniqueNodes filtered
        end

      fun adjacentRep (a, b) =
        memberNode (b, repNeighbors a)

      fun degree (removed, rep) =
        length (List.filter (fn n => not (memberNode (n, removed)))
                            (repNeighbors rep))

      fun conservative (a, b) =
        let
          val neigh = uniqueNodes (repNeighbors a @ repNeighbors b)
          val high =
            List.filter (fn n => degree ([], n) >= k) neigh
        in
          length high < k
        end

      fun coalesceOne (u, v) =
        let
          val ru = find u
          val rv = find v
          val tu = gtemp ru
          val tv = gtemp rv
        in
          if Graph.eq (ru, rv) then ()
          else if adjacentRep (ru, rv) then ()
          else if isPrecolored (initial, tu) andalso isPrecolored (initial, tv) then ()
          else if conservative (ru, rv) then union (ru, rv)
          else ()
        end

      val _ = app coalesceOne moves

      (* ---------- simplify / select on representative graph ---------- *)

      fun repTemp rep = gtemp rep

      fun findSimplifiable (removed, repsLeft) =
        let
          fun ok rep =
            let
              val t = repTemp rep
            in
              (not (isPrecolored (initial, t))) andalso degree (removed, rep) < k
            end

          fun loop [] = NONE
            | loop (r :: rs) = if ok r then SOME r else loop rs
        in
          loop repsLeft
        end

      fun findSpillCandidate (removed, repsLeft) =
        let
          fun loop [] best = best
            | loop (r :: rs) best =
                let
                  val t = repTemp r
                in
                  if isPrecolored (initial, t) orelse memberNode (r, removed) then
                    loop rs best
                  else
                    case best of
                      NONE => loop rs (SOME r)
                    | SOME b =>
                        if spillCost r < spillCost b then
                          loop rs (SOME r)
                        else
                          loop rs best
                end
        in
          loop repsLeft NONE
        end

      fun buildStack () =
        let
          fun loop (removed, stack) =
            let
              val rs = List.filter (fn r => not (memberNode (r, removed))) (reps ())
            in
              if null rs then
                stack
              else
                case findSimplifiable (removed, rs) of
                  SOME r => loop (r :: removed, r :: stack)
                | NONE =>
                    (case findSpillCandidate (removed, rs) of
                        SOME r => loop (r :: removed, r :: stack)
                      | NONE => stack)
            end
        in
          loop ([], [])
        end

      fun usedColorsOfNeighbors (repAlloc, rep) =
        let
          fun getColor r =
            let
              val t = repTemp r
            in
              case lookupAlloc (repAlloc, t) of
                SOME c => SOME c
              | NONE => lookupAlloc (initial, t)
            end

          fun loop [] used = used
            | loop (r :: rs) used =
                (case getColor r of
                    SOME c => loop rs (addTemp (c, used))
                  | NONE => loop rs used)
        in
          loop (repNeighbors rep) []
        end

      fun chooseColor used =
        let
          fun loop [] = NONE
            | loop (r :: rs) =
                if memberTemp (r, used) then loop rs else SOME r
        in
          loop registers
        end

      val stack = buildStack ()

      fun selectColors stack =
        let
          fun colorOne (rep, (repAlloc, spills)) =
            let
              val t = repTemp rep
            in
              if isPrecolored (initial, t) then
                (repAlloc, spills)
              else
                let
                  val used = usedColorsOfNeighbors (repAlloc, rep)
                in
                  case chooseColor used of
                    SOME c => (Temp.Table.enter (repAlloc, t, c), spills)
                  | NONE => (repAlloc, rep :: spills)
                end
            end
        in
          foldl colorOne (initial, []) stack
        end

      val (repAllocation, spills) = selectColors stack

      fun colorOfRep rep =
        let
          val t = repTemp rep
        in
          case lookupAlloc (repAllocation, t) of
            SOME c => SOME c
          | NONE => lookupAlloc (initial, t)
        end

      fun expandOne (n, alloc) =
        let
          val t = gtemp n
          val rep = find n
        in
          case colorOfRep rep of
            SOME c => Temp.Table.enter (alloc, t, c)
          | NONE => alloc
        end

      val allocation = foldl expandOne Temp.Table.empty allNodes
    in
      {allocation = allocation, spills = spills}
    end
end