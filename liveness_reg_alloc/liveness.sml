structure Liveness =
struct
  datatype igraph =
    IGRAPH of {
      graph : Graph.graph,
      tnode : Temp.temp -> Graph.node,
      gtemp : Graph.node -> Temp.temp,
      moves : (Graph.node * Graph.node) list
    }

  fun member (x, []) = false
    | member (x, y :: ys) = (x = y) orelse member (x, ys)

  fun add (x, xs) = if member (x, xs) then xs else x :: xs

  fun union (xs, ys) = foldl (fn (x, acc) => add (x, acc)) ys xs

  fun minus (xs, ys) = List.filter (fn x => not (member (x, ys))) xs

  fun equalSet (xs, ys) =
    let
      fun subset ([], _) = true
        | subset (x :: xs, ys) = member (x, ys) andalso subset (xs, ys)
    in
      subset (xs, ys) andalso subset (ys, xs)
    end

  fun lookupTemps (tbl, node) =
    case Graph.Table.look (tbl, node) of
      SOME xs => xs
    | NONE => []

  fun lookupBool (tbl, node) =
    case Graph.Table.look (tbl, node) of
      SOME b => b
    | NONE => false


  fun computeLiveSets (Flow.FGRAPH {control, def, use, ismove}) =
    let
      val nodes = Graph.nodes control

      fun initTable ns =
        foldl (fn (n, tbl) => Graph.Table.enter (tbl, n, []))
              Graph.Table.empty
              ns

      fun step (inTbl, outTbl) =
        let
          fun updateNode (n, (newIn, newOut, changed)) =
            let
              val oldIn = lookupTemps (inTbl, n)
              val oldOut = lookupTemps (outTbl, n)

              val succIns =
                foldl (fn (s, acc) => union (lookupTemps (inTbl, s), acc))
                      []
                      (Graph.succ n)

              val newOutN = succIns
              val newInN =
                union (lookupTemps (use, n),
                       minus (newOutN, lookupTemps (def, n)))

              val changed' =
                changed orelse
                not (equalSet (oldIn, newInN)) orelse
                not (equalSet (oldOut, newOutN))
            in
              ( Graph.Table.enter (newIn, n, newInN),
                Graph.Table.enter (newOut, n, newOutN),
                changed' )
            end

          val (inTbl', outTbl', changed) =
            foldl updateNode (Graph.Table.empty, Graph.Table.empty, false) nodes
        in
          if changed then step (inTbl', outTbl') else (inTbl', outTbl')
        end
    in
      step (initTable nodes, initTable nodes)
    end

  (* ---------- interference graph ---------- *)

  fun interferenceGraph (fg as Flow.FGRAPH {control, def, use, ismove}) =
    let
      val (liveIn, liveOut) = computeLiveSets fg

      val ig = Graph.newGraph ()
      val tempToNode = ref Temp.Table.empty
      val nodeToTemp = ref Graph.Table.empty

      fun ensureTempNode t =
        case Temp.Table.look (!tempToNode, t) of
          SOME n => n
        | NONE =>
            let
              val n = Graph.newNode ig
              val _ = tempToNode := Temp.Table.enter (!tempToNode, t, n)
              val _ = nodeToTemp := Graph.Table.enter (!nodeToTemp, n, t)
            in
              n
            end

      fun gtemp n =
        case Graph.Table.look (!nodeToTemp, n) of
          SOME t => t
        | NONE => ErrorMsg.impossible "Liveness.gtemp: unknown node"

      fun tnode t = ensureTempNode t

      fun addIGEdge (a, b) =
        if a = b then ()
        else
          let
            val na = ensureTempNode a
            val nb = ensureTempNode b
          in
            (Graph.mk_edge {from = na, to = nb}; ())
            handle Graph.GraphEdge => ()
          end

      val nodes = Graph.nodes control

      fun collectMoves [] acc = acc
        | collectMoves (n :: ns) acc =
            if lookupBool (ismove, n) then
              case (lookupTemps (def, n), lookupTemps (use, n)) of
                ([d], [u]) => collectMoves ns ((ensureTempNode d, ensureTempNode u) :: acc)
              | _ => collectMoves ns acc
            else
              collectMoves ns acc

      val moves = collectMoves nodes []

      fun processNode n =
        let
          val defs = lookupTemps (def, n)
          val outs = lookupTemps (liveOut, n)
          val isMove = lookupBool (ismove, n)
          val uses = lookupTemps (use, n)

          val outsForInterference =
            if isMove then minus (outs, uses) else outs

          fun addForDef d =
            app (fn t => addIGEdge (d, t)) outsForInterference
        in
          app addForDef defs
        end

      val _ = app processNode nodes
    in
      IGRAPH {
        graph = ig,
        tnode = tnode,
        gtemp = gtemp,
        moves = moves
      }
    end


  fun show (outstream, fg as Flow.FGRAPH {control, def, use, ismove}) =
    let
      val (liveIn, liveOut) = computeLiveSets fg
      val nodes = Graph.nodes control

      fun say s = TextIO.output (outstream, s)

      fun showTemps ts =
        let
          fun loop [] = ""
            | loop [t] = Temp.makestring t
            | loop (t :: ts) = Temp.makestring t ^ " " ^ loop ts
        in
          "{" ^ loop ts ^ "}"
        end

      fun showBool true = "true"
        | showBool false = "false"

      fun showNode n =
        let
          val name = Graph.nodename n
          val defs = lookupTemps (def, n)
          val uses = lookupTemps (use, n)
          val ins = lookupTemps (liveIn, n)
          val outs = lookupTemps (liveOut, n)
          val mv = lookupBool (ismove, n)
        in
          say (name ^ "\n");
          say ("  def    = " ^ showTemps defs ^ "\n");
          say ("  use    = " ^ showTemps uses ^ "\n");
          say ("  in     = " ^ showTemps ins ^ "\n");
          say ("  out    = " ^ showTemps outs ^ "\n");
          say ("  ismove = " ^ showBool mv ^ "\n")
        end
    in
      app showNode nodes
    end
end