structure Flow =
struct
    datatype flowgraph = FGRAPH of {control: Graph.graph,
				    def: Temp.temp list Graph.Table.table,
				    use: Temp.temp list Graph.Table.table,
				    ismove: bool Graph.Table.table}

  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)
    fun instrs2graph (instrs : Assem.instr list) =
        let
            val g = Graph.newGraph()

            (* one node per instruction, in program order *)
            val nodes = map (fn _ => Graph.newNode g) instrs

            (* pair each node with its instruction *)
            val nodeInstrs = ListPair.zip (nodes, instrs)

            fun addEdge (from, to) =
                Graph.mk_edge {from = from, to = to}
                handle Graph.GraphEdge => ()

            fun defsOf instr =
                case instr of
                    Assem.OPER {dst, ...} => dst
                  | Assem.MOVE {dst, ...} => [dst]
                  | Assem.LABEL _ => []

            fun usesOf instr =
                case instr of
                    Assem.OPER {src, ...} => src
                  | Assem.MOVE {src, ...} => [src]
                  | Assem.LABEL _ => []

            fun isMove instr =
                case instr of
                    Assem.MOVE _ => true
                  | _ => false

            (* label -> node map *)
            val labelMap =
                foldl
                  (fn ((node, instr), tbl) =>
                        case instr of
                            Assem.LABEL {lab, ...} => Symbol.enter (tbl, lab, node)
                          | _ => tbl)
                  Symbol.empty
                  nodeInstrs

            fun lookupLabel lab =
                case Symbol.look (labelMap, lab) of
                    SOME n => n
                  | NONE => ErrorMsg.impossible ("unknown label in flowgraph: " ^ Symbol.name lab)

            (* build tables *)

            val defTable =
                foldl
                  (fn ((node, instr), tbl) =>
                        Graph.Table.enter (tbl, node, defsOf instr))
                  Graph.Table.empty
                  nodeInstrs

            val useTable =
                foldl
                  (fn ((node, instr), tbl) =>
                        Graph.Table.enter (tbl, node, usesOf instr))
                  Graph.Table.empty
                  nodeInstrs

            val ismoveTable =
                foldl
                  (fn ((node, instr), tbl) =>
                        Graph.Table.enter (tbl, node, isMove instr))
                  Graph.Table.empty
                  nodeInstrs

            (* add control-flow edges *)

            fun addFlowEdges [] = ()
                  | addFlowEdges [(n, instr)] =
                        (case instr of
                              Assem.OPER {jump = SOME labs, ...} =>
                              app (fn lab => addEdge (n, lookupLabel lab)) labs
                        | _ => ())
                  | addFlowEdges ((n1, instr1) :: rest) =
                        let
                              val _ =
                              case instr1 of
                                    Assem.OPER {jump = SOME labs, ...} =>
                                          app (fn lab => addEdge (n1, lookupLabel lab)) labs
                                    | _ =>
                                          (case rest of
                                          (n2, _) :: _ => addEdge (n1, n2)
                                          | [] => ())
                        in
                              addFlowEdges rest
                        end

            val _ = addFlowEdges nodeInstrs

        in
            (FGRAPH {
                control = g,
                def = defTable,
                use = useTable,
                ismove = ismoveTable
             },
             nodes)
        end
end
