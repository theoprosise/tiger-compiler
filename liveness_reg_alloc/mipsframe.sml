structure MipsFrame : FRAME =
struct
  datatype access =
      InFrame of int
    | InReg of Temp.temp

  type frame =
    { name    : Temp.label
    , formals : access list
    , locals  : int ref
    , outgoing : int ref
    }

  val wordSize = 4

    type register = string

  val ZERO = Temp.newtemp()
  val V0   = Temp.newtemp()
  val A0   = Temp.newtemp()
  val A1   = Temp.newtemp()
  val A2   = Temp.newtemp()
  val A3   = Temp.newtemp()
  val T0   = Temp.newtemp()
  val T1   = Temp.newtemp()
  val T2   = Temp.newtemp()
  val T3   = Temp.newtemp()
  val T4   = Temp.newtemp()
  val T5   = Temp.newtemp()
  val T6   = Temp.newtemp()
  val T7   = Temp.newtemp()
  val T8   = Temp.newtemp()
  val T9   = Temp.newtemp()
  val S0   = Temp.newtemp()
  val S1   = Temp.newtemp()
  val S2   = Temp.newtemp()
  val S3   = Temp.newtemp()
  val S4   = Temp.newtemp()
  val S5   = Temp.newtemp()
  val S6   = Temp.newtemp()
  val S7   = Temp.newtemp()
  val SP   = Temp.newtemp()
  val FP   = Temp.newtemp()
  val RA   = Temp.newtemp()

  val RV = V0

  val argregs = [A0, A1, A2, A3]
  val calleesaves = [S0, S1, S2, S3, S4, S5, S6, S7]
  val callersaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
  val specialregs = [ZERO, RV, SP, FP, RA]
  val registers = callersaves @ calleesaves
  val savedRegs = RA :: FP :: calleesaves
  val saveAreaSize = length savedRegs * wordSize
  
  val tempMap =
    let
      val t = Temp.Table.empty
      val t = Temp.Table.enter(t, ZERO, "$zero")
      val t = Temp.Table.enter(t, V0,   "$v0")
      val t = Temp.Table.enter(t, A0,   "$a0")
      val t = Temp.Table.enter(t, A1,   "$a1")
      val t = Temp.Table.enter(t, A2,   "$a2")
      val t = Temp.Table.enter(t, A3,   "$a3")
      val t = Temp.Table.enter(t, T0,   "$t0")
      val t = Temp.Table.enter(t, T1,   "$t1")
      val t = Temp.Table.enter(t, T2,   "$t2")
      val t = Temp.Table.enter(t, T3,   "$t3")
      val t = Temp.Table.enter(t, T4,   "$t4")
      val t = Temp.Table.enter(t, T5,   "$t5")
      val t = Temp.Table.enter(t, T6,   "$t6")
      val t = Temp.Table.enter(t, T7,   "$t7")
      val t = Temp.Table.enter(t, T8,   "$t8")
      val t = Temp.Table.enter(t, T9,   "$t9")
      val t = Temp.Table.enter(t, S0,   "$s0")
      val t = Temp.Table.enter(t, S1,   "$s1")
      val t = Temp.Table.enter(t, S2,   "$s2")
      val t = Temp.Table.enter(t, S3,   "$s3")
      val t = Temp.Table.enter(t, S4,   "$s4")
      val t = Temp.Table.enter(t, S5,   "$s5")
      val t = Temp.Table.enter(t, S6,   "$s6")
      val t = Temp.Table.enter(t, S7,   "$s7")
      val t = Temp.Table.enter(t, SP,   "$sp")
      val t = Temp.Table.enter(t, FP,   "$fp")
      val t = Temp.Table.enter(t, RA,   "$ra")
    in
      t
    end

  fun regName r =
    case Temp.Table.look (tempMap, r) of
      SOME name => name
    | NONE => ErrorMsg.impossible "missing register name"
  
  fun name ({name, ...} : frame) = name
  fun formals ({formals, ...} : frame) = formals

  fun allocFormal (offset : int, escapes : bool) : access =
    if escapes then InFrame offset else InReg (Temp.newtemp())

  fun newFrame {name, formals} =
    let
      fun build ([], _, acc) = rev acc
        | build (esc :: rest, offset, acc) =
            build (rest, offset + wordSize, allocFormal(offset, esc) :: acc)
    in
      { name = name
      , formals = build (formals, 0, [])
      , locals = ref 0
      , outgoing = ref 0
      }
    end

  fun allocLocal ({locals, ...} : frame) escapes =
    if escapes then
      let
        val i = !locals + 1
        val _ = locals := i
      in
        InFrame (~(saveAreaSize + i * wordSize))
      end
    else
      InReg (Temp.newtemp())

  fun reserveOutgoing ({outgoing, ...} : frame) count =
    if count > !outgoing then
      outgoing := count
    else
      ()

  fun exp access framePtr =
    case access of
      InFrame k =>
        Tree.MEM (Tree.BINOP (Tree.PLUS, framePtr, Tree.CONST k))
    | InReg t =>
        Tree.TEMP t

  fun string (lab, s) =
    let
      fun byteLines [] = ""
        | byteLines (c :: cs) =
            ".byte " ^ Int.toString (Char.ord c) ^ "\n" ^ byteLines cs
    in
      ".align 2\n"
      ^ Symbol.name lab ^ ":\n"
      ^ ".word " ^ Int.toString (String.size s) ^ "\n"
      ^ byteLines (String.explode s)
      ^ ".align 2\n"
    end

  fun seq [] = Tree.EXP (Tree.CONST 0)
    | seq [s] = s
    | seq (s :: ss) = Tree.SEQ (s, seq ss)

  fun procEntryExit1 (frame : frame, body) =
    let
      val formals = formals frame

      fun incomingFormal n =
        if n < length argregs then
          Tree.TEMP (List.nth (argregs, n))
        else
          Tree.MEM
            (Tree.BINOP
              (Tree.PLUS,
               Tree.TEMP FP,
               Tree.CONST (n * wordSize)))

      fun moveFormal (access, n) =
        Tree.MOVE (exp access (Tree.TEMP FP), incomingFormal n)

      val moves = ListPair.map moveFormal (formals, List.tabulate (length formals, fn i => i))
    in
      Tree.SEQ (seq moves, body)
    end

  fun procEntryExit2 (frame, body) =
    body @
    [Assem.OPER
      { assem = ""
      , src = [ZERO, RA, SP, FP] @ calleesaves
      , dst = []
      , jump = SOME []
      }]
  
  fun asmInt i =
    if i < 0 then
      "-" ^ Int.toString (~i)
    else
      Int.toString i

  fun frameSize ({locals, outgoing, ...} : frame) =
    saveAreaSize + (!locals * wordSize) + (!outgoing * wordSize)

  fun procEntryExit3 (frame as {name, ...} : frame, body) =
    let
      val size = frameSize frame
      val raOffset = size - wordSize
      val fpOffset = size - (2 * wordSize)
      val calleeOffsets =
        List.tabulate (length calleesaves, fn i => size - ((i + 3) * wordSize))

      fun saveLine (regName, offset) =
        "sw " ^ regName ^ ", " ^ asmInt offset ^ "($sp)\n"

      fun restoreLine (regName, offset) =
        "lw " ^ regName ^ ", " ^ asmInt offset ^ "($sp)\n"

      val calleeSaveLines =
        String.concat (ListPair.map saveLine (map regName calleesaves, calleeOffsets))

      val calleeRestoreLines =
        String.concat (ListPair.map restoreLine (map regName calleesaves, calleeOffsets))
    in
    { prolog =
        "addiu $sp, $sp, -" ^ asmInt size ^ "\n"
        ^ saveLine ("$ra", raOffset)
        ^ saveLine ("$fp", fpOffset)
        ^ calleeSaveLines
        ^ "addiu $fp, $sp, " ^ asmInt size ^ "\n"
    , body = body
    , epilog =
        "move $sp, $fp\n"
        ^ "addiu $sp, $sp, -" ^ asmInt size ^ "\n"
        ^ restoreLine ("$ra", raOffset)
        ^ restoreLine ("$fp", fpOffset)
        ^ calleeRestoreLines
        ^ "addiu $sp, $sp, " ^ asmInt size ^ "\n"
        ^ "jr $ra\n"
        ^ "nop\n"
    }
    end
    
end
