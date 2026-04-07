structure Mips : CODEGEN =
struct
  structure T = Tree
  structure A = Assem
  structure Frame = MipsFrame

  val ilist = ref ([] : A.instr list)

  fun emit i = ilist := i :: !ilist

  val numArgRegs = length Frame.argregs

  fun take (0, _) = []
    | take (_, []) = []
    | take (n, x::xs) = x :: take (n-1, xs)

  fun result gen =
    let
      val r = Temp.newtemp()
    in
      gen r;
      r
    end

  fun relopAssem rop =
    case rop of
      T.EQ => "beq "
    | T.NE => "bne "
    | T.LT => "blt "
    | T.GT => "bgt "
    | T.LE => "ble "
    | T.GE => "bge "
    | T.ULT => "bltu "
    | T.ULE => "bleu "
    | T.UGT => "bgtu "
    | T.UGE => "bgeu "

  fun munchStm stm =
    case stm of
      T.LABEL lab =>
        emit (A.LABEL {assem = Symbol.name lab ^ ":\n", lab = lab})

    | T.JUMP(T.NAME lab, _) =>
        emit (A.OPER {assem = "j " ^ Symbol.name lab ^ "\n",
                      dst = [], src = [], jump = SOME [lab]})

    | T.CJUMP(rop, left, right, t, f) =>
        let
          val l = munchExp left
          val r = munchExp right
          val branch = relopAssem rop
        in
          emit (A.OPER {assem = branch ^ "`s0, `s1, `j0\n",
                        dst = [], src = [l, r], jump = SOME [t, f]});
          emit (A.OPER {assem = "j " ^ Symbol.name f ^ "\n",
                        dst = [], src = [], jump = SOME [f]})
        end

    | T.MOVE(T.TEMP t, T.CALL(T.NAME lab, args)) =>
        munchCall (SOME t, lab, args)

    | T.MOVE(T.MEM(T.BINOP(T.PLUS, e, T.CONST k)), src) =>
        let
          val addr = munchExp e
          val value = munchExp src
        in
          emit (A.OPER {assem = "sw `s1, " ^ Int.toString k ^ "(`s0)\n",
                        dst = [], src = [addr, value], jump = NONE})
        end

    | T.MOVE(T.MEM e, src) =>
        let
          val addr = munchExp e
          val value = munchExp src
        in
          emit (A.OPER {assem = "sw `s1, 0(`s0)\n",
                        dst = [], src = [addr, value], jump = NONE})
        end

    | T.MOVE(T.TEMP t, e) =>
        let val src = munchExp e
        in
          emit (A.MOVE {assem = "move `d0, `s0\n", dst = t, src = src})
        end

    | T.EXP(T.CALL(T.NAME lab, args)) =>
        munchCall (NONE, lab, args)

    | T.EXP(T.CONST _) =>
        ()

    | T.EXP e =>
        ignore (munchExp e)

    | _ =>
        ErrorMsg.impossible "unhandled stm in munchStm"

  and munchExp exp =
    case exp of
      T.CONST i =>
        result (fn r =>
          emit (A.OPER {assem = "li `d0, " ^ Int.toString i ^ "\n",
                        dst = [r], src = [], jump = NONE}))

    | T.TEMP t => t

    | T.NAME lab =>
        result (fn r =>
          emit (A.OPER {assem = "la `d0, " ^ Symbol.name lab ^ "\n",
                        dst = [r], src = [], jump = NONE}))

    | T.CALL(T.NAME lab, args) =>
        let
          val dst = Temp.newtemp()
          val _ = munchCall (SOME dst, lab, args)
        in
          dst
        end

    | T.ESEQ(stm, e) =>
        let
          val _ = munchStm stm
        in
          munchExp e
        end

    | T.BINOP(T.PLUS, a, T.CONST i) =>
        let val ra = munchExp a
        in
          result (fn r =>
            emit (A.OPER {assem = "addi `d0, `s0, " ^ Int.toString i ^ "\n",
                          dst = [r], src = [ra], jump = NONE}))
        end

    | T.BINOP(T.PLUS, a, b) =>
        let val ra = munchExp a
            val rb = munchExp b
        in
          result (fn r =>
            emit (A.OPER {assem = "add `d0, `s0, `s1\n",
                          dst = [r], src = [ra, rb], jump = NONE}))
        end

    | T.BINOP(T.MINUS, a, b) =>
        let val ra = munchExp a
            val rb = munchExp b
        in
          result (fn r =>
            emit (A.OPER {assem = "sub `d0, `s0, `s1\n",
                          dst = [r], src = [ra, rb], jump = NONE}))
        end

    | T.BINOP(T.MUL, a, b) =>
        let val ra = munchExp a
            val rb = munchExp b
        in
          result (fn r =>
            emit (A.OPER {assem = "mul `d0, `s0, `s1\n",
                          dst = [r], src = [ra, rb], jump = NONE}))
        end

    | T.BINOP(T.DIV, a, b) =>
        let val ra = munchExp a
            val rb = munchExp b
        in
          result (fn r =>
            emit (A.OPER {assem = "div `d0, `s0, `s1\n",
                          dst = [r], src = [ra, rb], jump = NONE}))
        end

    | T.MEM(T.BINOP(T.PLUS, e, T.CONST k)) =>
        let val re = munchExp e
        in
          result (fn r =>
            emit (A.OPER {assem = "lw `d0, " ^ Int.toString k ^ "(`s0)\n",
                          dst = [r], src = [re], jump = NONE}))
        end

    | T.MEM e =>
        let val re = munchExp e
        in
          result (fn r =>
            emit (A.OPER {assem = "lw `d0, 0(`s0)\n",
                          dst = [r], src = [re], jump = NONE}))
        end

    | _ =>
        ErrorMsg.impossible "unhandled exp in munchExp"

  and munchArgs (_, [], _) = ()

    | munchArgs (argreg :: regs, arg :: args, n) =
        let
          val t = munchExp arg
        in
          emit (A.MOVE {assem = "move `d0, `s0\n", dst = argreg, src = t});
          munchArgs (regs, args, n + 1)
        end

    | munchArgs ([], arg :: args, n) =
        let
          val t = munchExp arg
          val offset = (n - numArgRegs) * Frame.wordSize
        in
          emit (A.OPER {assem = "sw `s0, " ^ Int.toString offset ^ "(`s1)\n",
                        dst = [],
                        src = [t, Frame.SP],
                        jump = NONE});
          munchArgs ([], args, n + 1)
        end

  and munchCall (dstOpt, lab, args) =
    let
      val _ = munchArgs (Frame.argregs, args, 0)

      val regArgsUsed =
        take (Int.min(length args, numArgRegs), Frame.argregs)

      val uses =
        if length args > numArgRegs then
          regArgsUsed @ [Frame.SP]
        else
          regArgsUsed

      val defs = Frame.RV :: Frame.RA :: Frame.callersaves

      val _ =
        emit (A.OPER {assem = "jal " ^ Symbol.name lab ^ "\n",
                      dst = defs, src = uses, jump = SOME [lab]})
    in
      case dstOpt of
        NONE => ()
      | SOME t =>
          if t = Frame.RV then
            ()
          else
            emit (A.MOVE {assem = "move `d0, `s0\n", dst = t, src = Frame.RV})
    end

  fun codegen frame stm =
    (ilist := [];
     munchStm stm;
     rev (!ilist))
end
