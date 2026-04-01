structure Mips : CODEGEN =
struct
  structure T = Tree
  structure A = Assem
  structure Frame = MipsFrame

  val ilist = ref ([] : A.instr list)

  fun emit i = ilist := i :: !ilist

  fun result gen =
    let
      val r = Temp.newtemp()
    in
      gen r;
      r
    end

  fun munchStm stm =
    case stm of
      T.LABEL lab =>
        emit (A.LABEL {assem = Symbol.name lab ^ ":\n", lab = lab})

    | T.JUMP(T.NAME lab, _) =>
        emit (A.OPER {assem = "j " ^ Symbol.name lab ^ "\n",
                      dst = [], src = [], jump = SOME [lab]})

    | T.MOVE(T.TEMP t, e) =>
        let val src = munchExp e
        in
          emit (A.MOVE {assem = "move `d0, `s0\n", dst = t, src = src})
        end

    | T.EXP(T.CALL(T.NAME lab, args)) =>
        munchCall (NONE, lab, args)

    | T.MOVE(T.TEMP t, T.CALL(T.NAME lab, args)) =>
        munchCall (SOME t, lab, args)

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

  and munchArgs ([], [], n) = ()
    | munchArgs (argreg :: regs, arg :: args, n) =
        let val t = munchExp arg
        in
          emit (A.MOVE {assem = "move `d0, `s0\n", dst = argreg, src = t});
          munchArgs (regs, args, n+1)
        end
    | munchArgs (_, _, _) =
        ErrorMsg.impossible "too many args for current simple codegen"

  and munchCall (dstOpt, lab, args) =
    let
      val _ = munchArgs (Frame.argregs, args, 0)
      val defs = Frame.RV :: Frame.callersaves
      val uses = Frame.argregs
      val _ =
        emit (A.OPER {assem = "jal " ^ Symbol.name lab ^ "\n",
                      dst = defs, src = uses, jump = SOME [lab]})
    in
      case dstOpt of
        NONE => ()
      | SOME t => emit (A.MOVE {assem = "move `d0, `s0\n", dst = t, src = Frame.RV})
    end

  fun codegen frame stm =
    (ilist := [];
     munchStm stm;
     rev (!ilist))
end