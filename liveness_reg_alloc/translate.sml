structure Translate =
struct
  structure F = MipsFrame
  structure T = Tree

  datatype exp =
      Ex of T.exp
    | Nx of T.stm
    | Cx of (Temp.label * Temp.label -> T.stm)

  datatype level =
      OUTERMOST
    | LEVEL of { parent : level
               , frame  : F.frame
               , id     : unit ref
               }

  type access = level * F.access

  datatype frag =
      PROC of { body : T.stm, frame : F.frame }
    | STRING of Temp.label * string

  val frags : frag list ref = ref []

  val outermost = OUTERMOST

  fun eqLevel (OUTERMOST, OUTERMOST) = true
    | eqLevel (LEVEL {id=a, ...}, LEVEL {id=b, ...}) = a = b
    | eqLevel _ = false

  fun newLevel {parent, name, formals} =
    let
      val frame = F.newFrame {name = name, formals = true :: formals}
    in
      LEVEL {parent = parent, frame = frame, id = ref ()}
    end

  fun formals OUTERMOST = []
    | formals (lev as LEVEL {frame, ...}) =
        let
          val fs =
            case F.formals frame of
              [] => []
            | _ :: rest => rest
        in
          map (fn a => (lev, a)) fs
        end

  fun allocLocal OUTERMOST _ =
        raise Fail "allocLocal: cannot allocate local in outermost level"
    | allocLocal (lev as LEVEL {frame, ...}) escapes =
        (lev, F.allocLocal frame escapes)

  fun unEx (Ex e) = e
    | unEx (Nx s) = T.ESEQ (s, T.CONST 0)
    | unEx (Cx gen) =
        let
          val r = Temp.newtemp()
          val t = Temp.newlabel()
          val f = Temp.newlabel()
        in
          T.ESEQ
            (T.SEQ
              (T.MOVE (T.TEMP r, T.CONST 1),
               T.SEQ
                 (gen (t, f),
                  T.SEQ
                    (T.LABEL f,
                     T.SEQ
                       (T.MOVE (T.TEMP r, T.CONST 0),
                        T.LABEL t)))),
             T.TEMP r)
        end

  fun unNx (Nx s) = s
    | unNx (Ex e) = T.EXP e
    | unNx (Cx gen) =
        let
          val t = Temp.newlabel()
          val f = Temp.newlabel()
        in
          T.SEQ (gen (t, f), T.SEQ (T.LABEL t, T.LABEL f))
        end

  fun unCx (Cx g) = g
    | unCx (Ex e) = (fn (t, f) => T.CJUMP (T.NE, e, T.CONST 0, t, f))
    | unCx (Nx _) = raise Fail "unCx on Nx"

  fun nilExp () = Ex (T.CONST 0)

  fun intExp i = Ex (T.CONST i)

  fun stringExp s =
    let
      val lab = Temp.newlabel()
      val _ = frags := STRING (lab, s) :: !frags
    in
      Ex (T.NAME lab)
    end

  fun staticLink (useLevel, defParent) =
    let
      fun climb (OUTERMOST, _) = T.TEMP F.FP
        | climb (cur as LEVEL {parent, frame, ...}, targetParent) =
            if eqLevel (cur, targetParent) then
              T.TEMP F.FP
            else
              let
                val slAccess =
                  case F.formals frame of
                    sl :: _ => sl
                  | [] => raise Fail "staticLink: frame missing static link"
              in
                F.exp slAccess (climb (parent, targetParent))
              end
    in
      climb (useLevel, defParent)
    end

  fun levelEq (OUTERMOST, OUTERMOST) = true
    | levelEq (LEVEL {id=a, ...}, LEVEL {id=b, ...}) = (a = b)
    | levelEq _ = false

 
  fun fpAtLevel (useLevel, defLevel) =
    let
        fun climb (curLevel, fp) =
        if levelEq(curLevel, defLevel) then
            fp
        else
            case curLevel of
            OUTERMOST => raise Fail "simpleVar: bad nesting"
            | LEVEL {parent, frame, ...} =>
                let
                val slAcc = hd (F.formals frame)
                val parentFP = F.exp slAcc fp
                in
                climb(parent, parentFP)
                end
    in
        climb(useLevel, T.TEMP F.FP)
    end
  
  fun simpleVar ((defLevel, acc), useLevel) =
    Ex (F.exp acc (fpAtLevel(useLevel, defLevel)))

  fun fieldVar (base, fieldIndex) =
    let
      val offset = fieldIndex * F.wordSize
    in
      Ex (T.MEM (T.BINOP (T.PLUS, unEx base, T.CONST offset)))
    end

  fun subscriptVar (base, index) =
    Ex (T.MEM
          (T.BINOP
             (T.PLUS,
              unEx base,
              T.BINOP (T.MUL, unEx index, T.CONST F.wordSize))))

  fun assignExp (lhs, rhs) =
    Nx (T.MOVE (unEx lhs, unEx rhs))

  fun seqExp [] = Ex (T.CONST 0)
    | seqExp [e] = e
    | seqExp (e :: es) = Ex (T.ESEQ (unNx e, unEx (seqExp es)))

  datatype binop =
      PlusOp | MinusOp | TimesOp | DivideOp

  datatype relop =
      EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

  fun arithExp (oper, left, right) =
    let
      val bop =
        case oper of
          PlusOp   => T.PLUS
        | MinusOp  => T.MINUS
        | TimesOp  => T.MUL
        | DivideOp => T.DIV
    in
      Ex (T.BINOP (bop, unEx left, unEx right))
    end

  fun arrayExp (size, init) =
    Ex (T.CALL(T.NAME(Temp.namedlabel "initArray"), [unEx size, unEx init]))
  
  fun ifExp (test, thenE, NONE) =
    let
        val t = Temp.newlabel()
        val f = Temp.newlabel()
    in
        Nx (
        T.SEQ(unCx test (t, f),
            T.SEQ(T.LABEL t,
            T.SEQ(unNx thenE,
                T.LABEL f))))
    end
    | ifExp (test, thenE, SOME elseE) =
    let
        val r = Temp.newtemp()
        val t = Temp.newlabel()
        val f = Temp.newlabel()
        val join = Temp.newlabel()
    in
        Ex (
        T.ESEQ(
            T.SEQ(unCx test (t, f),
            T.SEQ(T.LABEL t,
                T.SEQ(T.MOVE(T.TEMP r, unEx thenE),
                T.SEQ(T.JUMP(T.NAME join, [join]),
                    T.SEQ(T.LABEL f,
                    T.SEQ(T.MOVE(T.TEMP r, unEx elseE),
                        T.LABEL join)))))),
            T.TEMP r))
    end

    fun forExp {var, lo, hi, body, done} =
        let
        val limit = Temp.newtemp()
        val testLab = Temp.newlabel()
        val bodyLab = Temp.newlabel()

        val varExp = unEx var
        in
        Nx (
            T.SEQ(
            T.MOVE(varExp, unEx lo),
            T.SEQ(
                T.MOVE(T.TEMP limit, unEx hi),
                T.SEQ(
                T.LABEL testLab,
                T.SEQ(
                    T.CJUMP(T.LE, varExp, T.TEMP limit, bodyLab, done),
                    T.SEQ(
                    T.LABEL bodyLab,
                    T.SEQ(
                        unNx body,
                        T.SEQ(
                        T.MOVE(varExp, T.BINOP(T.PLUS, varExp, T.CONST 1)),
                        T.SEQ(
                            T.JUMP(T.NAME testLab, [testLab]),
                            T.LABEL done
                        )
                        )
                    )
                    )
                )
                )
            )
            )
        )
        end

    fun recordExp fields =
        let
        val r = Temp.newtemp()
        val n = length fields

        fun buildStores ([], _) = T.EXP (T.CONST 0)
            | buildStores (e::es, i) =
                T.SEQ(
                T.MOVE(
                    T.MEM(
                    T.BINOP(T.PLUS, T.TEMP r, T.CONST(i * F.wordSize))
                    ),
                    unEx e
                ),
                buildStores(es, i + 1)
                )
        in
        Ex (
            T.ESEQ(
            T.SEQ(
                T.MOVE(
                T.TEMP r,
                T.CALL(
                    T.NAME(Temp.namedlabel "malloc"),
                    [T.CONST(n * F.wordSize)]
                )
                ),
                buildStores(fields, 0)
            ),
            T.TEMP r
            )
        )
        end

  fun relExp (oper, left, right) =
    let
      val rop =
        case oper of
          EqOp  => T.EQ
        | NeqOp => T.NE
        | LtOp  => T.LT
        | LeOp  => T.LE
        | GtOp  => T.GT
        | GeOp  => T.GE
    in
      Cx (fn (t, f) => T.CJUMP (rop, unEx left, unEx right, t, f))
    end

  fun callExp {label, args, funLevel, curLevel} =
    let
      val sl =
        case funLevel of
          OUTERMOST => []
        | LEVEL {parent, ...} => [staticLink (curLevel, parent)]
    in
      Ex (T.CALL (T.NAME label, sl @ map unEx args))
    end

  fun breakExp done = Nx (T.JUMP (T.NAME done, [done]))

  fun whileExp (test, body, done) =
    let
      val testLab = Temp.newlabel()
      val bodyLab = Temp.newlabel()
    in
      Nx
        (T.SEQ
          (T.LABEL testLab,
           T.SEQ
             (unCx test (bodyLab, done),
              T.SEQ
                (T.LABEL bodyLab,
                 T.SEQ
                   (unNx body,
                    T.SEQ
                      (T.JUMP (T.NAME testLab, [testLab]),
                       T.LABEL done))))))
    end

  fun procEntryExit (OUTERMOST, _) =
        raise Fail "procEntryExit: OUTERMOST has no frame"
    | procEntryExit (LEVEL {frame, ...}, body) =
        let
            val core =
              case body of
                  Ex e => T.MOVE(T.TEMP F.RV, e)
              | Nx s => s
              | Cx _ => T.MOVE(T.TEMP F.RV, unEx body)
            val stm = F.procEntryExit1 (frame, core)
        in
            frags := PROC {body = T.SEQ (T.LABEL (F.name frame), stm), frame = frame} :: !frags
        end
  fun getResult () = rev (!frags)
  fun resetResult () = frags := []
end
