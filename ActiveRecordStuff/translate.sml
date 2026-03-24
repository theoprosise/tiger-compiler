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

  fun simpleVar ((defLevel, acc), useLevel) =
    let
      val fpExp =
        case defLevel of
          OUTERMOST => T.TEMP F.FP
        | LEVEL {parent, ...} => staticLink (useLevel, parent)
    in
      Ex (F.exp acc fpExp)
    end

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
        frags := PROC {body = unNx body, frame = frame} :: !frags

  fun getResult () = rev (!frags)
end