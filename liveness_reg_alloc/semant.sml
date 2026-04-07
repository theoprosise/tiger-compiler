structure A = Absyn
structure TY = Types
structure E = Env
structure TR = Translate
structure S = Symbol

structure Semant =
struct

type venv = E.enventry S.table
type tenv = TY.ty S.table
type expty = {exp: TR.exp, ty: TY.ty}
type decresult = {venv: venv, tenv: tenv, exps: TR.exp list}

(*
  transVar : venv * tenv * Absyn.var -> expty
  transExp : venv * tenv * Absyn.exp -> expty
  transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  transTy  : tenv * Absyn.ty -> Types.ty
*)

(*  return BOTTOM on illegal/unknown cycles *)
fun actual_ty ty =
  let
    fun search (t, seen) =
      case t of
        TY.NAME (sym, r) =>
          if List.exists (fn s => s = sym) seen then
            TY.BOTTOM
          else
            (case !r of
               SOME t' => search (t', sym :: seen)
             | NONE => TY.BOTTOM)
      | _ => t
  in
    search (ty, [])
  end

fun sameType (t1, t2) =
  let
    val a = actual_ty t1
    val b = actual_ty t2
  in
    case (a, b) of
      (TY.INT, TY.INT) => true
    | (TY.STRING, TY.STRING) => true
    | (TY.UNIT, TY.UNIT) => true
    | (TY.NIL, TY.NIL) => true
    | (TY.BOTTOM, _) => true
    | (_, TY.BOTTOM) => true
    | (TY.RECORD (_, u1), TY.RECORD (_, u2)) => u1 = u2
    | (TY.ARRAY (_, u1), TY.ARRAY (_, u2)) => u1 = u2
    | _ => false
  end

fun assignable (dst, src) =
  let
    val d = actual_ty dst
    val s = actual_ty src
  in
    case (d, s) of
      (_, TY.BOTTOM) => true
    | (TY.BOTTOM, _) => true
    | (TY.RECORD _, TY.NIL) => true
    | _ => sameType (d, s)
  end

fun typeError (pos, msg) =
    (ErrorMsg.error pos msg; {exp = TR.nilExp (), ty = TY.BOTTOM})

fun requireInt (t, pos) =
  case actual_ty t of
    TY.INT => ()
  | TY.BOTTOM => ()
  | _ => ErrorMsg.error pos "expected int"

fun requireUnit (t, pos) =
  case actual_ty t of
    TY.UNIT => ()
  | TY.BOTTOM => ()
  | _ => ErrorMsg.error pos "expected unit"

fun requireAssignable (dst, src, pos) =
  if assignable (dst, src) then () else ErrorMsg.error pos "type mismatch"

fun lookupTy (tenv, sym, pos) =
  case S.look (tenv, sym) of
    SOME ty => ty
  | NONE => (ErrorMsg.error pos ("undefined type " ^ S.name sym); TY.BOTTOM)

fun lookupVarTy (venv, sym, pos) =
  case S.look (venv, sym) of
    SOME (E.VarEntry {ty, ...}) => ty
  | SOME _ => (ErrorMsg.error pos ("not a variable: " ^ S.name sym); TY.BOTTOM)
  | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name sym); TY.BOTTOM)

fun lookupFun (venv, sym, pos) =
  case S.look (venv, sym) of
    SOME (E.FunEntry {level, label, formals, result}) =>
      SOME {level=level, label=label, formals=formals, result=result}
  | SOME _ => (ErrorMsg.error pos ("not a function: " ^ S.name sym); NONE)
  | NONE => (ErrorMsg.error pos ("undefined function " ^ S.name sym); NONE)

(* Check for var being used *)
fun checkAssignableLHS (venv, v) =
  case v of
    A.SimpleVar (id, pos) =>
      (case S.look (venv, id) of
         SOME (E.VarEntry {readonly=true, ...}) =>
           ErrorMsg.error pos "loop variable can't be assigned"
       | _ => ())
  | _ => ()

(* Translate Absyn.ty to Types.ty (used by TypeDec pass2) *)
fun transTy (tenv: tenv, t: A.ty) : TY.ty =
  case t of
    A.NameTy (sym, pos) => lookupTy (tenv, sym, pos)
  | A.RecordTy fields =>
      let
        fun one ({name, typ, pos, ...}: A.field) =
          (name, lookupTy (tenv, typ, pos))
      in
        TY.RECORD (map one fields, ref ())
      end
  | A.ArrayTy (sym, pos) =>
      TY.ARRAY (lookupTy (tenv, sym, pos), ref ())

fun transExp (venv, tenv, level, e) : expty =
  let
    fun trexp (venv: venv, tenv: tenv, level: TR.level, breakLabel: Temp.label option) (e: A.exp) : expty =
      case e of
       A.VarExp v => trvar (venv, tenv, level, breakLabel)  v
      | A.NilExp => {exp = TR.nilExp (), ty = TY.NIL}
      | A.IntExp i => {exp = TR.intExp i, ty = TY.INT}
      | A.StringExp (s,_) => {exp = TR.stringExp s, ty = TY.STRING}

      | A.CallExp {func, args, pos} =>
        (case lookupFun (venv, func, pos) of
          NONE => {exp=TR.nilExp(), ty=TY.BOTTOM}
        | SOME {level=funLevel, label, formals, result} =>
            let
              val argsE = map (fn a => trexp (venv, tenv, level, breakLabel) a) args

              fun checkArgs ([], []) = ()
                | checkArgs (f::fs, a::as') =
                    (requireAssignable (f, a, pos); checkArgs (fs, as'))
                | checkArgs _ =
                    ErrorMsg.error pos "wrong number of arguments"

              val _ = checkArgs (formals, map #ty argsE)
            in
              {exp = TR.callExp{
                      label = label,
                      args = map #exp argsE,
                      funLevel = funLevel,
                      curLevel = level
                    },
                ty = actual_ty result}
            end)

      | A.OpExp {left, oper, right, pos} =>
          let
            val eL = trexp (venv, tenv, level, breakLabel) left
            val eR = trexp (venv, tenv, level, breakLabel) right
            val tL = #ty eL
            val tR = #ty eR

            fun bothInt bop =
              (requireInt (tL, pos);
              requireInt (tR, pos);
              {exp = TR.arithExp (bop, #exp eL, #exp eR), ty = TY.INT})

            fun cmpIntOrString rop =
              (case (actual_ty tL, actual_ty tR) of
                (TY.INT, TY.INT) => {exp = TR.relExp (rop, #exp eL, #exp eR), ty = TY.INT}
              | (TY.STRING, TY.STRING) => {exp = TR.relExp (rop, #exp eL, #exp eR), ty = TY.INT}
              | (TY.BOTTOM, _) => {exp = TR.nilExp (), ty = TY.INT}
              | (_, TY.BOTTOM) => {exp = TR.nilExp (), ty = TY.INT}
              | _ => (ErrorMsg.error pos "invalid comparison"; {exp = TR.nilExp (), ty = TY.INT}))

            fun eqOk (t1, t2) =
              let
                val a = actual_ty t1
                val b = actual_ty t2
              in
                case (a, b) of
                  (TY.INT, TY.INT) => true
                | (TY.STRING, TY.STRING) => true
                | (TY.RECORD(_,u1), TY.RECORD(_,u2)) => u1 = u2
                | (TY.ARRAY(_,u1), TY.ARRAY(_,u2)) => u1 = u2
                | (TY.NIL, TY.NIL) => true
                | (TY.NIL, TY.RECORD _) => true
                | (TY.RECORD _, TY.NIL) => true
                | (TY.BOTTOM, _) => true
                | (_, TY.BOTTOM) => true
                | _ => false
              end

            fun eqNeq rop =
              (if eqOk (tL, tR) then ()
              else ErrorMsg.error pos "invalid equality comparison";
              {exp = TR.relExp (rop, #exp eL, #exp eR), ty = TY.INT})
          in
            case oper of
              A.PlusOp   => bothInt TR.PlusOp
            | A.MinusOp  => bothInt TR.MinusOp
            | A.TimesOp  => bothInt TR.TimesOp
            | A.DivideOp => bothInt TR.DivideOp
            | A.LtOp     => cmpIntOrString TR.LtOp
            | A.LeOp     => cmpIntOrString TR.LeOp
            | A.GtOp     => cmpIntOrString TR.GtOp
            | A.GeOp     => cmpIntOrString TR.GeOp
            | A.EqOp     => eqNeq TR.EqOp
            | A.NeqOp    => eqNeq TR.NeqOp
          end

      | A.RecordExp {fields, typ, pos} =>
          let
            val t = actual_ty (lookupTy (tenv, typ, pos))
          in
            case t of
              TY.RECORD (fieldTys, _) =>
                let
                  fun findFieldTy s =
                    case List.find (fn (n, _) => n = s) fieldTys of
                      SOME (_, ty) => ty
                    | NONE => TY.BOTTOM

                  fun findProvidedExp s =
                    case List.find (fn (n, _, _) => n = s) fields of
                      SOME (_, e, _) => SOME e
                    | NONE => NONE

                  val provided = map (fn (name, _, _) => name) fields

                  fun occurs (x, xs) = List.exists (fn y => y = x) xs

                  fun checkDups [] = ()
                    | checkDups (x::xs) =
                        (if occurs (x, xs)
                         then ErrorMsg.error pos ("duplicate field " ^ S.name x)
                         else ();
                         checkDups xs)

                  fun checkMissing [] = ()
                    | checkMissing ((n, _)::rest) =
                        (if occurs (n, provided)
                         then ()
                         else ErrorMsg.error pos ("missing field " ^ S.name n);
                         checkMissing rest)

                  fun checkOne (name, exp, fpos) =
                    let
                      val expected = findFieldTy name
                      val actual = #ty (trexp (venv, tenv, level, breakLabel) exp)
                    in
                      if expected = TY.BOTTOM
                      then ErrorMsg.error fpos ("unknown field " ^ S.name name)
                      else requireAssignable (expected, actual, fpos)
                    end

                  fun orderedFieldExps [] = []
                    | orderedFieldExps ((name, _)::rest) =
                        (case findProvidedExp name of
                           SOME e => #exp (trexp (venv, tenv, level, breakLabel) e)
                                     :: orderedFieldExps rest
                         | NONE => orderedFieldExps rest)

                  val _ = checkDups provided
                  val _ = checkMissing fieldTys
                  val _ = app checkOne fields
                in
                  {exp = TR.recordExp (orderedFieldExps fieldTys), ty = t}
                end
            | TY.BOTTOM => {exp = TR.nilExp (), ty = TY.BOTTOM}
            | _ =>
                (ErrorMsg.error pos "record expression of non-record type";
                 {exp = TR.nilExp (), ty = TY.BOTTOM})
          end

      | A.SeqExp exps =>
          let
            fun lastTy [] = TY.UNIT
              | lastTy [(e,_)] = #ty (trexp (venv, tenv, level, breakLabel)  e)
              | lastTy ((e,_)::rest) = (ignore (trexp (venv, tenv, level, breakLabel)  e); lastTy rest)
            val exps = map (fn (e,_) => trexp (venv, tenv, level, breakLabel) e) exps

          in
            {exp = TR.seqExp (map #exp exps), ty = actual_ty (case rev exps of [] => TY.UNIT | e::_ => #ty e)}
          end

      | A.AssignExp {var, exp, pos} =>
          let
            val _  = checkAssignableLHS (venv, var)
            val tV = #ty (trvar (venv, tenv, level, breakLabel) var)
            val tE = #ty (trexp (venv, tenv, level, breakLabel)  exp)
            val _ = requireAssignable (tV, tE, pos)
            val v = trvar (venv, tenv, level, breakLabel) var
            val e = trexp (venv, tenv, level, breakLabel) exp
          in
            {exp = TR.assignExp(#exp v, #exp e), ty = TY.UNIT}
          end

      | A.IfExp {test, then', else', pos} =>
          let
            val testE = trexp (venv, tenv, level, breakLabel) test
            val thenE = trexp (venv, tenv, level, breakLabel) then'
            val _ = requireInt (#ty testE, pos)
          in
            case else' of
              NONE =>
                (requireUnit (#ty thenE, pos);
                 {exp = TR.ifExp(#exp testE, #exp thenE, NONE), ty = TY.UNIT})
            | SOME e2 =>
                let
                  val elseE = trexp (venv, tenv, level, breakLabel) e2
                in
                  if assignable (#ty thenE, #ty elseE) then
                    {exp = TR.ifExp(#exp testE, #exp thenE, SOME (#exp elseE)),
                     ty = actual_ty (#ty elseE)}
                  else if assignable (#ty elseE, #ty thenE) then
                    {exp = TR.ifExp(#exp testE, #exp thenE, SOME (#exp elseE)),
                     ty = actual_ty (#ty thenE)}
                  else
                    (ErrorMsg.error pos "then/else type mismatch";
                     {exp = TR.nilExp (), ty = TY.BOTTOM})
                end
          end

      | A.WhileExp {test, body, pos} =>
        let
          val done = Temp.newlabel()
          val testE = trexp (venv, tenv, level, breakLabel) test
          val bodyE = trexp (venv, tenv, level, SOME done) body
          val _ = requireInt (#ty testE, pos)
          val _ = requireUnit (#ty bodyE, pos)
        in
          {exp = TR.whileExp(#exp testE, #exp bodyE, done),
          ty = TY.UNIT}
        end

      | A.ForExp {var, lo, hi, body, pos, ...} =>
          let
            val done = Temp.newlabel()
            val loE = trexp (venv, tenv, level, breakLabel) lo
            val hiE = trexp (venv, tenv, level, breakLabel) hi
            val _ = requireInt (#ty loE, pos)
            val _ = requireInt (#ty hiE, pos)

            val iAccess = TR.allocLocal level true
            val venv' =
              S.enter (venv, var,
                E.VarEntry {ty = TY.INT, access = iAccess, readonly = true})

            val bodyE = trexp (venv', tenv, level, SOME done) body
            val _ = requireUnit (#ty bodyE, pos)
          in
            {exp =
               TR.forExp
                 {var = TR.simpleVar(iAccess, level),
                  lo = #exp loE,
                  hi = #exp hiE,
                  body = #exp bodyE,
                  done = done},
             ty = TY.UNIT}
          end

      | A.BreakExp pos =>
            (case breakLabel of
                SOME done =>
                  {exp = TR.breakExp done, ty = TY.UNIT}
              | NONE =>
                  (ErrorMsg.error pos "break not inside loop";
                    {exp = TR.nilExp (), ty = TY.UNIT}))

      | A.LetExp {decs, body, pos} =>
        let
          val {venv = venv', tenv = tenv', exps = decExps} =
            transDecs (venv, tenv, level, decs)
          val bodyE = trexp (venv', tenv', level, breakLabel) body
        in
          {exp = TR.seqExp (decExps @ [#exp bodyE]), ty = actual_ty (#ty bodyE)}
        end

      | A.ArrayExp {typ, size, init, pos} =>
          let
            val tArr = actual_ty (lookupTy (tenv, typ, pos))
            val tSize = #ty (trexp (venv, tenv, level, breakLabel)  size)
            val _ = requireInt (tSize, pos)
          in
            case tArr of
              TY.ARRAY (elemTy, _) =>
                let
                  val sizeE = trexp (venv, tenv, level, breakLabel) size
                  val initE = trexp (venv, tenv, level, breakLabel) init
                  val _ = requireAssignable (elemTy, #ty initE, pos)
                in
                  {exp = TR.arrayExp(#exp sizeE, #exp initE), ty = tArr}
                end
            | TY.BOTTOM => {exp = TR.nilExp (), ty=TY.BOTTOM}
            | _ => (ErrorMsg.error pos "array expression of non-array type";
                    {exp = TR.nilExp (), ty=TY.BOTTOM})
          end

    and trvar (venv: venv, tenv: tenv, level: TR.level, breakLabel: Temp.label option) (v: A.var) : expty =
      case v of
        A.SimpleVar (id, pos) =>
          (case S.look (venv, id) of
            SOME (E.VarEntry {ty, access, ...}) =>
              {exp = TR.simpleVar(access, level), ty = actual_ty ty}
              | _ => typeError (pos, "undefined variable"))

      | A.FieldVar (base, field, pos) =>
          let
            val baseE = trvar (venv, tenv, level, breakLabel) base
            val tBase = #ty baseE

            fun findField ([], _) = NONE
              | findField ((s, ty)::rest, i) =
                  if s = field then SOME(i, ty)
                  else findField(rest, i + 1)
          in
            case actual_ty tBase of
              TY.RECORD (fields, _) =>
                (case findField(fields, 0) of
                   SOME (i, fty) =>
                     {exp = TR.fieldVar(#exp baseE, i), ty = actual_ty fty}
                 | NONE => typeError (pos, "no such field " ^ S.name field))
            | TY.BOTTOM => {exp = TR.nilExp (), ty = TY.BOTTOM}
            | _ => typeError (pos, "field access on non-record")
          end

      | A.SubscriptVar (base, idx, pos) =>
          let
            val baseE = trvar (venv, tenv, level, breakLabel) base
            val idxE  = trexp (venv, tenv, level, breakLabel) idx
            val _ = requireInt (#ty idxE, pos)
          in
            case actual_ty (#ty baseE) of
              TY.ARRAY (elemTy, _) =>
                {exp = TR.subscriptVar(#exp baseE, #exp idxE), ty = actual_ty elemTy}
            | TY.BOTTOM => {exp = TR.nilExp (), ty = TY.BOTTOM}
            | _ => typeError (pos, "subscript on non-array")
          end

    and transDecs (venv: venv, tenv: tenv, level: TR.level, decs: A.dec list)
        : decresult =
      foldl
        (fn (d, {venv, tenv, exps}) =>
            let
              val {venv = venv', tenv = tenv', exps = exps'} =
                transDec (venv, tenv, level, d)
            in
              {venv = venv', tenv = tenv', exps = exps @ exps'}
            end)
        {venv = venv, tenv = tenv, exps = []}
        decs

    and transDec (venv: venv, tenv: tenv, level: TR.level, d: A.dec)
    : decresult =
  case d of
    A.VarDec {name, typ, init, pos, escape} =>
      let
        val initE = trexp (venv, tenv, level, NONE) init
        val tInit = #ty initE
        val varTy =
          (case typ of
             NONE =>
               (case actual_ty tInit of
                  TY.NIL => (ErrorMsg.error pos "cannot infer type from nil"; TY.BOTTOM)
                | t => t)
           | SOME (tySym, tyPos) =>
               let
                 val tDecl = lookupTy (tenv, tySym, tyPos)
                 val _ = requireAssignable (tDecl, tInit, pos)
               in
                 actual_ty tDecl
               end)
        val access = TR.allocLocal level (!escape)
        val venv' =
          S.enter (venv, name,
            E.VarEntry {ty = varTy, access = access, readonly = false})

        val initMove =
          TR.assignExp(TR.simpleVar(access, level), #exp initE)
      in
        {venv = venv', tenv = tenv, exps = [initMove]}
      end

 | A.TypeDec tds =>
    let
      fun enterHeader ({name, ty, pos}, tenvAcc) =
        S.enter (tenvAcc, name, TY.NAME (name, ref NONE))

      val tenv1 = foldl enterHeader tenv tds

      fun bindBody ({name, ty, pos}) =
        let
          val rhs = transTy (tenv1, ty)
          val r =
            case S.look (tenv1, name) of
              SOME (TY.NAME (_, r)) => r
            | _ => raise Fail "TypeDec: missing header"
        in
          r := SOME rhs
        end

      val _ = app bindBody tds
    in
      {venv = venv, tenv = tenv1, exps = []}
    end

  | A.FunctionDec funs =>
      let
        fun paramTy ({typ, pos, ...}: A.field) = lookupTy (tenv, typ, pos)
        fun resultTy NONE = TY.UNIT
          | resultTy (SOME (sym, pos)) = lookupTy (tenv, sym, pos)

        (* pass 1: enter headers for all functions (supports recursion) *)
        fun enterHeader ({name, params, result, body, pos}: A.fundec, venvAcc) =
          let
            val formals = map paramTy params
            val res = resultTy result
            val label = Temp.newlabel()
            val escapes = map (fn {escape, ...} => !escape) params
            val funLevel = TR.newLevel {parent = level, name = label, formals = escapes}
          in
            S.enter (venvAcc, name, E.FunEntry {level = funLevel, label = label, formals = formals, result = res})
          end
        val venv1 = foldl enterHeader venv funs

        (* pass 2: check bodies *)
          fun checkOne ({name, params, result, body, pos}: A.fundec) =
            (case lookupFun (venv1, name, pos) of
              NONE => ()
            | SOME {level = funLevel, label, formals, result = res} =>
                let
                  val _ =
                    if length params = length formals then ()
                    else ErrorMsg.error pos "wrong number of parameters"

                  val formalAccesses = TR.formals funLevel
                  fun bindParams ([], [], [], v) = v
                  | bindParams ((param : A.field)::ps, t::ts, acc::accs, v) =
                      let
                        val {name, typ, pos, escape} = param
                      in
                        bindParams (ps, ts, accs,
                          S.enter (v, name,
                            E.VarEntry {ty = t, access = acc, readonly = false}))
                      end
                  | bindParams _ = venv1

                  val venvBody =
                    if length params = length formals andalso length params = length formalAccesses then
                      bindParams (params, formals, formalAccesses, venv1)
                    else
                      venv1

                  val bodyE = transExp (venvBody, tenv, funLevel, body)

                  val _ =
                    case result of
                      NONE => requireUnit (#ty bodyE, pos)
                    | SOME _ => requireAssignable (res, #ty bodyE, pos)

                  val _ = TR.procEntryExit (funLevel, #exp bodyE)
                in
                  ()
                end)

          val _ = app checkOne funs
      in
        {venv = venv1, tenv = tenv, exps = []}
      end

  in
    trexp (venv, tenv, level, NONE) e
  end

fun transProg ast =
  let
    val _ = FindEscape.findEscape ast

    val mainLevel =
      TR.newLevel {parent = TR.outermost, name = Temp.namedlabel "tig_main", formals = []}

    val {exp, ty} =
      transExp (E.base_venv, E.base_tenv, mainLevel, ast)

    val _ = TR.procEntryExit (mainLevel, exp)
  in
    ()
  end

end