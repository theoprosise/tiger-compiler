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
  (ErrorMsg.error pos msg; {exp = (), ty = TY.BOTTOM})

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
      | A.IntExp _ => {exp = TR.intExp i, ty = TY.INT}
      | A.StringExp _ => {exp = TR.stringExp s, ty = TY.STRING}

      | A.CallExp {func, args, pos} =>
    (case lookupFun (venv, func, pos) of
       NONE =>
         (*  we already emitted an error *)
         {exp=(), ty=TY.BOTTOM}
     | SOME (formals, result) =>
         let
           val argTys = map (fn a => #ty (trexp (venv, tenv, level, breakLabel)  a)) args

           fun checkArgs ([], []) = ()
             | checkArgs (f::fs, a::as') =
                 (requireAssignable (f, a, pos); checkArgs (fs, as'))
             | checkArgs _ =
                 ErrorMsg.error pos "wrong number of arguments"

           val _ = checkArgs (formals, argTys)
         in
          case lookupFun (...) of
            SOME {level=funLevel, label, formals, result} =>
              let
                val argsE = map (fn a => trexp (venv, tenv, level, breakLabel) a) args
              in
                {exp = TR.callExp{
                        label = label,
                        args = map #exp argsE,
                        funLevel = funLevel,
                        curLevel = level
                      },
                ty = actual_ty result}
              end
         end)

      | A.OpExp {left, oper, right, pos} =>
          let
            val tL = #ty (trexp (venv, tenv, level, breakLabel)  left)
            val tR = #ty (trexp (venv, tenv, level, breakLabel)  right)

            fun bothInt () =
              (requireInt (tL, pos); requireInt (tR, pos); {exp=(), ty=TY.INT})

            fun cmpIntOrString () =
              (case (actual_ty tL, actual_ty tR) of
                 (TY.INT, TY.INT) => {exp=(), ty=TY.INT}
               | (TY.STRING, TY.STRING) => {exp=(), ty=TY.INT}
               | (TY.BOTTOM, _) => {exp=(), ty=TY.INT}
               | (_, TY.BOTTOM) => {exp=(), ty=TY.INT}
               | _ => (ErrorMsg.error pos "invalid comparison"; {exp=(), ty=TY.INT}))

            fun eqOk (t1, t2) =
        let
          val a = actual_ty t1
          val b = actual_ty t2
        in
          case (a,b) of
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

      fun eqNeq () =
        (if eqOk (tL, tR)
         then ()
         else ErrorMsg.error pos "invalid equality comparison";
         {exp=(), ty=TY.INT})

    in
      case oper of
        A.PlusOp => bothInt ()
      | A.MinusOp => bothInt ()
      | A.TimesOp => bothInt ()
      | A.DivideOp => bothInt ()
      | A.LtOp => cmpIntOrString ()
      | A.LeOp => cmpIntOrString ()
      | A.GtOp => cmpIntOrString ()
      | A.GeOp => cmpIntOrString ()
      | A.EqOp => eqNeq ()
      | A.NeqOp => eqNeq ()
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
                val actual = #ty (trexp (venv, tenv, level, breakLabel)  exp)
              in
                if expected = TY.BOTTOM
                then ErrorMsg.error fpos ("unknown field " ^ S.name name)
                else requireAssignable (expected, actual, fpos)
              end

            val _ = checkDups provided
            val _ = checkMissing fieldTys
            val _ = app checkOne fields
          in
            {exp=(), ty=t}
          end
      | TY.BOTTOM => {exp=(), ty=TY.BOTTOM}
            | _ => (ErrorMsg.error pos "record expression of non-record type";
           {exp=(), ty=TY.BOTTOM})
    end

      | A.SeqExp exps =>
          let
            fun lastTy [] = TY.UNIT
              | lastTy [(e,_)] = #ty (trexp (venv, tenv, level, breakLabel)  e)
              | lastTy ((e,_)::rest) = (ignore (trexp (venv, tenv, level, breakLabel)  e); lastTy rest)
          in
            {exp=(), ty=actual_ty (lastTy exps)}
          end

      | A.AssignExp {var, exp, pos} =>
          let
            val _  = checkAssignableLHS (venv, var)
            val tV = #ty (trvar (venv, tenv, level, breakLabel) var)
            val tE = #ty (trexp (venv, tenv, level, breakLabel)  exp)
            val _ = requireAssignable (tV, tE, pos)
          in
            {exp=(), ty=TY.UNIT}
          end

      | A.IfExp {test, then', else', pos} =>
          let
            val tTest = #ty (trexp (venv, tenv, level, breakLabel)  test)
            val _ = requireInt (tTest, pos)
            val tThen = #ty (trexp (venv, tenv, level, breakLabel)  then')
          in
            case else' of
              NONE =>
                (requireUnit (tThen, pos); {exp=(), ty=TY.UNIT})
            | SOME e2 =>
                let
                  val tElse = #ty (trexp (venv, tenv, level, breakLabel)  e2)
                   in
                      if assignable (tThen, tElse) then {exp=(), ty=actual_ty tElse}
                      else if assignable (tElse, tThen) then {exp=(), ty=actual_ty tThen}
                      else (ErrorMsg.error pos "then/else type mismatch"; {exp=(), ty=TY.BOTTOM})
                    end
          end

      | A.WhileExp {test, body, pos} =>
          let
            val done = Temp.newlabel()
            val tTest = #ty (trexp (venv, tenv, level, breakLabel)  test)
            val _ = requireInt (tTest, pos)
            val tBody = #ty (trexp (venv', tenv', level, breakLabel) body)
            val _ = requireUnit (tBody, pos)
          in
            let
              val done = Temp.newlabel()
              val testE = trexp (venv, tenv, level, breakLabel) test
              val bodyE = trexp (venv, tenv, level, SOME done) body
            in
              {exp = TR.whileExp(#exp testE, #exp bodyE, done),
              ty = TY.UNIT}
            end
          end

      | A.ForExp {var, lo, hi, body, pos, ...} =>
          let
            val done = Temp.newlabel()
            val tLo = #ty (trexp (venv, tenv, level, breakLabel)  lo)
            val tHi = #ty (trexp (venv, tenv, level, breakLabel)  hi)
            val _ = requireInt (tLo, pos)
            val _ = requireInt (tHi, pos)

            val venv' = S.enter (venv, var, E.VarEntry{ty=TY.INT, readonly=true})

            (* allow break in the body *)
            val tBody = #ty (trexp (venv', tenv, level, SOME done) body)

            val _ = requireUnit (tBody, pos)
          in
            {exp=(), ty=TY.UNIT}
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
            val {venv=venv', tenv=tenv'} = transDecs (venv, tenv, level, decs)
            val tBody = #ty (trexp (venv', tenv', label, breakLabel) body)
          in
            {exp=(), ty=actual_ty tBody}
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
                  val tInit = #ty (transExp (venv, tenv, level, init))
                  val _ = requireAssignable (elemTy, tInit, pos)
                in
                  {exp=(), ty=tArr}
                end
            | TY.BOTTOM => {exp=(), ty=TY.BOTTOM}
            | _ => (ErrorMsg.error pos "array expression of non-array type";
                    {exp=(), ty=TY.BOTTOM})
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
            val tBase = #ty (trvar (venv, tenv, level, breakLabel) base)
          in
            case actual_ty tBase of
              TY.RECORD (fields, _) =>
                (case List.find (fn (s, _) => s = field) fields of
                   SOME (_, fty) => {exp=(), ty=actual_ty fty}
                 | NONE => typeError (pos, "no such field " ^ S.name field))
            | TY.BOTTOM => {exp=(), ty=TY.BOTTOM}
            | _ => typeError (pos, "field access on non-record")
          end

      | A.SubscriptVar (base, idx, pos) =>
          let
            val tBase = #ty (trvar (venv, tenv, level, breakLabel)  base)
            val tIdx  = #ty (trexp (venv, tenv, level, breakLabel)  idx)
            val _ = requireInt (tIdx, pos)
          in
            case actual_ty tBase of
              TY.ARRAY (elemTy, _) => {exp=(), ty=actual_ty elemTy}
            | TY.BOTTOM => {exp=(), ty=TY.BOTTOM}
            | _ => typeError (pos, "subscript on non-array")
          end

    and transDecs (venv: venv, tenv: tenv, level: TR.level, decs: A.dec list)
        : {venv: venv, tenv: tenv} =
      foldl (fn (d, {venv, tenv}) => transDec (venv, tenv, level, d))
            {venv=venv, tenv=tenv}
            decs

   and transDec (venv: venv, tenv: tenv, level: TR.level, d: A.dec)
    : {venv: venv, tenv: tenv} =
  case d of
    A.VarDec {name, typ, init, pos, ...} =>
      let
        val tInit = #ty (transExp (venv, tenv, level, init))
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
        val access = TR.allocLocal level true
        val venv' = S.enter (venv, name, E.VarEntry {ty = varTy, access = access readonly = false})
      in
        {venv = venv', tenv = tenv}
      end

  | A.TypeDec tds =>
      let
        (* pass 1: enter NAME headers for all types *)
        fun enterHeader ({name, ty, pos}: {name:A.symbol, ty:A.ty, pos:A.pos}, tenvAcc) =
          S.enter (tenvAcc, name, TY.NAME (name, ref NONE))
        val tenv1 = foldl enterHeader tenv tds

        (* pass 2: translate RHS using tenv1, then fill the ref *)
        fun bindBody ({name, ty, pos}: {name:A.symbol, ty:A.ty, pos:A.pos}) =
          let
            val rhs = transTy (tenv1, ty)
            val r =
              case S.look (tenv1, name) of
                SOME (TY.NAME (_, r)) => r
              | _ => raise Fail "TypeDec: missing header"
            val _ = r := SOME rhs
          in
            ()
          end
        val _ = app bindBody tds

        fun checkOne ({name, ty, pos}: {name:A.symbol, ty:A.ty, pos:A.pos}) =
          (case actual_ty (lookupTy (tenv1, name, pos)) of
             TY.BOTTOM => ErrorMsg.error pos "illegal type cycle"
           | _ => ())
        val _ = app checkOne tds
      in
        {venv = venv, tenv = tenv1}
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
          in
            S.enter (venvAcc, name, E.FunEntry {formals = formals, result = res})
          end
        val venv1 = foldl enterHeader venv funs

        (* pass 2: check bodies *)
        fun bindParam (p: A.field, pty, v) =
          S.enter (v, #name p, E.VarEntry {ty = pty, readonly = false})

        fun checkOne ({name, params, result, body, pos}: A.fundec) =
            (case lookupFun (venv1, name, pos) of
              NONE => ()
            | SOME (formals, res) =>
                let
                  val _ =
                    if length params = length formals then ()
                    else ErrorMsg.error pos "wrong number of parameters"

                  val venvBody =
                    if length params = length formals then
                      ListPair.foldlEq bindParam venv1 (params, formals)
                    else
                      venv1
                 
                 val label = Temp.newlabel()
                  val funLevel =
                    TR.newLevel {parent = level, name = label, formals = map (fn _ => true) params}
                  
                  val tBody = #ty (transExp (venvBody, tenv, funLevel, body))
                  val _ =
                    case result of
                      NONE => requireUnit (tBody, pos)
                    | SOME _ => requireAssignable (res, tBody, pos)
                in
                  ()
                end)

        val _ = app checkOne funs
      in
        {venv = venv1, tenv = tenv}
      end

  in
    trexp (venv, tenv, level, NONE) e
  end

fun transProg ast =
  let
    val _ = transExp (E.base_venv, E.base_tenv, TR.outermost, ast)
  in
    ()
  end

end