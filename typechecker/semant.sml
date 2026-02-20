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

(* transVar : venv * tenv * Absyn.var -> expty
transExp : venv * tenv * Absyn.exp -> expty
transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
transTy:          tenv * Absyn.ty -> Types.ty
 *)

fun actual_ty ty = 
    let 
        fun search (t, seen) =
            case t of 
                TY.NAME(sym,r) =>
                if List.exists (fn s => s = sym) seen then
                    TY.BOTTOM
                else 
                    (case !r of SOME t' => search (t', sym :: seen)
                    | NONE => TY.BOTTOM)
                | _ => t
    in
        search(ty, [])
    end

fun sameType (t1,t2) =
    let 
        val a = actual_ty t1
        val b = actual_ty t2
    in 
        case (a,b) of
        (TY.INT, TY.INT) => true
        | (TY.STRING, TY.STRING) => true
        | (TY.UNIT, TY.UNIT) => true
        | (TY.NIL, TY.NIL) => true
        | (TY.BOTTOM, _) => true
        | (_, TY.BOTTOM) => true
        | (TY.RECORD(_, u1), TY.RECORD(_, u2)) => u1 = u2
        | (TY.ARRAY(_, u1), TY.ARRAY(_, u2)) => u1 = u2
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
    (ErrorMsg.error pos msg; {exp=(), ty=TY.BOTTOM})

fun requireInt (t, pos) =
    case actual_ty t of
        TY.INT => ()
        | TY.BOTTOM => ()
        | _ => ErrorMsg.error pos "expected int"

fun requireAssignable (dst, src, pos) =
    if assignable(dst, src) then ()
    else ErrorMsg.error pos "type mismatch"

fun transExp (venv, tenv, e) : expty =
  let
    fun trexp e : expty =
      case e of
          A.VarExp v => trvar v
        | A.NilExp => {exp=(), ty=TY.NIL}
        | A.IntExp _ => {exp=(), ty=TY.INT}
        | A.StringExp _ => {exp=(), ty=TY.STRING}

        | A.OpExp{left, oper=A.PlusOp, right, pos} =>
            let
              val {ty=tL, ...} = trexp left
              val {ty=tR, ...} = trexp right
              val _ = requireInt(tL, pos)
              val _ = requireInt(tR, pos)
            in
              {exp=(), ty=TY.INT}
            end
        | _ => typeError(0, "expression form not implemented yet")

    and trvar v : expty =
      case v of
          A.SimpleVar(id, pos) =>
            (case S.look(venv, id) of
                SOME (E.VarEntry {ty}) => {exp=(), ty=actual_ty ty}
              | SOME _ => typeError(pos, "not a variable: " ^ S.name id)
              | NONE => typeError(pos, "undefined variable " ^ S.name id))

        | A.FieldVar(base, field, pos) =>
            let
              val {ty=tBase, ...} = trvar base
            in
              case actual_ty tBase of
                TY.RECORD(fields, _) =>
                  (case List.find (fn (s, _) => s = field) fields of
                      SOME (_, fty) => {exp=(), ty=actual_ty fty}
                    | NONE => typeError(pos, "no such field " ^ S.name field))
              | TY.BOTTOM => {exp=(), ty=TY.BOTTOM}
              | _ => typeError(pos, "field access on non-record")
            end

        | A.SubscriptVar(base, idx, pos) =>
            let
              val {ty=tBase, ...} = trvar base
              val {ty=tIdx, ...}  = trexp idx
              val _ = requireInt(tIdx, pos)
            in
              case actual_ty tBase of
                TY.ARRAY(elemTy, _) => {exp=(), ty=actual_ty elemTy}
              | TY.BOTTOM => {exp=(), ty=TY.BOTTOM}
              | _ => typeError(pos, "subscript on non-array")
            end
  in
    trexp e
  end

fun transProg ast = 
    let
        val _ = transExp(E.base_venv, E.base_tenv, ast)
    in
    ()
    end


end