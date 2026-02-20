structure A = Absyn
structure TY = Types
structure E = Env

structure Semant = 
struct 

type venv = E.enventry S.table
type tenv = ty S.table
type expty = {ty:TY.ty}


transVar : venv * tenv * Absyn.var -> expty
transExp : venv * tenv * Absyn.exp -> expty
transDec : venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
transTy:          tenv * Absyn.ty -> Types.ty


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



fun checkInt ({exp,yt}, pos) = ()
fun transExp(venv,tenv) =
    let fun trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) =
            (checkInt(trexp left, pos);
             checkInt(trexp right, pos);
             {exp=(),ty=Types.INT})
      | trexp (A.RecordExp 

and trvar (A.SimpleVar(id,pos)) =
    (case Symbol.look(venv,id)
      of SOME(E.VarEntry{ty}) =>
         {exp=(), ty=actual_ty ty}
       | NONE => (error pos ("undefined variable "
                             ^ S.name id);
                  {exp=(), ty=Types.INT}))
  | trvar (A.FieldVar(v,id,pos)) = ...
in trexp
end

| trexp(A.LetExp{decs,body,pos}) =
  let val {venv=venv',tenv=tenv'} =
          transDecs(venv,tenv,decs)
   in transExp(venv',tenv') body
  end