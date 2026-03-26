structure FindEscape =
struct
  structure A = Absyn
  structure S = Symbol

  type depth = int
  type escEnv = (depth * bool ref) S.table

  fun findEscape (prog : A.exp) =
    let
      fun traverseVar (env : escEnv, d : depth, var : A.var) : unit =
        case var of
          A.SimpleVar (sym, _) =>
            (case S.look (env, sym) of
               SOME (declDepth, escapeRef) =>
                 if d > declDepth then escapeRef := true else ()
             | NONE => ())
        | A.FieldVar (v, _, _) =>
            traverseVar (env, d, v)
        | A.SubscriptVar (v, e, _) =>
            (traverseVar (env, d, v);
             traverseExp (env, d, e))

      and traverseExp (env : escEnv, d : depth, exp : A.exp) : unit =
        case exp of
          A.VarExp v =>
            traverseVar (env, d, v)

        | A.NilExp => ()
        | A.IntExp _ => ()
        | A.StringExp _ => ()

        | A.CallExp {args, ...} =>
            app (fn e => traverseExp (env, d, e)) args

        | A.OpExp {left, right, ...} =>
            (traverseExp (env, d, left);
             traverseExp (env, d, right))

        | A.RecordExp {fields, ...} =>
            app (fn (_, e, _) => traverseExp (env, d, e)) fields

        | A.SeqExp exps =>
            app (fn (e, _) => traverseExp (env, d, e)) exps

        | A.AssignExp {var, exp, ...} =>
            (traverseVar (env, d, var);
             traverseExp (env, d, exp))

        | A.IfExp {test, then', else', ...} =>
            (traverseExp (env, d, test);
             traverseExp (env, d, then');
             case else' of
               NONE => ()
             | SOME e => traverseExp (env, d, e))

        | A.WhileExp {test, body, ...} =>
            (traverseExp (env, d, test);
             traverseExp (env, d, body))

        | A.ForExp {var, escape, lo, hi, body, ...} =>
            let
              val _ = escape := false
              val _ = traverseExp (env, d, lo)
              val _ = traverseExp (env, d, hi)
              val env' = S.enter (env, var, (d, escape))
            in
              traverseExp (env', d, body)
            end

        | A.BreakExp _ => ()

        | A.LetExp {decs, body, ...} =>
            let
              val env' = traverseDecs (env, d, decs)
            in
              traverseExp (env', d, body)
            end

        | A.ArrayExp {size, init, ...} =>
            (traverseExp (env, d, size);
             traverseExp (env, d, init))

      and traverseDecs (env : escEnv, d : depth, decs : A.dec list) : escEnv =
        foldl (fn (dec, envAcc) => traverseDec (envAcc, d, dec)) env decs

      and traverseDec (env : escEnv, d : depth, dec : A.dec) : escEnv =
        case dec of
          A.VarDec {name, escape, init, ...} =>
            let
              val _ = escape := false
              val _ = traverseExp (env, d, init)
            in
              S.enter (env, name, (d, escape))
            end

        | A.TypeDec _ =>
            env

        | A.FunctionDec funs =>
            let
              fun oneFun ({params, body, ...} : A.fundec) =
                let
                  fun addParam ({name, escape, ...} : A.field, envAcc) =
                    (escape := false;
                     S.enter (envAcc, name, (d + 1, escape)))

                  val env' = foldl addParam env params
                in
                  traverseExp (env', d + 1, body)
                end
            in
              (app oneFun funs;
               env)
            end
    in
      traverseExp (S.empty, 0, prog)
    end
end