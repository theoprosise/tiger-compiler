signature ENV = 
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty:ty}
                    | FunEntry of {formals: ty list, result: ty}
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end

structure Env : ENV =
struct
  type access = unit
  type ty = Types.ty

  datatype enventry = VarEntry of {ty:ty}
                    | FunEntry of {formals: ty list, result: ty}

  (* fill in the base_tenv and the base_venv *)
  (* type environment *)
  (* the job of the tenv is to map the symbol int to the Ty.INT and string to Ty.STRING*)
  (* predifined types*)
  val base_tenv = Symbol.empty 
  (* value environment *)
  (* predifined functions aka contains the bindings of the predefined functions in the language these are defined bu the formals and the result of the FunEntry in our enventry datatype*)
  val base_venv =
  let val map : (enventry Symbol.table) = Symbol.empty
    val predifiendFunList = [
      (Symbol.symbol("print"),FunEntry{formals=[ty.STRING],results=ty.UNIT}),
      (Symbol.symbol("flush"),FunEntry{formals=[],result=ty.UNIT}),
      (Symbol.symbol("getchar"),FunEntry{formals=[],result=ty.STRING}),
      (Symbol.symbol("ord"),FunEntry{formals=[ty.STRING],result=ty.INT}),
      (Symbol.symbol("chr"),FunEntry{formals=[ty.INT], result=ty.STRING}),
      (Symbol.symbol("size"),FunEntry{formals=[ty.STRING],result=ty.INT}),
      (Symbol.symbol("substring"),FunEntry{formals=[ty.STRING,ty.INT,ty.INT], result=ty.STRING}),
      (Symbol.symbol("concat"),FunEntry{formals=[ty.STRING,ty.STRING],result=ty.STRING}),
      (Symbol.symbol("not"),FunEntry{formals=[ty.INT],result=ty.INT}),
      (Symbol.symbol("exit"),FunEntry{formals=[ty.INT],result=ty.BOTTOM})
    ]
    fun addToMap((sym,ty),currentMap) = Symbol.enter(currentMap,sym,ty)
  in 
  foldl addToMap map predifiedFunList
  end
end


(* why do we need two different environments? *)
(* we need to keep track of symbol bindings in the syntactic contexts where type identifiers are expected vs when variables are expected *)