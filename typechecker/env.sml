signature ENV = 
sig
    type access
    type ty
    datatype enventry = VarEntry of {ty:ty, readonly: bool}
                    | FunEntry of {formals: ty list, result: ty}
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end

structure Env : ENV =
struct
  type access = unit
  type ty = Types.ty

  datatype enventry = VarEntry of {ty:ty, readonly:bool}
                    | FunEntry of {formals: ty list, result: ty}

  (* fill in the base_tenv and the base_venv *)
  (* type environment *)
  (* the job of the tenv is to map the symbol int to the Ty.INT and string to Ty.STRING*)
  (* predifined types*)
  val base_tenv =
  let val map : (ty Symbol.table) = Symbol.empty 
      val type_List = [(Symbol.symbol("int"),Types.INT),(Symbol.symbol("string"),Types.STRING)]
      fun addToMap((sym,ty),currentMap) = Symbol.enter(currentMap,sym,ty)
  in 
    foldl addToMap map type_List
  end
  (* value environment *)
  (* predifined functions aka contains the bindings of the predefined functions in the language these are defined bu the formals and the result of the FunEntry in our enventry datatype*)
  val base_venv =
  let val map : (enventry Symbol.table) = Symbol.empty
    val predifinedFunList = [
      (Symbol.symbol("print"),FunEntry{formals=[Types.STRING],result=Types.UNIT}),
      (Symbol.symbol("printi"),FunEntry{formals=[Types.INT],result=Types.UNIT}),
      (Symbol.symbol("flush"),FunEntry{formals=[],result=Types.UNIT}),
      (Symbol.symbol("getchar"),FunEntry{formals=[],result=Types.STRING}),
      (Symbol.symbol("ord"),FunEntry{formals=[Types.STRING],result=Types.INT}),
      (Symbol.symbol("chr"),FunEntry{formals=[Types.INT], result=Types.STRING}),
      (Symbol.symbol("size"),FunEntry{formals=[Types.STRING],result=Types.INT}),
      (Symbol.symbol("substring"),FunEntry{formals=[Types.STRING,Types.INT,Types.INT], result=Types.STRING}),
      (Symbol.symbol("concat"),FunEntry{formals=[Types.STRING,Types.STRING],result=Types.STRING}),
      (Symbol.symbol("not"),FunEntry{formals=[Types.INT],result=Types.INT}),
      (Symbol.symbol("exit"),FunEntry{formals=[Types.INT],result=Types.BOTTOM})
    ]
    fun addToMap((sym,ty),currentMap) = Symbol.enter(currentMap,sym,ty)
  in 
  foldl addToMap map predifinedFunList
  end
end


(* why do we need two different environments? *)
(* we need to keep track of symbol bindings in the syntactic contexts where type identifiers are expected vs when variables are expected *)