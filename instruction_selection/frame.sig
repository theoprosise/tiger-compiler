signature FRAME =
sig
  type frame
  type access
  type register = string

  val wordSize : int

  val FP : Temp.temp
  val SP : Temp.temp
  val RA : Temp.temp
  val RV : Temp.temp
  val argregs : Temp.temp list
  val callersaves : Temp.temp list
  val calleesaves : Temp.temp list

  val newFrame : {name: Temp.label, formals: bool list} -> frame
  val name : frame -> Temp.label
  val formals : frame -> access list
  val allocLocal : frame -> bool -> access
  val tempMap : register Temp.Table.table

  val string : Temp.label * string -> string

  val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
  val procEntryExit3 : frame * Assem.instr list ->
      {prolog:string, body:Assem.instr list, epilog:string}
 
  val exp : access -> Tree.exp -> Tree.exp
end
