structure MipsFrame : FRAME =
struct
  datatype access =
      InFrame of int
    | InReg of Temp.temp

  type frame =
    { name    : Temp.label
    , formals : access list
    , locals  : int ref
    }

  val wordSize = 4

  val FP = Temp.newtemp()
  val RV = Temp.newtemp()

  fun name ({name, ...} : frame) = name
  fun formals ({formals, ...} : frame) = formals

  fun allocFormal (offset : int, escapes : bool) : access =
    if escapes then InFrame offset else InReg (Temp.newtemp())

  fun newFrame {name, formals} =
    let
      fun build ([], _, acc) = rev acc
        | build (esc :: rest, offset, acc) =
            build (rest, offset + wordSize, allocFormal(offset, esc) :: acc)
    in
      { name = name
      , formals = build (formals, 0, [])
      , locals = ref 0
      }
    end

  fun allocLocal ({locals, ...} : frame) escapes =
    if escapes then
      let
        val i = !locals + 1
        val _ = locals := i
      in
        InFrame (~i * wordSize)
      end
    else
      InReg (Temp.newtemp())

  fun exp access framePtr =
    case access of
      InFrame k =>
        Tree.MEM (Tree.BINOP (Tree.PLUS, framePtr, Tree.CONST k))
    | InReg t =>
        Tree.TEMP t
end