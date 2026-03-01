structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (unit(Symbol.symbol * ty) list) * unique 
          | NIL
          | INT
          | STRING
          | BOTTOM (* Type of impossibility *)
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT

end

