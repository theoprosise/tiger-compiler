structure Main = 
struct 
    fun main (filename : string) : unit =
        let 
            val ast = Parse.parse filename
            val _ = Semant.transProg ast
        in
            ()
        end
end