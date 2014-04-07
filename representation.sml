
structure InternalRepresentation = struct

  datatype value = VInt of int
  	         | VBool of bool
		 | VFun of string * expr
			    
  and expr = EVal of value
	   | EIf of expr * expr * expr
	   | ELet of string * expr * expr
	   | ELetFun of string * string * expr * expr
	   | EIdent of string
	   | EApp of expr * expr
           | EPrimCall2 of (value -> value -> value) * expr * expr

  fun stringOfExpr e = let
    fun $ ss = String.concat ss
    fun $+ ss = String.concatWith "," ss
    fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
    fun strS s = "\""^s^"\""
    fun strV (VInt i) = $ ["VInt ",Int.toString i]
      | strV (VBool true) = "VBool true"
      | strV (VBool false) = "VBool false"
      | strV (VFun (n,e)) = $ ["VFun (", n, ",", strE e, ")"]
    and strE (EVal v) = strCon "EVal" strV [v]
      | strE (EIf (e1,e2,e3)) = strCon "EIf" strE [e1,e2,e3]
      | strE (ELet (n,e1,e2)) = $ ["ELet (",strS n,",",strE e1,",",strE e2,")"]
      | strE (ELetFun (n,p,e1,e2)) = $ ["ELetFun (",strS n,",",
					strS p, ",",
					strE e1,",",strE e2,")"]
      | strE (EIdent n) = $ ["EIdent ", strS n]
      | strE (EApp (e1,e2)) = strCon "EApp" strE [e1,e2]
      | strE (EPrimCall2 (f,e1,e2)) = strCon "EPrimCall2" strE [e1,e2]
  in
    strE e
  end

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VBool true) = "true"
    | stringOfValue (VBool false) = "false"
    | stringOfValue (VFun (n,e)) = 
        String.concat ["<function (", n, ",", stringOfExpr e,")>"]

  fun printValue v = (print (stringOfValue v);
		      print "\n")
		       
end
